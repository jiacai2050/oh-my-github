;;; -*- lexical-binding: t -*-

(require 'eww)

(defcustom omg-db-file (expand-file-name "omg.db" user-emacs-directory)
  "File where omg will store its database."
  :group 'omg
  :type 'file)

(defcustom omg-pat nil
  "GitHub personal access token"
  :group 'omg
  :type 'string)

(defcustom omg-http-timeout 10
  "Timeout used when make HTTP request"
  :group 'omg
  :type 'integer)

(defcustom omg-download-directory eww-download-directory
  "Directory where gists/release assets will downloaded."
  :group 'omg
  :type '(choice directory function))

;; unload-feature hook, free C resources
(defun omg-unload-function ()
  (when (omg-dyn-teardown)
    (message "omg-dyn closed"))
  nil)

;; Private APIs
(defconst omg--pipe-eof "\n\n"
  "Same with PIPE_EOF in C API. Used to notify no more data will be written to pipe")

(defconst omg--log-buf-name "*omg-log*")

(defface omg-log-date-face
  '((t :inherit font-lock-type-face))
  "Face for showing the date in the omg log buffer."
  :group 'omg)

(defun omg--log (fmt &rest args)
  (with-current-buffer (get-buffer-create omg--log-buf-name)
    (goto-char (point-max))
    (insert
     (format
      (concat "[" (propertize "%s" 'face 'omg-log-date-face) "] %s\n")
      (format-time-string "%Y-%m-%d %H:%M:%S")
      (apply #'format fmt args)))))

(defun omg--download-file (filename raw-url)
  (let* ((dir (if (stringp omg-download-directory)
                  omg-download-directory
                (funcall omg-download-directory)))
         (dest (expand-file-name filename dir)))
    (when (or (not (file-exists-p dest))
              (yes-or-no-p (format "%s exists, overwrite? " dest)))
      (let* ((proc (make-pipe-process :name (format "*omg-download %s*" raw-url)
                                      :coding 'utf-8-emacs-unix
                                      :filter (lambda (proc output)
                                                (omg--log "[omg-download] %s\n" output)
                                                (when (string-match-p omg--pipe-eof output)
                                                  (delete-process proc))))))
        (when (omg-dyn-download proc raw-url dest)
          (message (format "Download %s in background.\nCheck %s buffer for progress."
                           raw-url
                           omg--log-buf-name)))))))

(provide 'omg-core)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-core.el ends here
