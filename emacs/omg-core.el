;;; -*- lexical-binding: t -*-

(defcustom omg-db-file (expand-file-name "omg.db" user-emacs-directory)
  "File where omg will store its database."
  :group 'omg
  :type 'file)

(defcustom omg-pat nil
  "GitHub personal access token"
  :group 'omg
  :type 'string)

(defcustom omg-http-timeout 6
  "Timeout used when make HTTP request"
  :group 'omg
  :type 'integer)

(defconst omg--pipe-eof "\n\n"
  "Same with PIPE_EOF in C API. Used to notify no more data will be written to pipe")

(defconst omg--log-buf-name "*omg-log*")

(defun omg--log (fmt &rest args)
  (with-current-buffer (get-buffer-create omg--log-buf-name)
    (end-of-buffer)
    (insert (apply 'format fmt args))))

;; unload-feature hook, free C resources
(defun omg-unload-function ()
  (when (omg-dyn-teardown)
    (message "omg-dyn closed"))
  nil)

(provide 'omg-core)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-core.el ends here
