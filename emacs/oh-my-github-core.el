;;; -*- lexical-binding: t -*-

(defcustom oh-my-github-db-file (expand-file-name "oh-my-github.db" user-emacs-directory)
  "File where oh-my-github will store its database."
  :group 'oh-my-github
  :type 'file)

(defcustom oh-my-github-pat ""
  "GitHub personal access token"
  :group 'oh-my-github
  :type 'string)

(defcustom oh-my-github-http-timeout 6
  "Timeout used when make HTTP request"
  :group 'oh-my-github
  :type 'integer)

(defconst oh-my-github--pipe-eof "\n\n"
  "Same with PIPE_EOF in C API. Used to notify no more data will be written to pipe")

(defconst oh-my-github--log-buf-name "*oh-my-github-log*")

(defun oh-my-github--log (fmt &rest args)
  (with-current-buffer (get-buffer-create oh-my-github--log-buf-name)
    (end-of-buffer)
    (insert (apply 'format fmt args))))

;; unload-feature hook, free C resources
(defun oh-my-github-unload-function ()
  (when (omg-dyn-teardown)
    (message "omg-dyn closed"))
  nil)

(provide 'oh-my-github-core)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github-core.el ends here
