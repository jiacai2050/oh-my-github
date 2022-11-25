;;; -*- lexical-binding: t -*-

(require 'omg-dyn)
(require 'tabulated-list)
(require 'seq)

(defcustom omg-commit-query-limit 50
  "Limit used when query latest commits (max 100)."
  :group 'oh-my-github
  :type 'integer)

(defun omg-commit--query ()
  (seq-into (omg-dyn-query-commits omg-repo--current-full-name
                                   omg-commit-query-limit)
            'list))

(defun omg-commit--get-sha ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 0)))

(defun omg-commit--get--url (&optional patch)
  (when-let* ((sha (omg-commit--get-sha)))
    (concat
     (format "https://github.com/%s/commit/%s"
             omg-repo--current-full-name
             sha)
     (when patch ".patch"))))

(defun omg-commit-browse ()
  "Browse commit at point."
  (interactive)
  (if-let* ((url (omg-commit--get--url t)))
      (browse-url url)
    (user-error "There is no commit at point")))

(defun omg-commit-copy-url ()
  "Copy commit url at point."
  (interactive)
  (if-let* ((url (omg-commit--get--url)))
      (progn
        (kill-new url)
        (message "Copied %s" url))
    (user-error "There is no commit at point")))

(defvar omg-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'omg-commit-browse)
    (define-key map (kbd "w") 'omg-commit-copy-url)
    map)
  "Local keymap for omg-commit-mode buffers.")

(define-derived-mode omg-commit-mode tabulated-list-mode "omg-commit" "Display Commits of GitHub repository"
  (setq tabulated-list-format [("Commit" 8)
                               ("Message" 70)
                               ("Author" 20 t)
                               ("Date" 20 t)]

        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Date" t)
        tabulated-list-entries 'omg-commit--query)
  (tabulated-list-init-header))

(provide 'omg-commit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-commit.el ends here
