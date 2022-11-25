;;; -*- lexical-binding: t -*-

(require 'omg-dyn)
(require 'tabulated-list)
(require 'seq)

(defvar-local oh-my-github-query-repo-full-name ""
  "The repository full-name used when query commits/releases.")

(defcustom oh-my-github-commit-query-limit 50
  "Limit used when query latest commits (max 100)."
  :group 'oh-my-github
  :type 'integer)

(defun oh-my-github--query-commits ()
  (seq-into (omg-dyn-query-commits oh-my-github-query-repo-full-name
                                   oh-my-github-commit-query-limit)
            'list))

(defun oh-my-github--get-commit-sha ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 0)))

(defun oh-my-github--get-commit-url (&optional patch)
  (when-let* ((sha (oh-my-github--get-commit-sha)))
    (concat
     (format "https://github.com/%s/commit/%s"
             oh-my-github-query-repo-full-name
             sha)
     (when patch ".patch"))))

(defun oh-my-github-browse-commit ()
  "Browse commit at point."
  (interactive)
  (if-let* ((url (oh-my-github--get-commit-url t)))
      (browse-url url)
    (user-error "There is no commit at point")))

(defun oh-my-github-copy-commit-url ()
  "Copy commit url at point."
  (interactive)
  (if-let* ((url (oh-my-github--get-commit-url)))
      (progn
        (kill-new url)
        (message "Copied %s" url))
    (user-error "There is no commit at point")))

(defun oh-my-github-query-commits (full-name)
  (interactive (list (if current-prefix-arg
                         (read-string (format "Repository name(%s): " oh-my-github-query-repo-full-name)
                                      nil nil oh-my-github-query-repo-full-name)
                       (oh-my-github--get-full-name))))
  (with-current-buffer (get-buffer-create (format "*oh-my-github %s commits*" full-name))
    (oh-my-github-commits-mode)
    (setq-local oh-my-github-query-repo-full-name full-name)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(defvar oh-my-github-commits-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'oh-my-github-browse-commit)
    (define-key map (kbd "w") 'oh-my-github-copy-commit-url)
    map)
  "Local keymap for oh-my-github-commits-mode buffers.")

(define-derived-mode oh-my-github-commits-mode tabulated-list-mode "oh-my-github commits" "Display Commits of GitHub repository"
  (setq tabulated-list-format [("Commit" 8)
                               ("Message" 70)
                               ("Author" 20 t)
                               ("Date" 20 t)]

        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Date" t)
        tabulated-list-entries 'oh-my-github--query-commits)
  (tabulated-list-init-header))

(provide 'oh-my-github-commit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github-commit.el ends here
