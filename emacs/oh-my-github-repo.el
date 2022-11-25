;;; -*- lexical-binding: t -*-

(defvar-local oh-my-github-query-keyword ""
  "The case-insensitive keyword used when query repositories.")

(defvar-local oh-my-github-query-language ""
  "The case-insensitive programming language used when query repositories.")

(defun oh-my-github--query-starred-repos ()
  (seq-into (omg-dyn-query-starred-repos oh-my-github-query-keyword oh-my-github-query-language)
            'list))

(defun oh-my-github--query-created-repos ()
  (seq-into (omg-dyn-query-created-repos oh-my-github-query-keyword oh-my-github-query-language)
            'list))

(defun oh-my-github--get-full-name()
  (when-let ((entry (tabulated-list-get-entry))
             (name-btn (aref entry 0)))
    (car name-btn)))

(defun oh-my-github--get-repo-url ()
  (when-let ((name (oh-my-github--get-full-name)))
    (format "https://github.com/%s" name)))
(defun oh-my-github-browse-repo ()
  "Browse GitHub repository at point."
  (interactive)
  (if-let ((url (oh-my-github--get-repo-url)))
      (browse-url url)
    (user-error "There is no repository at point")))

(defun oh-my-github-copy-repo-url ()
  "Copy repository URL at point."
  (interactive)
  (if-let ((url (oh-my-github--get-repo-url)))
      (progn
        (kill-new url)
        (message "Copied %s" url))
    (user-error "There is no repository at point")))

(defun oh-my-github-unstar-repo ()
  "Unstar repository at point."
  (interactive)
  (if-let ((full-name (oh-my-github--get-full-name)))
      (when (yes-or-no-p (format "Are you really want to unstar %s?" full-name))
        (omg-dyn-unstar-repo (string-to-number (tabulated-list-get-id)))
        (tabulated-list-delete-entry)
        (message "Unstarred %s" full-name))
    (user-error "There is no repository at point")))

(defun oh-my-github-tabulated-list-revert (&optional revert)
  (setq-local oh-my-github-query-keyword "")
  (setq-local oh-my-github-query-language ""))

(defun oh-my-github-query-repos (keyword language)
  (interactive (list (read-string (format "Keyword(%s): " oh-my-github-query-keyword)
                                  nil nil oh-my-github-query-keyword)
                     (read-string (format "Language(%s): " oh-my-github-query-language)
                                  nil nil oh-my-github-query-language)))
  (when (derived-mode-p 'oh-my-github-repos-mode)
    (setq-local oh-my-github-query-keyword keyword)
    (setq-local oh-my-github-query-language language)
    (tabulated-list-print t)))


(defun oh-my-github--init-repos-tabulated-list (query-entries-fn)
  (setq tabulated-list-format `[("Repository" 25)
                                ("Language" 8)
                                ("Description" 40)]

        tabulated-list-padding 2
        ;; tabulated-list-sort-key sort-key
        tabulated-list-entries query-entries-fn)

  (add-hook 'tabulated-list-revert-hook 'oh-my-github-tabulated-list-revert nil t)
  (tabulated-list-init-header))

(defvar oh-my-github-repos-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "b") 'oh-my-github-browse-repo)
    (define-key map (kbd "w") 'oh-my-github-copy-repo-url)
    (define-key map (kbd "s") 'oh-my-github-query-repos)
    (define-key map (kbd "r") 'oh-my-github-query-releases)
    (define-key map (kbd "RET") 'oh-my-github-query-commits)
    (define-key map (kbd "s-u") 'tabulated-list-revert)
    map)
  "Local keymap for oh-my-github-repos mode buffers.")


(define-derived-mode oh-my-github-repos-mode tabulated-list-mode "oh-my-github created repos" "Manage created repositories"
  (oh-my-github--init-repos-tabulated-list 'oh-my-github--query-created-repos))

(defvar oh-my-github-starred-repos-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map oh-my-github-repos-mode-map)
    (define-key map (kbd "u") 'oh-my-github-unstar-repo)
    map)
  "Local keymap for oh-my-github-starred-repos mode buffers.")

(define-derived-mode oh-my-github-starred-repos-mode oh-my-github-repos-mode "oh-my-github starred repos" "Manage starred repositories"
  (oh-my-github--init-repos-tabulated-list 'oh-my-github--query-starred-repos))

;;;###autoload
(defun oh-my-github-list-created-repositories ()
  "Display created repositories in table view."
  (interactive)
  (with-current-buffer (get-buffer-create "*oh-my-github created repositories*")
    (oh-my-github-repos-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun oh-my-github-list-starred-repositories ()
  "Display starred repositories in table view."
  (interactive)
  (with-current-buffer (get-buffer-create "*oh-my-github starred repositories*")
    (oh-my-github-starred-repos-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'oh-my-github-repo)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github-repo.el ends here
