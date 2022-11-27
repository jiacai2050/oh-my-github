;;; -*- lexical-binding: t -*-

(require 'omg-dyn)
(require 'tabulated-list)
(require 'seq)
(require 'omg-commit)
(require 'omg-release)

(defvar omg-repo--query-keyword ""
  "The case-insensitive keyword used when query repositories.")

(defvar omg-repo--query-language ""
  "The case-insensitive programming language used when query repositories.")

(defvar omg-repo--current-full-name nil
  "Current repository's full name, use for query commits/releases/issues.")

(defun omg-repo--query-starred ()
  (seq-into (omg-dyn-query-starred-repos omg-repo--query-keyword omg-repo--query-language)
            'list))

(defun omg-repo--query-created ()
  (seq-into (omg-dyn-query-created-repos omg-repo--query-keyword omg-repo--query-language)
            'list))

(defun omg-repo--get-full-name()
  (when-let ((entry (tabulated-list-get-entry))
             (name-btn (aref entry 0)))
    (car name-btn)))

(defun omg-repo--get-repo-url ()
  (when-let ((name (omg-repo--get-full-name)))
    (format "https://github.com/%s" name)))

(defun omg-repo-browse ()
  "Browse GitHub repository at point."
  (interactive)
  (if-let ((url (omg-repo--get-repo-url)))
      (browse-url url)
    (user-error "There is no repository at point")))

(defun omg-repo-copy-url ()
  "Copy repository URL at point."
  (interactive)
  (if-let ((url (omg-repo--get-repo-url)))
      (progn
        (kill-new url)
        (message "Copied %s" url))
    (user-error "There is no repository at point")))

(defun omg-repo-query-repos (keyword language)
  (interactive (list (read-string (format "Keyword(%s): " omg-repo--query-keyword)
                                  nil nil omg-repo--query-keyword)
                     (read-string (format "Language(%s): " omg-repo--query-language)
                                  nil nil omg-repo--query-language)))
  (when (derived-mode-p 'omg-repo-mode)
    (setq omg-repo--query-keyword keyword)
    (setq omg-repo--query-language language)
    (tabulated-list-print t)))

(defun omg-repo-query-commits (full-name)
  (interactive (list (if current-prefix-arg
                         (read-string (format "Repository name(%s): " omg-repo--current-full-name)
                                      nil nil omg-repo--current-full-name)
                       (omg-repo--get-full-name))))
  (with-current-buffer (get-buffer-create (format "*omg-commit(%s)*" full-name))
    (setq omg-repo--current-full-name full-name)
    (omg-commit-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(defun omg-repo-query-releases (full-name)
  (interactive (list (if current-prefix-arg
                         (read-string (format "Repository name(%s): " omg-repo--current-full-name)
                                      nil nil omg-repo--current-full-name)
                       (omg-repo--get-full-name))))
  (with-current-buffer (get-buffer-create (format "*omg-release(%s)" full-name))
    (omg-release-mode)
    (setq omg-repo--current-full-name full-name)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(defun omg-repo-star (full-name)
  "Star repository at point."
  (interactive (list (omg-repo--get-full-name)))
  (omg-dyn-star-repo full-name)
  (message "Starred %s" full-name))

(defun omg-repo--revert (&optional revert)
  (setq omg-repo--query-keyword "")
  (setq omg-repo--query-language ""))

(defun omg-repo--init-repos-tabulated-list (query-entries-fn)
  (setq tabulated-list-format `[("Repository" 25)
                                ("Language" 8)
                                ("Description" 40)]

        tabulated-list-padding 2
        ;; tabulated-list-sort-key sort-key
        tabulated-list-entries query-entries-fn)

  (add-hook 'tabulated-list-revert-hook 'omg-repo--revert nil t)
  (tabulated-list-init-header))

(defvar omg-repo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "b") 'omg-repo-browse)
    (define-key map (kbd "w") 'omg-repo-copy-url)
    (define-key map (kbd "s") 'omg-repo-query-repos)
    (define-key map (kbd "S") 'omg-repo-star)
    (define-key map (kbd "r") 'omg-repo-query-releases)
    (define-key map (kbd "RET") 'omg-repo-query-commits)
    (define-key map (kbd "s-u") 'tabulated-list-revert)
    map)
  "Local keymap for omg-repo mode buffers.")

(define-derived-mode omg-repo-mode tabulated-list-mode "omg-repo created repos" "Manage created repositories"
  (omg-repo--init-repos-tabulated-list 'omg-repo--query-created))

(defun omg-repo-unstar (full-name)
  "Unstar repository at point."
  (interactive (list  (omg-repo--get-full-name)))
  (when (yes-or-no-p (format "Are you really want to unstar %s?" full-name))
    (omg-dyn-unstar-repo (string-to-number (tabulated-list-get-id)))
    (tabulated-list-delete-entry)
    (message "Unstarred %s" full-name)))

(defvar omg-repo-starred-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map omg-repo-mode-map)
    (define-key map (kbd "u") 'omg-repo-unstar)
    map)
  "Local keymap for omg-repo-starred-repos mode buffers.")

(define-derived-mode omg-repo-starred-mode omg-repo-mode "omg-repo starred" "Manage starred repositories"
  (omg-repo--init-repos-tabulated-list 'omg-repo--query-starred))

;;;###autoload
(defun omg-repo-list-created ()
  "Display created repositories in table view."
  (interactive)
  (with-current-buffer (get-buffer-create "*omg-repo created*")
    (omg-repo-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun omg-repo-list-starred ()
  "Display starred repositories in table view."
  (interactive)
  (with-current-buffer (get-buffer-create "*omg-repo starred*")
    (omg-repo-starred-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'omg-repo)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-repo.el ends here
