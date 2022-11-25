;;; -*- lexical-binding: t -*-

(require 'omg-dyn)
(require 'tabulated-list)
(require 'seq)

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

(defun oh-my-github--query-starred-gists ()
  (seq-into (omg-dyn-query-starred-gists)
            'list))

(defun oh-my-github--query-created-gists ()
  (seq-into (omg-dyn-query-created-gists)
            'list))

(defun oh-my-github--get-gist-id ()
  (tabulated-list-get-id))

(defun oh-my-github--get-gist-url ()
  (when-let ((id (oh-my-github--get-gist-id)))
    (format "https://gist.github.com/%s" id)))

(defun oh-my-github--get-gist-file-url ()
  (when-let ((entry (tabulated-list-get-entry))
             (file-btn (seq-elt entry 1))
             (raw-url (plist-get (cdr file-btn) 'raw-url)))
    raw-url))

(defun oh-my-github--get-gist-file-name ()
  (when-let ((entry (tabulated-list-get-entry))
             (file-btn (seq-elt entry 1))
             (filename (car file-btn)))
    filename))

(defun oh-my-github-browse-gist ()
  (interactive)
  (when-let ((url (oh-my-github--get-gist-url)))
    (browse-url url)))

(defun oh-my-github-copy-gist-file-url ()
  (interactive)
  (when-let ((url (oh-my-github--get-gist-file-url)))
    (kill-new url)
    (message "Copied %s" url)))

(defun oh-my-github-browse-gist-file ()
  (interactive)
  (when-let ((raw-url (oh-my-github--get-gist-file-url)))
    (browse-url raw-url)))

(defun oh-my-github-download-gist-file ()
  (interactive)
  (when-let ((raw-url (oh-my-github--get-gist-file-url))
             (filename (oh-my-github--get-gist-file-name)))
    (oh-my-github--download-file filename raw-url)))

(defun oh-my-github-delete-gist ()
  (interactive)
  (when-let ((gist-id (oh-my-github--get-gist-id))
             (filename (oh-my-github--get-gist-file-name)))
    (when (yes-or-no-p (format "Are you really want to delete %s?" filename)))
    (omg-dyn-delete-gist gist-id)
    (tabulated-list-delete-entry)
    (message "Deleted. %s %s" filename gist-id)))

(defvar oh-my-github-gists-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "b") 'oh-my-github-browse-gist)
    (define-key map (kbd "x") 'oh-my-github-delete-gist)
    (define-key map (kbd "d") 'oh-my-github-download-gist-file)
    (define-key map (kbd "w") 'oh-my-github-copy-gist-file-url)
    (define-key map (kbd "RET") 'oh-my-github-browse-gist-file)
    (define-key map (kbd "s-u") 'tabulated-list-revert)
    map)
  "Local keymap for oh-my-github-gists mode buffers.")

(define-derived-mode oh-my-github-gists-mode tabulated-list-mode "oh-my-github created repos" "Manage created gists"
  (setq tabulated-list-format [("CreatedAt" 20 t)
                               ("File" 30 t)
                               ("Description" 50)]
        tabulated-list-padding 2
        tabulated-list-sort-key  (cons "CreatedAt" t)
        tabulated-list-entries 'oh-my-github--query-created-gists)

  (add-hook 'tabulated-list-revert-hook 'oh-my-github-tabulated-list-revert nil t)
  (tabulated-list-init-header))

(defun oh-my-github-unstar-gist ()
  (interactive)
  (when-let ((gist-id (oh-my-github--get-gist-id))
             (filename (oh-my-github--get-gist-file-name)))
    (when (yes-or-no-p (format "Are you really want to unstar %s?" filename)))
    (omg-dyn-unstar-gist gist-id)
    (tabulated-list-delete-entry)
    (message "Unstarred. %s %s" filename gist-id)))

(defvar oh-my-github-starred-gists-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map oh-my-github-gists-mode-map)
    (define-key map (kbd "u") 'oh-my-github-unstar-gist)
    map)
  "Local keymap for oh-my-github-starred-gists mode buffers.")

(define-derived-mode oh-my-github-starred-gists-mode oh-my-github-gists-mode "oh-my-github starred gists" "Manage starred gists"
  (setq tabulated-list-format [("CreatedAt" 20)
                               ("File" 30)
                               ("Description" 50)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil
        tabulated-list-entries 'oh-my-github--query-starred-gists)

  (add-hook 'tabulated-list-revert-hook 'oh-my-github-tabulated-list-revert nil t)
  (tabulated-list-init-header))
;;;###autoload
(defun oh-my-github-list-created-gists ()
  "Display created gists in table view."
  (interactive)
  (with-current-buffer (get-buffer-create "*oh-my-github created gists*")
    (oh-my-github-gists-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun oh-my-github-list-starred-gists ()
  "Display starred gists in table view."
  (interactive)
  (with-current-buffer (get-buffer-create "*oh-my-github starred gists*")
    (oh-my-github-starred-gists-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'oh-my-github-gist)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github-gist.el ends here
