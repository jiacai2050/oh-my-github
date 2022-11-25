;;; -*- lexical-binding: t -*-

(require 'omg-dyn)
(require 'tabulated-list)
(require 'seq)

(defun omg-gist--query-starred ()
  (seq-into (omg-dyn-query-starred)
            'list))

(defun omg-gist--query-created ()
  (seq-into (omg-dyn-query-created-gists)
            'list))

(defun omg-gist--get-id ()
  (tabulated-list-get-id))

(defun omg-gist--get-url ()
  (when-let ((id (omg-gist--get-id)))
    (format "https://gist.github.com/%s" id)))

(defun omg-gist--get-file-url ()
  (when-let ((entry (tabulated-list-get-entry))
             (file-btn (seq-elt entry 1))
             (raw-url (plist-get (cdr file-btn) 'raw-url)))
    raw-url))

(defun omg-gist--get-file-name ()
  (when-let ((entry (tabulated-list-get-entry))
             (file-btn (seq-elt entry 1))
             (filename (car file-btn)))
    filename))

(defun omg-gist-browse ()
  (interactive)
  (when-let ((url (omg-gist--get-url)))
    (browse-url url)))

(defun omg-gist-copy-file-url ()
  (interactive)
  (when-let ((url (omg-gist--get-file-url)))
    (kill-new url)
    (message "Copied %s" url)))

(defun omg-gist-browse-file ()
  (interactive)
  (when-let ((raw-url (omg-gist--get-file-url)))
    (browse-url raw-url)))

(defun omg-gist-download ()
  (interactive)
  (when-let ((raw-url (omg-gist--get-file-url))
             (filename (omg-gist--get-file-name)))
    (omg-gist--download filename raw-url)))

(defun omg-gist-delete ()
  (interactive)
  (when-let ((gist-id (omg-gist--get-id))
             (filename (omg-gist--get-file-name)))
    (when (yes-or-no-p (format "Are you really want to delete %s?" filename))
      (omg-dyn-delete-gist gist-id)
      (tabulated-list-delete-entry)
      (message "Deleted. %s %s" filename gist-id))))

(defvar omg-gist-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "b") 'omg-gist-browse)
    (define-key map (kbd "x") 'omg-gist-delete)
    (define-key map (kbd "d") 'omg-gist-download)
    (define-key map (kbd "w") 'omg-gist-copy-file-url)
    (define-key map (kbd "RET") 'omg-gist-browse-file)
    (define-key map (kbd "s-u") 'tabulated-list-revert)
    map)
  "Local keymap for omg-gist mode buffers.")

(define-derived-mode omg-gist-mode tabulated-list-mode "omg-gist created" "Manage created gists"
  (setq tabulated-list-format [("CreatedAt" 20 t)
                               ("File" 30 t)
                               ("Description" 50)]
        tabulated-list-padding 2
        tabulated-list-sort-key  (cons "CreatedAt" t)
        tabulated-list-entries 'omg-gist--query-created)

  (tabulated-list-init-header))

(defun omg-gist-unstar ()
  (interactive)
  (when-let ((gist-id (omg-gist--get-id))
             (filename (omg-gist--get-file-name)))
    (when (yes-or-no-p (format "Are you really want to unstar %s?" filename))
      (omg-gist-dyn-unstar-gist gist-id)
      (tabulated-list-delete-entry)
      (message "Unstarred. %s %s" filename gist-id))))

(defvar omg-gist-starred-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map omg-gist-mode-map)
    (define-key map (kbd "u") 'omg-gist-unstar)
    map)
  "Local keymap for omg-gist-starred mode buffers.")

(define-derived-mode omg-gist-starred-mode omg-gist-mode "omg-gist starred" "Manage starred gists"
  (setq tabulated-list-format [("CreatedAt" 20)
                               ("File" 30)
                               ("Description" 50)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil
        tabulated-list-entries 'omg-gist--query-starred)

  (tabulated-list-init-header))

;;;###autoload
(defun omg-gist-list-created ()
  "Display created gists in table view."
  (interactive)
  (with-current-buffer (get-buffer-create "*omg-gist created*")
    (omg-gist-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun omg-gist-list-starred ()
  "Display starred gists in table view."
  (interactive)
  (with-current-buffer (get-buffer-create "*omg-gist starred")
    (omg-gist-starred-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'omg-gist)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-gist.el ends here
