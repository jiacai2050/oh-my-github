;;; -*- lexical-binding: t -*-

(require 'omg-dyn)
(require 'tabulated-list)
(require 'seq)

(defvar-local oh-my-github-query-repo-full-name ""
  "The repository full-name used when query commits/releases.")

(defcustom oh-my-github-release-query-limit 50
  "Limit used when query latest releases (max 100)."
  :group 'oh-my-github
  :type 'integer)

(defcustom oh-my-github-download-directory eww-download-directory
  "Directory where asset files will downloaded."
  :group 'oh-my-github
  :type '(choice directory function))

(defun oh-my-github--query-releases ()
  (seq-into (omg-dyn-query-releases oh-my-github-query-repo-full-name
                                    oh-my-github-release-query-limit)
            'list))

(defun oh-my-github--download-file (filename raw-url)
  (let* ((dir (if (stringp oh-my-github-download-directory)
                  oh-my-github-download-directory
                (funcall oh-my-github-download-directory)))
         (dest (expand-file-name filename dir)))
    (when (or (not (file-exists-p dest))
              (yes-or-no-p (format "%s exists, overwrite? " dest)))
      (let* ((proc (make-pipe-process :name (format "*oh-my-github-download %s*" raw-url)
                                      :coding 'utf-8-emacs-unix
                                      :filter (lambda (proc output)
                                                (oh-my-github--log "oh-my-github-download: %s\n" output)
                                                (when (string-match-p oh-my-github--pipe-eof output)
                                                  (delete-process proc))))))
        (when (omg-dyn-download proc raw-url dest)
          (message (format "Start downloading %s in background.\nCheck %s buffer for progress." raw-url
                           oh-my-github--log-buf-name)))))))

(defun oh-my-github-query-releases (full-name)
  (interactive (list (if current-prefix-arg
                         (read-string (format "Repository name(%s): " oh-my-github-query-repo-full-name)
                                      nil nil oh-my-github-query-repo-full-name)
                       (oh-my-github--get-full-name))))
  (with-current-buffer (get-buffer-create (format "*oh-my-github %s releases*" full-name))
    (oh-my-github-releases-mode)
    (setq-local oh-my-github-query-repo-full-name full-name)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(defun oh-my-github--get-release-name ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 1)))

(defun oh-my-github--get-release-tag ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 3)))

(defun oh-my-github--get-release-url ()
  (when-let ((tag (oh-my-github--get-release-tag)))
    (format "https://github.com/%s/releases/%s"
            oh-my-github-query-repo-full-name
            tag)))

(defun oh-my-github-browse-release ()
  "Browse release at point."
  (interactive)
  (if-let* ((url (oh-my-github--get-release-url)))
      (browse-url url)
    (user-error "There is no release at point")))

(defun oh-my-github-copy-release-url ()
  "Browse release at point."
  (interactive)
  (if-let* ((url (oh-my-github--get-release-url)))
      (progn
        (kill-new url)
        (message "Copied %s" url))
    (user-error "There is no release at point")))

(defvar oh-my-github-releases-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "b") 'oh-my-github-browse-release)
    (define-key map (kbd "w") 'oh-my-github-copy-release-url)
    (define-key map (kbd "RET") 'oh-my-github-query-assets)
    map)
  "Local keymap for oh-my-github-releases-mode buffers.")

(define-derived-mode oh-my-github-releases-mode tabulated-list-mode "oh-my-github releases" "Display Releases of GitHub repository"
  (setq tabulated-list-format [("PublishedAt" 20 t)
                               ("Name" 60)
                               ("Author" 20 t)
                               ("Tag" 15 t)
                               ("Draft" 6)
                               ("Prerelease" 6)]

        tabulated-list-padding 2
        tabulated-list-sort-key (cons "PublishedAt" t)
        tabulated-list-entries 'oh-my-github--query-releases)
  (tabulated-list-init-header))

(defun oh-my-github-query-assets ()
  (interactive)
  (when-let* ((name (oh-my-github--get-release-name))
              (label (car name))
              (files (plist-get (cdr name) 'asset-files)))
    (with-current-buffer (get-buffer-create (format "*oh-my-github %s assets*" label))
      (oh-my-github-assets-mode)
      (setq tabulated-list-entries (seq-into files 'list))
      (tabulated-list-print t)
      (switch-to-buffer (current-buffer)))))

(defun oh-my-github--get-asset-name ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 0)))

(defun oh-my-github-copy-asset-url ()
  (interactive)
  (if-let* ((name (oh-my-github--get-asset-name))
            (raw-url (plist-get (cdr name) 'raw-url)))
      (progn
        (kill-new raw-url)
        (message "Copied %s" raw-url))
    (user-error "There is no asset at point")))

(defun oh-my-github-download-asset ()
  "Browse release at point."
  (interactive)
  (if-let* ((name (oh-my-github--get-asset-name))
            (label (car name))
            (raw-url (plist-get (cdr name) 'raw-url)))
      (oh-my-github--download-file label raw-url)
    (user-error "There is no asset at point")))

(defvar oh-my-github-assets-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "w") 'oh-my-github-copy-asset-url)
    (define-key map (kbd "RET") 'oh-my-github-download-asset)
    map)
  "Local keymap for oh-my-github-assets-mode buffers.")

(define-derived-mode oh-my-github-assets-mode tabulated-list-mode "oh-my-github assets" "Display Assets of GitHub repository"
  (setq tabulated-list-format [("Name" 40 t)
                               ("Size" 8)
                               ("Download Count" 5 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Name" t))
  (tabulated-list-init-header))

(provide 'oh-my-github-release)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github-release.el ends here
