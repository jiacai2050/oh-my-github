;;; github-star.el --- Manage your GitHub stars  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jiacai Liu

;; Author: Jiacai Liu <jiacai2050@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: GitHub
;; URL: https://github.com/jiacai2050/github-star

;;; Code:

(require 'ghs-dyn)
(require 'tabulated-list)
(require 'seq)

(defcustom github-star-db-file (expand-file-name "github-star.db" user-emacs-directory)
  "File where github-star will store its database."
  :group 'github-star
  :type 'file)

(defcustom github-star-pat ""
  "GitHub personal access token"
  :group 'github-star
  :type 'string)

(defcustom github-star-default-query-keyword nil
  "Default keyword when query"
  :group 'github-star
  :type '(choice (const :tag "Don't set" nil)
                 (string :tag "Keyword"))
  :set (lambda (name value)
         (custom-set-default name value)
         (setq github-star-query-keyword value)))

(defcustom github-star-default-query-language nil
  "Default programming language when query"
  :group 'github-star
  :type '(choice (const :tag "Don't set" nil)
                 (string :tag "Language"))
  :set (lambda (name value)
         (custom-set-default name value)
         (setq github-star-query-limit value)))

(defvar github-star-query-keyword github-star-default-query-keyword
  "The case-insensitive keyword used when query GitHub stars.")

(defvar github-star-query-language github-star-default-query-language
  "The case-insensitive programming language used when query GitHub stars.")

(defvar github-star-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'github-star-browse-repo)
    (define-key map (kbd "w") 'github-star-copy-repo-url)
    (define-key map (kbd "s") 'github-star-search)
    (define-key map (kbd "d") 'github-star-unstar)
    (define-key map (kbd "s-u") 'tabulated-list-revert)
    map)
  "Local keymap for github-star mode buffers.")

(defun github-star--search ()
  (seq-into
   (ghs-dyn-query github-star-query-keyword github-star-query-language)
   'list))

(defun github-star--get-full-name()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 1)))

(defun github-star--get-repo-url ()
  (when-let ((name (github-star--get-full-name)))
    (format "https://github.com/%s" name)))

(defun github-star-browse-repo ()
  "Browse GitHub repository at point."
  (interactive)
  (if-let ((url (github-star--get-repo-url)))
      (browse-url url)
    (user-error "There is no repository at point")))

(defun github-star-copy-repo-url ()
  "Copy repository URL at point."
  (interactive)
  (if-let ((url (github-star--get-repo-url)))
      (progn
        (message url)
        (kill-new url))
    (user-error "There is no repository at point")))

(defun github-star-unstar ()
  "Unstar repository at point."
  (interactive)
  (if-let ((entry (tabulated-list-delete-entry)))
      (progn
        (message "Unstar %s..."  (elt (cadr entry) 1)) ;; full_name
        (ghs-dyn-unstar (string-to-number (car entry))))
    (user-error "There is no repository at point")))

(defun github-star-tabulated-list-revert (&optional revert)
  (setq-local github-star-query-keyword github-star-default-query-keyword)
  (setq-local github-star-query-language github-star-default-query-language))

(defun github-star-search (keyword language)
  (interactive "sKeyword: \nsProgramming Language: ")
  (when (eq major-mode 'github-star-mode)
    (setq-local github-star-query-keyword keyword)
    (setq-local github-star-query-language language)
    (tabulated-list-print t)))

(defun github-star--integer-compare (column)
  (lambda (x y)
    (<
     (string-to-number (aref (cadr x) column))
     (string-to-number (aref (cadr y) column)))))

(define-derived-mode github-star-mode tabulated-list-mode "github-star" "Manage Github stars"
  (setq tabulated-list-format `[("Star Time" 20 t)
                                ("Repository" 25 t)
                                ("Language" 9 t)
                                ("Stars" 6 ,(github-star--integer-compare 3))
                                ;; GitHub bug: watcher_count is same with star_count
                                ;; ("Watchers" 6 (github-star--integer-compare 4))
                                ("Forks" 6 ,(github-star--integer-compare 4))
                                ("Description" 50 t)]

        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Star Time" t)
        tabulated-list-entries 'github-star--search)

  (add-hook 'tabulated-list-revert-hook 'github-star-tabulated-list-revert nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun github-star-setup()
  "Setup github-star"
  (if (string-empty-p github-star-pat)
      (error "Personal access token not set.")
    (ghs-dyn-setup (expand-file-name github-star-db-file)
                   github-star-pat)))

;;;###autoload
(defun github-star-teardown ()
  "Teardown github-star"
  (ghs-dyn-teardown))

;;;###autoload
(defun github-star-sync()
  "Sync GitHub star into local database"
  (ghs-dyn-sync))

;;;###autoload
(defun github-star-list ()
  "Display GitHub stars in table view"
  (interactive)
  (with-current-buffer (get-buffer-create "*github-star*")
    (github-star-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'github-star)

;; Local Variables:
;; coding: utf-8
;; End:

;;; github-star.el ends here
