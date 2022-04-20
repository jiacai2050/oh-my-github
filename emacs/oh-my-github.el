;;; oh-my-github.el --- Oh My GitHub is a delightful, open source tool for managing your GitHub repositories. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jiacai Liu

;; Author: Jiacai Liu <jiacai2050@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: GitHub
;; URL: https://github.com/jiacai2050/oh-my-github

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; See documentation in README.org or visit homepage

;;; Code:

(require 'omg-dyn)
(require 'tabulated-list)
(require 'seq)

(defcustom oh-my-github-db-file (expand-file-name "oh-my-github.db" user-emacs-directory)
  "File where oh-my-github will store its database."
  :group 'oh-my-github
  :type 'file)

(defcustom oh-my-github-pat ""
  "GitHub personal access token"
  :group 'oh-my-github
  :type 'string)

(defvar-local oh-my-github-query-keyword ""
  "The case-insensitive keyword used when query")

(defvar-local oh-my-github-query-language ""
  "The case-insensitive programming language used when query")

(defun oh-my-github--search-stars ()
  (seq-into (omg-dyn-query-stars oh-my-github-query-keyword oh-my-github-query-language)
            'list))

(defun oh-my-github--search-repos ()
  (seq-into (omg-dyn-query-repos oh-my-github-query-keyword oh-my-github-query-language)
            'list))

(defun oh-my-github--get-full-name()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 1)))

(defun oh-my-github--get-repo-url ()
  (when-let ((name (oh-my-github--get-full-name)))
    (format "https://github.com/%s" name)))

(defconst oh-my-github-whoami-col-sep ",,,")
(defconst oh-my-github-whoami-row-sep "\n")

(defun oh-my-github--whoami-insert-row (label value)
  (insert label
          oh-my-github-whoami-col-sep
          (if (numberp value)
              (number-to-string value)
            value)
          oh-my-github-whoami-row-sep))

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
        (message url)
        (kill-new url))
    (user-error "There is no repository at point")))

(defun oh-my-github-unstar ()
  "Unstar repository at point."
  (interactive)
  (if-let ((entry (tabulated-list-delete-entry)))
      (progn
        (message "Unstar %s..."  (elt (cadr entry) 1)) ;; full_name
        (omg-dyn-unstar (string-to-number (car entry))))
    (user-error "There is no repository at point")))

(defun oh-my-github-tabulated-list-revert (&optional revert)
  (setq-local oh-my-github-query-keyword "")
  (setq-local oh-my-github-query-language ""))

(defun oh-my-github-search (keyword language)
  (interactive (list (read-string (format "Keyword(%s): " oh-my-github-query-keyword)
                                  nil nil oh-my-github-query-keyword)
                     (read-string (format "Programming Language(%s): " oh-my-github-query-language)
                                  nil nil oh-my-github-query-language)))
  (when (or (eq major-mode 'oh-my-github-stars-mode)
            (eq major-mode 'oh-my-github-repos-mode))
    (setq-local oh-my-github-query-keyword keyword)
    (setq-local oh-my-github-query-language language)
    (tabulated-list-print t)))

(defun oh-my-github--integer-compare (column)
  (lambda (x y)
    (<
     (string-to-number (aref (cadr x) column))
     (string-to-number (aref (cadr y) column)))))

(defun oh-my-github--size-compare (column)
  (lambda (x y)
    (let ((size-btn-x (aref (cadr x) column))
          (size-btn-y (aref (cadr y) column)))
      (<
       (string-to-number (plist-get (cdr size-btn-x) 'help-echo))
       (string-to-number (plist-get (cdr size-btn-y) 'help-echo))))))

(defun oh-my-github--init-tabulated-list (first-column sort-key search-entries-fn)
  (setq tabulated-list-format `[,first-column
                                ("Repository" 25 t)
                                ("Language" 8 t)
                                ("Stars" 6 ,(oh-my-github--integer-compare 3))
                                ("Forks" 6 ,(oh-my-github--integer-compare 4))
                                ("License" 7 t)
                                ("Size" 7 ,(oh-my-github--size-compare 6))
                                ("Description" 50)]

        tabulated-list-padding 2
        tabulated-list-sort-key sort-key
        tabulated-list-entries search-entries-fn)

  (add-hook 'tabulated-list-revert-hook 'oh-my-github-tabulated-list-revert nil t)
  (tabulated-list-init-header))

(defvar oh-my-github-repos-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'oh-my-github-browse-repo)
    (define-key map (kbd "w") 'oh-my-github-copy-repo-url)
    (define-key map (kbd "s") 'oh-my-github-search)
    (define-key map (kbd "s-u") 'tabulated-list-revert)
    map)
  "Local keymap for oh-my-github-repos mode buffers.")

(define-derived-mode oh-my-github-repos-mode tabulated-list-mode "oh-my-github repos" "Manage GitHub owned repositories"
  (oh-my-github--init-tabulated-list '("CreatedAt" 20 t)
                                     (cons "CreatedAt" t)
                                     'oh-my-github--search-repos))

(defvar oh-my-github-stars-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map oh-my-github-repos-mode-map)
    (define-key map (kbd "d") 'oh-my-github-unstar)
    map)
  "Local keymap for oh-my-github-stars mode buffers.")

(define-derived-mode oh-my-github-stars-mode oh-my-github-repos-mode "oh-my-github stars" "Manage GitHub starred repositories"
  (oh-my-github--init-tabulated-list '("StarredAt" 20 t)
                                     (cons "StarredAt" t)
                                     'oh-my-github--search-stars))

;;;###autoload
(defun oh-my-github-setup()
  "Setup oh-my-github"
  (if (string-empty-p oh-my-github-pat)
      (error "Personal access token not set.")
    (omg-dyn-setup (expand-file-name oh-my-github-db-file)
                   oh-my-github-pat)))

;;;###autoload
(defun oh-my-github-teardown ()
  "Teardown oh-my-github"
  (omg-dyn-teardown))

;;;###autoload
(defun oh-my-github-sync ()
  "Sync GitHub repositories(both owned and starred) into local database.
Note: Emacs maybe hang for a while depending on how many repositories you have."
  (omg-dyn-sync))

;;;###autoload
(defun oh-my-github-star-list ()
  "Display GitHub starred repositories in table view."
  (interactive)
  (with-current-buffer (get-buffer-create "*oh-my-github starred repositories*")
    (oh-my-github-stars-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun oh-my-github-repo-list ()
  "Display GitHub owned repositories in table view."
  (interactive)
  (with-current-buffer (get-buffer-create "*oh-my-github owned repositories*")
    (oh-my-github-repos-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun oh-my-github-whoami (&optional username)
  "Display `username' information, or current user represented by GitHub personal access token(PAT)."
  (interactive (list (when current-prefix-arg
                       (read-string "Enter GitHub username: "))))
  (let ((buf (get-buffer-create "*oh-my-github whoami*"))
        (who (omg-dyn-whoami username)))
    (with-current-buffer buf
	  (read-only-mode -1)
	  (erase-buffer)
	  (oh-my-github--whoami-insert-row "Created At" (plist-get who 'created-at))
	  (oh-my-github--whoami-insert-row "ID" (plist-get who 'id))
	  (oh-my-github--whoami-insert-row "Login" (plist-get who 'login))
	  (oh-my-github--whoami-insert-row "Name" (plist-get who 'name))
	  (oh-my-github--whoami-insert-row "Company" (plist-get who 'company))
	  (oh-my-github--whoami-insert-row "Blog" (plist-get who 'blog))
	  (oh-my-github--whoami-insert-row "Location" (plist-get who 'location))
	  (oh-my-github--whoami-insert-row "Email" (plist-get who 'email))
	  (oh-my-github--whoami-insert-row "Hireable" (plist-get who 'hireable))
	  (oh-my-github--whoami-insert-row "Public Repos" (plist-get who 'public-gists))
	  (oh-my-github--whoami-insert-row "Public Gists" (plist-get who 'public-gists))
	  (oh-my-github--whoami-insert-row "Private Repos" (plist-get who 'private-repos))
	  (oh-my-github--whoami-insert-row "Private Gists" (plist-get who 'private-gists))
	  (oh-my-github--whoami-insert-row "Followers" (plist-get who 'followers))
	  (oh-my-github--whoami-insert-row "Following" (plist-get who 'following))
	  (oh-my-github--whoami-insert-row "Disk Usage" (plist-get who 'disk-usage))
	  (table-capture (point-min) (point-max)
				     oh-my-github-whoami-col-sep oh-my-github-whoami-row-sep
                     'left 15)
	  (read-only-mode)
	  (switch-to-buffer buf))))

(provide 'oh-my-github)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github.el ends here
