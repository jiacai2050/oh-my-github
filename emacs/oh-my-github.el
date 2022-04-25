;;; oh-my-github.el --- Oh My GitHub is a delightful, open source tool for managing your GitHub repositories. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jiacai Liu

;; Author: Jiacai Liu <jiacai2050@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
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

(defcustom oh-my-github-commit-query-limit 50
  "Limit used when query latest commits (max 100)."
  :group 'oh-my-github
  :type 'integer)

(defcustom oh-my-github-release-query-limit 50
  "Limit used when query latest releases (max 100)."
  :group 'oh-my-github
  :type 'integer)

(defcustom oh-my-github-download-directory eww-download-directory
  "Directory where asset files will downloaded."
  :group 'oh-my-github
  :type '(choice directory function))

(defconst oh-my-github-pipe-eof "\n\n"
  "Same with PIPE_EOF in C API. Used to notify no more data will be written to pipe")

(defvar-local oh-my-github-query-keyword ""
  "The case-insensitive keyword used when query repos.")

(defvar-local oh-my-github-query-language ""
  "The case-insensitive programming language used when query repos.")

(defvar-local oh-my-github-query-repo-full-name ""
  "The repository full-name used when query commits/releases.")

(defconst oh-my-github--log-buf-name "*oh-my-github-log*")

(defun oh-my-github--log (fmt &rest args)
  (with-current-buffer (get-buffer-create oh-my-github--log-buf-name)
    (end-of-buffer)
    (insert (apply 'format fmt args))))

(defun oh-my-github--query-stars ()
  (seq-into (omg-dyn-query-stars oh-my-github-query-keyword oh-my-github-query-language)
            'list))

(defun oh-my-github--query-repos ()
  (seq-into (omg-dyn-query-repos oh-my-github-query-keyword oh-my-github-query-language)
            'list))

(defun oh-my-github--query-commits ()
  (seq-into (omg-dyn-query-commits oh-my-github-query-repo-full-name
                                   oh-my-github-commit-query-limit)
            'list))

(defun oh-my-github--query-releases ()
  (seq-into (omg-dyn-query-releases oh-my-github-query-repo-full-name
                                    oh-my-github-release-query-limit)
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
        (kill-new url)
        (message "Copied %s" url))
    (user-error "There is no repository at point")))

(defun oh-my-github-unstar ()
  "Unstar repository at point."
  (interactive)
  (if-let ((entry (tabulated-list-get-entry))
           (full-name (elt entry 1)))
      (when (yes-or-no-p (format "Are you really want to unstar %s?" full-name))
        (omg-dyn-unstar (string-to-number (tabulated-list-get-id)))
        (tabulated-list-delete-entry)
        (message "Unstarred %s" full-name))
    (user-error "There is no repository at point")))

(defun oh-my-github-tabulated-list-revert (&optional revert)
  (setq-local oh-my-github-query-keyword "")
  (setq-local oh-my-github-query-language ""))

(defun oh-my-github-query-repos (keyword language)
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

(defun oh-my-github--init-repos-tabulated-list (first-column sort-key query-entries-fn)
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
        tabulated-list-entries query-entries-fn)

  (add-hook 'tabulated-list-revert-hook 'oh-my-github-tabulated-list-revert nil t)
  (tabulated-list-init-header))

(defvar oh-my-github-repos-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'oh-my-github-browse-repo)
    (define-key map (kbd "w") 'oh-my-github-copy-repo-url)
    (define-key map (kbd "s") 'oh-my-github-query-repos)
    (define-key map (kbd "c") 'oh-my-github-query-commits)
    (define-key map (kbd "r") 'oh-my-github-query-releases)
    (define-key map (kbd "s-u") 'tabulated-list-revert)
    map)
  "Local keymap for oh-my-github-repos mode buffers.")

(define-derived-mode oh-my-github-repos-mode tabulated-list-mode "oh-my-github repos" "Manage GitHub owned repositories"
  (oh-my-github--init-repos-tabulated-list '("CreatedAt" 20 t)
                                     (cons "CreatedAt" t)
                                     'oh-my-github--query-repos))

(defvar oh-my-github-stars-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map oh-my-github-repos-mode-map)
    (define-key map (kbd "u") 'oh-my-github-unstar)
    map)
  "Local keymap for oh-my-github-stars mode buffers.")

(define-derived-mode oh-my-github-stars-mode oh-my-github-repos-mode "oh-my-github stars" "Manage GitHub starred repositories"
  (oh-my-github--init-repos-tabulated-list '("StarredAt" 20 t)
                                     (cons "StarredAt" t)
                                     'oh-my-github--query-stars))

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
  ;; (add-hook 'tabulated-list-revert-hook 'oh-my-github-tabulated-list-revert nil t)
  (tabulated-list-init-header))

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
            (raw-url (plist-get (cdr name) 'raw-url))
            (dir (if (stringp oh-my-github-download-directory)
                     oh-my-github-download-directory
                   (funcall eww-download-directory)))
            (dest (expand-file-name label dir)))
        (when (or (not (file-exists-p dest))
                  (yes-or-no-p (format "%s exists, overwrite? " dest)))
          (let* ((proc (make-pipe-process :name (format "*oh-my-github-download %s*" raw-url)
                                          :coding 'utf-8-emacs-unix
                                          :filter (lambda (proc output)
                                                    (oh-my-github--log "oh-my-github-download: %s\n" output)
                                                    (when (string-match-p oh-my-github-pipe-eof output)
                                                      (delete-process proc))))))
            (omg-dyn-download proc raw-url dest)
            (message (format "Start downloading %s in background. Check %s buffer for progress." raw-url
                             oh-my-github--log-buf-name))))
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
Note: Emacs maybe hang a while depending on how many repositories you have."
  (interactive)
  (let* ((buf (get-buffer-create "*oh-my-github-sync*"))
         (sync-proc (make-pipe-process :name "oh-my-github-sync"
                                       :coding 'utf-8-emacs-unix
                                       :filter (lambda (proc output)
                                                 (oh-my-github--log "oh-my-github-sync: %s\n" output)
                                                 (when (string-match-p oh-my-github-pipe-eof output)
                                                   (delete-process proc)))
                                       :buffer buf)))
    (omg-dyn-sync sync-proc)
    (message (format "Start syncing repositories in background. Check %s buffer for progress."
                     oh-my-github--log-buf-name))))

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
