;;; oh-my-github.el --- Oh My GitHub is a delightful, open source tool for managing your GitHub repositories/gists. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jiacai Liu

;; Author: Jiacai Liu <jiacai2050@gmail.com>
;; Version: 0.6.0
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

(defcustom oh-my-github-trendings-default-spoken-language nil
  "2-letter spoken language code used when query trendings.
For more 2-letter codes, see https://www.w3.org/International/O-charset-lang.html"
  :group 'oh-my-github
  :type '(choice (string :tag "Spoken Language")
                 (const :tag "Any" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq-default oh-my-github-trendings-query-language value)))

(defcustom oh-my-github-trendings-default-language nil
  "Programming Language used when query trendings."
  :group 'oh-my-github
  :type '(choice (string :tag "Programming Language")
                 (const :tag "Any" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq-default oh-my-github-trendings-query-language value)))

(defconst oh-my-github--trendings-ranges '("daily" "weekly" "monthly"))

(defcustom oh-my-github-trendings-default-range "daily"
  "Range used when query trendings."
  :group 'oh-my-github
  :type 'string
  :options oh-my-github--trendings-ranges
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq-default oh-my-github-trendings-query-range value)))

(defconst oh-my-github--trendings-spoken-languages  #s(hash-table
                                                       test equal
                                                       data ("Any" nil
                                                             "Chinese" "zh"
                                                             "Dutch" "nl"
                                                             "English" "en"
                                                             "French" "fr"
                                                             "Japanese" "ja"
                                                             "Korean" "ko"
                                                             "Polish" "pl"
                                                             "Russian" "ru"
                                                             "Spanish" "es"
                                                             "Ukrainian" "uk"))
  "Port from https://github.com/huchenme/github-trending-api/blob/master/src/spoken-languages.json")

(defconst oh-my-github--trendings-languages  #s(hash-table
                                                test equal
                                                data ("Any" nil
                                                      "C" "c"
                                                      "Clojure" "clojure"
                                                      "CMake" "cmake"
                                                      "CoffeeScript" "coffeescript"
                                                      "Common Lisp" "common-lisp"
                                                      "Coq" "coq"
                                                      "Dart" "dart"
                                                      "Dockerfile" "dockerfile"
                                                      "Emacs Lisp" "emacs-lisp"
                                                      "GDB" "gdb"
                                                      "Go" "go"
                                                      "Haskell" "haskell"
                                                      "Haxe" "haxe"
                                                      "HTML" "html"
                                                      "Java" "java"
                                                      "JavaScript" "javascript"
                                                      "JSON" "json"
                                                      "Julia" "julia"
                                                      "Jupyter Notebook" "jupyter-notebook"
                                                      "Kotlin" "kotlin"
                                                      "LLVM" "LLVM"
                                                      "Lua" "lua"
                                                      "Makefile" "makefile"
                                                      "Markdown" "markdown"
                                                      "Mathematica" "mathematica"
                                                      "Matlab" "matlab"
                                                      "Nginx" "nginx"
                                                      "Nix" "nix"
                                                      "NumPy" "numpy"
                                                      "Objective-C" "objective-c"
                                                      "OCaml" "ocaml"
                                                      "Org" "org"
                                                      "Perl" "perl"
                                                      "PHP" "php"
                                                      "PLSQL" "plsql"
                                                      "PowerShell" "powershell"
                                                      "Protocol Buffer" "protocol-buffer"
                                                      "Python" "python"
                                                      "Ruby" "ruby"
                                                      "Rust" "rust"
                                                      "Scala" "scala"
                                                      "Shell" "shell"
                                                      "SQL" "sql"
                                                      "Swift" "swift"
                                                      "TypeScript" "typescript"
                                                      "Unix Assembly" "unix-assembly"
                                                      "Vim script" "vim-script"
                                                      "Vue" "vue"
                                                      "WebAssembly" "webassembly"
                                                      "Zig" "zig"))
  "Port from https://github.com/huchenme/github-trending-api/blob/master/src/languages.json")

(defconst oh-my-github-pipe-eof "\n\n"
  "Same with PIPE_EOF in C API. Used to notify no more data will be written to pipe")

(defvar-local oh-my-github-query-keyword ""
  "The case-insensitive keyword used when query repositories.")

(defvar-local oh-my-github-query-language ""
  "The case-insensitive programming language used when query repositories.")

(defvar-local oh-my-github-query-repo-full-name ""
  "The repository full-name used when query commits/releases.")

(defvar-local oh-my-github-trendings-query-spoken-language oh-my-github-trendings-default-spoken-language
  "Spoken language used when query trendings repos.")

(defvar-local oh-my-github-trendings-query-language oh-my-github-trendings-default-language
  "Language used when query trendings repos.")

(defvar-local oh-my-github-trendings-query-range oh-my-github-trendings-default-range
  "Range used when query trendings repos.")

(defconst oh-my-github--log-buf-name "*oh-my-github-log*")

(defun oh-my-github--log (fmt &rest args)
  (with-current-buffer (get-buffer-create oh-my-github--log-buf-name)
    (end-of-buffer)
    (insert (apply 'format fmt args))))

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

(defun oh-my-github--query-commits ()
  (seq-into (omg-dyn-query-commits oh-my-github-query-repo-full-name
                                   oh-my-github-commit-query-limit)
            'list))

(defun oh-my-github--query-releases ()
  (seq-into (omg-dyn-query-releases oh-my-github-query-repo-full-name
                                    oh-my-github-release-query-limit)
            'list))

(defun oh-my-github--query-trendings ()
  (seq-into (omg-dyn-query-trendings oh-my-github-trendings-query-spoken-language
                                     oh-my-github-trendings-query-language
                                     oh-my-github-trendings-query-range)
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
                                                (when (string-match-p oh-my-github-pipe-eof output)
                                                  (delete-process proc))))))
        (when (omg-dyn-download proc raw-url dest)
          (message (format "Start downloading %s in background.\nCheck %s buffer for progress." raw-url
                           oh-my-github--log-buf-name)))))))

(defun oh-my-github--get-full-name()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 1)))

(defun oh-my-github--get-repo-url ()
  (when-let ((name (oh-my-github--get-full-name)))
    (format "https://github.com/%s" name)))

(defun oh-my-github--trendings-buf-name ()
  (format "*oh-my-github [%s]-[%s]-[%s] trendings repos*"
          (capitalize (or oh-my-github-trendings-query-spoken-language "any"))
          (capitalize (or oh-my-github-trendings-query-language "any"))
          (capitalize oh-my-github-trendings-query-range)))

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

(defun oh-my-github-unstar-repo ()
  "Unstar repository at point."
  (interactive)
  (if-let ((entry (tabulated-list-get-entry))
           (full-name (elt entry 1)))
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
    (define-key map (kbd "b") 'oh-my-github-browse-repo)
    (define-key map (kbd "w") 'oh-my-github-copy-repo-url)
    (define-key map (kbd "s") 'oh-my-github-query-repos)
    (define-key map (kbd "c") 'oh-my-github-query-commits)
    (define-key map (kbd "RET") 'oh-my-github-query-releases)
    (define-key map (kbd "s-u") 'tabulated-list-revert)
    map)
  "Local keymap for oh-my-github-repos mode buffers.")

(define-derived-mode oh-my-github-repos-mode tabulated-list-mode "oh-my-github created repos" "Manage created repositories"
  (oh-my-github--init-repos-tabulated-list '("CreatedAt" 20 t)
                                           (cons "CreatedAt" t)
                                           'oh-my-github--query-created-repos))

(defvar oh-my-github-starred-repos-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map oh-my-github-repos-mode-map)
    (define-key map (kbd "u") 'oh-my-github-unstar-repo)
    map)
  "Local keymap for oh-my-github-starred-repos mode buffers.")

(define-derived-mode oh-my-github-starred-repos-mode oh-my-github-repos-mode "oh-my-github starred repos" "Manage starred repositories"
  (oh-my-github--init-repos-tabulated-list '("StarredAt" 20 t)
                                           (cons "StarredAt" t)
                                           'oh-my-github--query-starred-repos))

;; Gist modes
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

(defun oh-my-github-trendings-revert (&optional revert)
  (setq-local oh-my-github-trendings-query-spoken-language oh-my-github-trendings-default-spoken-language)
  (setq-local oh-my-github-trendings-query-language oh-my-github-trendings-default-language)
  (setq-local oh-my-github-trendings-query-range oh-my-github-trendings-default-range)
  (rename-buffer (oh-my-github--trendings-buf-name) t))

(defun oh-my-github-trendings-query (spoken-language language range)
  (interactive (list (completing-read  "Spoken language: " oh-my-github--trendings-spoken-languages)
                     (completing-read  "Programming Language: " oh-my-github--trendings-languages)
                     (completing-read "Range: " oh-my-github--trendings-ranges)))
  (when (eq major-mode 'oh-my-github-trendings-mode)
    (let ((spoken-language-code (gethash spoken-language oh-my-github--trendings-spoken-languages
                                         spoken-language))
          (programming-lang (gethash language oh-my-github--trendings-languages language)))
      (setq-local oh-my-github-trendings-query-spoken-language spoken-language-code)
      (setq-local oh-my-github-trendings-query-language programming-lang)
      (setq-local oh-my-github-trendings-query-range range))
    (tabulated-list-print t)
    (rename-buffer (oh-my-github--trendings-buf-name) t)))

(defvar oh-my-github-trendings-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map oh-my-github-repos-mode-map)
    (define-key map (kbd "s") 'oh-my-github-trendings-query)
    map)
  "Local keymap for oh-my-github-stars mode buffers.")

(define-derived-mode oh-my-github-trendings-mode oh-my-github-repos-mode "oh-my-github trendings" "Display Trendings of GitHub repository"
  (setq tabulated-list-format [("Recent Stars" 12 t)
                               ("Repository" 25)
                               ("Description" 5)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil
        tabulated-list-entries 'oh-my-github--query-trendings)

  (add-hook 'tabulated-list-revert-hook 'oh-my-github-trendings-revert nil t)
  (tabulated-list-init-header))

;; unload-feature hook, free C resources
(defun oh-my-github-unload-function ()
  (when (omg-dyn-teardown)
    (message "omg-dyn closed"))
  nil)

;;  Public API

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
  "Sync GitHub repositories/gists(both created and starred) into local database."
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
    (message (format "Start syncing repositories/gists in background. Check %s buffer for progress."
                     oh-my-github--log-buf-name))))

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

;;;###autoload
(defun oh-my-github-list-trendings ()
  (interactive)
  (with-current-buffer (get-buffer-create (oh-my-github--trendings-buf-name))
    (oh-my-github-trendings-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'oh-my-github)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github.el ends here
