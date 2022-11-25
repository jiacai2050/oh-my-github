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
(require 'oh-my-github-core)
(require 'oh-my-github-commit)
(require 'oh-my-github-gist)
(require 'oh-my-github-pull)
(require 'oh-my-github-release)
(require 'oh-my-github-repo)
(require 'oh-my-github-trending)
(require 'oh-my-github-whoami)

;;  Public main API

;;;###autoload
(defun oh-my-github-setup()
  "Setup oh-my-github"
  (if (string-empty-p oh-my-github-pat)
      (error "Personal access token not set.")
    (omg-dyn-setup (expand-file-name oh-my-github-db-file)
                   oh-my-github-pat
                   oh-my-github-http-timeout)))

;;;###autoload
(defun oh-my-github-teardown ()
  "Teardown oh-my-github"
  (omg-dyn-teardown))

;;;###autoload
(defun oh-my-github-sync ()
  "Sync GitHub repositories/gists(both created and starred) into local database."
  (interactive)
  (let* ((buf (get-buffer-create oh-my-github--log-buf-name))
         (sync-proc (make-pipe-process :name "oh-my-github-sync"
                                       :coding 'utf-8-emacs-unix
                                       :filter (lambda (proc output)
                                                 (oh-my-github--log "oh-my-github-sync: %s\n" output)
                                                 (when (string-match-p oh-my-github--pipe-eof output)
                                                   (delete-process proc)))
                                       :buffer buf)))
    (omg-dyn-sync sync-proc)
    (message (format "Start syncing repositories/gists in background. Check %s buffer for progress."
                     oh-my-github--log-buf-name))))

(provide 'oh-my-github)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github.el ends here
