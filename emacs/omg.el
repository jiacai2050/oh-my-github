;;; oh-my-github.el --- Oh My GitHub is a delightful, open source tool for managing your GitHub repositories/gists. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jiacai Liu

;; Author: Jiacai Liu <jiacai2050@gmail.com>
;; Version: 1.0.0
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
(require 'omg-core)
(require 'omg-commit)
(require 'omg-gist)
(require 'omg-pull)
(require 'omg-release)
(require 'omg-repo)
(require 'omg-trending)
(require 'omg-whoami)

;;  Public main API

;;;###autoload
(defun omg-setup()
  "Setup oh-my-github"
  (if (string-empty-p omg-pat)
      (error "Personal access token not set.")
    (omg-dyn-setup (expand-file-name omg-db-file)
                   omg-pat
                   omg-http-timeout)))

;;;###autoload
(defun omg-teardown ()
  "Teardown omg"
  (omg-dyn-teardown))

;;;###autoload
(defun omg-sync ()
  "Sync GitHub repositories/gists into local database."
  (interactive)
  (let* ((buf (get-buffer-create omg--log-buf-name))
         (sync-proc (make-pipe-process :name "omg-sync"
                                       :coding 'utf-8-emacs-unix
                                       :filter (lambda (proc output)
                                                 (omg--log "[omg-sync] %s\n" output)
                                                 (when (string-match-p omg--pipe-eof output)
                                                   (delete-process proc)))
                                       :buffer buf)))
    (omg-dyn-sync sync-proc)
    (message (format "Start syncing repositories/gists in background. Check %s buffer for progress."
                     omg--log-buf-name))))

(provide 'omg)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg.el ends here
