;;; -*- lexical-binding: t -*-

(require 'org)
(require 'ox-md)
(require 'omg-dyn)
(require 'vc)

(defcustom omg-pull-open-in-browser t
  "If non-nil open PR link in browser via `browse-url-default-browser' after created."
  :group 'omg
  :type 'boolean)

(defcustom omg-pull-title-from-commit t
  "If non-nil, use last commit message as pull title"
  :group 'omg
  :type 'boolean)

(defcustom omg-pull-guess-target-repo-from-remote "origin"
  "Guess target branch from remote when `omg-pull-target-repo' is `nil'"
  :group 'omg
  :type 'string)

(defvar-local omg-pull-target-repo nil
  "The full name of the repository you want the changes pulled into.
Such as `octocat/Hello-World'")

(defvar-local omg-pull-target-branch "main"
  "The name of the branch you want the changes pulled into.
This should be an existing branch on the current repository.")

(defvar-local omg-pull-username omg-username
  "Your GitHub username")

(defvar-local omg-pull-draft "false")

(defvar omg-pull--buf-basename "*omg-pull create(%s)*")
(defvar omg-pull--repo-root nil)
(defconst omg-pull--header
  "Edit, then submit with `\\[omg-pull-submit]', or cancel with `\\[omg-pull-cancel]'")

(defun omg-pull-submit ()
  (interactive)
  (setq-local org-export-options-alist '((:title "TITLE" nil nil t)
                                         (:target-repo "TARGET-REPO" nil nil t)
                                         (:target-branch "TARGET-BRANCH" nil nil t)
                                         (:source-head "SOURCE-HEAD" nil nil t)
                                         (:draft "DRAFT" nil nil t)))
  (let* ((metadata (org-export-get-environment))
         (title (plist-get metadata :title))
         (target-repo (plist-get metadata :target-repo))
         (target-branch (plist-get metadata :target-branch))
         (source-head (plist-get metadata :source-head))
         (draft (if (string-equal "true" (plist-get metadata :draft))
                    1
                  0))
         (body (with-current-buffer (org-export-to-buffer 'md "*OMG-PULL pull body export*")
                 (let ((body (buffer-substring-no-properties (point-min) (point-max))))
                   (kill-buffer)
                   body)))
         (ret (omg-dyn-create-pull title target-repo target-branch
                                   source-head draft body))
         (pull-link (format "https://github.com/%s/pull/%s" target-repo
                            (plist-get ret 'number))))
    (message "Pull created: %s, link: %s" ret pull-link)
    (kill-buffer)
    (when omg-pull-open-in-browser
      (browse-url-default-browser pull-link))))

(defun omg-pull-cancel ()
  (interactive)
  (when (y-or-n-p "Cancel this PR, are you sure?")
      (kill-buffer)))

(defvar omg-pull-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-c C-c") 'omg-pull-submit)
    (define-key map (kbd "C-c C-k") 'omg-pull-cancel)
    map)
  "Local keymap for omg-pull-create-pull-mode buffers.")

(define-derived-mode omg-pull-mode org-mode "omg-pull" "Create pull request for GitHub repository")

(defun omg-pull--get-template (root-dir)
  (let* ((template-file (concat root-dir "/.github/pull_request_template.md")))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        (buffer-string)))))

(defun omg-pull--guess-target-repo (url)
  (if (string-prefix-p "http" url)
      ;; https://github.com/jiacai2050/oh-my-github.git
      (thread-last url
                   (string-remove-prefix "https://github.com/")
                   (string-remove-suffix ".git"))
    ;; git@github.com:jiacai2050/oh-my-github.git
    (thread-last url
                 (string-remove-prefix "git@github.com:")
                 (string-remove-suffix ".git"))))

;;;###autoload
(defun omg-pull-create ()
  "Create PR based on current branch."
  (interactive)
  (if-let (root-dir (vc-git-responsible-p default-directory))
      (setq-local omg-pull--repo-root root-dir)
    (error "Not in a git repository"))

  (let* ((current-branch (omg--execute "branch" "--show-current"))
         (meta (concat "#+TITLE: "
                       (when omg-pull-title-from-commit
                          (omg--execute "show-branch" "--no-name" "HEAD"))
                       "\n#+TARGET-REPO: "
                       (or omg-pull-target-repo
                           (when-let ((url (omg--execute "remote" "get-url" omg-pull-guess-target-repo-from-remote)))
                             (omg-pull--guess-target-repo url)))
                       "\n#+TARGET-BRANCH: "
                       omg-pull-target-branch
                       "\n#+SOURCE-HEAD: "
                       (if omg-pull-username
                           (format "%s:%s" omg-pull-username current-branch)
                         current-branch)
                       "\n#+DRAFT: "
                       omg-pull-draft
                       "\n"))
         (template (omg-pull--get-template omg-pull--repo-root)))
    (with-current-buffer (get-buffer-create (format omg-pull--buf-basename omg-pull--repo-root))
      (beginning-of-buffer)
      (erase-buffer)
      (insert meta)
      (when template
        (insert (format "\n#+begin_export markdown\n%s\n#+end_export" template)))
      (omg-pull-mode)
      (setq header-line-format (substitute-command-keys omg-pull--header))
      (beginning-of-buffer)
      (switch-to-buffer (current-buffer)))))

(provide 'omg-pull)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-pull.el ends here
