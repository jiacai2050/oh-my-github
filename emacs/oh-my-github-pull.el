;;; -*- lexical-binding: t -*-

(require 'org)
(require 'ox-md)
(require 'omg-dyn)

(defcustom oh-my-github-pull-open-in-browser t
  "If non-nil open PR link in browser via `browse-url-default-browser' after created."
  :group 'oh-my-github
  :type 'boolean)

(defcustom oh-my-github-pull-title-from-commit t
  "If non-nil, use last commit message as pull title"
  :group 'oh-my-github
  :type 'boolean)

(defvar-local oh-my-github-pull-target-repo ""
  "The full name of the repository you want the changes pulled into.
Such as `octocat/Hello-World'")

(defvar-local oh-my-github-pull-target-branch "main"
  "The name of the branch you want the changes pulled into.
This should be an existing branch on the current repository.")

(defvar-local oh-my-github-pull-username nil
  "Your GitHub username")

(defvar-local oh-my-github-pull-draft "false")

(defvar oh-my-github--pull-buf-basename "*OH-MY-GITHUB Create Pull(%s)*")
(defvar oh-my-github--pull-repo-root nil)
(defconst oh-my-github--pull-header
  "Edit, then submit with `\\[oh-my-github-pull-submit]', or cancel with `\\[oh-my-github-pull-cancel]'")

(defun oh-my-github-pull-submit ()
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
         (body (with-current-buffer (org-export-to-buffer 'md "*OH-MY-GITHUB pull body export*")
                 (let ((body (buffer-substring-no-properties (point-min) (point-max))))
                   (kill-buffer)
                   body)))
         (ret (omg-dyn-create-pull title target-repo target-branch
                                   source-head draft body))
         (pull-link (format "https://github.com/%s/pull/%s" target-repo
                            (plist-get ret 'number))))
    (message "Pull created: %s, link: %s" ret pull-link)
    (kill-buffer)
    (when oh-my-github-pull-open-in-browser
      (browse-url-default-browser pull-link))))

(defun oh-my-github-pull-cancel ()
  (interactive)
  (when (y-or-n-p "Cancel this PR, are you sure?")
      (kill-buffer)))

(defvar oh-my-github-pull-create-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-c C-c") 'oh-my-github-pull-submit)
    (define-key map (kbd "C-c C-k") 'oh-my-github-pull-cancel)
    map)
  "Local keymap for oh-my-github-create-pull-mode buffers.")

(define-derived-mode oh-my-github-pull-create-mode org-mode "oh-my-github create pull" "Create pull request for GitHub repository")

(defun oh-my-github-pull-get-template (root-dir)
  (let* ((template-file (concat root-dir "/.github/pull_request_template.md")))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        (buffer-string)))))

;;;###autoload
(defun oh-my-github-pull-create ()
  "Create PR based on current branch."
  (interactive)
  (if-let (root-dir (locate-dominating-file default-directory ".git"))
      (setq-local oh-my-github--pull-repo-root root-dir)
    (error "Not in a git repository"))

  (let* ((current-branch (string-trim (shell-command-to-string "git branch --show-current")))
         (meta (concat "#+TITLE: "
                       (when oh-my-github-pull-title-from-commit
                         (string-trim (shell-command-to-string "git show-branch --no-name HEAD")))
                       "\n#+TARGET-REPO: "
                       oh-my-github-pull-target-repo
                       "\n#+TARGET-BRANCH: "
                       oh-my-github-pull-target-branch
                       "\n#+SOURCE-HEAD: "
                       (if oh-my-github-pull-username
                           (format "%s:%s" oh-my-github-pull-username current-branch)
                         current-branch)
                       "\n#+DRAFT: "
                       oh-my-github-pull-draft
                       "\n"))
         (template (oh-my-github-pull-get-template oh-my-github--pull-repo-root)))
    (with-current-buffer (get-buffer-create (format oh-my-github--pull-buf-basename oh-my-github--pull-repo-root))
      (beginning-of-buffer)
      (erase-buffer)
      (insert meta)
      (when template
        (insert (format "\n#+begin_export markdown\n%s\n#+end_export" template)))
      (oh-my-github-pull-create-mode)
      (setq header-line-format (substitute-command-keys oh-my-github--pull-header))
      (switch-to-buffer (current-buffer)))))

(provide 'oh-my-github-pull)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github-pull.el ends here
