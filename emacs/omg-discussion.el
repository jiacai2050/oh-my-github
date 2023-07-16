;;; -*- lexical-binding: t -*-

(require 'org)
(require 'ox-md)
(require 'omg-dyn)

(defcustom omg-discussion-open-in-browser t
  "If non-nil open discussion link in browser via `browse-url-default-browser' after created."
  :group 'omg
  :type 'boolean)

(defvar-local omg-discussion-repo-id nil)
(defvar-local omg-discussion-category-id nil)

(defvar omg-discussion--buf-basename "*omg-discussion create(%s)*")
(defvar omg-discussion--repo-root nil)
(defconst omg-discussion--header
  "Edit, then submit with `\\[omg-discussion-submit]', or cancel with `\\[omg-discussion-cancel]'")

(defun omg-discussion-submit ()
  (interactive)
  (setq-local org-export-options-alist '((:title "TITLE" nil nil t)
                                         (:repo-id "REPO-ID" nil nil t)
                                         (:category-id "CATEGORY-ID" nil nil t)))
  (let* ((metadata (org-export-get-environment))
         (title (plist-get metadata :title))
         (repo-id (plist-get metadata :repo-id))
         (category-id (plist-get metadata :category-id))
         (body (with-current-buffer (org-export-to-buffer 'md "*OMG-DISCUSSION export*")
                 (let ((body (buffer-substring-no-properties (point-min) (point-max))))
                   (kill-buffer)
                   body)))
         (ret (omg-dyn-create-discussion repo-id category-id title body))
         (link (plist-get ret 'url)))
    (message "Discussion created: %s" ret)
    (kill-buffer)
    (when omg-discussion-open-in-browser
      (browse-url-default-browser link))))

(defun omg-discussion-cancel ()
  (interactive)
  (when (y-or-n-p "Cancel this discussion, are you sure?")
      (kill-buffer)))

(defvar omg-discussion-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-c C-c") 'omg-discussion-submit)
    (define-key map (kbd "C-c C-k") 'omg-discussion-cancel)
    map)
  "Local keymap for omg-discussion-create-discussion-mode buffers.")

(define-derived-mode omg-discussion-mode org-mode "omg-discussion" "Create discussion for GitHub repository")

;;;###autoload
(defun omg-discussion-create ()
  (interactive)
  (let* ((meta (concat "#+TITLE:"
                       "\n#+REPO-ID:"
                       omg-discussion-repo-id
                       "\n#+CATEGORY-ID:"
                       omg-discussion-category-id
                       "\n")))
    (with-current-buffer (get-buffer-create (format omg-discussion--buf-basename
                                                    omg-discussion-repo-id))
      (beginning-of-buffer)
      (erase-buffer)
      (insert meta)
      (omg-discussion-mode)
      (setq header-line-format (substitute-command-keys omg-discussion--header))
      (beginning-of-buffer)
      (switch-to-buffer (current-buffer)))))

(provide 'omg-discussion)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-discussion.el ends here
