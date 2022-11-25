;;; -*- lexical-binding: t -*-

(defconst oh-my-github-whoami-col-sep ",,,")
(defconst oh-my-github-whoami-row-sep "\n")

(defun oh-my-github--whoami-insert-row (label value)
  (insert label
          oh-my-github-whoami-col-sep
          (if (numberp value)
              (number-to-string value)
            value)
          oh-my-github-whoami-row-sep))

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

(provide 'oh-my-github-whoami)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github-whoami.el ends here
