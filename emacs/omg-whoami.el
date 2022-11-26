;;; -*- lexical-binding: t -*-

(defconst omg-whoami--col-sep ",,,")
(defconst omg-whoami--row-sep "\n")

(defun omg-whoami--insert-row (label value)
  (insert label
          omg-whoami--col-sep
          (if (numberp value)
              (number-to-string value)
            value)
          omg-whoami--row-sep))

;;;###autoload
(defun omg-whoami (&optional username)
  "Display `username' information, or current user represented by GitHub personal access token(PAT)."
  (interactive (list (when current-prefix-arg
                       (read-string "GitHub username: "))))
  (let ((buf (get-buffer-create "*omg-whoami*"))
        (who (omg-dyn-whoami username)))
    (with-current-buffer buf
	  (read-only-mode -1)
	  (erase-buffer)
	  (omg-whoami--insert-row "Created At" (plist-get who 'created-at))
	  (omg-whoami--insert-row "ID" (plist-get who 'id))
	  (omg-whoami--insert-row "Login" (plist-get who 'login))
	  (omg-whoami--insert-row "Name" (plist-get who 'name))
	  (omg-whoami--insert-row "Company" (plist-get who 'company))
	  (omg-whoami--insert-row "Blog" (plist-get who 'blog))
	  (omg-whoami--insert-row "Location" (plist-get who 'location))
	  (omg-whoami--insert-row "Email" (plist-get who 'email))
	  (omg-whoami--insert-row "Hireable" (plist-get who 'hireable))
	  (omg-whoami--insert-row "Public Repos" (plist-get who 'public-gists))
	  (omg-whoami--insert-row "Public Gists" (plist-get who 'public-gists))
	  (omg-whoami--insert-row "Private Repos" (plist-get who 'private-repos))
	  (omg-whoami--insert-row "Private Gists" (plist-get who 'private-gists))
	  (omg-whoami--insert-row "Followers" (plist-get who 'followers))
	  (omg-whoami--insert-row "Following" (plist-get who 'following))
	  (omg-whoami--insert-row "Disk Usage" (plist-get who 'disk-usage))
	  (table-capture (point-min) (point-max)
				     omg-whoami--col-sep omg-whoami--row-sep
                     'left 15)
	  (read-only-mode)
	  (switch-to-buffer buf))))

(provide 'omg-whoami)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-whoami.el ends here
