;;; -*- lexical-binding: t -*-

(require 'org)
(require 'ox-md)
(require 'omg-dyn)
(require 'vc)

(defcustom omg-discussion-open-in-browser t
  "If non-nil open discussion link in browser via `browse-url-default-browser' after created."
  :group 'omg
  :type 'boolean)

;;;###autoload
(defun omg-discussion-create (repo-id category-id title text)
  (let* ((ret  (omg-dyn-create-discussion repo-id category-id title text))
         (link (plist-get ret 'url)))
    (message "Discussion created, ret:%s." ret)
    (when omg-discussion-open-in-browser
      (browse-url-default-browser link))))

(provide 'omg-discussion)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-discussion.el ends here
