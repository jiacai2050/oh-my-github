;;; -*- lexical-binding: t -*-

(add-to-list 'load-path default-directory)

(require 'ert)
(require 'omg)

(setq omg-db-file (make-temp-file "omg-test.db")
      omg-download-directory (make-temp-file "download" t))

(omg-setup (getenv "GITHUB_TOKEN"))

(defmacro comment (&rest body)
  "A macro that does nothing, effectively creating a comment block."
  nil)

(comment
 (ert-deftest test-download ()
   (let ((urls '(("https://httpbin.org/ip" . "ip.json")
                 ("https://github.com/" . "github.html"))))
     (dolist (url-file urls)
       (omg--download-file (cdr url-file) (car url-file)))

     ;; wait downloading
     (sleep-for 5)

     (dolist (url-file urls)
       (let* ((file (cdr url-file))
              (filepath (format "%s/%s" omg-download-directory file))
              (attrs (file-attributes filepath)))
         (should (> (file-attribute-size attrs) 0)))))))

(ert-deftest test-whoami ()
  (with-current-buffer (omg-whoami "jiacai2050")
    (let ((info (buffer-string)))
      (message "info is %s" info)
      (should (string-match-p "jiacai2050" info)))))

(comment
 (ert-deftest test-sync ()
   (omg-sync)
   ;; wait sync
   (sleep-for 20)

   (should (> (length (omg-repo--query-created)) 0))
   (should (> (length (omg-repo--query-starred)) 0))
   (should (> (length (omg-gist--query-created)) 0))
   (should (> (length (omg-gist--query-starred)) 0))))

(ert-deftest test-pull-guess-target-repo ()
  (should (string-equal "jiacai2050/oh-my-github" (omg-pull--guess-target-repo "git@github.com:jiacai2050/oh-my-github.git")))
  (should (string-equal "jiacai2050/oh-my-github" (omg-pull--guess-target-repo "https://github.com/jiacai2050/oh-my-github.git"))))
