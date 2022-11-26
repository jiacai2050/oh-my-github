;;; -*- lexical-binding: t -*-

(add-to-list 'load-path default-directory)

(require 'ert)
(require 'omg)

(setq omg-db-file (make-temp-file "omg-test.db")
      omg-download-directory (make-temp-file "download" t)
      omg-pat (getenv "GITHUB_TOKEN"))

(omg-setup)

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
        (should (> (file-attribute-size attrs) 0))))))

(ert-deftest test-sync ()
  (omg-sync)
  ;; wait sync
  (sleep-for 20)

  (should (> (length (omg-repo--query-created)) 0))
  (should (> (length (omg-repo--query-starred)) 0))
  (should (> (length (omg-gist--query-created)) 0))
  (should (> (length (omg-gist--query-starred)) 0)))
