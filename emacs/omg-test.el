;;; -*- lexical-binding: t -*-

(add-to-list 'load-path default-directory)

(require 'ert)
(require 'omg)

(setq omg-db-file "/tmp/omg-test.db"
      omg-download-directory "/tmp"
      omg-pat (getenv "GITHUB_TOKEN"))

(omg-setup)

(ert-deftest test-download ()
  (let ((urls '("https://httpbin.org/ip" . "ip.json"
                "https://github.com/" . "github.html")))
    (dolist (url-file urls)
      (omg--download-file (cdr url-file) (car url-file)))

    ;; wait downloading
    (sleep-for 5)

    (dolist (url-file urls)
      (let* ((file (cdr url-file))
             (attrs (file-attributes (concat "/tmp/" file))))
        (should (> (file-attribute-size attrs) 0))))
    ))
