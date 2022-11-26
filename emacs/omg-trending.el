;;; -*- lexical-binding: t -*-

(require 'omg-dyn)
(require 'tabulated-list)
(require 'seq)
(require 'omg-repo)

(defcustom omg-trending-default-spoken-language nil
  "2-letter spoken language code used when query trendings.
For more 2-letter codes, see https://www.w3.org/International/O-charset-lang.html"
  :group 'omg
  :type '(choice (string :tag "Spoken Language")
                 (const :tag "Any" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq omg-trending--query-spoken-language value)))

(defcustom omg-trending-default-language nil
  "Programming Language used when query trendings."
  :group 'omg
  :type '(choice (string :tag "Programming Language")
                 (const :tag "Any" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq-default omg-trending--query-language value)))

(defconst omg-trending--ranges '("daily" "weekly" "monthly"))

(defcustom omg-trending-default-range "daily"
  "Range used when query trendings."
  :group 'omg
  :type 'string
  :options omg-trending--ranges
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq-default omg-trending--query-range value)))

(defconst omg-trending--spoken-languages  #s(hash-table
                                                       test equal
                                                       data ("Any" nil
                                                             "Chinese" "zh"
                                                             "Dutch" "nl"
                                                             "English" "en"
                                                             "French" "fr"
                                                             "Japanese" "ja"
                                                             "Korean" "ko"
                                                             "Polish" "pl"
                                                             "Russian" "ru"
                                                             "Spanish" "es"
                                                             "Ukrainian" "uk"))
  "Port from https://github.com/huchenme/github-trending-api/blob/master/src/spoken-languages.json")

(defconst omg-trending--languages  #s(hash-table
                                                test equal
                                                data ("Any" nil
                                                      "C" "c"
                                                      "Clojure" "clojure"
                                                      "CMake" "cmake"
                                                      "CoffeeScript" "coffeescript"
                                                      "Common Lisp" "common-lisp"
                                                      "Coq" "coq"
                                                      "Dart" "dart"
                                                      "Dockerfile" "dockerfile"
                                                      "Emacs Lisp" "emacs-lisp"
                                                      "GDB" "gdb"
                                                      "Go" "go"
                                                      "Haskell" "haskell"
                                                      "Haxe" "haxe"
                                                      "HTML" "html"
                                                      "Java" "java"
                                                      "JavaScript" "javascript"
                                                      "JSON" "json"
                                                      "Julia" "julia"
                                                      "Jupyter Notebook" "jupyter-notebook"
                                                      "Kotlin" "kotlin"
                                                      "LLVM" "LLVM"
                                                      "Lua" "lua"
                                                      "Makefile" "makefile"
                                                      "Markdown" "markdown"
                                                      "Mathematica" "mathematica"
                                                      "Matlab" "matlab"
                                                      "Nginx" "nginx"
                                                      "Nix" "nix"
                                                      "NumPy" "numpy"
                                                      "Objective-C" "objective-c"
                                                      "OCaml" "ocaml"
                                                      "Org" "org"
                                                      "Perl" "perl"
                                                      "PHP" "php"
                                                      "PLSQL" "plsql"
                                                      "PowerShell" "powershell"
                                                      "Protocol Buffer" "protocol-buffer"
                                                      "Python" "python"
                                                      "Ruby" "ruby"
                                                      "Rust" "rust"
                                                      "Scala" "scala"
                                                      "Shell" "shell"
                                                      "SQL" "sql"
                                                      "Swift" "swift"
                                                      "TypeScript" "typescript"
                                                      "Unix Assembly" "unix-assembly"
                                                      "Vim script" "vim-script"
                                                      "Vue" "vue"
                                                      "WebAssembly" "webassembly"
                                                      "Zig" "zig"))
  "Port from https://github.com/huchenme/github-trending-api/blob/master/src/languages.json")

(defvar omg-trending--query-spoken-language omg-trending-default-spoken-language
  "Spoken language used when query trendings repos.")

(defvar omg-trending--query-language omg-trending-default-language
  "Language used when query trendings repos.")

(defvar omg-trending--query-range omg-trending-default-range
  "Range used when query trendings repos.")

(defun omg-trending--query ()
  (seq-into (omg-dyn-query-trendings omg-trending--query-spoken-language
                                     omg-trending--query-language
                                     omg-trending--query-range)
            'list))

(defun omg-trending--get-buf-name ()
  (format "*omg-trending [%s]-[%s]-[%s] trendings repos*"
          (capitalize (or omg-trending--query-spoken-language "any"))
          (capitalize (or omg-trending--query-language "any"))
          (capitalize omg-trending--query-range)))

(defun omg-trending-revert (&optional revert)
  (setq omg-trending--query-spoken-language omg-trending-default-spoken-language)
  (setq omg-trending--query-language omg-trending-default-language)
  (setq omg-trending--query-range omg-trending-default-range)
  (rename-buffer (omg-trending--get-buf-name) t))

(defun omg-trending-query (spoken-language language range)
  (interactive (list (completing-read  "Spoken language: " omg-trending--spoken-languages)
                     (completing-read  "Programming Language: " omg-trending--languages)
                     (completing-read "Range: " omg-trending--ranges)))
  (when (eq major-mode 'omg-trending-mode)
    (let ((spoken-language-code (gethash spoken-language omg-trending--spoken-languages
                                         spoken-language))
          (programming-lang (gethash language omg-trending--languages language)))
      (setq omg-trending-query-spoken-language spoken-language-code)
      (setq omg-trending--query-language programming-lang)
      (setq omg-trending--query-range range))
    (tabulated-list-print t)
    (rename-buffer (omg-trending--get-buf-name) t)))

(defvar omg-trending-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map omg-repo-mode-map)
    (define-key map (kbd "s") 'omg-trending-query)
    map)
  "Local keymap for omg-trending-stars mode buffers.")

(define-derived-mode omg-trending-mode omg-repo-mode "omg-trending" "Display trending repository"
  (setq tabulated-list-format [("Repository" 25)
                               ("Recent Stars" 12 t)
                               ("Description" 5)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil
        tabulated-list-entries 'omg-trending--query)

  (add-hook 'tabulated-list-revert-hook 'omg-trending-revert nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun omg-trending-list ()
  (interactive)
  (with-current-buffer (get-buffer-create (omg-trending--get-buf-name))
    (omg-trending-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'omg-trending)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-trending.el ends here
