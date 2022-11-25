;;; -*- lexical-binding: t -*-

(require 'omg-dyn)
(require 'tabulated-list)
(require 'seq)

(defcustom oh-my-github-trendings-default-spoken-language nil
  "2-letter spoken language code used when query trendings.
For more 2-letter codes, see https://www.w3.org/International/O-charset-lang.html"
  :group 'oh-my-github
  :type '(choice (string :tag "Spoken Language")
                 (const :tag "Any" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq-default oh-my-github-trendings-query-language value)))

(defcustom oh-my-github-trendings-default-language nil
  "Programming Language used when query trendings."
  :group 'oh-my-github
  :type '(choice (string :tag "Programming Language")
                 (const :tag "Any" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq-default oh-my-github-trendings-query-language value)))

(defconst oh-my-github--trendings-ranges '("daily" "weekly" "monthly"))

(defcustom oh-my-github-trendings-default-range "daily"
  "Range used when query trendings."
  :group 'oh-my-github
  :type 'string
  :options oh-my-github--trendings-ranges
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq-default oh-my-github-trendings-query-range value)))

(defconst oh-my-github--trendings-spoken-languages  #s(hash-table
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

(defconst oh-my-github--trendings-languages  #s(hash-table
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

(defvar-local oh-my-github-trendings-query-spoken-language oh-my-github-trendings-default-spoken-language
  "Spoken language used when query trendings repos.")

(defvar-local oh-my-github-trendings-query-language oh-my-github-trendings-default-language
  "Language used when query trendings repos.")

(defvar-local oh-my-github-trendings-query-range oh-my-github-trendings-default-range
  "Range used when query trendings repos.")

(defun oh-my-github--query-trendings ()
  (seq-into (omg-dyn-query-trendings oh-my-github-trendings-query-spoken-language
                                     oh-my-github-trendings-query-language
                                     oh-my-github-trendings-query-range)
            'list))

(defun oh-my-github--trendings-buf-name ()
  (format "*oh-my-github [%s]-[%s]-[%s] trendings repos*"
          (capitalize (or oh-my-github-trendings-query-spoken-language "any"))
          (capitalize (or oh-my-github-trendings-query-language "any"))
          (capitalize oh-my-github-trendings-query-range)))

(defun oh-my-github-trendings-revert (&optional revert)
  (setq-local oh-my-github-trendings-query-spoken-language oh-my-github-trendings-default-spoken-language)
  (setq-local oh-my-github-trendings-query-language oh-my-github-trendings-default-language)
  (setq-local oh-my-github-trendings-query-range oh-my-github-trendings-default-range)
  (rename-buffer (oh-my-github--trendings-buf-name) t))

(defun oh-my-github-trendings-query (spoken-language language range)
  (interactive (list (completing-read  "Spoken language: " oh-my-github--trendings-spoken-languages)
                     (completing-read  "Programming Language: " oh-my-github--trendings-languages)
                     (completing-read "Range: " oh-my-github--trendings-ranges)))
  (when (eq major-mode 'oh-my-github-trendings-mode)
    (let ((spoken-language-code (gethash spoken-language oh-my-github--trendings-spoken-languages
                                         spoken-language))
          (programming-lang (gethash language oh-my-github--trendings-languages language)))
      (setq-local oh-my-github-trendings-query-spoken-language spoken-language-code)
      (setq-local oh-my-github-trendings-query-language programming-lang)
      (setq-local oh-my-github-trendings-query-range range))
    (tabulated-list-print t)
    (rename-buffer (oh-my-github--trendings-buf-name) t)))

(defvar oh-my-github-trendings-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map oh-my-github-repos-mode-map)
    (define-key map (kbd "s") 'oh-my-github-trendings-query)
    map)
  "Local keymap for oh-my-github-stars mode buffers.")

(define-derived-mode oh-my-github-trendings-mode oh-my-github-repos-mode "oh-my-github trendings" "Display Trendings of GitHub repository"
  (setq tabulated-list-format [("Repository" 25)
                               ("Recent Stars" 12 t)
                               ("Description" 5)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil
        tabulated-list-entries 'oh-my-github--query-trendings)

  (add-hook 'tabulated-list-revert-hook 'oh-my-github-trendings-revert nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun oh-my-github-list-trending-repositories ()
  (interactive)
  (with-current-buffer (get-buffer-create (oh-my-github--trendings-buf-name))
    (oh-my-github-trendings-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'oh-my-github-trending)

;; Local Variables:
;; coding: utf-8
;; End:

;;; oh-my-github-trending.el ends here
