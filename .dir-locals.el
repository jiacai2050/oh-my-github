;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((c-mode . ((flycheck-cppcheck-standards . "gnu99")
            (flycheck-clang-include-path . ("/opt/homebrew/Cellar/jansson/2.14/include"
                                            "/opt/homebrew/Cellar/pcre2/10.40/include"))
            (eval . (progn
                      (eglot-ensure)
                      (add-hook 'before-save-hook 'eglot-format nil t))))))
