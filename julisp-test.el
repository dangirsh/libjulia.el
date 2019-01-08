(require 'ert)
(require 'julisp-mode)

(ert-deftest test-julisp ()
  (should (equal
           (julisp-sexpr-from-julia "1 + 1")
           '(:call :+ 1 1)))
  (should (equal 2 (julisp-eval-sexpr
                    (julisp-sexpr-from-julia "1 + 1"))))
  ;; (should (equal 1 (julisp-eval-sexpr
  ;;                   (julisp-sexpr-from-julia "a = 1"))))
  ;; (should (equal 2 (julisp-eval-sexpr
  ;;                   (julisp-sexpr-from-julia "a = 1; a + a"))))
  )
