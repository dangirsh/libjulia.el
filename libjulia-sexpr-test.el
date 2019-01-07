(require 'ert)
(require 'libjulia-sexpr)

(ert-deftest test-libjulia-sexpr ()
  (should (equal
           (libjulia-sexpr-from-julia "1 + 1")
           '(:call :+ 1 1)))
  (should (equal (libjulia-eval-sexpr
                  (libjulia-sexpr-from-julia "1 + 1"))
                 2)))
