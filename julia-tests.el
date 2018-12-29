(require 'julia)
(require 'ert)

(ert-deftest julia-smoke-test ()
  (should (= (julia-sqrt2) (sqrt 2))))

(ert-deftest julia-eval-float-test ()
  (should (= (julia-eval "sqrt(7)") (sqrt 7)))
  (should (floatp (julia-eval "sqrt(7)"))))

(ert-deftest julia-eval-int-test ()
  (should (= (julia-eval "1 + 1") 2))
  (should (integerp (julia-eval "1 + 1"))))
