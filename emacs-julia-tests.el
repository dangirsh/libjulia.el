(require 'julia)
(require 'ert)

(ert-deftest julia-smoke-test ()
  (should (= (julia-sqrt2) (sqrt 2))))

(ert-deftest julia-eval-test ()
  (should (= (julia-eval "sqrt(7)") (sqrt 7))))
