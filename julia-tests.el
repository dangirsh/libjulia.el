(require 'julia)
(require 'ert)

(ert-deftest julia-simple-test ()
  (should (= (julia-sqrt2) (sqrt 2))))

(ert-deftest julia-eval-exception-test ()
  (let ((ret (julia-eval "undefined_function(1)")))
    (should (equal ret "UndefVarError"))
    (should (stringp ret))))

(ert-deftest julia-eval-float-test ()
  (let ((ret (julia-eval "sqrt(7)")))
    (should (= ret (sqrt 7)))
    (should (floatp ret))))

(ert-deftest julia-eval-int-test ()
  (let ((ret (julia-eval "1 + 1")))
    (should (= ret 2))
    (should (integerp ret))))

(ert-deftest julia-eval-string-test ()
  (let* ((str "Hello from embedded Julia!")
         (ret (julia-eval (format "\"%s\"" str))))
    (should (equal ret str))
    (should (stringp ret))))
