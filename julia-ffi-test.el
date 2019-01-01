(require 'ert)
(require 'julia-ffi)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Comparison-of-Numbers.html
(defun float-equal (x y)
  (let ((float-tol 1.0e-6))
    (or (= x y)
        (< (/ (abs (- x y))
              (max (abs x) (abs y)))
           float-tol))))

(ert-deftest test-julia-ffi-primitive-julia-type-p ()
  (should (julia-ffi-primitive-julia-type-p "Int64"))
  (should (julia-ffi-primitive-julia-type-p "Float64"))
  (should-not (julia-ffi-primitive-julia-type-p "String"))
  (should-not (julia-ffi-primitive-julia-type-p "SomethingElse")))

(ert-deftest test-julia-ffi-get-type ()
  ;; Ints
  (should (equal (julia-ffi-get-julia-type "3") "Int64"))
  (should (equal (julia-ffi-get-julia-type "typemax(Int32)") "Int32"))
  (should (equal (julia-ffi-get-julia-type "typemin(Int64)") "Int64"))
  (should (equal (julia-ffi-get-julia-type "0x1") "UInt8"))
  ;; negating an unsigned literal creates an unsigned 2s complement
  (should (equal (julia-ffi-get-julia-type "-0x1000") "UInt16"))

  ;; floats
  (should (equal (julia-ffi-get-julia-type "3.14f0") "Float32"))
  (should (equal (julia-ffi-get-julia-type "3.14") "Float64")))

;; Implicitly tests unboxing
(ert-deftest test-julia-ffi-eval ()
  ;; primitives
  (should (equal (julia-ffi-eval "3") 3))
  (should (float-equal (julia-ffi-eval "3.14f0") 3.14))
  (should (float-equal (julia-ffi-eval "3.14") 3.14))

  ;; strings
  (should (equal (julia-ffi-eval "\"Hallo\"") "Hallo")))

;; (ert-deftest test-julia-ffi-null-ptr ()
;;   (condition-case )
;;   (unwind-protect
;;       (julia-ffi-eval "Julia sees this string unquoted, which returns a null pointer from jl_eval_string.")
;;     (should nil)))
