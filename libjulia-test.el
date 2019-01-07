(require 'ert)
(require 'libjulia)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Comparison-of-Numbers.html
(defun float-equal (x y)
  (let ((float-tol 1.0e-6))
    (or (= x y)
        (< (/ (abs (- x y))
              (max (abs x) (abs y)))
           float-tol))))

(ert-deftest test-libjulia-primitive-julia-type-p ()
  (should (libjulia-primitive-julia-type-p "Bool"))
  (should (libjulia-primitive-julia-type-p "Int64"))
  (should (libjulia-primitive-julia-type-p "Float64"))
  (should-not (libjulia-primitive-julia-type-p "String"))
  (should-not (libjulia-primitive-julia-type-p "SomethingElse")))

(ert-deftest test-libjulia-get-type ()
  ;; Ints
  (should (equal (libjulia-get-julia-type "3") "Int64"))
  (should (equal (libjulia-get-julia-type "typemax(Int32)") "Int32"))
  (should (equal (libjulia-get-julia-type "typemin(Int64)") "Int64"))
  (should (equal (libjulia-get-julia-type "0x1") "UInt8"))
  ;; negating an unsigned literal creates an unsigned 2s complement
  (should (equal (libjulia-get-julia-type "-0x1000") "UInt16"))

  ;; floats
  (should (equal (libjulia-get-julia-type "3.14f0") "Float32"))
  (should (equal (libjulia-get-julia-type "3.14") "Float64")))

;; Implicitly tests unboxing
(ert-deftest test-libjulia-eval-str ()
  ;; primitives
  (should (equal (libjulia-eval-str "3") 3))
  (should (float-equal (libjulia-eval-str "3.14f0") 3.14))
  (should (float-equal (libjulia-eval-str "3.14") 3.14))

  ;; strings
  (should (equal (libjulia-eval-str "\"Hallo\"") "Hallo")))

(ert-deftest test-libjulia-box-unbox ()

  (defun box-unbox (elisp-val julia-type)
    (let ((box (libjulia--get-jl-box-sym julia-type))
          (unbox (libjulia--get-jl-unbox-sym julia-type)))
      (funcall unbox (funcall box elisp-val))))


  (defun test-roundrip-is-identity (elisp-val julia-type)
    (should (equal (box-unbox elisp-val julia-type) elisp-val)))

  (test-roundrip-is-identity 3 "Int64")
  (test-roundrip-is-identity 3.4 "Float64")
  (test-roundrip-is-identity 't "Bool"))

(ert-deftest test-ffi-array ()
  (define-ffi-array test-array :int64 8)
  (ffi-set-aref test-array :int64 0 42)
  (should (equal (ffi-aref test-array :int64 0) 42))

  (ffi-set-aref test-array :int64 7 23)
  (should (equal (ffi-aref test-array :int64 7) 23)))


(ert-deftest test-libjulia-jl-call ()
  (should (equal 8.0 (libjulia-jl-call "sqrt" '(64.0))))
  (should (equal 2 (libjulia-jl-call "+" '(1 1))))
  (should (equal t (libjulia-jl-call "!" '(nil)))))

(ert-deftest test-libjulia-eval-sexpr ()
  (should (equal 8.0 (libjulia-eval-sexpr '(call sqrt 64.0))))
  (should (equal 2 (libjulia-eval-sexpr '(call + 1 1 ))))
  (should (equal t (libjulia-eval-sexpr '(call ! nil)))))



;; (ert-deftest test-libjulia-null-ptr ()
;;   (unwind-protect
;;       (setq res (libjulia-eval "Julia sees this string unquoted, which returns a null pointer from jl_eval_string."))
;;     (should nil)
;;     (message (format "%s" res))))
