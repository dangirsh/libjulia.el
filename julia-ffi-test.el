(require 'ert)

(add-to-list 'load-path (concat default-directory "/emacs-ffi"))

(require 'ffi)

(define-ffi-library julia-core.so "julia-core.so")

(define-ffi-function test-not "test_not" :bool (:bool) julia-core.so)

(define-ffi-function jl-eval-string "jl_eval_string" :pointer (:pointer) julia-core.so)

(define-ffi-function jl-unbox-float64 "jl_unbox_float32" :float (:pointer) julia-core.so)

(define-ffi-function jl-unbox-int64 "jl_unbox_int64" :int (:pointer) julia-core.so)

(define-ffi-function jl-typeof "jl_typeof_str" :pointer (:pointer) julia-core.so)

(defun jl-get-type (julia-expr-str)
  (ffi-get-c-string (jl-typeof (jl-eval-string (ffi-make-c-string julia-expr-str)))))

(ert-deftest test-julia-ffi-get-type ()
  (should (equal (jl-get-type "3") "Int64")))

(defun jl-eval-int (int-str-expr)
  (jl-unbox-int64 (jl-eval-string (ffi-make-c-string int-str-expr))))

(ert-deftest test-julia-ffi-eval-int ()
  (should (eq (jl-eval-int "3") 3))
  (should (eq (jl-eval-int "-3") -3))
  (let ((r (random)))
    (should (eq (jl-eval-int (format "r" r)) r))))

(defun jl-eval-float (fl-str-expr)
  (jl-unbox-float64 (jl-eval-string (ffi-make-c-string fl-str-expr))))

(ert-deftest test-julia-ffi-eval-float ()
  (should (eq (jl-eval-float "3.14") 3.1)))

(ert-deftest test-julia-ffi-boolean ()
  (should (eq (test-not nil) t))
  (should (eq (test-not t) nil))
  (should (eq (test-not 0) nil))
  (should (eq (test-not "hi") nil)))

;; (define-ffi-function test-function "test_function" :int nil julia-core.so)
;; (ert-deftest julia-ffi-basic ()
;;   (should (= (test-function) 27)))
