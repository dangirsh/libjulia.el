(require 'ert)

(add-to-list 'load-path (concat default-directory "/emacs-ffi"))

(require 'ffi)

(define-ffi-library julia-core.so "julia-core.so")

(define-ffi-function jl-eval-string "jl_eval_string" :pointer (:pointer) julia-core.so)

(define-ffi-function jl-unbox-float64 "jl_unbox_float64" :float (:pointer) julia-core.so)

(define-ffi-function jl-unbox-int64 "jl_unbox_int64" :int (:pointer) julia-core.so)

(define-ffi-function jl-string-ptr "jl_string_ptr" :pointer (:pointer) julia-core.so)

(define-ffi-function jl-typeof "jl_typeof_str" :pointer (:pointer) julia-core.so)

(defun jl-get-type (julia-expr-str)
  (ffi-get-c-string (jl-typeof (jl-eval-string (ffi-make-c-string julia-expr-str)))))

(ert-deftest test-julia-ffi-get-type ()
  (should (equal (jl-get-type "3") "Int64")))


(defun julia-eval (julia-expr-str)
  (let* ((ptr (jl-eval-string (ffi-make-c-string julia-expr-str)))
         (type-str (ffi-get-c-string (jl-typeof ptr))))
    (message (format "%s\n" type-str))
    (cond
     ((equal type-str "Int64")
      (jl-unbox-int64 ptr))
     ((equal type-str "Float64")
      (jl-unbox-float64 ptr))
     ((equal type-str "String")
      (ffi-get-c-string (jl-string-ptr ptr)))
     (t
      (error (format "Unboxing of Julia type %s unimplemented" type-str))))))

(ert-deftest test-julia-eval ()
  (should (eq (julia-eval "3") 3))
  (should (eq (julia-eval "3.14") 3.14))
  (should (equal (julia-eval "\"3.14\"") "3.14")))
