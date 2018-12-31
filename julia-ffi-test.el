(require 'ert)

(add-to-list 'load-path (concat default-directory "emacs-ffi"))

(require 'ffi)
(require 's)

(define-ffi-library libjulia.so "libjulia.so")

(defun elisp-to-c-name (elisp-name)
  (format "%s" (s-replace "-" "_" elisp-name)))

(defmacro libjulia-bind (name arg-types return-type)
  "Wrapper around define-ffi-function from the ffi library."
  (message (format "Generating Julia binding for %s." name))
  `(define-ffi-function
     ,name
     ,(elisp-to-c-name (symbol-name name))
     ,return-type
     ,arg-types
     libjulia.so))

(setq julia-fii-type-map
      '(("int8" . :int)
        ("uint8" . :int)
        ("int16" . :int)
        ("uint16" . :int)
        ("int32" . :int)
        ("uint32" . :int)
        ("int64" . :int)
        ("uint64" . :int)
        ("int64" . :int)
        ("float32" . :float)
        ("float64" . :double)))


(defun julia-ffi-gen-unboxers ()
  (dolist (type-entry julia-fii-type-map)
    (eval `(libjulia-bind
            ,(intern  (format "jl-unbox-%s" (car type-entry)))
            (:pointer)
            ,(cdr type-entry)))))


(defun julia-ffi-init ()
  (julia-ffi-gen-unboxers))


(libjulia-bind jl-string-ptr (:pointer) :pointer)
(libjulia-bind jl-typeof-str (:pointer) :pointer)
(libjulia-bind jl-eval-string (:pointer) :pointer)


(defun jl-get-type (julia-expr-str)
  (ffi-get-c-string (jl-typeof-str (jl-eval-string (ffi-make-c-string "3")))))

(ert-deftest test-julia-ffi-get-type ()
  (should (equal (jl-get-type "3") "Int64")))


(defun julia-unbox (type-str ptr)
  (pcase type-str
    ("Int64"
     (jl-unbox-int64 ptr))
    ("Float64"
     (jl-unbox-float64 ptr))
    ("String"
     (ffi-get-c-string (jl-string-ptr ptr)))
    (_
     (error (format "Unboxing of Julia type %s unimplemented" type-str)))))


(defun julia-eval (julia-expr-str)
  (let* ((ptr (jl-eval-string (ffi-make-c-string julia-expr-str)))
         (type-str (ffi-get-c-string (jl-typeof-str ptr))))
    (julia-unbox type-str ptr)))

(ert-deftest test-julia-eval ()
  (should (equal (julia-eval "3") 3))
  (should (equal (julia-eval "3.14") 3.14))
  (should (equal (julia-eval "\"Hallo\"") "Hallo")))
