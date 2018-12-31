(add-to-list 'load-path (concat default-directory "emacs-ffi"))

(require 'ffi)
(require 's)

(define-ffi-library libjulia.so "libjulia.so")

(defun julia-ffi-elisp-to-c-name (elisp-name)
  (format "%s" (s-replace "-" "_" elisp-name)))

(defmacro julia-ffi-bind (name arg-types return-type)
  "Wrapper around define-ffi-function from the ffi library."
  (message (format "Generating Julia binding for %s." name))
  `(define-ffi-function
     ,name
     ,(julia-ffi-elisp-to-c-name (symbol-name name))
     ,return-type
     ,arg-types
     libjulia.so))

(setq julia-ffi-type-map
      '(("int8" "Int8" :int)
        ("uint8" "UInt8" :int)
        ("int16" "Int16" :int)
        ("uint16" "UInt16" :int)
        ("int32" "Int32" :int)
        ("uint32" "UInt32" :int)
        ("int64" "Int64" :int)
        ("uint64" "UInt64" :int)
        ("int64" "Int64" :int)
        ("float32" "Float32" :float)
        ("float64" "Float64" :double)))

(defun julia-ffi-gen-unboxers ()
  (dolist (type-entry julia-ffi-type-map)
    ;; (message (format "%s" type-entry))
    ;; (eval `(julia-ffi-bind
    ;;         ,(intern  (format "jl-unbox-%s" (car type-entry)))
    ;;         (:pointer)
    ;;         ,(caddr type-entry)))
    (pcase-let ((`(,c-type ,_ ,emacs-type) type-entry))
      (eval `(julia-ffi-bind
              ,(intern  (format "jl-unbox-%s" c-type))
              (:pointer)
              ,emacs-type)))
    ))


(defun julia-ffi-get-julia-type (julia-expr-str)
  (ffi-get-c-string (jl-typeof-str (jl-eval-string (ffi-make-c-string julia-expr-str)))))

(defun julia-ffi-unbox (type-str ptr)
  (pcase type-str
    ("Int64"
     (jl-unbox-int64 ptr))
    ("Float64"
     (jl-unbox-float64 ptr))
    ("String"
     (ffi-get-c-string (jl-string-ptr ptr)))
    (_
     (error (format "Unboxing of Julia type %s unimplemented" type-str)))))


(defun julia-ffi-eval (julia-expr-str)
  (let* ((ptr (jl-eval-string (ffi-make-c-string julia-expr-str)))
         (type-str (ffi-get-c-string (jl-typeof-str ptr))))
    (julia-ffi-unbox type-str ptr)))

(defun julia-ffi-init ()
  (julia-ffi-gen-unboxers)
  (julia-ffi-bind jl-string-ptr (:pointer) :pointer)
  (julia-ffi-bind jl-typeof-str (:pointer) :pointer)
  (julia-ffi-bind jl-eval-string (:pointer) :pointer))

(julia-ffi-init)

(provide 'julia-ffi)
