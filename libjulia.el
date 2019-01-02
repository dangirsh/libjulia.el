(require 'ffi)

(defun libjulia-elisp-to-c-name (elisp-name)
  (replace-regexp-in-string (regexp-quote "-") "_" elisp-name t t))

(defmacro libjulia-bind (name arg-types return-type)
  "Wrapper around define-ffi-function from the ffi library."
  (message (format "Generating Julia binding for %s." name))
  `(define-ffi-function
     ,name
     ,(libjulia-elisp-to-c-name (symbol-name name))
     ,return-type
     ,arg-types
     libjulia.so))

;; TODO: there's likely a better datastructure for this...
(defvar libjulia-primitive-type-map
  '((:julia-type "Int8"    :c-type "int8"    :elisp-type :int)
    (:julia-type "UInt8"   :c-type "uint8"   :elisp-type :int)
    (:julia-type "Int16"   :c-type "int16"   :elisp-type :int)
    (:julia-type "UInt16"  :c-type "uint16"  :elisp-type :int)
    (:julia-type "Int32"   :c-type "int32"   :elisp-type :int)
    (:julia-type "UInt32"  :c-type "uint32"  :elisp-type :int)
    (:julia-type "Int64"   :c-type "int64"   :elisp-type :int)
    (:julia-type "UInt64"  :c-type "uint64"  :elisp-type :int)
    (:julia-type "Int64"   :c-type "int64"   :elisp-type :int)
    (:julia-type "Float32" :c-type "float32" :elisp-type :float)
    (:julia-type "Float64" :c-type "float64" :elisp-type :double)))

(defun libjulia-primitive-convert (from-type-name from-type-key to-type-key)
  (plist-get
   (seq-find
    #'(lambda (type-plist)
        (equal from-type-name (plist-get type-plist from-type-key)))
    libjulia-primitive-type-map)
   to-type-key))

(defun libjulia-primitive-julia-type-p (julia-type)
  (libjulia-primitive-convert julia-type :julia-type :julia-type))

(defun libjulia-get-jl-unbox-sym (c-type)
  (intern (format "jl-unbox-%s" c-type)))

(defun libjulia-gen-unboxers ()
  (dolist (type-entry libjulia-primitive-type-map)
    (pcase-let ((`(_ _ :c-type ,c-type :elisp-type ,elisp-type) type-entry))
      (eval `(libjulia-bind
              ,(libjulia-get-jl-unbox-sym c-type)
              (:pointer)
              ,elisp-type)))))

(defun libjulia-primitive-unbox (ptr julia-type)
(unless (libjulia-primitive-julia-type-p julia-type)
  (error "Cannot unbox non-primitive type."))
(let* ((c-type (libjulia-primitive-convert julia-type :julia-type :c-type))
       (unbox-f (libjulia-get-jl-unbox-sym c-type)))
  (unless (functionp unbox-f)
    (error (format "Failed to lookup autogenerated unbox function %s." unbox-f)))
  (funcall unbox-f ptr)))


(defun libjulia-get-julia-type (ptr)
  (ffi-get-c-string
   (jl-typeof-str
    (with-ffi-string (c-str-ptr ptr)
      (jl-eval-string c-str-ptr)))))


(defun libjulia-elisp-str-from-julia (ptr)
  (ffi-get-c-string (jl-string-ptr ptr)))


(defun libjulia-elisp-from-julia (ptr julia-type)
  (pcase julia-type
    ((pred libjulia-primitive-julia-type-p)
     (libjulia-primitive-unbox ptr julia-type))
    ("String"
     (libjulia-elisp-str-from-julia ptr))
    ;; If we can't convert to an Elisp type, return as a user-ptr.
    (_ ptr)))

(defun libjulia-eval (julia-expr-str)
  (with-ffi-string (julia-expr-c-string julia-expr-str)
    (let* ((ret-val-ptr (jl-eval-string julia-expr-c-string))
           (julia-type (ffi-get-c-string (jl-typeof-str ret-val-ptr))))
      (libjulia-elisp-from-julia ret-val-ptr julia-type))))


(defun libjulia-init ()
  ;; Ugly workaround to being required to load libjulia with RTLD_GLOBAL.
  ;; We load it first via the wrapper, which has a custom dlopen call
  ;; Then, emacs-ffi tries to re-load it via dtld, but it's already
  ;; been loaded with the RTLD_GLOBAL flag from the wrapper.
  ;; Note that ltld docs claim their dlopen shouldn't need RTLD_GLOBAL because
  ;; "back-tracing". This doesn't seem to be true for libjulia...
  (module-load "/home/dan/treemax/.spacemacs.d/layers/treemax-julia/local/libjulia/libjulia-wrapper.so")
  (libjulia--dlopen "/usr/local/lib/libjulia.so")

  (define-ffi-library libjulia.so "libjulia.so")
  (define-ffi-function jl-init "jl_init__threading" :void nil libjulia.so)
  (jl-init)
  (libjulia-gen-unboxers)
  (libjulia-bind jl-string-ptr (:pointer) :pointer)
  (libjulia-bind jl-typeof-str (:pointer) :pointer)
  (libjulia-bind jl-eval-string (:pointer) :pointer))

(libjulia-init)

(provide 'libjulia)
