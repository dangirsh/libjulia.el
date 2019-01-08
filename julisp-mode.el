(require 'libjulia)

;;; Exprs and Sexprs

(defun julisp-elisp-head-to-julia (head)
  (pcase head
    (:assign (jl-symbol (ffi-make-c-string ":(=)")))
    (_ (libjulia-julia-from-elisp head))))

(defun julisp-expr-from-sexpr (sexpr)
  (if (listp sexpr)
      ;; recursively build an Expr object
      (libjulia-jl-call "Expr" `(,(julisp-elisp-head-to-julia (car sexpr))
                                 ,@(mapcar #'julisp-expr-from-sexpr (cdr sexpr))))
    ;; atoms are their own exprs
    (libjulia-julia-from-elisp sexpr)))

(defun julisp-eval-expr (julia-expr-ptr &optional julia-module-name)
  (libjulia-elisp-from-julia
   (jl-toplevel-eval
    (libjulia-get-module julia-module-name)
    julia-expr-ptr)))

(defun julisp-eval-sexpr (sexpr &optional julia-module-name)
  (julisp-eval-expr (julisp-expr-from-sexpr sexpr) julia-module-name))

(defun julisp-sexpr-from-julia (julia-src-str)
  (read (libjulia-jl-call "clean_sexpr" `(,julia-src-str) "Julisp")))

;; (defun julisp-edebug-eval-defun (fn edebug-it)
;;   (let ((eval julisp-eval-sexpr)
;;         (funcall fn edebug-it))))

;; (advice-add 'edebug-eval-defun :around #'libjulia-edebug-eval-defun)

(defun julisp-load ()
  (message "Loading Julisp Julia module from %s." (concat (file-name-directory (locate-library "julisp-mode")) "Julisp"))
  (libjulia-eval-str (format
                      "cd(\"%s\"); using Pkg; Pkg.activate(\".\"); using Julisp"
                      (concat (file-name-directory (locate-library "julisp-mode")) "Julisp")))
  ;; (async-start
  ;;  ;; What to do in the child process
  ;;  (lambda ()
  ;;    (libjulia-eval-str (format
  ;;                        "cd(\"%s\"); using Pkg; Pkg.activate(\".\"); using Julisp"
  ;;                        (concat default-directory "Julisp"))))
  ;;  ;; What to do when it finishes
  ;;  (lambda (result)
  ;;    (message "Finished loading support Julia code for libjulia. Result: %s" result)))
  )

(julisp-load)

(define-derived-mode julisp-mode emacs-lisp-mode "Julisp"
  "Major mode for editing julia code as sexprs.")

(defun lispy--eval-julisp (str)
  (libjulia-eval-sexpr (read str)))

(provide 'julisp-mode)
