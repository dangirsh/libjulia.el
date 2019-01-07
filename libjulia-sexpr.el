(require 'libjulia)

;;; Exprs and Sexprs

(defun libjulia-expr-from-sexpr (sexpr)
  (if (listp sexpr)
      ;; recursively build an Expr object
      (libjulia-jl-call "Expr" `(,(car sexpr)
                                 ,(mapcar #'libjulia-expr-from-sexpr (cdr sexpr))))
    ;; atoms are their own exprs
    sexpr))

(defun libjulia-eval-expr (julia-expr-ptr &optional julia-module-name)
  (libjulia-elisp-from-julia
   (jl-toplevel-eval
    (libjulia-get-module julia-module-name)
    julia-expr-ptr)))

(defun libjulia-eval-sexpr (sexpr &optional julia-module-name)
  (libjulia-eval-expr (libjulia-expr-from-sexpr sexpr) julia-module-name))

(defun libjulia-sexpr-from-julia (julia-src-str)
  (read (libjulia-jl-call "clean_sexpr" `(,julia-src-str) "EmacsJulia")))

;; (defun libjulia-edebug-eval-defun (fn edebug-it)
;;   (let ((eval libjulia-eval-sexpr)
;;         (funcall fn edebug-it))))

;; (advice-add 'edebug-eval-defun :around #'libjulia-edebug-eval-defun)


(define-derived-mode julia-sexpr-mode emacs-lisp-mode "JuliaSexpr"
  "Major mode for editing julia code as sexprs.")

(defun lispy--eval-julia-sexpr (str)
  (libjulia-eval-sexpr (read str)))

(provide 'julia-sexpr-mode)
