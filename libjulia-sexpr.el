(require 'libjulia)

;;; Exprs and Sexprs

(defun libjulia-expr-from-sexpr (sexpr)
  (libjulia-jl-call "Expr" sexpr))

(defun libjulia-eval-expr (julia-expr-ptr &optional julia-module-name)
  (libjulia-elisp-from-julia
   (jl-toplevel-eval
    (libjulia-get-module julia-module-name)
    julia-expr-ptr)))

(defun libjulia-eval-sexpr (sexpr &optional julia-module-name)
  (libjulia-eval-expr (libjulia-expr-from-sexpr sexpr) julia-module-name))

(defun libjulia-sexpr-from-julia (julia-src-str)
  (read
   (libjulia-jl-call "clean_sexpr" `(,julia-src-str) "EmacsJulia")))

(setq test-sexpr (read (libjulia-sexpr-from-julia "1 + 1")))

(libjulia-eval-sexpr test-sexpr)

(provide 'libjulia-sexpr)
