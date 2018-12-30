(defconst julia-module-file
  (when module-file-suffix
    (expand-file-name
     (concat "julia-core" module-file-suffix)
     (file-name-directory (locate-library "julia"))))
  "The module file for Julia or nil if modules are not supported.")


(defun julia-sexpr-str (julia-code-str)
  (julia-eval (format "EmacsJulia.clean_sexpr(\"%s\")" julia-code-str)))


(defun julia-sexpr (julia-code-str)
  (read (julia-sexpr-str julia-code-str)))


(defmacro julia-within-lib-dir (&rest body)
  `(let ((default-directory (file-name-directory (locate-library "julia"))))
     (progn ,@body)))


(defun julia-load ()
  "Load the Julia dynamic module."
  (if julia-module-file
      (if (file-exists-p julia-module-file)
          (progn
            (load-file julia-module-file)
            (julia-within-lib-dir
             (julia-eval-blind
              (with-temp-buffer
                (insert-file-contents-literally "EmacsJulia/src/EmacsJulia.jl")
                (buffer-string))))
            ;; Broken for some reason...
            ;; Try again when there's better error reporting
            ;; (julia-eval (format
            ;;              "cd(\"%s\"); using Pkg; Pkg.activate(\".\"); using EmacsJulia"
            ;;              (concat (file-name-directory (locate-library "julia")) "EmacsJulia")
            ;;              ))
            ;; (add-hook 'post-gc-hook #'julia--cleanup-globrefs)
            )
        (when (y-or-n-p "Julia module not found. Build it? ")
          (julia-within-lib-dir
           (cl-labels
               ((load-julia
                 (_buf status)
                 (if (string= status "finished\n")
                     (julia-load)
                   (message "Something went wrong when compiling the Julia module!"))
                 (remove-hook 'compilation-finish-functions #'load-julia)
                 (exit-recursive-edit)))
             (add-hook 'compilation-finish-functions #'load-julia)
             (compile "make")
             (recursive-edit)))))
    (user-error "Modules are not supported")))

(julia-load)

(provide 'julia)
