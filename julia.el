(defconst julia-module-file
  (when module-file-suffix
    (expand-file-name
     (concat "julia-core" module-file-suffix)
     (file-name-directory (locate-library "julia"))))
  "The module file for Julia or nil if modules are not supported.")


(defun julia-load ()
  "Load the Julia dynamic module."
  (if julia-module-file
      (if (file-exists-p julia-module-file)
          (progn
            (load-file julia-module-file)
            ;; (add-hook 'post-gc-hook #'zmq--cleanup-globrefs)
            )
        (when (y-or-n-p "Julia module not found. Build it? ")
          (let ((default-directory (file-name-directory (locate-library "julia"))))
            (cl-labels
                ((load-julia
                  (_buf status)
                  (if (string= status "finished\n")
                      (julia-load)
                    (message "Something went wrong when compiling the Julia module!"))
                  (remove-hook 'compilation-finish-functions #'load-julia)
                  (exit-recursive-edit)))
              (add-hook 'compilation-finish-functions #'load-julia)
              (compile "make all")
              (recursive-edit)))))
    (user-error "Modules are not supported")))

(julia-load)

(provide 'julia)
