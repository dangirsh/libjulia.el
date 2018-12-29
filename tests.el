(add-to-list 'load-path (expand-file-name "."))


(defun test-emacs-libjulia ()
  (load-file "emacs-libjulia.so")
  (print (julia_tester)))
