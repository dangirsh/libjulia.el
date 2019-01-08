(require 'ert)
(require 'julisp-mode)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Comparison-of-Numbers.html
(defun float-equal (x y)
  (let ((float-tol 1.0e-6))
    (or (= x y)
        (< (/ (abs (- x y))
              (max (abs x) (abs y)))
           float-tol))))


(ert-deftest test-julisp ()
  ;; (should (equal
  ;;          (julisp-sexpr-from-julia "1 + 1")
  ;;          '(:call :+ 1 1)))
  ;; (should (equal
  ;;          (julisp-sexpr-from-julia "a = 1")
  ;;          '(:assign :a 1)))
  (should (equal
           2
           (julisp-eval-sexpr '(:call :+ 1 1))))
  ;; (should (equal 2 (julisp-eval-sexpr
  ;;                   (julisp-sexpr-from-julia "1 + 1"))))
  ;; (should (float-equal 8.0 (julisp-eval-sexpr
  ;;                           (julisp-sexpr-from-julia "sqrt(64.0)"))))
  ;; (should (equal 1 (julisp-eval-sexpr
  ;;                   (julisp-sexpr-from-julia "a = 1"))))
  ;; (should (equal 2 (julisp-eval-sexpr
  ;;                   (julisp-sexpr-from-julia "a = 1; a + a"))))
  )
