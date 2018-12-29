(add-to-list 'load-path (expand-file-name "."))

(require 'julia)
(require 'ert)

(ert-deftest julia-test ()
  (should (= (julia-tester) (sqrt 2))))
