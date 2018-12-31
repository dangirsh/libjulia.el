(require 'ert)
(require 'julia-ffi)

(ert-deftest test-julia-ffi-get-type ()
  (should (equal (julia-ffi-get-julia-type "3") "Int64"))
  (should (equal (julia-ffi-get-julia-type "typemax(Int32)") "Int32"))
  (should (equal (julia-ffi-get-julia-type "typemin(Int64)") "Int64"))
  (should (equal (julia-ffi-get-julia-type "0x1") "UInt8"))
  ;; negating an unsigned literal creates an unsigned 2s complement
  (should (equal (julia-ffi-get-julia-type "-0x1000") "UInt16")))

(ert-deftest test-julia-eval ()
  (should (equal (julia-ffi-eval "3") 3))
  (should (equal (julia-ffi-eval "\"Hallo\"") "Hallo"))
  (should (equal (julia-ffi-eval "3.14") 3.14)))
