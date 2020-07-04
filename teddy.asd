(defsystem "teddy"
  :name "teddy"
  :description "A data framework for Common Lisp, wanna be like Pandas for Python."
  :class :package-inferred-system
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "UNLICENSE"
  :pathname "src"
  :defsystem-depends-on ("asdf-finalizers")
  :depends-on ("teddy/data-frame"
               "teddy/index"
               "teddy/stats"
               "teddy/print"
               "teddy/plot"
	       "teddy/query")
  :around-compile "asdf-finalizers:check-finalizers-around-compile")

(register-system-packages "cl-ascii-table" '(#:ascii-table))
(register-system-packages "lhstats" '(#:statistics))
