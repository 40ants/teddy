(defsystem "teddy"
  :class :package-inferred-system
  :pathname "src"
  :defsystem-depends-on ("asdf-finalizers")
  :depends-on ("teddy/data-frame"
               "teddy/index"
               "teddy/stats"
               "teddy/print"
               "teddy/plot")
  :around-compile "asdf-finalizers:check-finalizers-around-compile")

(register-system-packages "cl-ascii-table" '(#:ascii-table))
(register-system-packages "lhstats" '(#:statistics))
