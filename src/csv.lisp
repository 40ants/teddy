(uiop:define-package teddy/csv
  (:use #:cl)
  (:import-from #:hu.dwim.def
                #:def
                #:function
                #:method
                #:generic))
(in-package teddy/csv)


(def (function e) dataframe-to-csv (data-frame filename)
  (declare (ignorable data-frame filename))
  (error "Implement me!"))

