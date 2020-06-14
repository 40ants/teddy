(uiop:define-package teddy/stats
  (:use #:cl)
  (:import-from #:statistics)
  (:import-from #:hu.dwim.def
                #:def
                #:function
                #:method
                #:generic)
  (:import-from #:rutils
                #:_
                #:with
                #:fn)
  (:import-from #:teddy/data-frame
                #:get-column
                #:get-columns
                #:get-column-names
                #:get-types
                #:make-data-frame))
(in-package teddy/stats)


(def (function e) stats (data-frame)
  "Returns a new dataframe where each column holds
   different stats on corresponding columns
   from original data-frame."
  (make-data-frame
   (list "Column" "Min" "p25" "p50" "p75" "Max" "Mean" "SD" "Sum")
   :rows (loop for name across (get-column-names data-frame)
               for column across (get-columns data-frame)
               for type across (get-types data-frame)
               when (subtypep type 'number)
               collect (list name
                             (reduce #'min column)
                             (statistics:percentile column 25)
                             (statistics:percentile column 50)
                             (statistics:percentile column 75)
                             (reduce #'max column)
                             (coerce (statistics:mean column)
                                     'single-float)
                             (coerce (statistics:sd column)
                                     'single-float)
                             (reduce #'+ column)))))

