(uiop:define-package teddy/data-frame
  (:use #:cl)
  (:import-from #:ascii-table)
  (:import-from #:hu.dwim.def
                #:def
                #:function
                #:method
                #:generic
                #:global-variable)
  (:import-from #:rutils
                #:fn
                #:fmt)
  (:import-from #:teddy/utils
                #:vector-to-list))
(in-package teddy/data-frame)


(def function make-value-formatter (num-digits-after-point)
  (let ((single-float-pattern
          (fmt "~~,~AF"
               num-digits-after-point))
        (double-float-pattern
          (fmt "~~,~AFd0"
               num-digits-after-point))
        (ratio-pattern
          (fmt "~~,~AF"
               num-digits-after-point)))
    (fn value-formatter (value)
      (format nil
              (typecase value
                (single-float single-float-pattern)
                (double-float double-float-pattern)
                (ratio ratio-pattern)
                (t "~A"))
              value))))


(def global-variable *value-formatter*
  (make-value-formatter 2))


(def (function e) set-num-digits-after-point (num)
  (setf *value-formatter*
        (make-value-formatter num)))


(defmethod print-object ((data-frame data-frame) stream)
  (let ((ascii-table:*default-value-formatter*
          *value-formatter*))
    (loop with ascii = (ascii-table:make-table
                        (vector-to-list
                         (get-column-names data-frame)))
          with iterator = (make-iterator data-frame)
          for row = (funcall iterator) then (funcall iterator)
          while row
          do (ascii-table:add-row ascii row)
          finally (ascii-table:display ascii stream))))
