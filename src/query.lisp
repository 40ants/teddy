(uiop:define-package teddy/query
  (:use #:cl)
  (:import-from #:hu.dwim.def
                #:def
                #:function
                #:method
                #:generic)
  (:import-from #:teddy/data-frame
                #:get-column
                #:get-columns
                #:get-column-names
                #:get-types
                #:make-data-frame
		#:make-iterator
		#:slice))
(in-package teddy/query)

(defun select (data-frame &rest columns)
  "Returns a subset of columns by name from a data frame."
  (slice data-frame :columns columns))

;; (defun filter (data-frame condition)
;;   "Returns a subset of rows in a data frame that satisfy a condition."
;;   data-frame)

;; (defun mutate (data-frame bindings)
;;   data-frame)

;; (defun arrange (data-frame &rest columns)
;;   data-frame)

;; (defun group-by (data-frame &rest columns)
;;   nil)

;; (defun summarize (data-frame aggregations)
;;   data-frame)

;; (defun head (data-frame num-rows)
;;   data-frame)
