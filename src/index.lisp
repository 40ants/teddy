(uiop:define-package teddy/index
  (:use #:cl)
  (:import-from #:eazy-gnuplot)
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
                #:data-frame
                #:column-idx
                #:make-iterator))
(in-package teddy/index)


(def class index ()
  ((data-frame :type data-frame
               :initarg :data-frame
               :reader index-data-frame)
   (column :type t
           :initarg :column
           :reader index-column)
   (positions :type hash-table
              :initform (make-hash-table :test 'equal)
              :reader index-positions)))


(def (function e) make-index (data-frame column)
  (loop with iterator = (make-iterator data-frame)
        with column-idx = (column-idx data-frame column)
        with index = (make-instance 'index
                                    :data-frame data-frame
                                    :column column)
        with positions = (index-positions index)
        ;; Now we need to fill a hash table with row indices
        for row = (funcall iterator)
        for row-idx upfrom 0
        while row
        do (with ((value (nth column-idx row))
                  (existing-row already-exists (gethash value positions)))
             ;; Here we are building an UNIQUE index.
             (when already-exists
               (error "Duplicate value. Rows ~A and ~A both has the ~S value."
                      existing-row
                      row-idx
                      value))
             
             (setf (gethash value positions)
                   row-idx))
        finally (return index)))


(declaim (ftype (function (index t) fixnum)
                row-index))
(def function row-index (index value)
  "Internal function to getting the row index by the value."
  (let ((row-idx (gethash value
                          (index-positions index))))
    (unless row-idx
      (error "Index does not countain ~S value" value))
    row-idx))


(def (method e) scalar ((index index) value column-name)
  "Returns a cell content corresponding to the value and column-name."
  (let ((row-idx (row-index index value)))
    (scalar (index-data-frame index)
            row-idx
            column-name)))
