(uiop:define-package teddy/data-frame
  (:use #:cl)
  (:import-from #:ascii-table)
  (:import-from #:hu.dwim.def
                #:def
                #:function
                #:method
                #:generic)
  (:import-from #:list-of
                #:list-of)
  (:import-from #:rutils
                #:_
                #:with
                #:fn)
  (:import-from #:alexandria
                #:non-negative-integer)
  (:import-from #:simplified-types
                #:simplified-type-of)
  (:import-from #:teddy/utils
                #:ensure-vector
                #:infer-type
                #:vector-to-list)
  (:import-from #:hu.dwim.def
                #:global-variable))
(in-package teddy/data-frame)


(def (class ea) data-frame ()
  ((names :initarg :names
          :documentation "Column names"
          :type (simple-array string (*))
          :reader get-column-names)
   (types :initarg :types
          :documentation "A list of columns"
          :type simple-vector
          :reader get-types)
   (columns :initarg :columns
            :documentation "A vector of column vectors"
            :type (simple-array simple-vector (*))
            :reader get-columns)))


(declaim (ftype (function (data-frame) integer)
                num-columns
                num-rows))

(def (function eoi) num-rows (data-frame)
  (let ( (columns (get-columns data-frame)))
    (if (> (length columns)
           0)
        (length (aref columns 0))
        0)))


(def (function eoi) num-columns (data-frame)
  (length (get-column-names data-frame)))


(def (function ei) make-data-frame (column-names &key rows columns types)
  (check-type column-names (list-of string))
  
  (when (and rows columns)
    (error "Use only :rows or :columns argument"))

  (when rows
    (check-type rows (list-of (list-of t)))
    
    (unless (= (length column-names)
               (length (first rows)))
      (error "Names and columns should have the same length")))
  
  (when columns
    (check-type columns (or (list-of (list-of t))
                            (list-of simple-array)))
    
    (unless (= (length column-names)
               (length columns))
      (error "Names and columns should have the same length")))

  
  (let* ((types (ensure-vector
                 (cond
                   (types (unless (= (length column-names)
                                     (length types))
                            (error "Names and types should have the same length"))
                          types)
                   (rows (mapcar #'infer-type
                                 (first rows)))
                   (t
                    (loop for column in columns
                          for first-element = (first column)
                          collect (infer-type first-element))))))
         (columns (ensure-vector
                   (cond
                     (rows
                      ;; If rows were provided, we need to
                      ;; prepare empty rows
                      (loop with num-rows = (length rows)
                            for item in (first rows)
                            for element-type across types
                            collect (make-array num-rows
                                                :element-type element-type
                                                :adjustable nil
                                                :fill-pointer nil)))
                     (columns (mapcar #'ensure-vector
                                      columns)))))
         (names (ensure-vector column-names))
         (data-frame (make-instance 'data-frame
                                    :names names
                                    :types types
                                    :columns columns)))
    (when rows
      (loop for row in rows
            for row-idx of-type fixnum upfrom 0
            do (loop for element in row
                     for idx upfrom 0
                     for column across columns
                     for column-name across names
                     ;; TODO: remove this debug code
                     when (not (typep element
                                      (array-element-type column)))
                     do (format t "~A -> ~A = ~A~%" idx column-name element)
                     do (setf (aref column row-idx)
                              element))))
    data-frame))


(def (function eoi) make-iterator (data-frame)
  "Returns a function from zero arguments which will return
   a next row on each call. When all rows will be returned,
   iterator will return nil.

   Rows are returned as lists."
  (check-type data-frame data-frame)
  
  (let ((idx 0))
    (declare (type non-negative-integer idx))
    (fn data-table-iterator ()
      (when (< idx (num-rows data-frame))
        (prog1 (loop for column across (get-columns data-frame)
                     collect (aref column idx))
          (incf idx))))))


;; Slicing
(def (function e) slice (data-frame &key columns from to)
  ;; TODO: write a test for corner cases when column does not exists
  ;; or to/from are out of the array or in wrong order
  (let ((num-rows (num-rows data-frame)))
    (flet ((convert-negative-idx (idx)
             (if (< idx 0)
                 (+ num-rows idx)
                 idx)))
      (loop with columns = (uiop:ensure-list columns)
            with slice = (when (or from to)
                           (cons (convert-negative-idx
                                  (or from 0))
                                 (convert-negative-idx
                                  (or to num-rows))))
            for name across (get-column-names data-frame)
            for type across (get-types data-frame)
            for col across (get-columns data-frame)
            when (or (null columns)
                     (member name columns
                             :test #'string-equal))
            collect name into new-names
            and collect type into new-types
            and collect
               (if slice
                   (subseq col
                           (car slice)
                           (cdr slice))
                   col) into new-columns
            finally (return
                      (make-data-frame new-names
                                       :columns new-columns
                                       :types new-types))))))

(def (function e) head (data-frame &optional (rows 10))
  (slice data-frame :to rows))

(def (function e) tail (data-frame &optional (rows 10))
  (slice data-frame :from (- rows)))


(def (function e) get-column (data-frame name &key (as :vector))
  "Returns column by name"
  (check-type as (member :vector :list))
  
  (loop for column-name across (get-column-names data-frame)
        for column across (get-columns data-frame)
        when (string-equal name column-name)
        do (return (ecase as
                     (:vector column)
                     (:list (vector-to-list column))))
        finally (error "Column ~S not found."
                       name)))


(def (function e) column-idx (data-frame name)
  "Returns a number of the column"
  (loop for column-name across (get-column-names data-frame)
        for idx from 0
        when (string-equal column-name name)
          do (return idx)
        finally (error "Column \"~A\" not found"
                       name)))


(def (generic e) scalar (obj row-idx column-name)
  (:documentation "Returns a cell content corresponding to the row-idx and column-name."))


(def (method e) scalar ((data-frame data-frame) row-idx column-name)
  "Returns a cell content corresponding to the row-idx and column-name."
  (check-type row-idx (integer 0 *))
  (check-type column-name string)
  
  (let* ((column-idx (column-idx data-frame column-name))
         (columns (get-columns data-frame))
         (column (aref columns column-idx))
         (max-idx (1- (length column))))
    (when (> row-idx max-idx)
      (error "Index should be in [0, ~A] range" max-idx))
    (aref column row-idx)))


(asdf-finalizers:final-forms)
