(uiop:define-package teddy/utils
  (:use #:cl)
  (:import-from #:hu.dwim.def
                #:def
                #:function
                #:method
                #:generic)
  (:import-from #:simplified-types
                #:simplified-type-of))
(in-package teddy/utils)


(def function infer-type (value)
  (cond
    (value
     (let ((simplified-types::*precise-integer-types* nil))
       (simplified-type-of value)))
    (t t)))


(declaim (ftype (function (&rest t) simple-array)
                ensure-vector))
(def (function d) ensure-vector (obj &key element-type)
  (declare (dynamic-extent obj))
  (etypecase obj
    (list
     (let ((len (length obj)))
       (make-array len
                   :element-type (or element-type
                                     (infer-type (first obj)))
                   :adjustable nil
                   :fill-pointer nil
                   :initial-contents obj)))
    (simple-array
     obj)))


(declaim (ftype (function (simple-vector) list)
                vector-to-list))
(def (function oi) vector-to-list (vector)
  (loop for item across vector
        collect item))
