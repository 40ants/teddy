(in-package #:cl-user)

(require :asdf)

(push (make-pathname :name nil :type nil :version nil
                     :defaults (truename *load-pathname*))
      asdf:*central-registry*)

(handler-case
    (progn
      (ql:quickload :teddy)
      (let ((package (find-package :teddy/data-frame)))
        (unless package
          (error "Package TEDDY/DATA-FRAME does not exist after loading TEDDY"))
        (multiple-value-bind (symbol status)
            (find-symbol "MAKE-DATA-FRAME" package)
          (format t "TEDDY/DATA-FRAME:MAKE-DATA-FRAME status = ~S~%" status)
          (unless (and symbol (eq status :external))
            (error "TEDDY/DATA-FRAME:MAKE-DATA-FRAME is not an external symbol (status ~S). README.md refers to it as teddy/data-frame:make-data-frame." status)))
        (let ((constructor
                (read-from-string "teddy/data-frame:make-data-frame"))
              (data-frame
                (eval
                 (read-from-string
                  "(teddy/data-frame:make-data-frame
                    '(\"Idx\" \"Integers\")
                    :rows '((0 41) (1 98)))"))))
          (unless (fboundp constructor)
            (error "teddy/data-frame:make-data-frame is not fboundp"))
          (unless (and (symbolp (class-name (class-of data-frame)))
                       (string= (symbol-name (class-name (class-of data-frame)))
                                "DATA-FRAME")
                       (eq (symbol-package (class-name (class-of data-frame)))
                           package))
            (error "README usage returned ~S instead of a TEDDY/DATA-FRAME DATA-FRAME instance"
                   data-frame))
          (multiple-value-bind (rows-sym rows-status)
              (find-symbol "NUM-ROWS" package)
            (let ((num-rows (and (eq rows-status :internal)
                                 (fboundp rows-sym)
                                 (funcall rows-sym data-frame))))
              (format t "README example data-frame rows = ~S~%" num-rows)
              (unless (eql num-rows 2)
                (error "README example data-frame has ~S rows instead of 2" num-rows))))))
      (format t "PASS: TEDDY/DATA-FRAME:MAKE-DATA-FRAME is an external symbol and the README example works~%")
      (sb-ext:exit :code 0))
  (error (condition)
    (format *error-output* "REGRESSION-FAILED: ~A~%" condition)
    (sb-ext:exit :code 1)))
