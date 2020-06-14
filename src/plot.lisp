(uiop:define-package teddy/plot
  (:use #:cl)
  (:import-from #:eazy-gnuplot)
  (:import-from #:hu.dwim.def
                #:def
                #:function
                #:method
                #:generic)
  (:import-from #:teddy/data-frame
                #:get-column
                #:get-columns
                #:get-column-names)
  (:import-from #:alexandria
                #:make-keyword))
(in-package teddy/plot)


(def function filename->terminal (filename)
  "Transforms string or pathname into a list suitable for passing
   as :terminal value to eazy-gnuplot:gp-setup."
  (etypecase filename
    (string (filename->terminal
             (pathname filename)))
    (pathname
     (let ((type (pathname-type filename))
           (supported-types '("svg" "png")))
       (unless (member type supported-types
                       :test #'string-equal)
         (error "Unsupported file extension ~S. Use ~{~S~#[~; or ~:;, ~]~}."
                type
                supported-types))
       (list (make-keyword (string-upcase type)))))))


(def (function e) plot-timeseries (dataframe filename &key (x "date") y title)
  "Plots a timeseries where axis x has a timestamp values.
   If column for axis y is not given, then second column will be used."
  (eazy-gnuplot:with-plots (*standard-output* :debug nil)
    (eazy-gnuplot:gp-setup :terminal (filename->terminal filename)
                           :output filename
                           :title title)
    (eazy-gnuplot:gp :set :xdata 'time)
    (eazy-gnuplot:gp :set :timefmt "%Y-%m-%d")
    (eazy-gnuplot:gp :set :format '(x "%Y-%m-%d"))
    (eazy-gnuplot:gp :set :xtics '(rotate))
    (eazy-gnuplot:gp :unset :key)
    (unless y
      (setf y (aref (get-column-names dataframe)
                    1)))
    (eazy-gnuplot:plot
     (lambda ()
       (loop for date across (get-column dataframe x)
             for value across (get-column dataframe y)
             do (format t "~&~A ~A"
                        date value)))
     :using '(1 2) :with :lines)
    filename))


(def (function e) plot (data-frame filename &key title debug)
  (eazy-gnuplot:with-plots (stream :debug debug)
    (eazy-gnuplot:gp-setup :terminal (filename->terminal filename)
                           :output filename
                           :title title)
    (loop for column-name across (get-column-names data-frame)
          for column across (get-columns data-frame)
          do (eazy-gnuplot:plot
              (lambda ()
                (loop for idx upfrom 1
                      for value across column
                      do (format stream "~&~A ~A" idx value)))
              :using 2
              :title column-name
              :with :lines))
    filename))
