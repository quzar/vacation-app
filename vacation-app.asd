(defpackage :polyforms
  (:use :common-lisp)

  (:export
   :vacation-hours-awarded
   :vacation-days-awarded
   :vacation-hours-used
   :vacation-days-used
   :vacation-hours-balance
   :vacation-days-balance
   :vacation-days-available
   :today
   :yesterday
   :tomorrow
   :last-week
   :next-week
   :last-month
   :next-month
   :last-year
   :next-year
   ))

(defsystem vacation-app
  :components
  ((:file "constants")
   (:file "utilities"
          :depends-on ("constants"))
   (:file "vacation-details"
          :depends-on ("constants" "utilities"))
   (:file "vacation-calculations"
          :depends-on ("constants" "utilities" "vacation-details"))))
