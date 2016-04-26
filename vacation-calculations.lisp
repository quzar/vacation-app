(in-package :polyforms)

(defconstant +initialize-hours+ 65.02)
(defconstant +hours-awarded-per-pay-period+ 6.67)

;; DAY PREDICATES

(defun weekend-day-p (&optional (date nil))
  (> (get-day-of-week date) 4))

(defun week-day-p (&optional (date nil))
  (not (weekend-day-p date)))

(defun intuit-holiday-p (&optional (date nil))
  (setf date (get-absolute-date date))
  (find date *holidays-list* :key #'first))

(defun intuit-work-day-p (&optional (date nil))
  (setf date (get-absolute-date date))
  (not (or (weekend-day-p date)
           (intuit-holiday-p date))))

(defun dave-holiday-p (&optional (date nil))
  (setf date (get-absolute-date date))
  (full-day-in-days-list? *holidays-list* date))

(defun vacation-day-p (&optional (date nil))
  (setf date (get-absolute-date date))
  (full-day-in-days-list? *vacation-days-list* date))

(defun dave-work-day-p (&optional (date nil))
  (setf date (get-absolute-date date))
  (and (>= date +intuit-start-date+)
       (not (or (weekend-day-p date)
                (dave-holiday-p date)
                (vacation-day-p date)))))

(defun dave-off-day-p (&optional (date nil))
  (not (dave-work-day-p date)))

;; VACATION BALANCE CALCULATIONS

(defun vacation-hours-awarded (&optional (date nil))
  (setf date (get-absolute-date date))
  (if (>= date +initialize-date+)
      (let ((temp (find-next-pay-date +initialize-date+))
            (hours-awarded +initialize-hours+))
        (loop named find-pay-dates do
              (if (<= temp date)
                  (incf hours-awarded +hours-awarded-per-pay-period+)
                  (return-from find-pay-dates))
              (setf temp (find-next-pay-date temp)))
        hours-awarded)
      0))

(defun vacation-days-awarded (&optional (date nil))
  (/ (vacation-hours-awarded date) 8))

(defun vacation-days-used (&optional (date nil))
  (setf date (get-absolute-date date))
  (reduce #'+ (remove-if-not #'(lambda (date-in-list)
                                (> date date-in-list))
                             *vacation-days-list*
                             :key #'first) :key #'second))

(defun vacation-hours-used (&optional (date nil))
  (* (vacation-days-used date) 8))

(defun vacation-hours-balance (&optional (date nil))
  (- (vacation-hours-awarded date) (vacation-hours-used date)))

(defun vacation-days-balance (&optional (date nil))
  (/ (vacation-hours-balance date) 8))

(defun vacation-days-available (&optional (date nil) (extra 0))
  (setf date (get-absolute-date date))
  (let ((next-holiday (find-next-holiday date))
        (next-pay-date (find-next-pay-date date))
        (temp-days-left (+ (vacation-days-balance date) extra)))
    (loop named check-days for inner from date do
         (when (or (eq inner next-pay-date)
                   (eq inner next-holiday))
           (incf temp-days-left
                 (- (vacation-days-balance inner)
                    (vacation-days-balance date)))
           (setf next-holiday (find-next-holiday inner))
           (setf next-pay-date (find-next-pay-date inner)))
         (when (dave-work-day-p inner)
           (decf temp-days-left)
           (if (< temp-days-left 0)
               (return-from check-days (- inner date)))))))

;; FIND SPECIAL DATES
(defun find-next-pay-date (&optional (date nil) (fmt? nil))
  (+  (find-pay-date :direction 'after :date date :fmt? fmt?)))

(defun find-prev-pay-date (&optional (date nil) (fmt? nil))
  (find-pay-date :direction 'before :date date :fmt? fmt?))

(defun find-next-work-day (&optional (date nil) (fmt? nil))
  (find-nth-day 1 :direction 'after
                :test 'intuit-work-day-p
                :date date :fmt? fmt?))

(defun find-prev-work-day (&optional (date nil) (fmt? nil))
  (find-nth-day 1 :direction 'before
                :test 'intuit-work-day-p
                :date date :fmt? fmt?))

(defun find-next-holiday (&optional (date nil) (fmt? nil))
  (setf date (get-absolute-date date))
  (let ((next-holiday nil))
    (loop named check-days for i
          from 0 to (1- (length *holidays-list*)) do
          (setf next-holiday (first (nth i *holidays-list*)))
          (when (> next-holiday date)
            (if fmt? (format t "~A" (third (nth i *holidays-list*))))
            (let ((temp-next-holiday
                    (loop named fix-start for inner
                          from next-holiday downto date do
                          (if (dave-work-day-p (1- inner))
                              (return-from fix-start inner)))))
              (if temp-next-holiday
                  (setf next-holiday temp-next-holiday)
                  (setf next-holiday date)))
            (return-from check-days)))
    (return-date next-holiday fmt?)))

(defun find-pay-date (&key (direction 'after) (date nil) (fmt? nil))
  (let ((absolute-date (get-absolute-date date)))
    (if (>= absolute-date +intuit-first-pay-date+)
        (multiple-value-bind (mon day yr)
                             (values-list (get-gregorian-date date))
                             (if (<= day 15)
                                 ; find a "backup" pay date in case dates overlap
                                 (let ((backup (find-pay-date-in-lieu
                                                 :date (list mon 15 yr) :fmt? nil)))
                                   (if (eq direction 'before)
                                       (if (> absolute-date backup)
                                           (return-date backup fmt?)
                                           (find-pay-date-in-lieu
                                             :date (list (1- mon) (last-day-of-gregorian-month
                                                                    (1- mon) yr) yr) :fmt? fmt?))
                                       ; else (eq direction 'after)
                                       (if (< absolute-date backup)
                                           (return-date backup fmt?)
                                           (find-pay-date-in-lieu
                                             :date (list mon (last-day-of-gregorian-month
                                                               mon yr) yr) :fmt? fmt?))))
                                 ; else (> day 15)
                                 (let ((backup (find-pay-date-in-lieu
                                                 :date (list mon (last-day-of-gregorian-month
                                                                   mon yr) yr) :fmt? nil)))
                                   (if (eq direction 'before)
                                       (if (> absolute-date backup)
                                           (return-date backup fmt?)
                                           (find-pay-date-in-lieu
                                             :date (list mon 15 yr) :fmt? fmt?))
                                       ; else (eq direction 'after)
                                       (if (< absolute-date backup)
                                           (return-date backup fmt?)
                                           (find-pay-date-in-lieu
                                             :date (list (1+ mon) 15 yr) :fmt? fmt?))))))
        +intuit-first-pay-date+))) ; fallback option

(defun find-pay-date-in-lieu (&key (date nil) (fmt? nil))
  (find-nth-day 0 :direction 'before
                :test 'intuit-work-day-p
                :date date :fmt? fmt?))

(defun find-next-day-off (&key (date nil) (fmt? nil))
  (find-nth-day 0 :direction 'after
                :test 'dave-off-day-p
                :date date :fmt? fmt?))

(defun find-date-with-days-available (days &key (date nil) (weekends? t)
                                           (extra 0) (fmt? t))
  (find-date-with-days-off days :date date
                           :weekends? weekends?
                           :extra extra :fmt? fmt?))

(defun find-date-with-days-off (days &key (date nil) (weekends? nil)
                                     (extra 0) (fmt? t))
  (setf date (get-absolute-date date))
  (let ((temp date) (temp-days)
        (next-pay-date) (second-pay-date))
    (return-date
      (loop named main do
            (setf next-pay-date (find-next-pay-date temp))
            (setf second-pay-date (find-next-pay-date next-pay-date))
            (setf temp-days (+ (vacation-days-balance temp) extra))
            (if (<= days temp-days)
                (return-from main temp))
            (if weekends?
                (let ((num-holidays-in-period
                        (length (list-days-between
                                  :start temp :end second-pay-date
                                  :test 'dave-holiday-p :fmt? nil)))
                      (num-weekend-days-to-check
                        (max 2 (* 2 (ceiling (/ temp-days 5))))))
                  (when (<= days (+ temp-days
                                    num-holidays-in-period
                                    num-weekend-days-to-check))
                    (let ((temp-start
                            (max date
                                 (find-nth-day
                                   (max 0 (1- (floor temp-days)))
                                   :direction 'before :date temp
                                   :test 'dave-work-day-p :fmt? nil)))
                          (temp-finish) (temp-days-left))
                      (loop named fix-start for fixed-start
                            from temp-start downto (1+ date) do
                            (when (dave-work-day-p (1- fixed-start))
                              (setf temp-start fixed-start)
                              (return-from fix-start)))
                      (loop named find-block for block-start
                            from temp-start do
                            (setf temp-finish (+ block-start
                                                 (ceiling days)))
                            (setf temp-days-left temp-days)
                            (when (= block-start next-pay-date)
                              (return-from find-block nil))
                            (loop named check-days for inner
                                  from block-start do
                                  (if (< temp-days-left 0)
                                      (return-from check-days nil))
                                  (if (= inner temp-finish)
                                      (if (>= temp-days-left 0)
                                          (return-from main block-start)
                                          (return-from check-days)))
                                  (if (dave-work-day-p inner)
                                      (decf temp-days-left))))))))
            (setf temp next-pay-date))
      ; loop returns date
      fmt?)))

(defun find-nth-day (days &key (direction 'after)
                          (date nil) (test '=) (fmt? t))
  (setf date (get-absolute-date date))
  (when (< days 0)
    (setf days (abs days))
    (setf direction 'before))
  (when (= days 0)
    (setf days 1)
    (if (eq direction 'before)
        (incf date 1)
        (decf date 1)))
  (nth (1- days) (list-n-days days :direction direction
                              :test test :date date
                              :fmt? fmt?)))

;; LIST SPECIAL DATES
(defun list-n-days (days &key (direction 'after)
                         (test '=) (date nil) (fmt? t))
  (setf date (get-absolute-date date))
  (when (< days 0)
    (setf days (abs days))
    (setf direction 'before))
  (when (= days 0)
    (setf days 1)
    (if (eq direction 'before)
        (incf date 1)
        (decf date 1)))
  (unless (and (< date +intuit-start-date+)
               (eq direction 'before))
    (let ((direction
            (if (eq direction 'before)
                #'1- #'1+))
          (days-list ()))
      (loop
        (when (= (length days-list) days)
          (return))
        (when (funcall test (setf date (funcall direction date)))
          (push date days-list)))
      (return-date-list (nreverse days-list) fmt?))))

(defun list-days-between (&key (start +intuit-start-date+)
                               (end nil) (consecutive? nil)
                               (test '=) (fmt? t))
  (setf start (get-absolute-date start))
  (setf end (get-absolute-date end))
  (let ((consec-started? nil)
        (days-list ()))
    (loop
      (when (>= start end)
        (return))
      (cond ((funcall test (incf start))
             (setf consec-started? t)
             (push start days-list))
            ((and consecutive? consec-started?)
             (return))))
    (return-date-list (nreverse days-list) fmt?)))

(defun list-days-including (&key (start +intuit-start-date+)
                                 (end nil) (consecutive? nil)
                                 (test '=) (fmt? t))
  (setf end (get-absolute-date end))
  (list-days-between :start start :end (1+ end)
                     :consecutive? consecutive?
                     :test test :fmt? fmt?))

;; TIME FUNCTIONS
(defun find-time-after-interval (amount &key (unit :seconds) (fmt? nil))
  (setq amount
        (case unit
              ((:hours) (* amount 3600))
              ((:minutes) (* amount 60))
              ((:seconds) amount)
              (t (error "Bad :unit keyword value: ~s." unit))))
  (let ((new-time (+ (get-universal-time) amount)))
    (return-time-of-day new-time fmt?)))

;; DATE FUNCTIONS
(defun quotient (m n)
  (floor (/ m n)))

(defun extract-month (date)
  ;; Month field of $date$ = (month day year).
  (first date))

(defun extract-day (date)
  ;; Day field of $date$ = (month day year).
  (second date))

(defun extract-year (date)
  ;; Year field of $date$ = (month day year).
  (third date))

(defmacro sum (expression index initial condition)
  ;; Sum $expression$ for $index$ = $initial$ and successive integers,
  ;; as long as $condition$ holds.
  (let* ((temp (gensym)))
    `(do ((,temp 0 (+ ,temp ,expression))
          (,index ,initial (1+ ,index)))
         ((not ,condition) ,temp))))

(defun last-day-of-gregorian-month (month year)
  ;; Last day in Gregorian $month$ during $year$.
  (if ;; February in a leap year
      (and (= month 2)
           (= (mod year 4) 0)
           (not (member (mod year 400) (list 100 200 300))))
      ;; Then return
      29
      ;; Else return
      (nth (1- month)
           (list 31 28 31 30 31 30 31 31 30 31 30 31))))

(defun last-day-of-month (date)
  (let ((gregorian-date
          (get-gregorian-date date)))
    (last-day-of-gregorian-month
      (extract-month gregorian-date)
      (extract-year gregorian-date))))

(defun absolute-from-gregorian (date)
  ;; Absolute date equivalent to the Gregorian $date$.
  (let* ((month (extract-month date))
         (year (extract-year date)))
    ;; Return
    (+ (extract-day date)    ;; Days so far this month.
       (sum                  ;; Days in prior months this year.
            (last-day-of-gregorian-month m year) m 1 (< m month))
       (* 365 (1- year))     ;; Days in prior years.
       (quotient (1- year) 4);; Julian leap days in prior years...
       (-                    ;; ...minus prior century years...
          (quotient (1- year) 100))
       (quotient             ;; ...plus prior years divisible...
                 (1- year) 400))))    ;; ...by 400.

(defun gregorian-from-absolute (date)
  ;; Gregorian (month day year) corresponding absolute $date$.
  (let* ((approx (quotient date 366));; Approximation from below.
         (year          ;; Search forward from the approximation.
               (+ approx
                  (sum 1 y approx
                       (>= date
                           (absolute-from-gregorian
                             (list 1 1 (1+ y)))))))
         (month         ;; Search forward from January.
                (1+ (sum 1 m 1
                         (> date
                            (absolute-from-gregorian
                              (list m
                                    (last-day-of-gregorian-month m year)
                                    year))))))
         (day           ;; Calculate the day by subtraction.
              (- date (1- (absolute-from-gregorian
                            (list month 1 year))))))
    ;; Return
    (list month day year)))

(defmacro make-days-list (days-list)
  `(sort
     (remove-duplicates
       (mapcar #'expand-date-record ,days-list))
     #'(lambda (x y) (< (first x) (first y)))))

(defun full-day-in-days-list? (days-list &optional (date nil))
  (setf date (get-absolute-date date))
  (let ((temp (find date days-list :key #'first)))
    (and (listp temp) (equalp (second temp) 1.0))))

(defun expand-date-record (record)
  (nconc (list (get-absolute-date (first record))) (rest record)))

(defparameter *day-names*
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defparameter *month-names*
  '("January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))

(defparameter *short-month-names*
  '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))

(defun get-day-name (&optional (day nil))
  (if (and (integerp day) (<= day 6))
      (unless (< day 0)
        (nth day *day-names*))
      (nth (get-day-of-week day) *day-names*)))

(defun get-month-name (&optional (mon nil))
  (if (and (integerp mon) (<= mon 12))
      (unless (< mon 1)
        (nth (1- mon) *month-names*))
      (nth (1- (first (get-gregorian-date mon))) *month-names*)))

(defun get-short-month-name (&optional (mon nil))
  (if (and (integerp mon) (<= mon 12))
      (unless (< mon 1)
        (nth (1- mon) *short-month-names*))
      (nth (1- (first (get-gregorian-date mon))) *short-month-names*)))

(defun get-absolute-date (&optional (date nil))
  (if (eq date nil)
      (multiple-value-bind (sec min hr day mon yr)
                           (get-decoded-time)
                           (declare (ignore sec min hr))
                           (absolute-from-gregorian (list mon day yr)))
      (if (listp date)
          (absolute-from-gregorian date) ; calculate it
          date))) ; given date is already absolute

(defun get-gregorian-date (&optional (date nil))
  (if (eq date nil)
      (multiple-value-bind (sec min hr day mon yr)
                           (get-decoded-time)
                           (declare (ignore sec min hr))
                           (list mon day yr))
      (if (listp date)
          date ; given date is already gregorian
          (gregorian-from-absolute date)))) ; calculate it

(defun get-day-of-week (&optional (date nil))
  (multiple-value-bind (mon day yr)
                       (values-list (get-gregorian-date date))
                       (multiple-value-bind (sec min hr day* mon* yr* dow)
                                            (decode-universal-time
                                              (encode-universal-time 0 0 0 day mon yr))
                                            (declare (ignore sec min hr day* mon* yr*)) dow)))

(defun daylight-savings? (&optional (date nil))
  (multiple-value-bind (mon day yr)
                       (values-list (get-gregorian-date date))
                       (nth-value 7 (decode-universal-time
                                      (encode-universal-time 0 0 3 day mon yr)))))

;; FORMATTING FUNCTIONS
(defun fmt-date (&optional (date (get-absolute-date)))
  (multiple-value-bind (mon day yr)
                       (values-list (get-gregorian-date date))
                       (format nil "~A, ~A ~2,1,0,'0@A, ~A"
                               (get-day-name (get-day-of-week date))
                               (get-month-name mon) day yr)))

(defun fmt-short-date (&optional (date (get-absolute-date)))
  (multiple-value-bind (mon day yr)
                       (values-list (get-gregorian-date date))
                       (format nil "~A-~A-~2,1,0,'0@A"
                               yr (get-short-month-name mon) day)))

(defun fmt-time-of-day (&optional (time (get-universal-time)))
  (multiple-value-bind (sec min hr day* mon* yr* dow)
                       (decode-universal-time time)
                       (format t "~A, ~A ~2,1,0,'0@A, ~A"
                               (get-day-name dow)
                               (get-month-name mon*) day* yr*)
                       (format nil "~A:~2,1,0,'0@A:~2,1,0,'0@A" hr min sec)))

(defun fmt-duration (time &key (unit :hours))
  (setq time
        (case unit
              ((:days) (* time 24))
              ((:hours) time)
              ((:minutes) (/ time 60))
              ((:seconds) (/ time 3600))
              (t (error "Bad :unit keyword value: ~s." unit))))
  (let* ((days (floor (/ time 24)))
         (hours (floor time))
         (minutes (* (- time hours) 60))
         (seconds (round (* (- minutes (floor minutes)) 60))))
    (setf minutes (floor minutes))
    (when (= seconds 60)
      (setf seconds 0) (incf minutes 1))
    (when (= minutes 60)
      (setf minutes 0) (incf hours 1))
    (with-output-to-string (out)
                           (when (> hours 24)
                             (format out "~A day~[s~;~:;s~], " days days)
                             (decf hours (* days 24)))
                           (when (> hours 0)
                             (format out "~A hour~[s~;~:;s~], " hours hours))
                           (when (> (+ days hours minutes) 0)
                             (format out "~A minute~[s~;~:;s~], " minutes minutes))
                           (format out "~A second~[s~;~:;s~]" seconds seconds))))

;; RETURN-DATE FUNCTIONS
(defun return-time-of-day (time &optional (fmt? nil))
  (if fmt? (fmt-time-of-day time) time))

(defun return-duration (time &optional (fmt? nil))
  (if fmt? (fmt-duration time) time))

(defun return-date (date &optional (fmt? nil))
  (if fmt? (fmt-date date) date))

(defun return-date-list (date-list &optional (fmt? nil))
  (if fmt? (mapcar #'fmt-date date-list) date-list))

(defun today (&optional (fmt? nil))
  (return-date (get-absolute-date) fmt?))

(defun yesterday (&optional (fmt? nil))
  (return-date (1- (get-absolute-date)) fmt?))

(defun tomorrow (&optional (fmt? nil))
  (return-date (1+ (get-absolute-date)) fmt?))

(defun last-week (&optional (fmt? nil))
  (return-date (- (get-absolute-date) 7) fmt?))

(defun next-week (&optional (fmt? nil))
  (return-date (+ (get-absolute-date) 7) fmt?))

(defun last-month (&optional (fmt? nil))
  (return-date (- (get-absolute-date) 30) fmt?))

(defun next-month (&optional (fmt? nil))
  (return-date (+ (get-absolute-date) 30) fmt?))

(defun last-year (&optional (fmt? nil))
  (return-date (- (get-absolute-date) 365) fmt?))

(defun next-year (&optional (fmt? nil))
  (return-date (+ (get-absolute-date) 365) fmt?))

;; URL FUNCTIONS
(defvar *escapes* " ")

(defun escape-char (char)
  (case char
        (#\Space "+")
        (t (format nil "&#~d;" (char-code char)))))

(defun escape (in)
  (flet ((needs-escape-p (char) (find char *escapes*)))
        (with-output-to-string
          (out)
          (loop for start = 0 then (1+ pos)
                for pos = (position-if #'needs-escape-p in :start start)
                do (write-sequence in out :start start :end pos)
                when pos do (write-sequence (escape-char (char in pos)) out)
                while pos))))

(defun query-to-form-urlencoded (query)
  (let (res)
    (dolist (ent query)
      (if res (push "&" res))
      (push (car ent) res)
      (push "=" res)
      (if (cdr ent) (push (escape (cdr ent)) res)))
    (apply #'concatenate 'string (nreverse res))))

; (defun match-re (string-or-regexp to-match)
;   (let ((scanner (cl-ppcre:create-scanner string-or-regexp)))
;     (values-list
;      (concatenate
;       'list (nth-value 1 (cl-ppcre:scan-to-strings scanner to-match))))))

;; MEMOIZATION FUNCTIONS
(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((cache (make-hash-table :test test)))
    (setf (get name 'memo) cache) ; (??)
    #'(lambda (&rest args)
       (let ((k (funcall key args)))
         (multiple-value-bind (val found-p)
                              (gethash k cache)
                              (if found-p val
                                  (setf (gethash k cache)
                                        (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((cache (get fn-name 'memo)))
    (when cache (clrhash cache))))

(defparameter *temporary-hash-table* nil
  "Temporary table to store hash table loaded from a saved file.")

(defun memo-cache-source-file (fn-name)
  "Gives pathname to source file for table associated with function."
  (concatenate 'string "c:/Lisp/code/"
               (string-downcase (symbol-name fn-name)) ".lisp"))

(defun save-memo-table (fn-name &key (size-factor 1.5))
  "Save the hash hash associated with a memoized function."
  (let* ((cache (get fn-name 'memo))
         (size (hash-table-count cache))
         (filename (memo-cache-source-file fn-name)))
    (with-open-file
      (file filename :direction :output :if-exists :supersede)
      (format file "(in-package ~s)~2%" (package-name *package*))
      (format file "(setq *temporary-hash-table*~%")
      (format file "      (make-hash-table :size ~s :test #'~s))~2%"
              (round (* size-factor size)) (hash-table-test cache))
      (format file "(flet ((f (key value)~%")
      (format file
              "         (setf (gethash key *temporary-hash-table*) value)))~%")
      (maphash #'(lambda (key value)
                  (format file "  (f '~s '~s)~%" key value)) cache)
      (format file ")"))
    (format t "~%Wrote ~s entries to ~a. Now compiling:"
            size (namestring filename))
    (compile-file filename)
    (values)))

(defun print-hash-table (cache)
  (loop for k being the hash-keys in cache using (hash-value v)
        do (format t "~a => ~a~%" k v)))

;; GRAPHING FUNCTIONS
(defun asterisk-plot (fn min max step &key
                         (fmt-x-axis #'(lambda (x) x))
                         (fmt-y-axis #'(lambda (y) y))
                         (y-axis-min 0.0)
                         (y-axis-scale 1.0))
  (do ((x min (if (numberp step) (+ x step) (funcall step x))))
      ((>= x max)) ; exit condition
      (let ((y (funcall fn x)))
        (when y
          (format t "~&~A " (funcall fmt-x-axis x))
          (let ((numtimes (floor (* (max (- y y-axis-min) 0) y-axis-scale)))
                (meridian (round (* (- y-axis-min) y-axis-scale))))
            (do ((x 0 (1+ x)))
                ((= x numtimes))
                (if (eq meridian x)
                    (format t "|")
                    (format t "*"))))
          (format t "(~$)~%" (funcall fmt-y-axis y))))))

;; SAMPLE PLOTS
(defun plot-days-remaining (from to)
  (asterisk-plot #'vacation-days-remaining
                 (get-absolute-date from)
                 (get-absolute-date to)
                 #'find-next-pay-date
                 :fmt-x-axis #'fmt-short-date
                 :y-axis-min -2.5 :y-axis-scale 5.0))

(defun plot-days-available (from to &key (extra 0))
  (asterisk-plot (lambda (x) (vacation-days-available x extra))
                 (get-absolute-date from)
                 (get-absolute-date to)
                 (lambda (x) (1+ x))
                 :fmt-x-axis #'fmt-short-date))
