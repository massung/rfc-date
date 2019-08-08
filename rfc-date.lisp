;;;; RFC Date/Time Format Parsers for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :rfc-date
  (:use :cl :re)
  (:export
   #:encode-universal-rfc-time
   #:decode-universal-rfc-time))

(in-package :rfc-date)

;;; ----------------------------------------------------

;;; y = 2-digit year (1970 to 2069)
;;; Y = 4-digit year
;;; m = 1- or 2- digit month
;;; M = Abbreviated month name
;;; d = 1- or 2- digit date
;;; D = Abbreviated day of week name
;;; l = Long day of the week name
;;; H = 2-digit hour (24-hour)
;;; i = 2-digit minute (00)
;;; s = 2-digit second and ignored fraction (00)
;;; P = time zone in clock format (+00:00)
;;; O = time zone in ordinal format (+0000)
;;; Z = time zone in named format (UTC)

(defparameter *rfc-formats*
  '((:rfc822  "D, d M y H:i:s O")
    (:rfc850  "l, d-M-y H:i:s Z")
    (:rfc1036 "D, d M y H:i:s O")
    (:rfc1123 "D, d M Y H:i:s O")
    (:rfc2822 "D, d M Y H:i:s O")
    (:rfc3339 "Y-m-dTH:i:sP")

    ;; extra internet date/time formats
    (:atom    "Y-m-dTH:i:sP")
    (:cookie  "l, d-M-Y H:i:s Z")
    (:rss     "D, d M Y H:i:s O")
    (:iso8601 "Y-m-dTH:i:sO")
    (:w3c     "Y-m-dTH:i:sP")))

;;; ----------------------------------------------------

(defparameter *days*
  '("Monday" "Tuesday" "Wednesday" "Thurday" "Friday" "Saturday" "Sunday")
  "Days of the week.")

;;; ----------------------------------------------------

(defparameter *months*
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "Abbreviated months of the year.")

;;; ----------------------------------------------------

(defparameter *zones*
  '(("GMT" +000) ("UTC" +000)
    ("EST" -500) ("EDT" -400)
    ("CST" -600) ("CDT" -500)
    ("MST" -700) ("MDT" -600)
    ("PST" -800) ("PDT" -700))
  "Named time zones.")

;;; ----------------------------------------------------

(defparameter *rfc-format-re-mapping*
  '((#\y :year       "(%d%d)")
    (#\Y :year       "(%d%d%d%d)")
    (#\M :month-name "(%a+)")
    (#\m :month      "(%d%d?)")
    (#\D :day        "(%a+)")
    (#\l :day-long   "(%a+)")
    (#\d :date       "(%d%d?)")
    (#\H :hour       "(%d%d?)")
    (#\i :minute     "(%d%d)")
    (#\s :second     "(%d%d)(?%.%d+)?")
    (#\P :zone-clock "([+-]%d?%d):(%d%d)")
    (#\O :zone-ord   "([+-]%d?%d)(%d%d)")
    (#\Z :zone-name  "(%a+)"))
  "Various components of an RFC date format.")

;;; ----------------------------------------------------

(defun compile-date-re (format)
  "Given a date format string, compile a RE for it."
  (flet ((reserved-char-p (c)
           (find c "%|-*+?()[].^$" :test #'char=)))
    (loop
       with s = (make-string-output-stream)

       ;; each character in the pattern
       for c across format

       ;; lookup the character for a sub-pattern
       for (name m) = (rest (assoc c *rfc-format-re-mapping*))

       ;; write the sub-pattern or literal character
       do (format s "~:[~*~:[~;%~]~a~;~a~]" m m (reserved-char-p c) c)

       ;; collect the names, so we know the order of each element
       when m collect name into date-properties

       ;; compile the pattern
       finally (let ((pattern (get-output-stream-string s)))
                 (return (list date-properties (compile-re pattern)))))))

;;; ----------------------------------------------------

(defparameter *rfc-re-mapping*
  (flet ((map-rfc-format (pair)
           (cons (first pair) (compile-date-re (second pair)))))
    (mapcar #'map-rfc-format *rfc-formats*))
  "All the *rfc-formats*, but with compiled re patterns.")

;;; ----------------------------------------------------

(defun military-time-zone (c)
  "Convert a character to a time zone."
  (let ((n (char-code (char-upcase c))))
    (cond ((<= 78 n 89) (- 78 n 1))      ; [N,Y]
          ((<= 65 n 73) (- n 64))        ; [A,I]
          ((<= 75 n 77) (+ 10 (- n 75))) ; [K,M]

          ;; unknown
          (t (error "Unknown military time zone ~c" c)))))

;;; ----------------------------------------------------

(defun month-of-year (m)
  "Returns the index into the year for the month name."
  (flet ((begins-with (m s)
           (string-equal m s :start1 0 :end1 (min (length m) 3))))
    (let ((i (position m *months* :test #'begins-with)))
      (if i
          (1+ i)
        (error "Invalid month ~s" m)))))

;;; ----------------------------------------------------

(defun short-year (year)
  "Return the year in 2-digit format."
  (if (<= 1970 year 2069)
      (rem year 100)
    (error "Invalid 2-digit year ~d (valid range is [1970-2069])" year)))

;;; ----------------------------------------------------

(defun gmt-time (ss mm hh d m y &optional (zone 0))
  "Adjust the date/time to get the hour, day, month, and year in GMT."
  (let ((local-time (encode-universal-time ss mm hh d m y zone)))
    (decode-universal-time (+ local-time (* zone 3600)))))

;;; ----------------------------------------------------

(defun encode-universal-rfc-properties (props match)
  "Given a list of match groups, find the various date properties in it."
  (flet ((get-prop (p)
           (let ((i (position p props)))
             (when i
               (nth i (match-groups match))))))

    ;; find all the properties for this match
    (let* ((year       (get-prop :year))
           (month-name (get-prop :month-name))
           (month      (get-prop :month))
           (date       (get-prop :date))
           (hour       (get-prop :hour))
           (minute     (get-prop :minute))
           (sec        (get-prop :second))
           (zone-name  (get-prop :zone-name))

           ;; the time zone hour can be from ordinal or a clock
           (zone-hour  (or (get-prop :zone-ord)
                           (get-prop :zone-clock)))

           ;; time zone minutes are always last
           (zone-min   (car (last (match-groups match))))

           ;; get the time zone from the name or the hour/min
           (zone (if zone-name
                     (let ((tz (assoc zone-name *zones* :test #'string=)))
                       (if tz
                           (/ (second tz) 100)
                         (military-time-zone (char zone-name 0))))

                   ;; otherwise use the hour and minute offset
                   (let ((mm (parse-integer zone-min))
                         (hh (parse-integer zone-hour)))
                     (+ hh (/ (* mm 60) 3600))))))

      ;; encode the properties into a universal time
      (encode-universal-time
       (parse-integer sec)
       (parse-integer minute)
       (parse-integer hour)
       (parse-integer date)

       ;; months might be named or the integer
       (if month
           (parse-integer month)
         (1+ (position month-name *months* :test #'search)))

       ;; the year might be in 2- or 4-digit format
       (let ((yy (parse-integer year)))
         (if (>= yy 100)
             yy
           (+ yy (if (>= yy 70) 1900 2000))))

       ;; time zone is optional, default to GMT
       (if (null zone)
           0
         (- zone))))))

;;; ----------------------------------------------------

(defun encode-universal-rfc-time (string &optional (format :rfc822))
  "Encode an RFC date/time string into universal time."
  (destructuring-bind (&optional props re)
      (rest (assoc format *rfc-re-mapping*))
    (if re
        (with-re-match (m (match-re re string :exact t))
          (encode-universal-rfc-properties props m))
      (error "Unknown RFC date/time format ~s" format))))

;;; ----------------------------------------------------

(defun decode-universal-rfc-time (time &optional (format :rfc822))
  "Decode a universal time into an RFC date/time string."
  (multiple-value-bind (ss mm hh date month year day-of-week dst-p zone)
      (decode-universal-time time)

    ;; determine if the local time zone is fractional
    (multiple-value-bind (tz tzf)
        (truncate (abs zone))

      ;; get the day of the week and month name
      (let* ((day (nth day-of-week *days*))
             (month-name (nth (1- month) *months*))

             ;; get the time zone information
             (tzh (if dst-p (1- tz) tz))
             (tzn (minusp tzh))
             (tzm (if tzf (* tzf 60) 0))

             ;; get the date format that should be applied
             (date-format (assoc format *rfc-formats*)))
        (if (null date-format)
            (error "Unknown RFC date/time format ~s" format)
          (with-output-to-string (s)

            ;; if the date format wants zone-name, use GMT
            (when (find #\Z (second date-format))
              (multiple-value-setq (ss mm hh date month year)
                (gmt-time ss mm hh date month year tzh)))

            ;; generate the output string
            (flet ((output-format (c)
                     (case c
                       (#\y (format s "~2,'0d" (short-year year)))
                       (#\Y (format s "~4,'0d" year))
                       (#\M (format s "~a" month-name))
                       (#\m (format s "~d" month))
                       (#\D (format s "~a" (subseq day 0 3)))
                       (#\l (format s "~a" day))
                       (#\d (format s "~d" date))
                       (#\H (format s "~d" hh))
                       (#\i (format s "~2,'0d" mm))
                       (#\s (format s "~2,'0d" ss))
                       (#\P (format s "~:[-~;+~]~2,'0d:~2,'0d" tzn tzh tzm))
                       (#\O (format s "~:[-~;+~]~2,'0d~2,'0d" tzn tzh tzm))

                       ;; time zone names aren't unique, use GMT
                       (#\Z (format s "GMT"))

                       ;; just a character to output
                       (otherwise (princ c s)))))
              (map nil #'output-format (second date-format)))))))))

;;; ----------------------------------------------------

(defun unit-test (&optional (now (get-universal-time)))
  "Make sure that all the formats work."
  (format t "~9a| ~40a | ~10a | Fail~%" "Format" "Encoding" "Time")
  (format t "~9@{~c~:*~}+~42@{~c~:*~}+~12@{~c~:*~}+------~%" #\- #\- #\-)
  (loop
     for (f) in *rfc-formats*

     ;; encode the current time into a string, and decode it back
     for d = (decode-universal-rfc-time now f)
     for e = (encode-universal-rfc-time d f)

     ;; did this format pass?
     for okp = (= now e)

     ;; show the debug output?
     do (format t "~9a| ~40a | ~10d | ~:[X~;~]~%" f d e okp))
  (format t "~9@{~c~:*~}+~42@{~c~:*~}+~12@{~c~:*~}+------~%" #\- #\- #\-))
