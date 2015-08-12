;;;; RFC822 & RFC3339 Date/Time Parser for Common Lisp
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
  (:use :cl :re :lexer :parse)
  (:export
   #:encode-universal-rfc822-time
   #:encode-universal-rfc3339-time
   #:decode-universal-rfc822-time
   #:decode-universal-rfc3339-time))

(in-package :rfc-date)

;;; ----------------------------------------------------

(defconstant +days+
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "Days of the week in RFC822 format.")

;;; ----------------------------------------------------

(defconstant +months+
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "Months of the year in RFC822 format.")

;;; ----------------------------------------------------

(defconstant +rfc822+
  "~a, ~a ~a ~a ~2,'0d:~2,'0d:~2,'0d ~:[+~2,'0d~2,'0d~;-~2,'0d~2,'0d~]"
  "Format used for RFC822 dates.")

;;; ----------------------------------------------------

(defconstant +rfc3339+
  "~a-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~:[+~2,'0d:~2,'0d~;-~2,'0d:~2,'0d~]"
  "Format used for RFC3339 dates.")

;;; ----------------------------------------------------

(defconstant +zones+
  '(("GMT" +000) ("UT"  +000)
    ("EST" -500) ("EDT" -400)
    ("CST" -600) ("CDT" -500)
    ("MST" -700) ("MDT" -600)
    ("PST" -800) ("PDT" -700))
  "Named time zones for RFC822 dates.")

;;; ----------------------------------------------------

(defun military-time-zone (c)
  "Convert a character to a time zone."
  (let ((n (char-code (char-upcase c))))
    (cond
     ((<= 78 n 89) (- 78 n 1))      ; [N,Y]
     ((<= 65 n 73) (- n 64))        ; [A,I]
     ((<= 75 n 77) (+ 10 (- n 75))) ; [K,M]
     (t 0))))

;;; ----------------------------------------------------

(defun time-zone (hh mm)
  "Convert an -hh:mm time to a time zone fraction."
  (+ hh (/ (* mm 60) 3600)))

;;; ----------------------------------------------------

(defun rfc822-time-zone (name)
  "Use either +zones+ or a military time zone."
  (if (= (length name) 1)
      (military-time-zone (char name 0))
    (let ((zone (assoc name +zones+ :test #'string-equal)))
      (if (null zone)
          0
        (/ (second zone) 100)))))

;;; ----------------------------------------------------

(deflexer rfc822-date-lexer (s)
  ("%s+"   :next-token)
  (","     :comma)

  ;; time of day
  ("(%d%d):(%d%d):(%d%d)"
   (values :time (list (parse-integer $1)
                       (parse-integer $2)
                       (parse-integer $3))))

  ;; day, year, time, time zone
  ("([+%-]%d%d)(%d%d)"
   (values :tz (time-zone (parse-integer $1) (parse-integer $2))))
  ("%d%d%d%d"
   (values :year (parse-integer $$)))
  ("%d%d?"
   (values :day (parse-integer $$)))

  ;; days, months, and time zones
  ("%a+"
   (cond
     ((position $$ +days+ :test #'string-equal)
      (values :day-of-week (position $$ +days+ :test #'string-equal)))
     ((position $$ +months+ :test #'string-equal)
      (values :month (position $$ +months+ :test #'string-equal)))
     (t
      (values :tz (rfc822-time-zone $$))))))

;;; ----------------------------------------------------

(deflexer rfc3339-date-lexer (s)
  ("T(%d%d):(%d%d):(%d%d)"
   (values :time (list (parse-integer $1)
                       (parse-integer $2)
                       (parse-integer $3))))

  ;; ignore partial seconds
  ("%.%d%d*"
   (values :next-token))

  ;; year, month, day
  ("(%d%d%d%d)%-"
   (values :year (parse-integer $1)))
  ("(%d%d)%-"
   (values :month (parse-integer $1)))
  ("(%d%d)"
   (values :day (parse-integer $1)))

  ;; time + zone letter
  ("([+%-]%d%d):(%d%d)"
   (values :tz (time-zone (parse-integer $1) (parse-integer $2))))
  ("%a"
   (values :tz (military-time-zone (char $$ 0)))))

;;; ----------------------------------------------------

(defparser rfc822-date-parser
  (>> (.opt (>> (.is :day-of-week) (.is :comma)))
      (.let* ((dd   (.is :day))
              (mm   (.is :month))
              (yy   (.is :year))
              (tt   (.is :time))

              ;; the time zone is optional, default to GMT
              (tz     (.opt (.is :tz) 0)))

        ;; construct a universal time
        (destructuring-bind (h m s)
            tt
          (.ret (encode-universal-time s m h dd (1+ mm) yy (- tz)))))))

;;; ----------------------------------------------------

(defparser rfc3339-date-parser
  (.let* ((yy (.is :year))
          (mm (.is :month))
          (dd (.is :day))
          (tt (.is :time))

          ;; timezone is optional, default to GMT
          (tz (.opt (.is :tz) 0)))

    ;; construct a universal time
    (destructuring-bind (h m s)
        tt
      (.ret (encode-universal-time s m h dd mm yy (- tz))))))

;;; ----------------------------------------------------

(defun encode-universal-rfc822-time (date-time-string)
  "Encode a universal time from the format ddd, dd MMM yyyy HH:mm:ss tz."
  (with-lexer (lexer 'rfc822-date-lexer date-time-string)
    (with-token-reader (token-reader lexer)
      (parse 'rfc822-date-parser token-reader))))

;;; ----------------------------------------------------

(defun encode-universal-rfc3339-time (date-time-string)
  "Encode a universal time from the format yyyy-MM-ddTHH:mm:ss.fracTZ."
  (with-lexer (lexer 'rfc3339-date-lexer date-time-string)
    (with-token-reader (token-reader lexer)
      (parse 'rfc3339-date-parser token-reader))))

;;; ----------------------------------------------------

(defun decode-universal-rfc822-time (time)
  "Decode a universal time into the format ddd, dd MMM yyyy HH:mm:ss tz."
  (multiple-value-bind (ss mm hh date month year day-of-week dst-p tz)
      (decode-universal-time time)
    (multiple-value-bind (tz tzf)
        (truncate (abs tz))
      (let ((day (nth day-of-week +days+))
            (month (nth (1- month) +months+))
            (tzh (if dst-p (1- tz) tz))
            (tzm (if tzf (* tzf 60) 0)))
        (format nil +rfc822+ day date month year hh mm ss (plusp tz) tzh tzm)))))

;;; ----------------------------------------------------

(defun decode-universal-rfc3339-time (time)
  "Decode a universal time into the format yyyy-MM-ddYHH:mm:ss+/-hh:mm."
  (multiple-value-bind (ss mm hh date month year day-of-week dst-p tz)
      (decode-universal-time time)
    (declare (ignore day-of-week))
    (multiple-value-bind (tz tzf)
        (truncate (abs tz))
      (let ((tzh (if dst-p (1- tz) tz))
            (tzm (if tzf (* tzf 60) 0)))
        (format nil +rfc3339+ year month date hh mm ss (plusp tz) tzh tzm)))))
