(defpackage :rfc-date-asd
  (:use :cl :asdf))

(in-package :rfc-date-asd)

(defsystem :rfc-date
  :name "rfc-date"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "RFC822 and RFC3339 date encoding and decoding for LispWorks."
  :serial t
  :components ((:file "rfc-date"))
  :depends-on ("lexer"))
