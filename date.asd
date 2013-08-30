(defpackage :date-asd
  (:use :cl :asdf))

(in-package :date-asd)

(defsystem :date
  :name "date"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "RFC822 and RFC3339 date encoding and decoding for LispWorks."
  :serial t
  :components ((:file "date"))
  :depends-on ("lexer"))
