(defpackage #:binary-data-tests
  (:use :cl :eos :flexi-streams :binary-data))

(in-package :binary-data-tests)

(defmacro out->seq (&body body)
  "All output of BODY goes to a vector."
  `(with-output-to-sequence (*standard-output*)
     ,@body))
