(defpackage #:nixeagle.binary-data
  (:use :cl)
  (:nicknames :binary-data))
(in-package :nixeagle.binary-data)

(defclass binary () ()
  (:documentation "Abstract class for all classes dealing with binary data.

This will be used in generic functions and method specializers as the base
class. All classes have to be compatable with these methods or implement
modifications so they do the right thing."))


;;; END