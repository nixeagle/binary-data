(defpackage #:nixeagle.binary-data
  (:use :cl))
(in-package :nixeagle.binary-data)

#+ ()
(defclass primitive-type-maybe? ()
  ((value :type foo)
   (bit-size :type (integer 0 arch-max-size))))