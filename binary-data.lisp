(defpackage #:nixeagle.binary-data
  (:use :cl :closer-mop :nutils)
  (:nicknames :binary-data)
  #.(let (list)
      (do-external-symbols (s :closer-mop)
        (push s list))
      `(:shadowing-import-from :closer-mop ,@list)))

(in-package :nixeagle.binary-data)

(defclass binary () ()
  (:documentation "Abstract class for all classes dealing with binary data.

This will be used in generic functions and method specializers as the base
class. All classes have to be compatable with these methods or implement
modifications so they do the right thing."))

(defclass primitive-binary (binary)
  ((value :type (integer 10 20))
   size)
  (:documentation "Simple binary types such as integers."))

(defclass endian-slot-definition (standard-slot-definition)
  ((endian :initarg :endian :initform :little-endian)))

(defclass endian-direct-slot-definition (standard-direct-slot-definition
                                         endian-slot-definition)
  ())

(defclass endian-effective-slot-definition (standard-effective-slot-definition
                                            endian-slot-definition)
  ())

(defclass bit-field-metaclass (standard-class)
  ())

(defclass bit-field-slot-definition (standard-slot-definition)
  ((bit-field-size :accessor bit-field-size :initarg :bits
                   :initform nil)))

(defclass bit-field-direct-slot-definition (standard-direct-slot-definition
                                            bit-field-slot-definition)
   ())

(defclass bit-field-effective-slot-definition (standard-effective-slot-definition
                                               bit-field-slot-definition)
   ())


;;; END