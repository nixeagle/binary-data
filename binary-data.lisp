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

(defclass binary-data-object (standard-object)
  ())

(defclass binary-data-metaclass (standard-class)
  ()
  (:default-initargs :direct-superclasses (list (find-class 'binary-data-object))))

(defgeneric endian (object)
  (:documentation "Returns a keyword indicating the `endian' of OBJECT.

Values that make sense as of [2010-05-06 Thu 01:59] are:
    - :little-endian
    - :big-endian"))

(defclass endian-mixin ()
  ((endian :initform :little-endian
           :initarg :endian
           :accessor endian
           :documentation "Endian of the class as a whole.")))

(defgeneric bit-size-of (thing)
  (:documentation "Size of THING in bits."))

(defgeneric size-of (thing)
  (:documentation "Size of THING in octets which are also bytes."))

(defclass bit-field-slot-definition (standard-slot-definition)
  ((bit-field-size :accessor bit-size-of :initarg :bits
                   :initform nil)))

(defmethod initialize-instance :around ((slot bit-field-slot-definition) &rest initargs &key octets)
  (declare (type (or null positive-fixnum) octets))
  (if octets
      (apply #'call-next-method slot :bits (* 8 octets) initargs)
      (call-next-method)))

(defclass bit-field-direct-slot-definition (standard-direct-slot-definition
                                            bit-field-slot-definition)
   ())

(defclass bit-field-effective-slot-definition (standard-effective-slot-definition
                                               bit-field-slot-definition)
   ())

(defmethod validate-superclass ((class binary-data-metaclass)
                                (super standard-class))
  "bit-field classes may inherit from standard classes."
  t)

(defmethod direct-slot-definition-class ((class binary-data-metaclass) &key)
  (find-class 'bit-field-direct-slot-definition))


;;; END
