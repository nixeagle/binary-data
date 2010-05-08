(defpackage #:nixeagle.binary-data
  (:use :cl :closer-mop :nutils)
  (:nicknames :binary-data)
  #.(let (list)
      (do-external-symbols (s :closer-mop)
        (push s list))
      `(:shadowing-import-from :closer-mop ,@list)))

(in-package :nixeagle.binary-data)

(defclass binary-data-object (standard-object)
  ()
  (:documentation
   "Base object class for all classes dealing with binary data.

This will be used in generic functions and method specializers as the base
class. All classes have to be compatable with these methods or implement
modifications so they do the right thing."))

(defclass binary-data-metaclass (standard-class)
  ()
  (:default-initargs :direct-superclasses (list (find-class 'binary-data-object))))

(defgeneric endian (object)
  (:documentation "Returns a keyword indicating the `endian' of OBJECT.

Values that make sense as of [2010-05-06 Thu 01:59] are:
    - :little-endian
    - :big-endian"))

(defclass endian-mixin ()
  ())

(defclass big-endian (endian-mixin)
  ())

(defclass little-endian (endian-mixin)
  ())

(defgeneric bit-size-of (thing)
  (:documentation "Size of THING in bits."))

(defmethod bit-size-of ((object binary-data-object))
  (reduce #'+ (class-slots (class-of object)) :key #'bit-size-of))

(defgeneric size-of (thing)
  (:documentation "Size of THING in octets which are also bytes.

If machine has a different number of bits to represent an octet, define an
additional method that specializes on that machine's class."))

(defmethod size-of ((object binary-data-object))
  "Standard machine byte is 8 bits."
  (ceiling (bit-size-of object) 8))

(defclass bit-field-slot-definition (standard-slot-definition)
  ((bit-field-size :accessor bit-size-of :initarg :bits
                   :initform nil)))

(defmethod initialize-instance :around ((slot bit-field-slot-definition) &rest initargs &key octets)
  (declare (type (or null positive-fixnum) octets))
  (if octets
      (apply #'call-next-method slot :bits (* 8 octets) initargs)
      (call-next-method)))

(defmethod compute-effective-slot-definition :around
    ((class binary-data-metaclass) (name t) slot)
  (let ((effective-slot (call-next-method)))
    (setf (bit-size-of effective-slot)
          (bit-size-of (car slot)))
    effective-slot))

(defclass bit-field-direct-slot-definition (standard-direct-slot-definition
                                            bit-field-slot-definition)
   ())

(defclass bit-field-effective-slot-definition (standard-effective-slot-definition
                                               bit-field-slot-definition)
  ((bit-field-relative-position
    :initform 0
    :type non-negative-fixnum
    :initarg :position
    :accessor bit-field-relative-position
    :documentation "Position relative to first effective slot in class.")))

(defun %compute-little-endian-slot-positions (class)
  (declare (type (or binary-data-metaclass binary-data-object) class))
  (loop
     for slot in (class-slots class)
     for position = 0 then (+ position last-position)
     for last-position = (bit-size-of slot)
     collect position))

(defgeneric compute-slot-positions (class)
  (:documentation "Compute slot positions for binary output."))

(defmethod compute-slot-positions ((class binary-data-metaclass))
  (%compute-little-endian-slot-positions class))

(defmethod compute-slot-positions ((class binary-data-object))
  (%compute-little-endian-slot-positions (class-of class)))

(defmethod validate-superclass ((class binary-data-metaclass)
                                (super standard-class))
  "bit-field classes may inherit from standard classes."
  t)


(defmethod initialize-instance :around
    ((class binary-data-metaclass)
     &rest initargs &key direct-superclasses)
  (declare (list initargs direct-superclasses))
  (if (loop for super in direct-superclasses
         thereis (subclassp super 'binary-data-object))
      (call-next-method)
      (apply #'call-next-method class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'binary-data-object)))
             initargs)))

(defmethod reinitialize-instance :around
    ((class binary-data-metaclass)
     &rest initargs &key direct-superclasses)
  (declare (list initargs direct-superclasses))
  (if (loop for super in direct-superclasses
         thereis (subclassp super 'binary-data-object))
      (call-next-method)
      (apply #'call-next-method class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'binary-data-object)))
             initargs)))

(defmethod direct-slot-definition-class ((class binary-data-metaclass) &key)
  (find-class 'bit-field-direct-slot-definition))
(defmethod effective-slot-definition-class ((class binary-data-metaclass) &key)
  (find-class 'bit-field-effective-slot-definition))

(defmethod compute-slots ((class binary-data-metaclass))
  "Computes slots returning most specific first."
  ;; Copied from sbcl's src/pcl/std-class.lisp [2010-05-06 Thu 21:30]
  ;;
  ;; We do this to preserve the computed slot ordering as this order is
  ;; unspecified in the ANSI spec as well as the AMOP.
  ;;
  ;; We need our slots to be ordered most specific first.
  (let ((name-dslotds-alist ()))
    (dolist (c (reverse (class-precedence-list class)))
      (dolist (slot (class-direct-slots c))
        (let* ((name (slot-definition-name slot))
               (entry (assoc name name-dslotds-alist :test #'eq)))
          (if entry
              (push slot (cdr entry))
              (push (list name slot) name-dslotds-alist)))))
    (mapcar (lambda (direct)
              (compute-effective-slot-definition class
                                                 (car direct)
                                                 (cdr direct)))
            (nreverse name-dslotds-alist))))

(defgeneric primary-byte-size (binary-data)
  (declare (optimize (speed 3) (safety 1)))
  (:documentation "Basic unit for output, most things use 8 bits.")
  (:method ((class binary-data-object))
    "Most everything is 8 bits"
    8))


(defgeneric write-object (object stream))

(defmethod write-object ((obj binary-data-object) stream)
  (let ((slot-positions (compute-slot-positions obj))
        (result-octets-as-integer 0))
    (mapc (lambda (slot slot-position)
            (setf (ldb (byte (bit-size-of slot) slot-position) result-octets-as-integer)
                  (binary-slot-value (slot-value-using-class (class-of obj)
                                                             obj slot)
                                     slot obj)))
          (class-slots (class-of obj)) slot-positions)
    (write-octets result-octets-as-integer obj stream)))

(defgeneric write-octets (value object stream))

(defmethod write-octets ((value integer) (obj little-endian) stream)
  (write-octets (loop for i from (- (* 8 (size-of obj)) 8) downto 0 by 8
                   collect (ldb (byte 8 i) value))
                obj stream))

(defun write-octet-list (list stream)
  "Write LIST to binary stream STREAM.

LIST must contain integers in the range 0 to 255 inclusive."
  (declare ((cons (mod 256)) list)
           (stream stream)
           (optimize (speed 2) (safety 3) (debug 1) (space 0)))
  (write-byte (car list) stream)
  (when (cdr list)
    (write-octet-list (cdr list) stream)))

(defmethod write-octets ((value cons) (obj endian-mixin) stream)
  "Write VALUEs to STREAM.

Values is assumed to be a list of integers already split up into
octets. All values in VALUEs must be in the range 0 to 255 inclusive.

Finally as there is no way to know which order the list VALUE is in, it is
written to STREAM in the exact order as given.

See `write-octet-list' for more details."
  (write-octet-list value stream))

(defgeneric read-object (object stream))

(defgeneric binary-slot-value (new-value slot object))

(defmethod binary-slot-value ((value integer) (slot bit-field-slot-definition) (object t))
  (assert (typep value `(mod ,(expt 2 (bit-size-of slot)))))
  value)

;;; END
