(in-package :binary-data)

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
  "Computes total octets/bytes that OBJECT takes.

For most BINARY-DATA-OBJECT's the primary machine size can be specified in
`primary-machine-byte-size' and defaults to 8.

If some types are defined in terms of octets/bytes instead of bits, this
can be defined to return that number instead of computing one."
  (ceiling (bit-size-of object) (primary-machine-byte-size object)))

(defmethod size-of ((object binary-data-metaclass))
  "Works with metaclasses, won't work with resizable classes though."
  (ceiling (bit-size-of object) (primary-machine-byte-size object)))

(defclass bit-field-slot-definition (standard-slot-definition)
  ((bit-field-size :accessor bit-size-of :initarg :bits
                   :initform nil)))

(defmethod initialize-instance :around ((slot bit-field-slot-definition) &rest initargs &key octets)
  (declare (type (or null positive-fixnum) octets))
  (if octets
      (apply #'call-next-method slot :bits (* 8 octets) initargs)
      (call-next-method)))

(defmethod compute-effective-slot-definition :around
    ((class binary-data-metaclass) (name t) slots)
  (let ((effective-slot (call-next-method)))
    (setf (bit-size-of effective-slot)
          (bit-size-of (or (find-if #'bit-size-of slots) 0)))
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

(defun %compute-little-endian-slot-positions (object &aux (class (class-of object)))
  (declare (type (or binary-data-object) object))
  (loop
     for slot in (class-direct-slots class)
     for position = (- (bit-size-of object) (bit-size-of slot)) then (- position (bit-size-of slot))
     collect position))

(defgeneric compute-slot-positions (class)
  (:documentation "Compute slot positions for binary output."))

(defmethod compute-slot-positions ((class binary-data-metaclass))
  "Signals an error, we require object instances, not classes."
  (error "COMPUTE-SLOT-POSITIONS works with class instances only, not ~A"
         class))

(defgeneric compute-endian-slot-positions (class)
  (:documentation "Compute slots based on endianess."))

(defmethod compute-slot-positions ((class binary-data-object))
  (compute-endian-slot-positions class))

(defmethod compute-endian-slot-positions ((class little-endian))
  (%compute-little-endian-slot-positions class))


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
              (unless (bit-size-of (car (cdr direct)))
                (setf (bit-size-of (car (cdr direct)))
                      (or (bit-size-of (find-if #'bit-size-of (cddr direct)))
                          (error ":bits or :octets not found in a slot for ~A."
                                 (car direct)))))
              (compute-effective-slot-definition class
                                                 (car direct)
                                                 (cdr direct)))
            (nreverse name-dslotds-alist))))

(defgeneric primary-machine-byte-size (binary-data)
  (declare (optimize (speed 3) (safety 1)))
  (:documentation "Basic unit for output, most things use 8 bits.")
  (:method (thing)
    "Most everything is 8 bits"
    8))


(defgeneric write-object (object stream))

(defmethod write-object ((obj binary-data-object) stream)
  (let ((slot-positions (compute-slot-positions obj))
        (result-octets-as-integer 0))
    (mapc (lambda (slot slot-position)
            (setf (ldb (byte (bit-size-of slot) slot-position) result-octets-as-integer)
                  (binary-slot-value (slot-value obj (slot-definition-name slot))
                                     slot (slot-definition-name slot) obj)))
          (class-direct-slots (class-of obj)) slot-positions)
    (write-octets result-octets-as-integer obj stream)))

(defgeneric write-octets (value object stream))

(defmethod write-octets ((value integer) (obj little-endian) (stream stream))
  (write-octets (loop for i from (* 8 (floor (log (1+ value) 255))) downto 0 by 8
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

aSee `write-octet-list' for more details."
  (write-octet-list value stream))

(defgeneric read-object (object stream))

(defgeneric binary-slot-value (new-value slot name object))

(defmethod binary-slot-value ((value integer) (slot bit-field-slot-definition) (name symbol) (object t))
  (assert (typep value `(mod ,(expt 2 (bit-size-of slot)))))
  value)

;;; END
