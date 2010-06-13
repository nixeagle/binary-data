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

(defmethod validate-superclass ((class binary-data-metaclass)
                                (super standard-class))
  "binary classes may inherit from standard classes."
  t)

(defmethod initialize-instance :around
    ((class binary-data-metaclass)
     &rest initargs &key direct-superclasses)
  "Make sure that BINARY-DATA-OBJECT is in supers list."
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
  "Make sure that BINARY-DATA-OBJECT is in supers list."
  (declare (list initargs direct-superclasses))
  (if (loop for super in direct-superclasses
         thereis (subclassp super 'binary-data-object))
      (call-next-method)
      (apply #'call-next-method class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'binary-data-object)))
             initargs)))

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
              #+ () (unless (bit-size-of (car (cdr direct)))
                (setf (bit-size-of (car (cdr direct)))
                      (or (bit-size-of (find-if #'bit-size-of (cddr direct)))
                          (error ":bits or :octets not found in a slot for ~A."
                                 (car direct)))))
              (compute-effective-slot-definition class
                                                 (car direct)
                                                 (cdr direct)))
            (nreverse name-dslotds-alist))))

(defclass binary-slot-definition (standard-slot-definition)
  ())
(defclass binary-direct-slot-definition (standard-direct-slot-definition
                                            binary-slot-definition)
  ())

(defclass binary-effective-slot-definition (standard-effective-slot-definition
                                               binary-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class binary-data-metaclass) &key)
  (find-class 'binary-direct-slot-definition))
(defmethod effective-slot-definition-class ((class binary-data-metaclass) &key)
  (find-class 'binary-effective-slot-definition))

(defmethod initialize-instance :around ((slot binary-slot-definition) &rest initargs &key octets)
  (declare (type (or null positive-fixnum) octets))
  (if octets
      (apply #'call-next-method slot :bits (* 8 octets) initargs)
      (call-next-method)))
