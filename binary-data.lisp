;;; All this source is based on practical common lisp's chapter 24 with a
;;; MOP twist. The license of that code is BSD and follows
;;;
;;; Copyright (c) 2005, Peter Seibel All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials provided
;;;       with the distribution.
;;;
;;;     * Neither the name of the Peter Seibel nor the names of its
;;;       contributors may be used to endorse or promote products derived
;;;       from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; My modifications will also be released under the same license.

(in-package :binary-data)

(defgeneric write-value (type value stream &key)) ; &key slot object ??
(defgeneric read-value (type stream &key)) ; &key slot object ??
(defgeneric read-object (object stream))
(defgeneric write-object (object stream))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))


(defun type-reader-body (spec stream)
  (case (length spec)
    (1 (destructuring-bind (type &rest args) (alexandria:ensure-list (first spec))
         `(read-value ',type ,stream ,@args)))
    (otherwise (destructuring-bind ((in) &body body) (cdr (assoc :reader spec))
         `(let ((,in ,stream)) ,@body)))))

(defun type-writer-body (spec stream value)
  (case (length spec)
    (1 (destructuring-bind (type &rest args) (alexandria:ensure-list (first spec))
         `(write-value ',type ,stream ,value ,@args)))
    (otherwise (destructuring-bind ((out v) &body body) (cdr (assoc :writer spec))
         `(let ((,out ,stream) (,v ,value)) ,@body)))))

(defmacro define-binary-type (name (&rest args) &body spec)
  (nutils:with-gensyms (type stream value)
    `(progn
       ,(when (and (< 1 (length spec)) (assoc-value spec :type))
              `(deftype ,name (,@(if args
                                     (append (list '&key) args)))
                 ,@(assoc-value spec :type)))
       (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
         (declare (ignorable ,@(mapcar #'ensure-car args)))
         ,(type-reader-body spec stream))
       (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
         (declare (ignorable ,@(mapcar #'ensure-car args)))
         ,(type-writer-body spec stream value)))))

(defun slot-to-defclass-slot (specification)
  "Convert specification to a slotname with initarg."
  (let ((name (first specification)))
    `(,name :initarg ,(alexandria:make-keyword name)
            :type ,(second specification))))

(defun normalize-slot-specification (specification)
  (list (first specification) (alexandria:ensure-list (second specification))))

(defun slot->read-value (specification stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-specification specification)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->binding (specification stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-specification specification)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (specification)
  (let ((name (first specification)))
    `(,(alexandria:make-keyword name) ,name)))

(defun slot->write-value (specification stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-specification specification)
    `(setf ,name (read-value ',type ,stream ,@args))))


(defun write-slotd-value (object slotd stream)
  (declare (closer-mop:slot-definition slotd)
           (stream stream))
  (write-value (closer-mop:slot-definition-type slotd)
               (closer-mop:slot-value-using-class (class-of object) object slotd)
               stream))

(defun read-slotd-value (slotd stream)
  (declare (closer-mop:slot-definition slotd)
           (stream stream))
  (read-value (closer-mop:slot-definition-type slotd)

              stream))

(defmethod write-object ((object closer-mop:standard-object) stream)
  (mapc (lambda (slotd)
          (write-slotd-value object slotd stream))
   (closer-mop:class-slots (class-of object))))

(defmethod read-object ((object closer-mop:standard-object) stream)
  (mapc
   (lambda (slotd)
     (setf (closer-mop:slot-value-using-class (class-of object) object slotd)
           (read-slotd-value slotd stream)))
   (closer-mop:class-slots (class-of object)))
  object)

(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  `(progn
     (defclass ,name ,superclasses
       ,(mapcar #'slot-to-defclass-slot slots)
       (:metaclass binary-data-metaclass))
     ,read-method))

(defmacro define-binary-class (name (&rest superclasses) slots)
  `(define-generic-binary-class ,name ,superclasses ,slots ()))
