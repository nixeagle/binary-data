(in-package :cl-user)

(defpackage #:nixeagle.binary-data
  (:use :cl :closer-mop :nutils :eos)
  (:nicknames :binary-data)
  #.(let (list)
      (do-external-symbols (s :closer-mop)
        (push s list))
      `(:shadowing-import-from :closer-mop ,@list))
  (:export #:define-binary-class
           #:big-endian
           #:little-endian
           #:endian-mixin
           #:bit-size-of
           #:size-of
           #:compute-slot-positions
           #:compute-endian-slot-positions
           #:primary-machine-byte-size
           #:write-object
           #:write-octets
           #:read-object
           #:binary-slot-value
           #:binary-data-object
           #:binary-data-metaclass))

;;; END
