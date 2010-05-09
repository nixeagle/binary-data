(in-package :cl-user)

(defpackage #:nixeagle.binary-data
  (:use :cl :closer-mop :nutils :eos)
  (:nicknames :binary-data)
  #.(let (list)
      (do-external-symbols (s :closer-mop)
        (push s list))
      `(:shadowing-import-from :closer-mop ,@list))
  (:export #:define-binary-class))