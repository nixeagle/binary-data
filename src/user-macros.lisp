(in-package :binary-data)

(defmacro define-binary-class (name superclasses slots &rest options)
  `(defclass ,name ,superclasses ,slots
     ,@(if (find :metaclass options :key #'car)
          options                     ;leave as is
          `((:metaclass binary-data-metaclass)
            ,@(remove :metaclass options :key #'car)))))


