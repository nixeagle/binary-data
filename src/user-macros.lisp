(in-package :binary-data)

(defmacro define-binary-class (name superclasses slots &rest options)
  "Defines a BINARY class in the same manner defclass would.

This macro is not api stable, expect breakage for a few days.
  -- nixeagle [2010-05-08 Sat 21:40]"
  `(defclass ,name ,superclasses ,slots
     ,@(if (find :metaclass options :key #'car)
          options                     ;leave as is
          `((:metaclass binary-data-metaclass)
            ,@(remove :metaclass options :key #'car)))))


