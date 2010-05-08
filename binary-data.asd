(asdf:defsystem :binary-data
  :depends-on (:closer-mop :nutils :eos)
  :serial t
  :components
  ((:file "package")
   (:file "binary-data")))