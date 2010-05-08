(in-package :binary-data)

(in-suite* :nisp)
(in-suite* root :in :nisp)
(in-suite* little-endian :in root)

(test (multi-byte-object :suite little-endian)
  (flet ((test-multi-byte-object ()
           (defclass test-multi-byte-object (little-endian)
             ((opcode :octets 1 :initform #x90)
              (mod :bits 2 :initform #b01)
              (reg/opcode :bits 3 :initform #b001)
              (r/m :bits 3 :initform #b001))
             (:metaclass binary-data-metaclass))
           (unwind-protect
                (flexi-streams:with-output-to-sequence (s)
                  (write-object (make-instance 'test-multi-byte-object) s))
             (unintern 'test-multi-byte-object))))
    (is (equalp #(144 73) (test-multi-byte-object)))))