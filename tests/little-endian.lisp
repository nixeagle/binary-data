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


(test (slot-positions :suite little-endian)
  "Make sure we signal error when given a metaclass for `slot-positions'."
  (signals error (compute-slot-positions (find-class 'binary-data-metaclass))))


(test (write-octets :suite little-endian)
  (flet ((test-write-octets (integer)
           (flexi-streams:with-output-to-sequence (s)
                (write-octets integer (make-instance 'little-endian) s))))
    (is (equalp #(24) (test-write-octets 24)))
    (is (equalp #(0) (test-write-octets 0)))
    (is (equalp #(1 0) (test-write-octets 256)))
    (is (equalp #(#x90 #x73 #x43) (test-write-octets 9466691)))))


;;; applies to more then just little endian...
(test (primary-machine-byte-size/general-case :suite root)
  "Primary bytesize is 8 on more or less everything."
  (is (= 8 (primary-machine-byte-size t))))
;;; END
