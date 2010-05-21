(in-package :binary-data)



(define-binary-class 32-bit-elf-identifier (little-endian)
  ((magic-octet :octets 1 :initform #x7f)
   (file-identifier-string :octets 3 :initform "ELF")
   (file-class :octets 1 :initform 1
               :documentation "Identifies file's class. 1 for 32bit, 2 for 64bit.")
   (data-encoding :octets 1)
   (file-version :octets 1 :initform 1
                 :documentation "Only valid version is number 1.")
   (padding-bytes :octets 8)
   (size-of :octets 1)))

(define-binary-class 32-bit-elf-header (little-endian)
  ((identifier :octets 16 :initform #xF1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1)
   (type :octets 2 :initform 0)
   (machine :octets 2 :initform 0)
   (version :octets 4 :initform 1)
   (entry :octets 4 :initform 0 :type (unsigned-byte 32))
   (program-offset :octets 4 :initform 0 :type (unsigned-byte 32))
   (section-offset :octets 4 :initform 0 :type (unsigned-byte 32))
   (flags :octets 4 :initform 0)
   (elf-header-size :octets 2 :initform 52 :type (unsigned-byte 16))
   (program-entry-size :octets 2 :initform 0 :type (unsigned-byte 16))
   (program-entry-count :octets 2 :initform 0 :type (unsigned-byte 16))
   (section-entry-size :octets 2 :initform 0 :type (unsigned-byte 16))
   (section-entry-count :octets 2 :initform 0 :type (unsigned-byte 16))
   (string-section-table-index :octets 2 :initform 0 :type (unsigned-byte 16))))

;;; END
