;; I did try to add a block of memory, using `memset`, but that didn't work.
(defparameter *my-special-address0* 0x201010)
(defparameter *my-special-address1* 0x201011)
(defparameter *my-special-address2* 0x201012)
(defparameter *my-special-address3* 0x201013)

(defmethod loading (ptr)
    (when (= ptr *my-special-address0*)
          (memory-write ptr 0xFA)
    )
)

(defmethod loading (ptr)
    (when (= ptr *my-special-address1*)
          (memory-write ptr 0xFB)
    )
)

(defmethod loading (ptr)
    (when (= ptr *my-special-address2*)
          (memory-write ptr 0xFC)
    )
)

(defmethod loading (ptr)
    (when (= ptr *my-special-address3*)
          (memory-write ptr 0xFD)
    )
)
