(defparameter *struct-address* 0x201020)

;; My knowledge of lisp is rudimentary, so I've yet to work out
;; how to step through ptr below to read its bytes.
(defmethod loading (ptr)
  (when (= ptr *struct-address*)
    (memory-read ptr )
    (msg ">>>> struct read $0" ptr)
    )
)

;;(defun memory-written (x a)
;;	(declare (advice :before memory-write))
;;        (msg "write $x to $a"))

