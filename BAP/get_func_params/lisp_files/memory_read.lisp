
;;(defmacro read-word (t a)
;;  "(read-word T A) reads a word of type T at address A"
;;  (let ((p a)
;;        (x (memory-read p))
;;        (n (-1 (sizeof t))))
;;    (while n
;;      (incr p)
;;      (decr n)
;;      (set x (endian concat x (memory-read p))))
;;    x))


(defparameter *struct-address* 0x201020)

;; This guy isn't working yet, at least I amnot seeing the
;; printed msg in 'incident';.
(defmethod loading (ptr)
  (when (= ptr *struct-address*)
    (let ((p ptr)
	  (x (memory-read ptr))
	  (n (-1 (sizeof int))))
      (while n
	     (incr p)
	     (decr n)
	     (set x (endian concat x (memory-read p))))
      (msg "***** $0 $1" p x))
;;    (memory-read ptr )
;;    (msg ">>>> struct read $0" ptr)
;;    (read-word (int ptr))
    )
)


;;(defun memory-written (x a)
;;	(declare (advice :before memory-write))
;;        (msg "write $x to $a"))

