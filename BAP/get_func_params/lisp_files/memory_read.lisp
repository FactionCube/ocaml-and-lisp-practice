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

(defmethod call-return (name ptr ret)
  (when 
    (and (= ptr *struct-address*) (not (points-to-null ret)))
      (msg "***** $0 $1 " name ret))
    )

;;(defun memory-written (x a)
;;	(declare (advice :before memory-write))
;;        (msg "write $x to $a"))

