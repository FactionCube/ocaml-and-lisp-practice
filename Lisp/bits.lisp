;; Some samples from Land of Lisp.
(defun my-length (list)
  ( if list
       (1+ (my-length (cdr list)))
       0))


(if (= (+ 1 4) 6)
  'yes
  'no
  )

(if '(1)
  'the-list-is-full
  'the-list-is-empty
  )

(defun looper ()
(loop repeat 10
      collect 5))

(defun loopy ()
(loop for n from 4 to 17
      collect n))


