;; Use this lisp file if you wish to over-ride the parameters which occur within test.c
;; for func0.
(defun entry-func0 () 
  (declare (external entry-func0)) 
  (invoke-subroutine @func0 0x66 0x77 0x88 ))


