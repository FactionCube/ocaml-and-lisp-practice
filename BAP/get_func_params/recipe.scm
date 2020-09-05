(parameter depth 32768 "a depth of analysis")
;; Over-ride the parameters within test.c for func0
;; with those which you set in the entry.lisp file.
(parameter entry-pointsA entry-func0 "where to search")
(parameter entry-pointsB main "")
(parameter entry-pointsC struct_set_a "")
(parameter entry-pointsD struct_set_b "")
(parameter entry-points all-subroutines "where to search")
(parameter optimization 0 "optimization level")

(option primus-lisp-load
  posix
  entry
  memory_read
  memory_write
)

(option primus-lisp-add "./lisp_files")

(option passes
        run)

(option primus-lisp-channel-redirect
  <stdin>:${prefix}/stdin
  <stdout>:${prefix}/stdout
  <stderr>:${prefix}/stderr
  )

(option report-progress)
(option log-dir log)

(option run-entry-points
	${entry-points}
;;	${entry-pointsA}
;;	${entry-pointsB}
;;	${entry-pointsC}
;;	${entry-pointsD}
	)
(option constant-tracker-enable)

(option primus-promiscuous-mode)
(option primus-greedy-scheduler)
(option primus-print-output incidents)
(option primus-limit-max-length $depth)

(option primus-print-obs
  call
  call-return
  lisp-message
  stored
  written
;;  read
  loaded
  )

(option api-path "./api")

(option optimization-level $optimization)
