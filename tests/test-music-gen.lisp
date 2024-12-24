(load "./src/music-gen.lisp")

;; Test function to generate and display a melody
(defun run-tests ()
  (let* ((root 261.63)
         (major-scale-intervals '(0 2 4 5 7 9 11 12))
         (major-scale (generate-scale root major-scale-intervals))
         (melody-length 16)
         (melody (generate-melody major-scale melody-length)))
    ;(format t "Generated Melody: ~a~%" melody)))
    ; space delimited:
    (format t "~{~a ~}~%" melody)))

;; Automatically run tests when the script is executed
(run-tests)
