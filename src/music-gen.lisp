(defun generate-scale (root frequencies)
  "Generate a scale given a root frequency and a list of intervals in semitones."
  (mapcar (lambda (interval)
            (* root (expt 2 (/ interval 12.0))))
          frequencies))

(defun generate-melody (scale length)
  "Generate a melody of given length from the scale."
  (loop for i from 1 to length
        collect (nth (random (length scale)) scale)))

;; Example Usage:
(let* ((root 261.63) ; Root frequency for C4
       (major-scale-intervals '(0 2 4 5 7 9 11 12)) ; Major scale intervals in semitones
       (major-scale (generate-scale root major-scale-intervals))
       (melody-length 16) ; Number of notes in the melody
       (melody (generate-melody major-scale melody-length)))
  melody)
