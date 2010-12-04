(defun random-line ()
  "Go to a random line in the current buffer."
  (interactive)
  (let ((lines (count-lines (point-min) (point-max))))
    (goto-line (+ 1 (random lines)))))
