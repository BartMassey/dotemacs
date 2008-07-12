(defun safe-next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one,
it is an error.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (= arg 1)
      (let ((opoint (point)))
	(forward-line 1)
	(if (or (= opoint (point))
		(not (eq (preceding-char) ?\n)))
	    (error "At end of buffer")
	  (goto-char opoint)
	  (safe-next-line-internal arg)))
    (safe-next-line-internal arg))
  nil)

(defun safe-next-line-internal (arg)
  (if (not (or (eq last-command 'safe-next-line)
	       (eq last-command 'previous-line)))
      (setq temporary-goal-column
	    (if (and track-eol (eolp))
		t
	      (current-column))))
  (if (not (integerp selective-display))
      (forward-line arg)
    ;; Move by arg lines, but ignore invisible ones.
    (while (> arg 0)
      (vertical-motion 1)
      (forward-char -1)
      (forward-line 1)
      (setq arg (1- arg)))
    (while (< arg 0)
      (vertical-motion -1)
      (beginning-of-line)
      (setq arg (1+ arg))))
  (if (eq (or goal-column temporary-goal-column) t)
      (end-of-line)
    (move-to-column (or goal-column temporary-goal-column)))
  nil)
