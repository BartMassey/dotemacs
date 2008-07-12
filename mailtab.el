;; copy of (indent-relative) from indent.el with slightly
;; less agressive search for indentation points.
(defun indent-relative-close ()
  "Space out to under next indent point in previous line.
An indent point is a non-whitespace character following whitespace.
If the previous line has no indent points beyond the
column point starts at, `tab-to-tab-stop' is done instead."
  (interactive)
  (if (and abbrev-mode
	   (eq (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
  (let ((start-column (current-column))
	indent)
    (save-excursion
      (if (= (forward-line -1) 0)
	  (let ((end (save-excursion (forward-line 1) (point))))
	    (move-to-column start-column)
	    ;; Is start-column inside a tab on this line?
	    (if (> (current-column) start-column)
		(backward-char 1))
	    (if (not (looking-at "[ \t]"))
		(skip-chars-forward "^ \t" end))
	    (skip-chars-forward " \t" end)
	    (or (= (point) end) (setq indent (current-column))))))
    (if indent
	(let ((opoint (point-marker)))
	  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
	  (indent-to indent 0)
	  (if (> opoint (point))
	      (goto-char opoint))
	  (move-marker opoint nil))
      (tab-to-tab-stop))))
