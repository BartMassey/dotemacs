(defun id-replace-regexp (REGEXP TO-STRING)
  "Case-insensitive but case-preserving replace-regexp
   for words.
   Maps lowercase to lowercase,
   uppercase to uppercase. Mixed-case preserves capitalization of TO-STRING."
  (interactive "")
  (let ((case-ignore-search t))
    (while (re-search-forward REGEXP nil t)
      (let* ((start (match-beginning))
	     (end (match-end))
	     (has-upper (save-excursion)
	(replace-match TO-STRING t nil))
