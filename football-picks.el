;; Major Mode To Pick Football Games
;; Shawn Wolfe and Bart Massey 10/92
;; Last hacked by Bart 11/93

;This is a "full-featured" football-picks-mode for emacs.
;The only things it expects from the e-mail message containing
;the week's picks are (the things in slashes are just regexps):
;
;	- A line somewhere of the form /^For Week *[0-9]*/
;	(actually optional, but recommended)
;
;	- The picks start with a line beginning with
;	/^Favorite/.  /Underdog$/ must end this line, and must
;	start in the same column as the underdog teams below it.
;
;	- The second line of the picks is a bunch of dashes
;	which "underline" the stuff in the first line.
;
;	- At least two columns separate each favorite team name from
;	the spread value (don't ask).
;
;	- The picks end with the line  /^---$/ or at EOF.
;
;	- Lines starting /^[A-Za-z0-9]/ are picks.  All
;	others aren't.  Blank lines are removed.
;
;To use this, save the emacs lisp code below in an emacs
;lisp library directory (personal or system) and then stick
;the line below in your .emacs :
;  (autoload 'football-picks "football-picks" "Pick football games." t)
;To make the picks, just get the mail message containing them
;into an emacs-buffer, and say \M-x football-picks .
;You will now find yourself staring at a picks buffer.  Use ^N and
;^P to move through the picks, and ^F, ^B, ^A, ^E to select a team.
;When you are pointing at a team you wish to pick, use ^@ to
;"mark" your pick.  You can change the person you're picking
;for with ^C-p , and the week you're picking for with ^C-w .
;You can comment the current pick with ^C-c .
;
;When you've picked all the games, say ^C-s .  This will prompt
;you for the name of a file, and then save a formatted mail
;message containing your picks into this file.  Alternatively
;say ^C-^C , which will actually send the mail message off directly.
;
;Things still to be done:
;	- Better error recovery (esp. when not all games
;	have been picked).
;       - Better file handling.
;       - emacs 19 compatibility (when the lisp manual comes out)

(autoload 'mail-send "sendmail" nil t)

(defun football-picks-build-mode-map ()
  "Build the mode map for football picks mode."
  (let  ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "\C-n" 'football-picks-next)
    (define-key map "\C-p" 'football-picks-prev)
    (define-key map "\C-a" 'football-picks-favorite)
    (define-key map "\C-e" 'football-picks-underdog)
    (define-key map "\C-f" 'football-picks-other)
    (define-key map "\C-b" 'football-picks-other)
    (define-key map "\C-@" 'football-picks-pick)
    (define-key map "\C-cc" 'football-picks-comment-pick)
    (define-key map "\C-cp" 'football-picks-new-person)
    (define-key map "\C-cw" 'football-picks-new-week)
    (define-key map "\C-cs" 'football-picks-mail-save)
    (define-key map "\C-c\C-c" 'football-picks-mail-send)
    map
    )
  )
;; the keymap 
(defvar football-picks-mode-map (football-picks-build-mode-map)
  "Football picks keymap."
  )

;; the destination
(defvar football-picks-mail-addr "shawn@ptolemy.arc.nasa.gov (Shawn Wolfe)"
  "Person to e-mail football picks to."
  )

;; the user
(defvar football-picks-person (capitalize (user-login-name))
  "*Person to credit for football picks."
  )
(make-variable-buffer-local 'football-picks-person)

;; the week
(defvar football-picks-week -1
  "*Week of football picks."
  )
(make-variable-buffer-local 'football-picks-week)

;; the underdog column
(defvar football-picks-underdog-column -1
  "Column for football picks underdog."
  )
(make-variable-buffer-local 'football-picks-underdog-column)

;; saved buffer name, for building new buffer name
(defvar football-picks-old-buffer-name "?"
  "Old name of football picks buffer."
  )
(make-variable-buffer-local 'football-picks-old-buffer-name)

;; string to use for comments
(defvar football-picks-comment-string "--- "
  "Comment indicator in football picks."
  )
(make-variable-buffer-local 'football-picks-comment-string)

;; this is the command to enter picks mode
(defun football-picks-mode ()
  "Major mode for making football picks.
The picks are e-mailed by the pool manager in a special format.

\\{football-picks-mode-map}"
  (interactive)
  (setq major-mode 'football-picks-mode)
  (setq mode-name "Football-Picks")
  (use-local-map football-picks-mode-map)
  (auto-save-mode nil)
  (football-picks-set-buffer-name)
  (save-excursion
    (if (= football-picks-week -1)
	(progn
	 (goto-char (point-min))
	 (forward-line 1)
	 (let ((nl (point)))
	   (goto-char (point-min))
	   (let ((w (re-search-forward "[Ww]eek [0-9]+" nl t)))
	     (if w
		 (progn
		  (goto-char (point-min))
		  (re-search-forward "[Ww]eek " nl)
		  (setq football-picks-week
			(string-to-int (buffer-substring (point) w)))))))))
    (goto-char (point-min))
    (forward-line 1)
    (let (eol (point))
      (goto-char (point-min))
      (re-search-forward "^- Favorite" eol nil)
      (re-search-forward "- Underdog$" eol nil)
      (setq football-picks-underdog-column (- (current-column) 10))
      )
    )
  (beginning-of-line)
  (if (not (football-picks-selection-char (char-after (point))))
      (football-picks-next))
  )

(defun football-picks ()
  "Make football picks."
  (interactive)
  (set-visited-file-name nil)
  (setq buffer-read-only nil)
  (setq football-picks-old-buffer-name (buffer-name))
  (football-picks-format-buffer)
  (football-picks-mode)
  )

(defun forward-line-wrapping ()
  "Do forward-line, `wrapping around' the end of the buffer.
If the last line is empty, it is skipped on wrap."
  (if (= (forward-line 1) 0)
      (if (not (= (point) (point-max)))
	  t
	(goto-char (point-min))
	nil
	)
    (goto-char (point-min))
    nil
    )
  )

(defun backward-line-wrapping ()
  "Do backward-line, `wrapping around' the start of the buffer.
If the last line is empty, it is skipped on wrap."
  (if (= (forward-line -1) 0)
      t
    (goto-char (point-max))
    (beginning-of-line ())
    (if (= (point) (point-max))
	(forward-line -1)
      )
    nil
    )
  )

(defun next-alphanum-line ()
  "Move forward to a line starting with an alphanumeric."
  (let ((result (re-search-forward "^[a-zA-Z0-9]" (point-max) t)))
    (beginning-of-line ())
    result
    )
  )

(defun football-picks-buffer-local-variables ()
  "Interesting state variables in football-picks-mode."
  (list
   (cons 'football-picks-person football-picks-person)
   (cons 'football-picks-week football-picks-week)
   (cons 'football-picks-underdog-column football-picks-underdog-column)
   )
  )

(defun restore-buffer-locals (vl)
  "Restore buffer local variables."
  (while vl
    (set (car (car vl)) (cdr (car vl)))
    (setq vl (cdr vl))
    )
  )

(defun football-picks-format-buffer ()
  "Turn an e-mail message into a football picks buffer."
  (interactive)
  (let (bol eol)
    ;; find week
    (goto-char (point-min))
    (if (not (re-search-forward "^For Week *" (point-max) t))
	nil
      (setq bol (point))
      (end-of-line nil)
      (setq football-picks-week
	    (string-to-int (buffer-substring bol (point)))
	    )
      )
    ;; delete garbage
    (goto-char (point-min))
    (re-search-forward "^Favorite" (point-max) nil)
    (beginning-of-line nil)
    (delete-region (point-min) (point))
    (forward-line 2)
    (re-search-forward "---" (point-max) 'move)
    (delete-region (point) (point-max))
    (goto-char (point-min))
    (delete-matching-lines "^[ 	]*$")
    ;; find underdog column
    (goto-char (point-min))
    (end-of-line nil)
    (setq eol (point))
    (beginning-of-line nil)
    (re-search-forward "Underdog$" eol nil)
    (setq football-picks-underdog-column (- (current-column) 6))
    ;; fixup favorite line
    (beginning-of-line nil)
    (setq bol (point))
    (end-of-line nil)
    (untabify bol (point))
    (beginning-of-line nil)
    (insert-char ?- 1)
    (insert-char ?\  1)
    (move-to-column football-picks-underdog-column)
    (insert-char ?- 1)
    (insert-char ?\  1)
    ;; fixup separator line
    (forward-line 1)
    (setq bol (point))
    (end-of-line nil)
    (untabify bol (point))
    (beginning-of-line nil)
    (insert-char ?- 2)
    (move-to-column football-picks-underdog-column)
    (insert-char ?- 2)
    ;; fixup actual picks lines
    (while (next-alphanum-line)
      (setq bol (point))
      (end-of-line nil)
      (untabify bol (point))
      (beginning-of-line nil)
      (insert-char ?\  2)
      (move-to-column football-picks-underdog-column)
      (insert-char ?\  2)
      )
    ;; leave cursor at first pick
    (goto-char (point-min))
    (football-picks-next)
    )
  )

(defun football-picks-mail-format-buffer ()
  "Turn a football picks buffer into an e-mail message."
  (interactive)
  (let ((done nil) bol eol)
    ;; first two lines are always garbage
    (goto-char (point-min))
    (forward-line 2)
    (delete-region (point-min) (point))
    (while (not done)
      (if (football-picks-selection-char (char-after (point)))
	  (let ()
	    (football-picks-mail-fixup-pick)
	    (setq done (= (point) (point-max)))
	    (if (and (not done) (football-picks-at-comment))
		(let ((tmpbol (point)))
		  (backward-char 1)
		  (delete-region tmpbol (point))
		  (indent-to-column 20)
		  (forward-line 1)
		  )
	      )
	    )
	(beginning-of-line nil)
	(setq bol (point))
	(forward-line 1)
	(delete-region bol (point))
	)
      (setq done (= (point) (point-max)))
      )
    )
;  (goto-char (point-min))
;  (replace-regexp "^" "  ")
  (goto-char (point-min))
  (insert "To: " football-picks-mail-addr "\n")
  (insert "Subject: [pool] " football-picks-person
	  "'s Picks (For Week " (football-picks-week-string) ")\n")
  (insert "--text follows this line--\n")
; (insert football-picks-person
;	"'s Picks (For Week " (football-picks-week-string) "):\n")
  (goto-char (point-min))
  )

(defun football-picks-mail-fixup-pick ()
  "Turn one football picks buffer line into an e-mail message line."
  (move-to-column football-picks-underdog-column)
  (let ((udc (point)))
    (beginning-of-line ())
    (cond
     ((= (char-after (point)) ?* )
      (delete-region (point) (+ (point) 2))
      (let (bol eol)
	(end-of-line ())
	(setq eol (point))
	(beginning-of-line ())
	(setq bol (point))
	(re-search-forward "  " eol t)
	(delete-region (- (point) 2) eol)
	)
      (forward-line 1)
      )
     ((= (char-after udc) ?* )
      (delete-region (point) (+ udc 2))
      (forward-line 1)
      )
     (t
      (error "No pick for line.")
      )
     )
    )
  )

(defun football-picks-mail-save (filename)
  "Save the current picks in a file as a mail message."
  (interactive "FFile: ")
  (let ((v (football-picks-buffer-local-variables)) (b (current-buffer)))
    (find-file-other-window filename)
    (let ((c (current-buffer)))
      (delete-region (point-min) (point-max))
      (pop-to-buffer b)
      (copy-to-buffer c (point-min) (point-max))
      (pop-to-buffer c)
      )
    (restore-buffer-locals v)
    (football-picks-mail-format-buffer)
    (save-buffer)
    (pop-to-buffer b)
    (not-modified)
    )
  )

    
(defun football-picks-mail-send ()
  "Send the picks to the pick-manager."
  (interactive)
  (let ((v (football-picks-buffer-local-variables)) (b (current-buffer)))
    (let ((c (get-buffer-create "Football picks mail message")))
      (pop-to-buffer c)
      (delete-region (point-min) (point-max))
      (pop-to-buffer b)
      (copy-to-buffer c (point-min) (point-max))
      (pop-to-buffer c)
      )
    (restore-buffer-locals v)
    (football-picks-mail-format-buffer)
    (let ((mail-interactive t))
      (mail-send)
      )
    (pop-to-buffer b)
    (not-modified)
    )
)

(defun football-picks-week-string ()
  "String representing current football picks week."
  (if (> football-picks-week 0)
      (format "%d" football-picks-week )
    "?"
    )
  )

(defun football-picks-set-buffer-name ()
  "Set the football picks buffer name from the buffer state."
  (rename-buffer (concat
		  football-picks-person
		  "'s week "
		  (football-picks-week-string)
		  " picks"
		  )
		 )
  )

(defun football-picks-new-person (who)
  "Set the person for whom the football picks are being made."
  (interactive "sWho: ")
  (setq football-picks-person who)
  (football-picks-set-buffer-name)
  )

(defun football-picks-new-week (when)
  "Set the person for whom the football picks are being made."
  (interactive "NWeek: ")
  (setq football-picks-week when)
  (football-picks-set-buffer-name)
  )

(defun football-picks-at-comment ()
  (let ((current (point))
	(answer nil))
    (forward-line 1)
    (let ((comment (buffer-substring current (point))))
      (if (and
	   (>= (length comment) 4)
	   (string-equal
	    (substring comment 0 4)
	    football-picks-comment-string))
	  (setq answer comment)))
    (goto-char current)
    answer))

(defun football-picks-get-comment ()
  (let ((current (point))
	(old-comment ""))
    (forward-line 1)
    (let ((comment (football-picks-at-comment)))
      (if comment
	  (let ((current (point)))
	    (forward-line 1)
	    (delete-region current (point))
	    (setq
	     old-comment
	     (substring comment 4 (- (length comment) 1))))))
    (goto-char current)
    (list (read-string "Comment: " old-comment))))

(defun football-picks-comment-pick (comment)
  "Attach a COMMENT to the current football pick."
  (interactive (football-picks-get-comment))
  (if (and comment (not (string-equal comment "")))
      (let ((current (point)))
	(forward-line 1)
	(open-line 1)
	(insert "--- " comment)
	(goto-char current))))

(defun football-picks-selection-char (ch)
  "Character is a football picks selection char."
  (or (= ch ?\  ) (= ch ?* ))
  )

;; this moves the cursor to the "next pick"
(defun football-picks-next ()
  "Move to the next game to be picked."
  (interactive)
  (beginning-of-line nil)
  (let ((not-wrapped (forward-line-wrapping)))
    (while (not (football-picks-selection-char (char-after (point))))
      (if (not (forward-line-wrapping))
	  (setq not-wrapped nil)
	)
      )
    not-wrapped
    )
  )

;; this moves the cursor to the "previous pick"
(defun football-picks-prev ()
  "Move to the previous game to be picked."
  (interactive)
  (beginning-of-line nil)
  (let ((not-wrapped (backward-line-wrapping)))
    (while (not (football-picks-selection-char (char-after (point))))
      (if (not (backward-line-wrapping))
	  (setq not-wrapped nil)
	)
      )
    not-wrapped
    )
  )

(defun football-picks-favorite ()
  "Point at the favorite for the current pick."
  (interactive)
  (move-to-column 0)
  )

(defun football-picks-underdog ()
  "Point at the underdog for the current pick."
  (interactive)
  (move-to-column football-picks-underdog-column)
  )

(defun football-picks-other ()
  "Switch to the other team for the current pick."
  (interactive)
  (if (= (current-column) 0)
      (football-picks-underdog)
    (football-picks-favorite)
    )
  )

(defun football-picks-pick ()
  "Pick who we're currently pointing at in the current pick."
  (interactive)
  (if (= (current-column) 0)
      (football-picks-pick-favorite)
    (football-picks-pick-underdog)
    )
  )

(defun football-picks-pick-favorite ()
  "Pick the favorite for the current pick."
  (interactive)
  (football-picks-underdog)
  (delete-char 1)
  (insert-char ?\  1)
  (football-picks-favorite)
  (delete-char 1)
  (insert-char ?\* 1)
  (football-picks-favorite)
  )

(defun football-picks-pick-underdog ()
  "Pick the underdog for the current pick."
  (interactive)
  (football-picks-favorite)
  (delete-char 1)
  (insert-char ?\  1)
  (football-picks-underdog)
  (delete-char 1)
  (insert-char ?\* 1)
  (football-picks-underdog)
  )

;; replaced by football-picks-comment-pick
;(defun football-picks-insert-comment (comment)
;  "Insert COMMENT below this line."
;  (interactive "s")
;  (end-of-line nil)
;  (newline 1)
;  (indent-to-column 4)
;  (insert comment)
;)
