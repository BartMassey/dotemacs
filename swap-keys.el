; Article 1818 of comp.emacs:
; Path: reed!tektronix!cae780!amdcad!ames!ll-xn!husc6!hao!boulder!driscoll
; From: driscoll@boulder.Colorado.EDU (Jim Driscoll)
; Newsgroups: comp.emacs
; Subject: Gnu-Emacs key bindings: the final(?) chapter.
; Message-ID: <980@boulder.Colorado.EDU>
; Date: 17 Sep 87 22:33:38 GMT
; Organization: University of Colorado, Boulder
; Lines: 396
; Posting-Front-End: GNU Emacs 18.47.1 of Tue Jun 30 1987 on boulder (berkeley-unix)
; 
; 
;;;
;;;
;;; This is code that will (I hope) end all traces of the
;;; backspace/delete and related flames.
;;; 
;;; Written by:
;;;
;;;   James Driscoll                             Sept 16, 1987
;;;   University of Colorado, Boulder
;;;   Department of Computer Science
;;;   Campus Box 430
;;;   Boulder CO 80302
;;;
;;;
;;; EXPLANATION
;;;
;;; This code allows you to change the *meaning* of keys
;;; without changing their *names*. (see detailed list of 
;;; exceptions below)  This code is intended to serve the 
;;; needs of those who have complaints with the standard
;;; key bindings of gnu-emacs that are of the form:
;;;
;;;         I think of backspace in terms of 
;;;         backward deletion.
;;;
;;; Gnu-emacs does indeed provide mechanisms to change
;;; the binding of keys to functions.  Thus, one solution
;;; to the above complaint is to switch the meanings of
;;; the delete keys and backspace keys by globally setting
;;; backspace to run the function backward-delete-character
;;; and delete to the help prefix character.  Unfortunately
;;; different modes have different ideas of backward deletion
;;; and a particular mode might like to replace the binding of
;;; of (what it thought was) delete to backward-delete-char with
;;; a binding of delete to backward-delete-char-untabify.  The
;;; essential problem is that the implementors of the various
;;; modes associate the abstract concept of backward deletion
;;; with the particular key delete.  A (functionally) acceptable
;;; solution would be to add hooks to every mode that would 
;;; exchange the bindings of backspace and delete. ;;; automated by writing code that scans through your load path
;;; looking at .el and .elc files for for the mode, and even force it to run a hook if it
;;; doesn't do so already.  This would be an acceptable solutions best used to
;;; to make a non-standard terminal compatible with Gnu-Emacs.  If
;;; your terlate that Z back to X.  Thus you press X, Gnu-Emacs
;;; gets a Z, immediately translates it to X, and in-Emacs are happy with this.
;;;
;;; This appears to be the ideal solution to the backspace/delete
;;; problem: simply use the translation table to make Gnu-Emacs see
;;; delete when you type backspace, and the reverse.  The majority
;;; of the problem is solved.  When you type backspace, it deletes
;;; backwarpe delete, the help prompt
;;; comes up.  Unfortunately, if you ask what key delete-backward-char
;;; is on, you will be told it is on delete.  If you type C-q delete
;;; it inserts a ^H.  The problem is that when you switched the codes
;;; of the keys, you really should have pryed the keys off your 
;;; keyboard and switched them as well.  In summary, the keyboard
;;; translation table allows you to change the *location* of keys,
;;; not their *meaning*.
;;;
;;; For some, this might not be a problem.  They could use the 
;;; keyboard-translation-table to switch backspace and delete and
;;; then stick a piece of tape on their delete key with backspace
;;; written on it, and another on the backspace key with delete
;;; written on it.  At least the key they associate with backward
;;; deletion is in the right place, but its name has just changed have are that: 1) When they are told to type 
;;; backspace, it is in an unexpected place, and 2) when they 
;;; need to insert ^H that key is in the wrong place.  This
;;; way of thinking about things changes the *meaning* of the key,
;;; as well as its *name*.
;;;
;;; This code takes the keyboard-translation-table as its base,
;;; but then avoids the name change.  You can exchange the meaning 
;;; of ^N and ^P.  When you type ^N, you move to the previFinally everyone 
;;; should be happy and we can move on to more interesting issues.
;;; (Well, almost. There are a small number of problems involving
;;; key-sequences being displayed incorrectly, all of which are 
;;; described in detail below.  These can be fixed [in a general
;;; and natural way as described later], but this requires
;;; altering the c code.)
;;;
;;;
;;; USING THIS CODE
;;;
;;; To use this code, load this file in your .emacs.  For example:
;;;
;;;    (setq load-path (cons "<your-dir>" load-path))
;;;    (load "<this-file>")
;;;
;;; will do the trick if you name this file <this-file> and put it
;;; in the directory <your-dir>.  After this you should go about
;;; swapping the keys you are particularly annoyed at.  For instance,
;;; if you use HP terminals and would be happy to have the meaning 
;;; of DEL and C-H swapped, you should follow the command to load
;;; with the command:
;;;
;;;    (swap-meanings-of-keys ?^H ?^?)
;;;
;;; where you got ^H by typing C-q C-h, and similarly ^? by
;;; typing C-q DEL.  If this is what you had in mind, you can
;;; save yourself a few keystrokes by instead typing:
;;;
;;;    (swap-del-and-bs nil)
;;;
;;; which will accomplish the same thing.  If you think of 
;;; the backspace key as meaning go back one character, C-b as
;;; meaning delete back a character, and DEL as meaning help there
;;; is still help.  You should put the following lines in your
;;; .emacs after the load:
;;;
;;;    (swap-meanings-of-keys ?^H ?^B)
;;;    ; now C-h means what C-b did; C-b what C-h did
;;;    (swap-meanings-of-keys ?^B ?^?)
;;;    ; this swaps the meaning of C-b and DEL.  Since
;;;    ; DEL is unaltered, C-b now does what DEL used to do.
;;;    ; DEL now does what C-b previously did, which is what C-h
;;;    ; originally did.
;;; 
;;; If this seems confusing and you have a complicated
;;; rearrangement in mind, you should get out an introductory
;;; group theory book and figure out how decompose an arbitrary
;;; permutation into transpostions.  Or write a more general
;;; routine.
;;;
;;; If you prefer interactive swapping, load this file via
;;;
;;;  ESC-x load-libraray RET <this-file> RET
;;;
;;; Then you can swap keys by typing:
;;;
;;;  ESC-x swap-two-keys RET <key1> <key2>
;;;
;;; If you get hopelessly confused, type (if you still can!)
;;;
;;;  ESC-x reset-swappings RET
;;;
;;; to get back to the original meanings of the keys.
;;;
;;;
;;; ELABORATION
;;;
;;; There are three minor(?) exceptions to the description of
;;; how this works, and one major exception.
;;;
;;;  1. This is the major one.  C-h M (describe-mode) is wrong
;;;     in its identification of key sequences.  If you need to
;;;     know what key a command is on, C-h W (where-is) works
;;;     properly.  If you need to know what command a key runs
;;;     C-h C (describe-key-briefly) and C-h K (describe-key)
;;;     work properly.  The close cousin of describe-mode,
;;;     describe bindings (C-h B), is also wrong.
;;;
;;;  2. If you swap the self inserting keys m and n and then
;;;     type C-h c m (describe-key-briefly) you will be told that
;;;     it is bound to the self-insert-command, even though typing
;;;     m inserts the character n.  Be advised C-q m still (i.e.
;;;     correctly [or at least consistently]) inserts m.
;;;
;;;  3. Some very low-level echoing can not be affected without
;;;     changing c code.  If you swap k with a prefix character
;;;     (i.e. C-c, C-x, ESC, C-u, etc.) and press k it will echo
;;;     as the prefix character rather than k.
;;;
;;;  4. Lisp code that uses (interactive "k") will not echo
;;;     the key you type, but rather what it is swapped with.
;;;     The only standard lisp code that uses this is
;;;     describe-key and describe-key-briefly.  If you swap
;;;     keys a and b, then invoke either describe-key or 
;;;     describe-key-briefly and press a, it will initially
;;;     echo as b, but you will be correctly told that "a runs
;;;     the command ... "
;;;
;;;  To help compensate for this problem, some characters now
;;;  echo in a somewhat different format.  In unaltered gnu-emacs
;;;  the delete key echoes as DEL.  It should now echo as Del, 
;;;  the space bar as Spc rather than SPC, etc.  Thus, if some piece
;;;  of code I couldn't get around tells you that SPC is the key to
;;;  press, and you have swapped the space bar, it is wrong.  If
;;;  it on the other hand it tells you Spc is the key of choice, you
;;;  can procede with greater confidence.  Control characters echo
;;;  as C-X rather than the original C-x.  If C-x is typed at you,
;;;  be wary; C-X is likely to be right.
;;;
;;;  All the above could be fixed by changing the c implementation
;;;  of key_description to use a lisp vector that maps keys to 
;;;  print names.  This vector could then be manipulated by this
;;;  or other code.  There remains a problem: code that prompts
;;;  the user for input must be carefully written.  For instance
;;;
;;;    (message "Type ? for more help")
;;;
;;;  must be written something like
;;;
;;;    (message (concat "Type " (key-with-print-name "?")
;;;                     " for more help"))
;;;  
;;;  or it will not work as anticpated by the user if the meaning
;;;  of the ? key has been swapped.
;;;
;;;
;;;
;;;  BUGS:
;;;
;;;   (In addition to the above clarifications)
;;;
;;;   This code will only work for you if you are not already
;;;   using the keyboard translation table.  This is not an 
;;;   insurmountable problem if the table is static and global,
;;;   but the effort required to make it work properly seemed 
;;;   not worth it  to me.  If you have buffer local values
;;;   for the keyboard translate table, that throws a wrench in
;;;   the works.  If the keyboard translation table is dynamically
;;;   changing (as, for instance, this code allows) then the code
;;;   that alters it will need to cooperate with a much enhanced
;;;   version of this code.
;;;
;;;   Although in the above description it always refers to
;;;   swapping two "keys," only the two character codes are
;;;   swapped.  That is, if you swap a and b, then ^A and ^B
;;;   remain unaltered.  If what you really want to do is 
;;;   swap the meanings of the physical keys a and b, you must
;;;   additionally swap A and B, C-a and C-b, M-a and M-b, and 
;;;   M-C-a and M-C-b.
;;;


; Function that creates a vector that maps from
; characters to print-names

(defun init-descriptions ()
  (let ((descriptions (make-vector 256 "???"))
	(i 32) ; 32=SPC
	(control-to-upcase (- ?A 1))) ; 1=?^A
    (while (< i 127) ; 127=DEL
      (aset descriptions i (char-to-string i))
      (setq i (+ 1 i)))
    (setq i 0); 0=?^@
    (while (< i 32) ; 32=SPC
      (aset descriptions i
	    (concat "C-"
		    (char-to-string (+ i control-to-upcase))))
      (setq i (+ 1 i)))
    (aset descriptions 32  "Spc")
    (aset descriptions 127 "Del")
    (aset descriptions 27  "Esc")
    (aset descriptions 10  "Lfd")
    (aset descriptions 9   "Tab")
    (aset descriptions 13 "Ret")
    (setq i 128) ; 128=M-C-@
    (while (< i 256)
      (aset descriptions i
	    (concat "M-" (aref descriptions (- i 128))))
      (setq i (+ 1 i)))
    descriptions))

(defvar key-descriptions-vector (init-descriptions) 
        "Vector of pretty descriptions of keys")

; function to compute print names of a key sequence
; represented as a string.  This replaces the c definition
; in keymap.c for emacs-lisp calls, but it cannot replace
; the c calls to key-description in:
;
;    callint.c     echoing for (interactive "k") calls
;    keyboard.c    echoing for prefix, etc. characters
;    keymap.c      keymap manipulation, C-h M consequently in error

(defun key-description (s)
"Return a pretty description of key-sequence KEYS.
Control characters turn into C-foo, spaces are put 
between elements, etc."
  (let ((description ""))
    (while (> (length s) 0)
      (setq description
	    (concat description
		    (aref key-descriptions-vector
			  (aref s 0))
		    (if (> (length s) 1) " " "")))
      (setq s (substring s 1)))
    description))
		     
; this creates the ``identity'' translation table

(defun init-translate-table ()
  (let ((table (make-string 256 0))
	(i 0))
    (while (< i 255)
      (aset table i i)
      (setq i (+ 1 i)))
    table))

; init the keyboard-translate-table to the identity.
; create the variable inverse-translate-table that is
; used by quoted-insert.  If only this code manipulates
; the keyboard-translation-table (if other code does, it
; won't in general work unless it keeps this code in mind)
; keyboard-translation-table will be a permutation (i.e. a
; bijective function) and inverse-translation-table will be
; its inverse

(setq keyboard-translate-table (init-translate-table))
(defvar inverse-translate-table (init-translate-table)
  "Should be inverse of keyboard-translate-table.")

; in case you get totally confused, you can try to bail out
; by cleaning the slate.

(defun reset-swappings (arg)
"Undoes all meaning swaps of keys to restore initial meanings."
  (interactive "p")
  (progn
    (message "Unswapping all keys...")
    (setq keyboard-translate-table (init-translate-table))
    (setq inverse-translate-table (init-translate-table))
    (setq key-descriptions-vector (init-descriptions))
    (message "Unswapping all keys...Done")))

; Code that actually does a swap
;  It swaps the entries in the translation table,
;  swaps the names of the keys so their names remains unchanged,
;  and fixes inverse-translate-table so it remains the inverse.

(defun swap-meanings-of-keys (key1 key2)
  (let ((des1 (aref key-descriptions-vector key1))
	(des2 (aref key-descriptions-vector key2))
	(trans1 (aref keyboard-translate-table key1))
	(trans2 (aref keyboard-translate-table key2)))
    (aset key-descriptions-vector key2 des1)
    (aset key-descriptions-vector key1 des2)
    (aset keyboard-translate-table key1 trans2)
    (aset keyboard-translate-table key2 trans1)
    (aset inverse-translate-table trans1 key2)
    (aset inverse-translate-table trans2 key1)))

; Interactively callable routine to change meaning of DEL an BS

(defun swap-del-and-bs (arg)
"Swap the meanings of the delete key and the backspace key.
Please don't be upset that pressing delete causes C-h to echo,
as you must change c code to fix that. Don't trust C-h M!"
 (interactive "p")
 (swap-meanings-of-keys 8 127))

; General interactively callable routine to swap the meaning
; of two keys pressed by the user.

(defun swap-two-keys (arg)
"Swap the meanings of two keys globally.  Be advised that keys
swapped with ESC, C-X, or C-h will not always echo exactly right;
fixing that requires rewriting c code.  Don't trust C-h M!"
  (interactive "p")
  (let ((k1 0) (k2 0))
    (message "Swap the key: ")
    (setq k1 (read-quoted-char))
    (message "Swap the key %s with the key:"
	     (key-description (char-to-string k1)))
    (setq k2 (read-quoted-char))   
    (swap-meanings-of-keys
     (aref inverse-translate-table k1)
     (aref inverse-translate-table k2))
    (message "Swapped meaning of keys %s and %s"
	     (key-description (char-to-string k2))
	     (key-description (char-to-string k1)))))  

; replaces original version of quoted-insert from simple.el
; Instead of inserting the translation of the key the user typed,
; it inserts the character code generated by the keyboard.

(defun quoted-insert (arg)
  "Read next input character and insert its inverse translation.
Useful for inserting control characters.
You may also type up to 3 octal digits, to insert a character with that code"
  (interactive "*p")
  (let ((char (aref inverse-translate-table (read-quoted-char))))
    (while (> arg 0)
      (insert char)
      (setq arg (1- arg)))))

;;;
;;; end of code
;;;
