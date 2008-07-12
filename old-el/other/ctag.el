;;
;; Craig McPheeters
;; cmcpheeters@alias.com
;;
;; ctag.el
;;
;; Author: Craig McPheeters (cmcpheeters@alias.com) 1991/1992.
;;
;; This file is not part of GNU Emacs, but I am willing to donate it.
;;
;; ctag.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY.  No author or distributor accepts responsibility to anyone
;; for the consequences of using it or for whether it serves any particular
;; purpose or works at all, unless they say so in writing.  Refer to the GNU
;; Emacs General Public License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute ctag.el,
;; but only under the conditions described in the GNU Emacs General Public
;; License.  A copy of this license is supposed to have been given to you
;; along with GNU Emacs so you can know your rights and responsibilities.
;; It should be in a file named COPYING.  Among other things, the copyright
;; notice and this notice must be preserved on all copies.
;;
;;
;; The contents of this file were based originally upon tags.el, although
;; there have been extensive modifications made to it.  The tags file this
;; package file operates on are compatable with those for VI.
;;
;; Additions were made where they were considered useful given the local
;; environment.
;;
;; ----------------------------------------------------------------------

(provide 'ctag)

;;
;; Declare variables
;;

(defvar ctag-vi-compatability t
  "*Were the tags generated to be compatable with VI?  VI has a limit of 30
characters for each tag.")

(defvar ctag-case-fold-search nil
  "*Should tag searches be case insensitive?")

(defvar ctag-file-names nil
   "List of tag files to search")

(defvar ctag-last-file nil
   "List of tag files remaining to search")

(defvar ctag-last-point nil
  "Point of the last successful search")

(defvar ctag-last nil
  "Last tag pattern searched for")

(defvar ctag-apropos-last nil
  "Last pattern searched for by a tag-apropos")

(defvar ctag-method-last nil
  "Last pattern searched for by a tag-methods")

(defvar ctag-class-last nil
  "Last pattern searched for by a tag-class")

(defvar ctag-ag-oneliner-last nil
  "Last pattern searched for by a ctag-ag-oneliner")

(defvar ctag-recurse nil
  "If non-nil, enter a recursive whenever a tag search completes successfully.")

(defvar ctag-next-form nil
  "Form to eval for the ctag-next-search command")

(defvar ctag-visit-file-name nil
  "File name to visit.  Set by the ctag-visit-file command.")

(defvar ctag-visited-files nil
  "List of files already visited by this instance of the ctag-visit-file cmd")

(defvar ctag-temp-project nil)
(defvar ctag-temp-subp nil)
(defvar ctag-temp-lib nil)
(defvar ctag-temp-file nil)

;;
;; Utility functions
;;

(defun ctag-visit-tag-file-buffer (name) "\
Select the named tag buffer.  If the tag file has not been read in then read
the tag file.  If the file has been changed, then offer to re-read it. (CWM)"
  (let ((cur-dir default-directory) buf)
        (cond (name)
                  (ctag-file-names (setq name (car ctag-file-names)))
                  (t (call-interactively 'ctag-file-add)
                         (setq name (car ctag-file-names))))
        (set-buffer
         (or (get-file-buffer name)
                 (progn
                   (message "Reading tags file %s..." name)
                   (setq buf (find-file-noselect name))
                   buf)))
        (setq case-fold-search ctag-case-fold-search)
        (or (verify-visited-file-modtime (get-file-buffer name))
                (cond ((yes-or-no-p
                                (concat "Tag file " name" has changed, read new contents? "))
                           (revert-buffer t t))))
        (setq default-directory cur-dir)
        (message "")))

(defun skip-white-space () "\
Skip forward in buffer over the white-space (CWM)"
  (while (looking-at "\\s-")
    (forward-char 1)))

(defun skip-to-white-space () "\
Skip forward in buffer to the first white-space (CWM)"
  (while (not (looking-at "\\s-"))
    (forward-char 1)))

(defun string-in-list (list str) "\
Given the LIST, return t if the STRING is in it. (CWM)"
  (let (success)
        (while list
          (if (string-equal (car list) str)
                  (progn
                        (setq list nil)
                        (setq success t))
                (setq list (cdr list))))
        success))

(defun ctag-get-tag () "\
Return the tag string of the current line.  Assumes the tag table is the
current buffer. (CWM)"
  (beginning-of-line)
  (buffer-substring (point)
                                        (progn (skip-to-white-space) (point))))

(defun ctag-get-filename () "\
Return the file name of the tag on the line point is at.
Assumes the tag table is the current buffer. (CWM)"
  (beginning-of-line)
  (skip-to-white-space)
  (skip-white-space)
  (buffer-substring (point)
                                        (progn (skip-to-white-space) (point))))

(defun ctag-get-line () "\
Get the line pattern from the current tag line.  May be a number or a
pattern (CWM)"
  (let (pat)
        (beginning-of-line)
        (skip-to-white-space)
        (skip-white-space)
        (skip-to-white-space)
        (skip-white-space)
        (setq pat (ctag-quote-ctag-pattern
                           (buffer-substring (point)
                                                                 (progn (end-of-line) (point)))))
        (if (string-equal (substring pat 0 1) "?")
                (setq pat (substring (substring pat 1) 0 -1)))

        (if (string-equal (substring pat 0 1) "/")
                (setq pat (substring (substring pat 1) 0 -1)))

        (if (string-equal (substring pat (- (length pat) 2) (length pat)) " $")
                (progn
                  (setq pat (substring pat 0 (- (length pat) 1)))
                  (while
                          (string-equal (substring pat (- (length pat) 1) (length pat)) " ")
                        (setq pat (substring pat 0 (- (length pat) 1))))))
        pat))

(defun ctag-clean-file-name (name)
  (let (char)
        (if name
                (progn
                  ;; Account for <#> in the buffer name
                  (if (string-equal ">" (substring name -1 nil))
                          (progn
                                (setq char "")
                                (while (not (string-equal char "<"))
                                  (setq char (substring name -1 nil))
                                  (setq name (substring name 0 -1)))))

                  ;; Account for -G following the buffer name
                  (if (string-equal "-G" (substring name -2 nil))
                          (setq name (substring name 0 -2))))))
  name)

(defun ctag-get-repo-file-name () "\
Find repo file associated with current buffer (CWM)"
  (let ((name (buffer-name))
                file-name)
        (if name
                (progn
                  (setq name (ctag-clean-file-name name))
                  (setq file-name (ctag-find-file-name name))))
        file-name))

(defun ctag-get-file-lock (file-name) "\
Return the name of the person who has the file locked.
The name passed in is expanded by matching it in the tags file.  (CWM)"
  (let (file-name-lock buff person)
           (setq file-name-lock (concat file-name "-L"))
           (if file-name
                   (if (file-exists-p file-name-lock)
                           (progn
                                 (setq buff (find-file-noselect file-name-lock))
                                 (set-buffer buff)
                                 (setq person (buffer-substring 1
                                                                                                (save-excursion
                                                                                                  (skip-to-white-space)
                                                                                                  (point))))
                                 (kill-buffer buff))
                         (setq person "")))
           person))

(defun ctag-set-psl (name) "\
Set ctag-temp-project, ctag-temp-subp, ctag-temp-lib and
ctag-temp-file given a full pathname (CWM)."
  (setq ctag-temp-project nil)
  (setq ctag-temp-subp nil)
  (setq ctag-temp-lib nil)
  (setq ctag-temp-file nil)
  (let (char)
        (if name
                (progn
                  ;; Set the file
                  (setq char "")
                  (while (not (string-equal char "/"))
                        (setq ctag-temp-file (concat char ctag-temp-file))
                        (setq char (substring name -1 nil))
                        (setq name (substring name 0 -1)))

                  ;; Set the library
                  (setq char "")
                  (while (not (string-equal char "/"))
                        (setq ctag-temp-lib (concat char ctag-temp-lib))
                        (setq char (substring name -1 nil))
                        (setq name (substring name 0 -1)))

                  ;; Set the sub-project
                  (setq char "")
                  (while (not (string-equal char "/"))
                        (setq ctag-temp-subp (concat char ctag-temp-subp))
                        (setq char (substring name -1 nil))
                        (setq name (substring name 0 -1)))

                  ;; The rest is the project
                  (setq ctag-temp-project name)))

        ctag-temp-project))

(defun ctag-default-tag (cpp) "\
Return a default tag string based upon the text surrounding point in the
current buffer (CWM)."
  (let ((valid-chars "\\sw\\|\\s_")
                start end)
        (if cpp
                (setq valid-chars (concat valid-chars "\\|:\\|~")))
        (save-excursion
          (while (looking-at valid-chars) (forward-char))
          (if (re-search-backward valid-chars nil t)
                  (progn
                        (forward-char 1)
                        (setq end (point))
                        (forward-char -1)
                        (while (looking-at valid-chars) (forward-char -1))
                        (forward-char 1)
                        (setq start (point))
                        (buffer-substring start end)
                        )
                nil))))

(defun ctag-query-tag (prompt tag) "\
Read a tag to search for after querying the user. (CWM)"
  (let ((default tag))
        (setq tag (read-string
                           (if default
                                   (format "%s(default %s) " prompt default)
                                 prompt)))
        (if (string-equal tag "")
                default
          tag)))

(defun ctag-query-file (prompt) "\
Read a file name to search for after querying the user. (CWM)"
  (let (default name)
        (save-excursion
          (while (looking-at "\\sw\\|\\s_\\|/\\|\\.") (forward-char))
          (setq default
                        (if (re-search-backward "\\sw\\|\\s_\\|/\\|\\." nil t)
                                (progn
                                  (forward-char 1)
                                  (buffer-substring
                                   (point)
                                   (progn
                                         (backward-char 1)
                                         (while
                                                 (and (> (point) 1)
                                                          (looking-at "\\sw\\|\\s_\\|/\\|\\."))
                                           (backward-char 1))
                                         (forward-char 1)
                                         (point))))
                          nil)))
        (if (and (> (length default) 1)
                         (string-equal (substring default 0 1) "<"))
                (setq default (substring default 1 nil)))
        (if (and (> (length default) 1)
                         (string-equal (substring default -1 nil) ">"))
                (setq default (substring default 0 -1)))
        (if (and (> (length default) 2)
                         (string-equal (substring default -2 nil) ".o"))
                (setq default (concat (substring default 0 -2) ".c")))

        (setq name (read-string prompt default))
        (if (string-equal name "")
                default
          name)))

;; This is needed because the line patterns in a ctag file do not quote all
;; Magic characters in GNU-emacs regular expressions.  It does some, so this
;; function must only do the needed ones.  The ctag regular expression was
;; made to work with VI.
;;
;; This function quotes:  * [ ] +

(defun ctag-quote-ctag-pattern (string) "\
Quote the characters required in the pattern describing the line. (CWM)"
  (let (char
                (new "")
                (rest string))
        (while (not (string-equal rest ""))
          (progn
                (setq char (substring rest 0 1))
                (setq rest (substring rest 1))
                (if (or
                         (string-equal char "*")
                         (string-equal char "[")
                         (string-equal char "]")
                         (string-equal char "+"))
                        (setq new (concat new (char-to-string ?\134) char))
                  (setq new (concat new char)))))
        new))

;; The tags file is damaged.  In order to support VI, the tags are at most
;; 30 characters long.  This means that if we specify a tag longer than this,
;; it won't be found in the tags file.  So we truncate the tag if we're in
;; VI compatibility mode.

(defun ctag-truncate-tag (tag) "\
If ctag-vi-compatibility, truncate tags to 30 characters. (CWM)"
  (if ctag-vi-compatability
          (if (> (length tag) 30)
                  (substring tag 0 30)
                tag)
        tag))

(defun ctag-file-add (file) "\
Add a tag file to the start of the list of tag files to search."
  (interactive (list (read-file-name "Visit ctag table: (default tags) "
                                                                         default-directory
                                                                         (concat default-directory "tags")
                                                                         t)))
  (setq file (expand-file-name file))
  (if (file-directory-p file)
      (setq file (concat file "tags")))
  (if (file-readable-p file)
          (setq ctag-file-names (append (list file) ctag-file-names))
        (message "Can't read the tag file: %s" file)))

(defun ctag-file-remove () "\
Remove the last tag file to be added."
  (interactive)
  (if (listp ctag-file-names)
          (setq ctag-file-names (cdr ctag-file-names)))
  (ctag-show-files))

(defun ctag-show-files () "\
Display the tag file names searched. (CWM)"
  (interactive)
  (message "%s" ctag-file-names))

;;
;;
;; Main tag functions
;;
;;

(defun ctag (tag) "\
Find TAG, starting at the beginning of the tag file list.  If tagname
is nil, the expression in the buffer at or around point is used as the
tagname. (CWM)"
  (interactive (list (ctag-query-tag "Find tag: " (ctag-default-tag t))))
  (ctag-main tag))

(defun ctag-at-point () "\
Find the tag under point. (CWM)"
  (interactive)
  (ctag-main (ctag-default-tag t)))

(defun ctag-main (tag) "\
Search for the tag given. (CWM)"
  (if (not ctag-file-names)
          (call-interactively 'ctag-file-add))
  (if (and tag ctag-file-names)
          (progn
                (setq tag (ctag-truncate-tag tag))
                (setq ctag-last-file ctag-file-names)
                (setq ctag-last-point 1)
                (setq ctag-last tag)
                (setq ctag-next-form '(ctag-find-next-tag 1))
                (setq ctag-recurse t)
                (ctag-find-next-tag 1))
        (message "Empty tagname or tagfile list")))

(defun ctag-find-next-tag (n) "\
Find the N'th next occurence of a tag found by ctag.
Handle recursion if needed. (CWM)"
  (interactive "p")
  (let
          ((window-config (ctag-window-list))
           (found-one nil))
        (if ctag-recurse
                (progn
                  (save-window-excursion
                        (setq found-one (ctag-find-next n))
                        (if found-one
                                (recursive-edit)))
                  (if found-one
                          (ctag-window-restore window-config)))
          (setq found-one (ctag-find-next n)))
        (if (not found-one)
                (message "Not found, tagname: %s" ctag-last))))

(defun ctag-find-next (n) "\
Find the N'th next occurence of a tag found by ctag.
Return nil if not found, otherwise do the search. (CWM)"
  (interactive "p");; The arg is an integer.
  (if (and
           ctag-last-file
           (> n 0))
          (progn
                (ctag-visit-tag-file-buffer (car ctag-last-file))
                (goto-char ctag-last-point)
                (if (re-search-forward (concat "^" ctag-last) nil t)
                        (progn
                          (setq ctag-last-point (point))
                          (setq n (1- n))
                          (if (= n 0)
                                  (ctag-do-search)
                                (ctag-find-next n)))
                  (progn
                        (setq ctag-last-file (cdr ctag-last-file))
                        (setq ctag-last-point 1)
                        (ctag-find-next n))))))

;;
;; Support routines to ctag
;;

(defun ctag-do-search () "\
Assume point is on a tag line.  Extract the file and line and then go there.
If the search is successful, enter a recursive edit if ctag-recurse is non-nil
 (CWM)"
  (let ((filename (ctag-get-filename))
                (cur-dir default-directory)
                (linepat (ctag-get-line))
                (case case-fold-search)
                found-point linenum found-it)
        (setq linenum (string-to-int linepat))
        (find-file-other-window (substitute-in-file-name filename))
        (widen)
        (goto-char (point-min))
        (setq default-directory cur-dir);; Restore default
        (if (= linenum 0)
                (progn
                  (setq case-fold-search nil)
                  (setq found-it (re-search-forward linepat nil t))
                  (setq case-fold-search case))
          (progn
                (goto-line linenum)
                (setq found-it t)))
        (setq found-point (point))
        (if found-it
                (beginning-of-line))
        found-it))

(defun ctag-next-search () "\
Do another tag search, just like the last one (CWM)"
  (interactive)
  (if ctag-next-form
          (progn
                (if current-prefix-arg
                        (setq ctag-recurse t)
                  (setq ctag-recurse nil))
                (eval ctag-next-form))
        (message "No next to search for.")))

(defun ctag-apropos (string) "\
Display list of all tags in tag tables that REGEXP match.
By default it only prints out the tag itself, given an argument it will
display the whole tag entry.  The whole entry includes the filename and line
number or pattern used in the tag search. (CWM)"
  (interactive (list
                                (let ((pattern (read-from-minibuffer
                                                                (concat (if current-prefix-arg
                                                                                        "Full listing tag "
                                                                                  "Tag ")
                                                                                "apropos (regexp): ")
                                                                ctag-apropos-last)))
                                  (setq ctag-apropos-last pattern))))
  (let ((file-list ctag-file-names)
                (match 0)
                end)
        (save-window-excursion
          (with-output-to-temp-buffer "*Tags List*"
                (princ "Tags matching regexp ")
                (prin1 string)
                (terpri)
                (while file-list
                  (princ (format "------  In file: %s\n" (car file-list)))
                  (ctag-visit-tag-file-buffer (car file-list))
                  (goto-char 1)
                  (while (re-search-forward string nil t)
                        (save-excursion
                          (beginning-of-line)
                          (skip-to-white-space)
                          (setq end (point))
                          (beginning-of-line)
                          (if (re-search-forward string end t)
                                  (progn
                                        (setq match (1+ match))
                                        (beginning-of-line)
                                        (princ (buffer-substring
                                                        (point)
                                                        (progn
                                                          (if current-prefix-arg
                                                                  (progn
                                                                        (end-of-line)
                                                                        (point))
                                                                end))))
                                        (terpri))))
                        (forward-line 1))
                  (setq file-list (cdr file-list))))

          ;; Decide whether or not to display the tag buffer.  Don't if there were
          ;; no matches found, otherwise do.
          (if (> match 0)
                  (progn
                        (display-buffer "*Tags List*")
                        (message "Found %d matching tag%s" match (if (> match 1) "s" ""))
                        (recursive-edit))
                (message "No tags match that pattern: %s" string)))))

(defun ctag-method-apropos (string) "\
Display a list of all C++ methods in tag tables that contain the PATTERN. (CWM)"
  (interactive
   (list
        (let ((pattern (read-from-minibuffer "Method contains pattern: "
                                                                                 (ctag-default-tag nil))))
                                  (setq ctag-method-last pattern))))
  (let ((last-ctag-apropos ctag-apropos-last))
        (ctag-apropos (concat "::.*" string))
        (setq ctag-apropos-last last-ctag-apropos)))

(defun ctag-method-list (name) "\
Display a list of all C++ methods in tag tables that belong to CLASS. (CWM)"
  (interactive
   (list
        (let ((pattern (read-from-minibuffer "Class name : "
                                                                                 (ctag-default-tag nil))))
                                  (setq ctag-class-last pattern))))
  (let ((last-ctag-apropos ctag-apropos-last))
        (ctag-apropos (concat name "::"))
        (setq ctag-apropos-last last-ctag-apropos)))

(defun ctag-visit-file (name) "\
Visit the files in the tag file which match NAME.  The next occurence of a
file matching name can be found with the ctag-next-search command. (CWM)"
  (interactive (list (ctag-query-file "Visit tag file: ")))
  (setq ctag-last-file ctag-file-names)
  (setq ctag-last-point 1)
  (setq ctag-visit-file-name name)
  (setq ctag-visited-files nil)
  (setq ctag-next-form '(ctag-visit-next-file 1))
  (setq ctag-recurse t)
  (ctag-visit-next-file 1))

(defun ctag-visit-next-file (n) "\
Find the N'th next occurence of a tag file matching the previously set name.
Use ctag-visit-file to set the tag file name to match. (CWM)"
  (interactive "p")
  (let (start end file (cur-dir default-directory) found)
        (while (and
                        (not found)
                        ctag-last-file
                        (> n 0))
          (ctag-visit-tag-file-buffer (car ctag-last-file))
          (goto-char ctag-last-point)
          ;; Search for a match
          (if (re-search-forward ctag-visit-file-name nil t)
                  (save-excursion
                        (end-of-line)
                        (setq ctag-last-point (point))
                        (beginning-of-line)
                        (skip-to-white-space)
                        (save-excursion
                          (skip-white-space)
                          (skip-to-white-space)
                          (setq end (point)))
                        ;; Restrict this search to just the filename portion of tag
                        (if (re-search-forward ctag-visit-file-name end t)
                                (progn
                                  ;; Got a file match
                                  (setq file (ctag-get-filename))
                                  (if (not (string-in-list ctag-visited-files file))
                                          (progn
                                                ;; First time we've visited this file
                                                (setq found t)
                                                (setq ctag-visited-files
                                                          (append ctag-visited-files
                                                                          (list file)))
                                                ;; We may recurse here...
                                                (ctag-visit-file-doit file))))))
                (progn
                  ;; No match in this file, try the next file in list
                  (setq ctag-last-file (cdr ctag-last-file))
                  (setq ctag-last-point 1))))
        (if (not found)
                (message "File not found: %s" ctag-visit-file-name))))

(defun ctag-visit-file-doit (name) "\
Visit the file found in a ctag-visit-next-file call. (CWM)"
  (let ((cur-dir default-directory))
        (if ctag-recurse
                (save-window-excursion
                  (find-file-other-window (substitute-in-file-name name))
                  (setq default-directory cur-dir)
                  (message "Visiting file: %s" name)
                  (recursive-edit))
          (progn
                (find-file-other-window (substitute-in-file-name name))
                (setq default-directory cur-dir)
                (message "Visiting file: %s" name)))))

(defun ctag-window-list () "\
Returns a list of Lisp window objects and the point at the top of the window
for all Emacs windows. (CWM)"
  (let* ((first-window (selected-window))
                 (top-char (save-excursion
                                         (move-to-window-line 0)
                                         (point)))
                 (windows (cons
                                   (cons first-window
                                                 (cons top-char
                                                           (cons (point) nil))) nil))
                 (current-cons windows)
                 (w (next-window first-window nil)))
    (while (not (eq w first-window))
          (select-window w)
      (setq current-cons
                        (setcdr current-cons
                                        (cons
                                         (cons w
                                                   (cons (save-excursion
                                                                   (move-to-window-line 0)
                                                                   (point))
                                                                 (cons (point) nil))) nil)))
      (setq w (next-window w nil)))
        (select-window first-window)
    windows))

(defun ctag-window-restore (windows) "\
Restore the positioning of the windows from the window LIST
Used in conjunction with ctag-window-list (CWM)"
  (let* ((this (car windows))
                 (rest (cdr windows))
                 (top-char nil)
                 (window-from-list nil)
                 (first-window (selected-window))
                 the-point)
        (while this
          (setq window-from-list (car this))
          (setq top-char (car (cdr this)))
          (setq the-point (car (cdr (cdr this))))
          (select-window window-from-list)
          (set-window-start nil top-char)
          (goto-char the-point)
          (setq this (car rest))
          (setq rest (cdr rest)))
        (select-window first-window)))

;;
;; Extended software repository commands.
;;
;;   softwhere - shows you where a variable/function/file is defined/used.
;;   info      - shows you if a file is locked and by whom.
;;   diff      - find differences between current buffer and repo file.
;;   lget      - check out a file from the repo.
;;   lcncl     - cancel a lock on a repo file.
;;   lput      - put a file back into the repo.
;;

(defun ctag-softwhere (name) "\
Call the alias 'softwhere' program with the name.  (CWM)"
  (interactive (list (read-string "Softwhere on which string: "
                                                                        (ctag-default-tag nil))))
  (let (curbuf newbuf)
        (save-window-excursion
          (setq curbuf (current-buffer))
          (setq newbuf (get-buffer-create "*Softwhere*"))
          (pop-to-buffer newbuf)
          (erase-buffer)
          (call-process "softwhere" nil t t name)
          (pop-to-buffer curbuf)
          (recursive-edit))))

(defun ctag-find-file-name (file-name) "\
Find the full pathname of the filename given by finding a match in the tags
file. (CWM)"
  (let ((tag-file ctag-file-names)
                (found-name nil))
        (setq file-name (concat "/" file-name))
        (while (and tag-file (not found-name))
          (ctag-visit-tag-file-buffer (car tag-file))
          (goto-char 0)
          (setq tag-file (cdr tag-file))
          (if (search-forward file-name nil t)
                  (setq found-name (ctag-get-filename))))
        found-name))

;;
;; Get some information on the repo-file
;;
(defun ctag-file-info () "\
Display information on the file associated with the current buffer. (CWM)"
  (interactive)
  (let ((file-name (ctag-get-repo-file-name))
                person)
        (setq person (ctag-get-file-lock file-name))
        (if person
                (if (string-equal person "")
                        (message "File is not locked.   (%s)" file-name)
                  (message "File is locked by %s   (%s)" person file-name))
          (message "There is no repo file associated with current buffer"))))

;;
;; Check the file out of the repo.
;;
(defun ctag-lget (arg) "\
Check the file associated with the current buffer out of the repo.
If a file is obtained, then visit the file.
If there is an argument to this function, then ask for which version
to obtain.  If you specify a version, then a lock is not obtained. (CWM)"
  (interactive "p")
  (let (repo-file-name old-buff command yes repo-buff got-file to-point
                                           proj subp lib file a-window new-buff version)
        (setq old-buff (current-buffer))
        (setq repo-file-name (ctag-get-repo-file-name))
        (if (> arg 1)
                (progn
                  (setq version (read-string "Which version do you want: " nil))
                  (if (string-equal "" version)
                          (setq version nil))))
        (if repo-file-name
                (progn
                  (ctag-set-psl repo-file-name)

                  (setq proj ctag-temp-project)
                  (setq subp ctag-temp-subp)
                  (setq lib ctag-temp-lib)
                  (setq file ctag-temp-file)

                  (setq command (concat "lget"
                                                                " -p" proj
                                                                " -s" subp
                                                                " -l" lib
                                                                (if version
                                                                        (concat " -c -r " version)
                                                                  "")
                                                                " " file
                                                                " "))

                  (if (yes-or-no-p command)
                          (save-window-excursion
                                (setq repo-buff (get-buffer-create "*Repo buffer*"))
                                (pop-to-buffer repo-buff)
                                (erase-buffer)
                                (insert "Getting a repo file:\n\n" command "\n\n")
                                (if version
                                        (call-process "lget" nil repo-buff t
                                                                  (concat "-p" proj)
                                                                  (concat "-s" subp)
                                                                  (concat "-l" lib)
                                                                  "-c"
                                                                  "-r"
                                                                  version
                                                                  file)
                                  (call-process "lget" nil repo-buff t
                                                                (concat "-p" proj)
                                                                (concat "-s" subp)
                                                                (concat "-l" lib)
                                                                file))
                                (insert "\nDone.\n")
                                (goto-char 0)
                                (if (search-forward "-- " nil t)
                                        (progn
                                          (setq got-file (buffer-substring (point)
                                                                                                           (progn (end-of-line)
                                                                                                                          (point))))
                                          (set-buffer old-buff)
                                          (setq to-point (point))
                                          (find-file-other-window (substitute-in-file-name got-file))
                                          (goto-char to-point)
                                          (c-mode)
                                          (recursive-edit))
                                  (message "Lget failed: the file is already locked by %s"
                                                   (ctag-get-file-lock repo-file-name))))
                        (message "Lget aborted."))

                 (if got-file
                         (progn
                           (setq new-buff (get-file-buffer got-file))
                           (setq a-window (get-buffer-window old-buff))
                           (if a-window
                                   (set-window-buffer a-window new-buff)))))

          (message "There is no repo file associated with the current buffer."))))

;;
;; Put a file back into the repo.
;;
(defun ctag-lput () "\
Put a file back into the repo.  The file used is the one associated with the
current buffer.  (CWM)."
  (interactive)
  (let (repo-file-name proj subp lib file command yes repo-buff old-buff)
        (setq old-buff (current-buffer))
        (if (buffer-modified-p)
                (save-some-buffers))
        (setq repo-file-name (ctag-get-repo-file-name))
        (if repo-file-name
                (progn
                  (ctag-set-psl repo-file-name)

                  (setq proj ctag-temp-project)
                  (setq subp ctag-temp-subp)
                  (setq lib ctag-temp-lib)
                  (setq file ctag-temp-file)

                  (setq command (concat "lput -p" proj " -s" subp
                                                                " -l" lib " " file " "))

                  (save-window-excursion
                        (find-file-other-window ".repo_message")
                        (insert "Compose log message.  ^x^c to continue\n\n")
                        (recursive-edit)
                        (set-buffer (get-file-buffer ".repo_message"))
                        (goto-char 0)
                        (while (not (looking-at "\n")) (delete-char 1))
                        (delete-char 1)
                        (write-file ".repo_message")
                        (if (yes-or-no-p command)
                                (progn
                                  (setq repo-buff (get-buffer-create "*Repo buffer*"))
                                  (switch-to-buffer repo-buff)
                                  (erase-buffer)
                                  (insert "Putting a file into the repo:\n\n"
                                                  "lput -p" proj " -s" subp " -l" lib " " file "\n\n")
                                  (insert-file ".repo_message")
                                  (goto-char (point-max))
                                  (message "Lput in progress...")
                                  (call-process "lput" ".repo_message" repo-buff t
                                                                (concat "-p" proj)
                                                                (concat "-s" subp)
                                                                (concat "-l" lib)
                                                                file)
                                  (message "")
                                  (insert "\nDone.\n")
                                  (goto-char (point-max))
                                  (recursive-edit))
                          (message "Lput aborted."))))
          (message "There is no repo file associated with the current buffer."))))

;;
;; Cancel a repo lock.
;;
(defun ctag-lcncl () "\
Cancel the lock on the file associated with the current buffer from
the repo. (CWM)"
  (interactive)
  (let (repo-file-name proj subp lib file command yes repo-buff)
        (setq repo-file-name (ctag-get-repo-file-name))
        (if repo-file-name
                (progn
                  (ctag-set-psl repo-file-name)

                  (setq proj ctag-temp-project)
                  (setq subp ctag-temp-subp)
                  (setq lib ctag-temp-lib)
                  (setq file ctag-temp-file)

                  (setq command (concat "lcncl -k -p" proj " -s" subp
                                                                " -l" lib " " file " "))

                  (if (yes-or-no-p command)
                          (save-window-excursion
                                (setq repo-buff (get-buffer-create "*Repo buffer*"))
                                (pop-to-buffer repo-buff)
                                (erase-buffer)
                                (insert "Canceling a lock:\n\n"
                                                "lcncl -k -p" proj " -s" subp " -l" lib " " file "\n\n")
                                (call-process "lcncl" nil repo-buff t
                                                          "-k"
                                                          (concat "-p" proj)
                                                          (concat "-s" subp)
                                                          (concat "-l" lib)
                                                          file)
                                (insert "\nDone.\n")
                                (goto-char 0)
                                (recursive-edit))
                        (message "Lcncl aborted.")))
          (message "There is no repo file associated with the current buffer."))))

;;
;; Find differences between current buffer, and repo-file.
;;
(defun ctag-file-diff () "\
Display differences between current buffer and its repo file.
next-error can be used to move between the differences. (CWM)"
  (interactive)
  (require 'compile)
  (let (orig-file-name file-name curbuf newbuf)
        (setq orig-file-name (buffer-file-name))
        (if orig-file-name
                (setq orig-file-name (file-name-nondirectory orig-file-name))
          (setq orig-file-name "<No file name>"))
        (setq curbuf (current-buffer))
        (setq file-name (ctag-get-repo-file-name))
        (if (and file-name
                         (file-exists-p file-name))
                (save-window-excursion
                  (setq newbuf (get-buffer-create "*compilation*"))
                  (pop-to-buffer newbuf)
                  (compilation-mode)
                  (toggle-read-only -1)
                  (erase-buffer)
                  (insert "Diff with " file-name "\n\n")
                  (pop-to-buffer curbuf)
                  (message "Diff in progress...")
                  (call-process-region (point-min)
                                                           (point-max)
                                                           "diff" nil newbuf t "-b" "-w" "-" file-name)
                  (pop-to-buffer newbuf)
                  (goto-char 0)
                  (replace-regexp "^>" "old>")
                  (goto-char 0)
                  (replace-regexp "^<" "new<")
                  (goto-char 0)
                  (replace-regexp "^\\([0-9]+\\)" (concat "\nDiff error: "
                                                                                                  orig-file-name
                                                                                                  ", line \\1:  "))
                  (toggle-read-only 1)
                  (message "")
                  (goto-char 0)
                  (setq compilation-error-list t)
                  (recursive-edit))
          (message "There is no repo file associated with current buffer"))))

;;
;; Get the RCS log from the repo.
;;
(defun ctag-rlog () "\
Get the RCS log from the repo file associated with buffer."
  (interactive)
  (let (file-name newbuf)
        (setq file-name (ctag-get-repo-file-name))
        (if file-name
                (save-window-excursion
                  (setq newbuf (get-buffer-create "*Repo*"))
                  (pop-to-buffer newbuf)
                  (erase-buffer)
                  (insert "Rlog for: " file-name "\n\n")
                  (message (concat "Executing: rlog " file-name))
                  (call-process-region (point-min)
                                                           (point-max)
                                                           "rlog" nil newbuf t file-name)
                  (message "")
                  (goto-char 0)
                  (recursive-edit))
          (message "There is no repo file associated with current buffer"))))

;;
;; Grep the AG one-liners file for a pattern.
;;
(defun ctag-ag-oneliner (string) "\
Find all occurences of the STRING in the AG one-liners file (CWM)"
  (interactive (list
                                (let ((pattern (read-from-minibuffer
                                                                "AG oneliner search for: "
                                                                ctag-ag-oneliner-last)))
                                  (setq ctag-ag-oneliner-last pattern))))
  (let ((location) newbuf)
        (save-window-excursion
          (setq newbuf (get-buffer-create "*AG oneliners*"))
          (setq location (concat (getenv "PROJECT") "/AGv2.5/man/oneliner"))
          (pop-to-buffer newbuf)
          (erase-buffer)
          (insert "Oneliner file: " location "\n\n")
          (insert "AG oneliner search for: " ctag-ag-oneliner-last "\n\n")
          (message (concat "Searching for: " ctag-ag-oneliner-last))
          (call-process-region (point-min)
                                                   (point-max)
                                                   "/bin/sh" nil newbuf t
                                                   "-c" (concat "grep" " -i"
                                                                                " " ctag-ag-oneliner-last
                                                                                " " location))
          (message "")
          (goto-char 0)
          (recursive-edit))))

;; Make a keymap for the tag stuff

(setq ctag-map (make-sparse-keymap))
(define-key     ctag-map        "."     'ctag)
(define-key     ctag-map        "?"     'ctag-apropos)
(define-key     ctag-map        ":"     'ctag-method-apropos)
(define-key ctag-map    "+"     'ctag-file-add)
(define-key ctag-map    "-"     'ctag-file-remove)
(define-key ctag-map    "="     'ctag-show-files)
(define-key ctag-map    "a"     'ctag-ag-oneliner)
(define-key ctag-map    "c"     'ctag-lcncl)
(define-key ctag-map    "d"     'ctag-file-diff)
(define-key ctag-map    "g"     'ctag-lget)
(define-key ctag-map    "i"     'ctag-file-info)
(define-key ctag-map    "l"     'ctag-rlog)
(define-key ctag-map    "m"     'ctag-method-list)
(define-key ctag-map    "p"     'ctag-lput)
(define-key ctag-map    "v"     'ctag-visit-file)
(define-key ctag-map    "w"             'ctag-softwhere)
(define-key ctag-map    "A"     'ctag-ag-oneliner)
(define-key ctag-map    "\^a"   'ctag-ag-oneliner)
(define-key ctag-map    "\^n"   'ctag-next-search)
(define-key ctag-map    "\^v"   'ctag-visit-file)
(define-key     ctag-map        "\^w"   'ctag-softwhere)

;; Do this to put the map where you want it:
;;(define-key global-map        "\^t" ctag-map)
