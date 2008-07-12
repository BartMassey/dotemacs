(defun my-init ()

  (defvar new-emacs (null (string-match "18.*" emacs-version)))
  (defvar xemacs (string-match "Xemacs\\|Lucid" emacs-version))

;; make sure things get loaded correctly
  (setq load-path
	(append (relative-paths "etc/emacs" "etc/emacs/gnus")
		load-path))

;; swap backspace and delete
;; map ^\ to ^S
;; map ^^ to ^Q
  (if (not xemacs)
      (progn
	(load "swap-keys")
	(swap-meanings-of-keys
	 (string-to-char "\C-\\")
	 (string-to-char "\C-s"))
	(swap-meanings-of-keys
	 (string-to-char "\C-^")
	 (string-to-char "\C-q"))
	(swap-del-and-bs nil)))

;;; make ^N not open a line
;  (load "safe-next-line")
;  (global-set-key "\C-n" 'safe-next-line)

; use cbreak mode
  (if new-emacs
    (set-input-mode nil t nil)
    (set-input-mode nil t))

; don't leave a lot of garbage files around
  (setq make-backup-files nil)
  (setq vc-make-backup-files nil)

; don't wrap lines
;  (setq default-truncate-lines t)

; let me do lisp, please
  (put 'eval-expression 'disabled nil)

; my news files aren't in a standard place
;  (setq news-startup-file "$HOME/news/.newsrc")
;  (setq news-certification-file "$HOME/news/.news-dates")

; Why not use the bourne shell when you have history and job control?
  (setq shell-file-name "/bin/sh")
  (setq explicit-shell-file-name "/bin/sh")
  (setq shell-prompt-pattern "$ \\|> \\|-> \\|<[0-9]> \\|(c[0-9][0-9]*) ")

; let me scroll down a line with ^N
  (setq scroll-step 1)

; this can take *forever* to load...
;  (display-time)

; shell stuff
  (global-set-key "\e!" 'shell)
  (global-set-key "\C-X!" 'shell-command)

; some jove-isms of mine
  (global-set-key "\C-Xp" 'prev-win)
  (global-set-key "\C-Xn" 'next-win)
  (global-set-key "\C-T" 'scroll-down)
  (global-set-key "\ej" 'fill-paragraph)
  (global-set-key "\C-m" 'newline-and-indent)
  (global-set-key "\C-j" 'newline-and-indent)
  (global-set-key "\C-X4t" 'find-tag-other-window)
  (global-set-key "\et" 'find-tag)
  (global-set-key "\C-X\C-I" 'insert-file)
  (global-set-key "\eq" 'query-replace-regexp)
  (global-set-key "\C-X<" 'shift-region-left)
  (global-set-key "\C-X>" 'shift-region-right)
  (global-set-key "\er" 'replace-regexp)
  (global-set-key "\C-Xd" 'delete-window)
  (setq insert-default-directory nil)
  (setq-default fill-column 60)
  (setq-default command-line-default-directory ".")
  (setq-default default-directory ".")
;  (setq find-file-hooks
;      (cons 'clear-buffer-default-directory find-file-hooks))

; make sure we can flame on demand
  (autoload 'flame "flame" nil t)
  (autoload 'psychoanalyze-flamer "flame" nil t)

; set things up so we can get an interactive klisp buffer
  (autoload 'make-shell "shell")

; the terminal emulator isn't very good, but we try to make it usable
  (setq terminal-escape-char ?\C-Q)
  (setq terminal-scrolling t)
  (setq terminal-redisplay-interval 1)
  (setq terminal-more-processing nil)

; do a visible bell if possible
  (setq visible-bell t)

; make sure there's a newline on the last line of files
  (setq require-final-newline t)

; put text mode in auto-indent, please
  (setq text-mode-hook (function (lambda () (auto-fill-mode 1))))

; since regular expressions are also case-folded...
  (setq case-fold-search nil)
  (setq default-case-fold-search nil)

;;; superceded by newer emacs version
;;; make c-mode useable
;;;  (autoload 'set-c-style "c-style" nil t)
;;;  (setq c-tab-always-indent nil)
;;;  (setq c-mode-hook '(lambda () 
;;;		       (set-c-style) 
;;;		       (local-set-key "\C-M" (function newline-and-indent))
;;;		       ))
  (global-set-key "\C-x\C-i" 'set-indent)
  (set-indent 4)
;;;(add-hook 'c-mode-hook (function (lambda () (set-indent 4))))

;; this is more important than "eval expression at point"
  (global-set-key "\C-X\C-E" 'my-compile)
  (global-set-key "\C-X\C-N" 'next-error)
  (global-set-key "\C-X\C-P" 'previous-error)

;; GOT to be able to play that go-bang!
;  (autoload 'gomoku "go-bang" nil t)

;; Why?
;  (autoload 'why "why" nil t)
;  (global-set-key "\C-X\C-_" 'why)

;; Use real tags instead of emacs tags
;  (autoload 'find-ctag "ctags" nil t)
;  (autoload 'find-ctag-other-window "ctags" nil t)

;; I've been playing with sml
;  (if (not new-emacs)
;    (load-library "sml-init"))

;; GNU Smalltalk
;  (add-to-list 'auto-mode-alist '("\\.st$" . smalltalk-mode))
;  (autoload 'smalltalk-mode "st" "Major mode for editing Smalltalk code." t)

;; GNUS is kinda neat
;  (autoload 'gnus "gnus" "Read network news." t)
;  (autoload 'gnus-post-news "gnuspost" "Post a network news article." t)
;  (setq gnus-default-article-saver 'gnus-Subject-save-in-file)
;  (setq gnus-article-save-directory "~/news")
;  (setq gnus-use-generic-from t)
;  (setq gnus-Info-directory "~/class/gnus/info")
;  (setq gnus-novice-user nil)
;  (setq gnus-save-all-headers t)
;  (setq gnus-auto-select-first nil)
;  (setq gnus-auto-select-next 'quietly)
;  (setq gnus-break-pages nil)
;  (setq gnus-mail-reply-method (function gnus-mail-reply-using-mhe))
;  (setq gnus-mail-forward-method (function gnus-mail-reply-using-mhe))
;  (setq gnus-local-timezone t)
;  (let ((ev (getenv "NNTPSERVER")))
;    (if ev
;	(setq gnus-nntp-server ev)
;      )
;    )
;  (setq gnus-Mark-article-hook
;	(function
;	 (lambda ()
;	   (gnus-Subject-mark-as-unread gnus-current-article)
;	   (gnus-Subject-set-current-mark "+")
;	   )
;	 )
;	)

;; I should integrate sortnewsrc with gnus-subscribe-newsgroup-method
;; gnus-Article-prepare-hook seems perfect...

;; football picks
;  (autoload 'football-picks "football-picks" "Pick football games." t)
;  (autoload 'football-picks-mode "football-picks" "Football games picks mode." t)
  
;; Bill Trost-isms follow...

;; just *let* us "narrow-to-region", dammit
  (put 'narrow-to-region 'disabled nil)

;; more jove-isms
  (global-set-key "\eg" 'goto-line)
  (global-set-key "\C-S" 'isearch-forward-regexp)
  (global-set-key "\C-R" 'isearch-backward-regexp)

;; minibuffer setup
  (if (not new-emacs)
    (setq minibuffer-local-completion-map
	  '(keymap
	    (?? . minibuffer-completion-help)
	    (9 . minibuffer-complete-word)
	    (32 . minibuffer-complete)
	    (10 . exit-minibuffer)
	    (13 . exit-minibuffer)
	    (7 . abort-recursive-edit)
	    )
	  )
    )

;; stupid menu bar
  (if (and new-emacs (not xemacs))
      (menu-bar-mode nil))

  (defalias 'read-only 'toggle-read-only)

;; fixups for Scheme
  (add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))
  (setq scheme-mode-hook
	(function (lambda () 
		    (define-key
		      scheme-mode-map
		      "\M-j"
		      (function indent-sexp)))))

;; ZETA Z environment
  (if xemacs
      (progn
	(setq zeta-dir (expand-file-name "/local/apps/zeta/"))
	(setq zeta-mode-default-on t)
	(setq zeta-vtex-default-on t)
	(setq zeta-style-hook-on nil)
	(require 'zeta-site-init
		 (concat zeta-dir "lib/emacs/zeta-site-init.el"))))

  ;; HTML mode outlines
  (add-hook 'html-mode-hook
	    '(lambda () (define-key
			  html-mode-map
			  "\C-xl"
			  'html-outline-level)))

  ;; LaTeX mode outlines
  (add-to-list 'auto-mode-alist '("\\.slide$" . latex-mode))
  (add-hook 'latex-mode-hook
	    '(lambda () (define-key
			  tex-mode-map
			  "\C-xl"
			  'latex-outline-level)))

  ;; get some suffixes right: Lex
  (add-to-list 'auto-mode-alist '("\\.l$" . c-mode))


  ;; for Z/EVES mode
  (setq load-path
	(append load-path '("/local/apps/z-eves/system")))
  (add-to-list 'auto-mode-alist '("\\.z" . z-latex-mode))
  (add-to-list 'auto-mode-alist '("\\.zed" . z-latex-mode))
  (autoload 'z-latex-mode "z-eves" "Z-EVES LaTeX mode." t)
  (autoload 'run-z-eves "z-eves" "Run Z-EVES." t)
  (setq z-eves-program "z-eves")


; lilypond mode
  (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
  (add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
  (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

; Nickle mode
 (load "nickle-mode")

; Weblogger
;; (init-weblogger)

; Markdown
  (add-to-list 'auto-mode-alist '("\\.mdwn$" . text-mode))

; Octave
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

; RefTeX and AucTeX
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (load "auctex.el" nil t t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

; Page mode
  (autoload 'page-mode "page-mode" "Page-oriented display" t)

; UTF-8
  (prefer-coding-system 'utf-8)

; Thank you newer emacs for choosing the wrong default
  (setq inhibit-splash-screen t)
)

(defun next-win ()
  "Go to next window."
  (interactive)
  (select-window (next-window (selected-window)))
  )

(defun prev-win ()
  "Go to previous window."
  (interactive)
  (select-window (previous-window (selected-window)))
  )

; (defun run-klisp ()
;  "Run an inferior Lisp process, input and output via buffer *lisp*."
;  (interactive)
;  (switch-to-buffer (make-shell "klisp" "klisp"))
;  (inferior-lisp-mode))



;; mail setup
; (defun mh-gmorning ()
;  "correctly setup and start the mh-e interface"
;  (interactive)
;  (setq mh-progs "/local/apps/mh/bin/")
;  (setq mh-lib "/local/apps/mh/etc/")
;  (setq mh-clean-message-header t)
;  (setq mh-ins-buf-prefix "> ")
;  (setq mh-folder-mode-hook '(lambda nil
;			       (local-set-key "\C-Xn" 'next-win)
;			       (local-set-key "\C-Xp" 'prev-win)
;			       )
;	)
;  (mh-rmail)
;)

;(defun dungeon ()
;  "play an adventure game"
;  (interactive)
;  (load-library "dungeon")
;  )

;; allow relative paths, to be resolved at run-time
(defun relative-paths (&rest pl)
  (if (null pl)
      nil
    (let ((fs (substring (car pl) 1 2)))
      (if (or (equal fs "/") (equal fs "."))
	  (cons (car pl) (apply 'relative-paths (cdr pl)))
	(cons
	 (concat (getenv "HOME") "/" (car pl))
	 (apply 'relative-paths (cdr pl)))))))

(defun shift-region-left (START END ARG)
  "indent-rigidly left (default 8 columns)"
  (interactive "r\nP")
  (if ARG
      (indent-rigidly START END (- ARG))
    (indent-rigidly START END -8)))

(defun shift-region-right (START END ARG)
  "indent-rigidly right (default 8 columns)"
  (interactive "r\nP")
  (if ARG
      (indent-rigidly START END ARG)
    (indent-rigidly START END 8)))

;; for find-file-hooks, but not currently used
; (defun clear-buffer-default-directory () (setq default-directory "."))

(defvar my-compilation-command "make" "*Default command for (my-compile)")
(defun my-compile (prefix)
  "compile and point at first error message"
  (interactive "P")
  (if (not (null prefix))
      (setq my-compilation-command (read-input "Command: ")))
  (compile my-compilation-command)
  (next-error))

(defun replace-in-region (start end regexp to-string)
  "Replace restricted to REGION of REGEXP with TO-STRING."
  (interactive "*r\nsReplace regexp: \nswith: " )
  (narrow-to-region start end)
  (save-excursion
    (goto-char start)
    (while (re-search-forward regexp nil t)
      (replace-match to-string nil nil))
    (widen)))

(defun html-outline-level ()
  "Create a new level in an HTML outline."
  (interactive)
  (save-excursion
    (let ((c (current-column)))
      (end-of-line)
      (let ((e (point)))
	(beginning-of-line)
	(if (not (re-search-forward "^[ \t]*$" e t))
	    (error "Would kill text on current line"))
	(beginning-of-line))
      (kill-line 1)
      (let ((p (point)))
	(insert "<ul>\C-j<li>\C-j</ul>\C-j")
	(indent-region p (point) (+ c 2)))))
  (end-of-line 2))

(defun latex-outline-level ()
  "Create a new level in a LaTeX outline."
  (interactive)
  (save-excursion
    (let ((c (current-column)))
      (end-of-line)
      (let ((e (point)))
	(beginning-of-line)
	(if (not (re-search-forward "^[ \t]*$" e t))
	    (error "Would kill text on current line"))
	(beginning-of-line))
      (kill-line 1)
      (let ((p (point)))
	(insert "\\begin{itemize}\C-j\\item \C-j\\end{itemize}\C-j")
	(indent-region p (point) (+ c 2)))))
  (end-of-line 2))

(defun set-indent (INDENTATION)
  "Programming languages should indent INDENTATION spaces."
  (interactive "P")
  (if INDENTATION
      (progn (setq c-basic-offset INDENTATION)
	     (setq perl-indent-level INDENTATION))))

(defun init-weblogger ()
  (load "xml-rpc")
  (load "weblogger")
  (global-set-key "\C-xw" 'weblogger-start-entry)

  (setq weblogger-config-alist
	'(("default"
	   ("user" . "bart")
	   ("server-url" . "http://fob.po8.org/xmlrpc.php")
	   ("weblog" . "blog")))))