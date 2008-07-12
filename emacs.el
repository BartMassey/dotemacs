(defun my-init ()

  (setq new-emacs nil)
  (if (not (null (string-match "19.*" emacs-version)))
     (setq new-emacs t))

; make sure things get loaded correctly
  (setq load-path
	(append (relative-paths "etc/emacs" "etc/emacs/gnus") load-path))

; swap backspace and delete
; map ^\ to ^S
; map ^^ to ^Q
  (load "swap-keys")
  (swap-del-and-bs nil)
  (swap-meanings-of-keys (string-to-char "\C-\\") (string-to-char "\C-s"))
  (swap-meanings-of-keys (string-to-char "\C-^") (string-to-char "\C-q"))

; make ^N not open a line
  (load "safe-next-line")
  (global-set-key "\C-n" 'safe-next-line)

; use cbreak mode
  (if new-emacs
    (set-input-mode nil t nil)
    (set-input-mode nil t))

; don't leave a lot of garbage files around
  (setq make-backup-files nil)

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
  (setq insert-default-directory nil)

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
  (setq text-mode-hook '(lambda nil (auto-fill-mode 1)))

; since regular expressions are also case-folded...
  (setq case-fold-search nil)
  (setq default-case-fold-search nil)

; make c-mode useable
  (autoload 'set-c-style "c-style" nil t)
  (setq c-tab-always-indent nil)
  (setq c-mode-hook '(lambda () 
		       (set-c-style) 
		       (local-set-key "\C-M" (function newline-and-indent))
		       ))

; this is more important than "eval expression at point"
  (global-set-key "\C-X\C-E" 'my-compile)
  (global-set-key "\C-X\C-N" 'next-error)

; GOT to be able to play that go-bang!
  (autoload 'gomoku "go-bang" nil t)

; Why?
  (autoload 'why "why" nil t)
  (global-set-key "\C-X\C-_" 'why)

;; Use real tags instead of emacs tags
;  (autoload 'find-ctag "ctags" nil t)
;  (autoload 'find-ctag-other-window "ctags" nil t)

; I've been playing with sml
  (if (not new-emacs)
    (load-library "sml-init"))

; GNU Smalltalk
  (setq auto-mode-alist (cons '("\\.st$" . smalltalk-mode) auto-mode-alist))
  (autoload 'smalltalk-mode "st" "Major mode for editing Smalltalk code." t)

; GNUS is kinda neat
  (autoload 'gnus "gnus" "Read network news." t)
  (autoload 'gnus-post-news "gnuspost" "Post a network news article." t)
  (setq gnus-default-article-saver 'gnus-Subject-save-in-file)
  (setq gnus-article-save-directory "~/news")
  (setq gnus-use-generic-from t)
  (setq gnus-Info-directory "~/class/gnus/info")
  (setq gnus-novice-user nil)
  (setq gnus-save-all-headers t)
  (setq gnus-auto-select-first nil)
  (setq gnus-auto-select-next 'quietly)
  (setq gnus-break-pages nil)
  (setq gnus-mail-reply-method (function gnus-mail-reply-using-mhe))
  (setq gnus-mail-forward-method (function gnus-mail-reply-using-mhe))
  (setq gnus-local-timezone t)
  (let ((ev (getenv "NNTPSERVER")))
    (if ev
	(setq gnus-nntp-server ev)
      )
    )
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

; football picks
  (autoload 'football-picks "football-picks" "Pick football games." t)
  (autoload 'football-picks-mode "football-picks" "Football games picks mode." t)
  
; Bill Trost-isms follow...

; just *let* us "narrow-to-region", dammit
  (put 'narrow-to-region 'disabled nil)

; more jove-isms
  (global-set-key "\eg" 'goto-line)
  (global-set-key "\C-S" 'isearch-forward-regexp)
  (global-set-key "\C-R" 'isearch-backward-regexp)

; minibuffer setup
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

(defun next-win ()
  (interactive)
  (select-window (next-window (selected-window)))
  )

(defun prev-win ()
  (interactive)
  (select-window (previous-window (selected-window)))
  )

(defun run-klisp ()
  "Run an inferior Lisp process, input and output via buffer *lisp*."
  (interactive)
  (switch-to-buffer (make-shell "klisp" "klisp"))
  (inferior-lisp-mode))



; mail setup
(defun mh-gmorning ()
  "correctly setup and start the mh-e interface"
  (interactive)
  (setq mh-progs "/local/apps/mh/bin/")
  (setq mh-lib "/local/apps/mh/etc/")
  (setq mh-clean-message-header t)
  (setq mh-ins-buf-prefix "> ")
  (setq mh-folder-mode-hook '(lambda nil
			       (local-set-key "\C-Xn" 'next-win)
			       (local-set-key "\C-Xp" 'prev-win)
			       )
	)
  (mh-rmail)
)

(defun my-compile (command)
  "compile and point at first error message"
  (compile command)
  (next-error)
)

(defun dungeon ()
  "play an adventure game"
  (interactive)
  (load-library "dungeon")
  )

(defun relative-paths (&rest pl)
  (if (null pl)
      nil
    (let ((fs (substring (car pl) 1 2)))
      (if (or (equal fs "/") (equal fs "."))
	  (cons (car pl) (apply 'relative-paths (cdr pl)))
	(cons
	 (concat (getenv "HOME") "/" (car pl))
	 (apply 'relative-paths (cdr pl))
	 )
	)
      )
    )
  )
