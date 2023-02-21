(defun my-init ()

  (defvar xemacs (string-match "Xemacs\\|Lucid" emacs-version))

; make sure things get loaded correctly
  (setq load-path
	(append (relative-paths "etc/emacs" "etc/emacs/gnus") load-path))

;; swap backspace and delete
;; map ^\ to ^S
;; map ^^ to ^Q
; (if (not xemacs)
;     (progn
;	(load "swap-keys")
;	(swap-meanings-of-keys
;	 (string-to-char "\C-\\")
;	 (string-to-char "\C-s"))
;	(swap-meanings-of-keys
;	 (string-to-char "\C-^")
;	 (string-to-char "\C-q"))))
;	(swap-del-and-bs nil)

;; make ^N not open a line
;  (load "safe-next-line")
;  (global-set-key "\C-n" 'safe-next-line)

;; use cbreak mode
; (set-input-mode nil t nil)

; don't leave a lot of garbage files around
  (setq make-backup-files nil)
  (setq vc-make-backup-files nil)

;; don't wrap lines
;  (setq default-truncate-lines t)

; let me do lisp, please
  (put 'eval-expression 'disabled nil)

;; my news files aren't in a standard place
;  (setq news-startup-file "$HOME/news/.newsrc")
;  (setq news-certification-file "$HOME/news/.news-dates")

; Why not use the bourne shell when you have history and job control?
  (setq shell-file-name "/bin/sh")
  (setq explicit-shell-file-name "/bin/sh")
  (setq shell-prompt-pattern "$ \\|> \\|-> \\|<[0-9]> \\|(c[0-9][0-9]*) ")

; let me scroll down a line with ^N
  (setq scroll-step 1)

;; this can take *forever* to load...
;  (display-time)

; shell stuff
  (global-set-key "\e!" 'shell)
  (global-set-key "\C-X!" 'shell-command)

; some jove-isms of mine
  (global-set-key "\C-Xp" 'prev-win)
  (global-set-key "\C-Xn" 'next-win)
  (global-set-key "\C-T" 'scroll-down)
  (global-set-key "\ej" 'maybe-fill-paragraph)
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
; (add-to-hook find-file-hooks (clear-buffer-default-directory))

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
  (add-to-hook text-mode-hook (auto-fill-mode 1))

;; since regular expressions are also case-folded...
  (setq case-fold-search nil)
  (customize-set-variable 'case-fold-search nil)

;; Set default indent.
  (global-set-key "\C-x\C-i" 'set-indent)
  (set-indent 4)

;; this is more important than "eval expression at point"
  (global-set-key "\C-X\C-E" 'my-compile)
  (global-set-key "\C-X\C-N" 'next-error)
  (global-set-key "\C-X\C-P" 'previous-error)

;; football picks
;  (autoload 'football-picks "football-picks" "Pick football games." t)
;  (autoload 'football-picks-mode "football-picks" "Football games picks mode." t)
  
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
	  (7 . abort-recursive-edit)))

; stupid menu bar
  (if (not xemacs)
      (menu-bar-mode -1))

  (defalias 'read-only 'toggle-read-only)

; fixups for Scheme
  (add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))
  (add-to-hook scheme-mode-hook
     (define-key scheme-mode-map "\M-j"	(function indent-sexp)))

; HTML mode
;  (autoload 'html-mode "html-mode" "HTML major mode." t)
   (auto-invoke-mode "\\.html\\'" 'html-mode)
   (auto-invoke-mode "\\.htm\\'" 'html-mode)

; A jove-ism, really
   (defalias 'read-only 'toggle-read-only)

; MH mode setup
   (setq mh-progs "/pkgs/nmh/bin/")
   (setq mh-lib "/pkgs/nmh/etc/")
   (add-to-hook mh-letter-mode-hook
      (local-set-key "\C-c\C-A" 'mh-insert-attachment))

; The new TAB behavior for text is *almost* right...
; (see indent-relative-close below)
   (add-to-hook mh-letter-mode-hook
      (local-set-key "\t" 'indent-relative-close))
   (add-to-hook text-mode-hook
      (local-set-key "\t" 'indent-relative-close))

; ZETA Z environment
  (if xemacs
      (progn
	(setq zeta-dir (expand-file-name "/local/apps/zeta/"))
	(setq zeta-mode-default-on t)
	(setq zeta-vtex-default-on t)
	(setq zeta-style-hook-on nil)
	(require 'zeta-site-init
		 (concat zeta-dir "lib/emacs/zeta-site-init.el"))))

; HTML mode outlines
  (add-to-hook html-mode-hook
     (define-key html-mode-map "\C-xl" 'html-outline-level))

;; LaTeX mode outlines
;  (add-to-list 'auto-mode-alist '("\\.slide$" . latex-mode))
;  (add-to-hook latex-mode-hook
;     (define-key tex-mode-map "\C-xl" 'latex-outline-level))

; get some suffixes right: Lex
  (add-to-list 'auto-mode-alist '("\\.l$" . c-mode))


;; for Z/EVES mode
;  (setq load-path
;	(append load-path '("/local/apps/z-eves/system")))
;  (add-to-list 'auto-mode-alist '("\\.z" . z-latex-mode))
;  (add-to-list 'auto-mode-alist '("\\.zed" . z-latex-mode))
;  (autoload 'z-latex-mode "z-eves" "Z-EVES LaTeX mode." t)
;  (autoload 'run-z-eves "z-eves" "Run Z-EVES." t)
;  (setq z-eves-program "z-eves")


; lilypond mode
  (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
  (add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
  (add-to-hook LilyPond-mode-hook (turn-on-font-lock))

; Nickle mode
  (autoload 'nickle-mode "nickle-mode" "Nickle Mode" t)
  (add-to-list 'auto-mode-alist '("\\.5c\\'" . nickle-mode))

;; Weblogger
; (init-weblogger)

; Octave
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; RefTeX and AucTeX
;  (add-to-hook LaTeX-mode-hook (turn-on-reftex))
;  (load "auctex.el" nil t t)
;  (setq TeX-auto-save t)
;  (setq TeX-parse-self t)
;  (setq-default TeX-master nil)

; Logic mode
  (autoload 'logic-mode "logic-mode" "Entry of logic symbols" t)

; UTF-8
  (utf8-ify)

; Thank you newer emacs for choosing the wrong default
  (setq inhibit-splash-screen t)

; Coq via Proof General
  (add-to-hook coq-mode-hook
    (local-set-key "\C-c\C-m" 'proof-goto-point))

; Random line
  (autoload 'random-line "random-line" "Go to a random line" t)
  (global-set-key "\C-cr" 'random-line)

; Tired of fighting with the tabs
  (setq-default indent-tabs-mode nil)

; Add wc-mode for writing
  (require 'wc-mode)
  (setq wc-modeline-format "%tww")
  (add-to-hook text-mode-hook (wc-mode t))
  (global-set-key "\C-cw" 'wc-mode)

;; Screenwriter mode
;  (require 'screenwriter)
;  (add-to-list 'auto-mode-alist
;	'("\\.play" . screenwriter-mode))

; Org mode
  (require 'org-install)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)

; Improve Inform 7 editing
  (add-to-list 'auto-mode-alist '("\\.ni" . i7-mode))
  (add-to-list 'auto-mode-alist '("\\.i7x" . i7-mode))

; Markdown mode
  (autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.mdlhs\\'" . markdown-mode))

; Page mode
  (autoload 'page-mode "page-mode" "Page-oriented display" t)
  (add-to-list 'auto-mode-alist '("\\.p\\.txt$" . page-mode))
  (add-to-list 'auto-mode-alist '("\\.p\\.md$" . page-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.p\\.mdwn$" . page-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.p\\.markdown$" . page-markdown-mode))

; ASCIIdoc mode
  (autoload 'adoc-mode "adoc-mode" "ASCIIdoc" t)
  (add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))

; Haskell mode
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  ; Choose one
  ; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  (eval-after-load "haskell-mode"
    '(progn
       (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
       (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

;;   ; Enable MELPA support (http://github.com/milkypostman/melpa)
;;   (require 'package)
;;   (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;   ;; For important compatibility libraries like cl-lib
;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;;   (package-initialize)
;;   (unless (package-installed-p 'use-package)
;;     (package-refresh-contents)
;;     (package-install 'use-package))

  ;; https://robert.kra.hn/posts/rust-emacs-setup/
  ;(load-library "custom")
  ;(load-library "rust-init")

  (load-library "rust-mode")
  ;; https://fasterthanli.me/articles/the-bottom-emoji-breaks-rust-analyzer
  ;(load-rust-stuff)

  ;; Rustdoc edit mode.
  ;(require 'separedit)
  ;; Key binding for modes you want edit
  ;; or simply bind ‘global-map’ for all.
  ; (define-key prog-mode-map        (kbd "C-c '") #'separedit)
  ; (define-key minibuffer-local-map (kbd "C-c '") #'separedit)
  ;(define-key help-mode-map        (kbd "C-c '") #'separedit)
  ;(define-key helpful-mode-map     (kbd "C-c '") #'separedit)
  ;; Default major-mode for edit buffer
  ;; can also be other mode e.g. ‘org-mode’.
  ; (setq separedit-default-mode 'markdown-mode)
  ;; Feature options
  ;; (setq separedit-preserve-string-indentation t)
  ;; (setq separedit-continue-fill-column t)
  ;; (setq separedit-write-file-when-execute-save t)
  ;; (setq separedit-remove-trailing-spaces-in-comment t)

;; Replace M-j with new binding:
  (global-set-key "\M-j" 'maybe-fill-paragraph)

;; COBOL Mode!
  (autoload 'cobol-mode "cobol-mode" "Major mode for highlighting COBOL files." t nil)

  (setq auto-mode-alist
        (append
         '(("\\.cob\\'" . cobol-mode)
           ("\\.cbl\\'" . cobol-mode)
           ("\\.cpy\\'" . cobol-mode))
         auto-mode-alist))

;; sed mode!
  (autoload 'sed-mode "sed-mode" "Major mode for editing sed scripts" t nil)
  (setq auto-mode-alist
        (append
         '(("\\.sed\\'" . sed-mode))
         auto-mode-alist))
)

(defun load-rust-stuff ()
  ;; in `~/.emacs`

  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))

  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package-ensure)
  (setq use-package-always-ensure t)

  (use-package rustic)
  (use-package company)
  (use-package lsp-mode
      :ensure
        :commands lsp
          :custom
            ;; what to use when checking on-save. "check" is default, I prefer clippy
            (lsp-rust-analyzer-cargo-watch-command "clippy")
            (lsp-eldoc-render-all t)
            (lsp-idle-delay 0.6)
            (lsp-rust-analyzer-server-display-inlay-hints t)
            (lsp-log-io t)
          :config
            (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (use-package lsp-ui
      :ensure
        :commands lsp-ui-mode
          :custom
            (lsp-ui-peek-always-show t)
            (lsp-ui-sideline-enable nil)
            (lsp-ui-doc-enable t))

  (custom-set-variables
    ;; custom-set-variables was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    '(package-selected-packages '(lsp-ui rustic lsp-mode ## cmake-mode)))
  (custom-set-faces
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    )
  )

(defmacro add-to-hook (hook &rest body)
  `(add-hook (quote ,hook) (function (lambda () ,@body))))

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
      (setq my-compilation-command (read-from-minibuffer "Command: ")))
  (compile my-compilation-command t)
  (next-error))

(defun auto-invoke-mode (pat mode)
  (or (assoc pat auto-mode-alist)
      (setq auto-mode-alist
	    (cons (cons pat mode)
	    auto-mode-alist))))

(defun use-text-mode ()
       (setq default-major-mode 'text-mode))

(defun replace-in-region (start end regexp to-string)
  "Replace restricted to REGION of REGEXP with TO-STRING."
  (interactive "*r\nsReplace regexp: \nswith: " )
  (narrow-to-region start end)
  (save-excursion
    (goto-char start)
    (while (re-search-forward regexp nil t)
      (replace-match to-string nil nil))
    (widen)))

(defun set-basic-offset (ARG)
  "Bind c-basic-offset to given value."
  (interactive "P")
  (if ARG
      (setq c-basic-offset ARG)))

(defun right-margin-here ()
  "Set fill column to current column"
  (interactive)
  (setq fill-column (current-column)))

;; Stolen from indent-relative in indent.el -- it appears
;; to be difficult to delegate here.
(defun indent-relative-close (&optional unindented-ok)
  "Like `indent-relative', but looks for an indent point
   only in the immediately preceding line."
  (interactive "P")
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
	    (or (looking-at "[ \t]")
		unindented-ok
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
	     (setq perl-indent-level INDENTATION)
	     (setq sh-basic-offset INDENTATION)
             (setq rust-indent-offset INDENTATION)
             (setq pascal-indent-level INDENTATION))))

(defun init-weblogger ()
  (load "xml-rpc")
  (load "weblogger")
  (global-set-key "\C-xw" 'weblogger-start-entry)

  (setq weblogger-config-alist
	'(("default"
	   ("user" . "bart")
	   ("server-url" . "http://fob.po8.org/xmlrpc.php")
	   ("weblog" . "blog")))))

(setq mh-attach-command "$HOME/bin/mi/mh-attach")
(defun mh-insert-attachment (file)
  "Insert a properly-formatted attachment line for FILE into an MH message."
  (interactive "*fAttachment: ")
  (insert (shell-command-to-string (concat mh-attach-command " \"" file "\""))))

; I don't want anything but utf-8 really.
; It seems there should be a single call to do all this, but if so I
; haven't found it.  -- Carl Worth 26 June 2005
(defun utf8-ify ()
  (prefer-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  (setq default-file-name-coding-system 'utf-8)
  (setq default-process-coding-system (cons 'utf-8 'utf-8))
  (setq message-draft-coding-system 'utf-8)
  (setq message-send-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq mm-coding-system-priorities '(utf-8))
  (setq file-coding-system-alist
	(cons '("" . utf-8) file-coding-system-alist))
  (setq default-mime-charset 'utf-8))

(defun fix-ikiwiki-links ()
  "Replace spaces with underbars in all the ikiwiki links in the current buffer."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "[[" (point-max) t)
    (let ((start (point))
	  (max nil))
      (end-of-line)
      (setq max (point))
      (goto-char start)
      (setq end (search-forward "]]" max t))
      (if end
	  (replace-in-region start end " " "_")
	(goto-char max)))))

(defun copyright ()
  "Insert a UTF-8 copyright notice for the current year."
  (interactive)
  (insert (concat "Copyright © "
		  (format-time-string "%Y" (current-time))
		  " Bart Massey")))

(defun Copyright ()
  "Insert an ASCII copyright notice for the current year."
  (interactive)
  (insert (concat "Copyright (c) "
		  (format-time-string "%Y" (current-time))
		  " Bart Massey")))

;;; From Trey Jackson
;;; http://stackoverflow.com/questions/344966/sane-tab-in-emacs
;(defvar just-tab-keymap (make-sparse-keymap) "Keymap for just-tab-mode")
;(define-minor-mode just-tab-mode
;  "Just want the TAB key to be a TAB"
;  :global t :lighter " TAB" :init-value 0 :keymap just-tab-keymap
;  (define-key just-tab-keymap (kbd "TAB") 'self-insert-command))

(define-minor-mode i7-mode
  "Set up some primitive formatting things to make writing Inform 7 easier. This is NOT a full-on Inform 7 mode."
  :lighter " I7"
  (visual-line-mode)
  (local-set-key (kbd "TAB") 'self-insert-command))

(defun hard-tabs ()
  "Switch the current buffer to 8-space hard tabs for indentation."
  (interactive)
  (setq indent-tabs-mode t)
  (set-indent 8))

(defun soft-tabs ()
  "Switch the current buffer to soft tabs for indentation."
  (interactive)
  (setq indent-tabs-mode nil))

(defun page-markdown-mode ()
  "Put the buffer in markdown mode with page mod on."
  (interactive)
  (markdown-mode)
  (page-mode))

;; https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; https://stackoverflow.com/a/1416207/364875
(defun maybe-fill-paragraph (&optional justify region)
  "Fill paragraph at or after point (see `fill-paragraph').

Unfills paragraph if `visual-line-mode' is on."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'full) t)))
  (if visual-line-mode
      (unfill-paragraph region)
    (fill-paragraph justify region)))

(defun wrapped-text-mode ()
  "Turn on conveniences for working with wrapped-paragraph text."
  (interactive (progn
                 (visual-line-mode t)
                 (auto-fill-mode -1))))
