;; Nickle mode
;; Copyright © 2012 Bart Massey

;; This program is licensed under the GPL version 2 or
;; later. Please see the file COPYING in this distribution
;; for license terms.

;; This mode definition closely follows the cc-mode derived
;; mode example at
;;   http://cc-mode.sourceforge.net/derived-mode-ex.el
;; and its derivative at
;;   http://www.kgarner.com/code/git/?p=arduino.git;a=patch;h=bc6ea0a7

(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  (c-add-language 'nickle-mode 'c-mode))

;; delete-all is non-destructive
(defun delete-all (elems l)
  (cond ((null elems) (append l nil))
        (t (delete
            (car elems)
            (delete-all (cdr elems) l)))))

;; Nickle's types are quite different than C's
(c-lang-defconst
 c-primitive-type-kwds nickle
 (append
    '("bool" "real" "rational")
    (delete-all
     '("float" "double" "short" "char" "long")
     (c-lang-const c-primitive-type-kwds))))

(c-lang-defconst
 c-modifier-kwds nickle
 (cons "function" (c-lang-const c-modifier-kwds)))

(defvar nickle-mode-map
  (c-make-inherited-keymap)
  "Keymap used in Nickle mode buffers.")

(easy-menu-define nickle-menu nickle-mode-map "Nickle Mode Commands"
  (cons "Nickle" (c-lang-const c-mode-menu nickle)))

(defconst nickle-font-lock-keywords-1 (c-lang-const c-matchers-1 nickle)
  "Minimal highlighting for Nickle mode.")

(defconst nickle-font-lock-keywords-2 (c-lang-const c-matchers-2 nickle)
  "Fast normal highlighting for Nickle mode.")

(defconst nickle-font-lock-keywords-3 (c-lang-const c-matchers-3 nickle)
  "Accurate normal highlighting for Nickle mode.")

(defvar nickle-font-lock-keywords nickle-font-lock-keywords-3
  "Default expressions to highlight in Nickle mode.")

;;;###autoload
(define-derived-mode nickle-mode c-mode "Nickle"
  "Major mode for editing Nickle code."
  (c-init-language-vars nickle-mode)
  (setq
   comment-start "#"
   comment-end "$"
   c-comment-prefix-regexp "[#*]*"
   c-line-comment-starter "#"
   c-comment-start-regexp "/\\*\\|^[ \t]*#\\|\\s!"
   comment-start-skip "\\(^[ \t]*#\\|/\\*+\\)\\s *")
  (modify-syntax-entry ?# "< b" nickle-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" nickle-mode-syntax-table)
  (c-common-init 'nickle-mode)
  (easy-menu-add nickle-menu)
  (run-hooks 'nickle-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.5c\\'" . nickle-mode))

(provide 'nickle-mode)
