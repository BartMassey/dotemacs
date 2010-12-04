;; Copyright (C) 2010 Bart Massey
;; ALL RIGHTS RESERVED
;; 
;; [This program is licensed under the MIT License.]
;; Please see the file COPYING in the source
;; distribution of this software for license terms.

;; "Logic Mode"

;; Minor mode for conveniently entering Unicode for various
;; logic symbols.  Basically just a keymap with some bindings.

;; Bart Massey 2010/03/22

;; XXX for some reason the logical negation
;; symbol will not self-insert
(defun insert-not ()
  "insert logical not symbol"
  (interactive)
  (ucs-insert ?¬))

(defun logic-mode-build-keymap ()
  "Build the mode keymap for logic mode."
  (let  ((map (make-keymap)))
    (define-key map "\C-c\C-ba" "∧") ; and
    (define-key map "\C-c\C-bo" "∨") ; or
    (define-key map "\C-c\C-bn" 'insert-not) ; not
    (define-key map "\C-c\C-b=" "≡") ; equiv
    (define-key map "\C-c\C-bi" "→") ; implies
    (define-key map "\C-c\C-bt" "→") ; type
    (define-key map "\C-c\C-bx" "⊕") ; xor
    (define-key map "\C-c\C-bu" "∀") ; universal
    (define-key map "\C-c\C-be" "∃") ; existential
    (define-key map "\C-c\C-bq" "∴") ; qed
    (define-key map "\C-c\C-bb" "□") ; box
    map))

;; Define the mode.
(define-minor-mode logic-mode
  "Toggle minor mode for entering logic symbols."
  nil " Logic" (logic-mode-build-keymap) nil)
