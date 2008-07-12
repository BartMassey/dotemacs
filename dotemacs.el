(load "~/etc/emacs/emacs")
(my-init)
;; ; refine
;; (defvar *refine-emacs-directory*
;;   "/local/apps/reasoning/refine/3-1/editor/franz")
;; (setq load-path (cons *refine-emacs-directory* load-path))
;; (condition-case ()
;;     (require 'refine-autoload-info
;;             "refine-autoload-info")
;;   (error))
;; ; training world
;; (if (equal window-system 'x)
;;   (require 'rea-x-mouse))
;; (defun Buffer-menu-this-window ()
;;   "Select this line's buffer in this window, and bury menu buffer."
;;   (interactive)
;;   (let ((menu-buffer (current-buffer)))
;;     (switch-to-buffer (Buffer-menu-buffer t))
;;     (bury-buffer menu-buffer)))
;; ; kids
;; (if window-system
;;  (condition-case
;;   nil       
;;   (load "/kids/dev3/editor/kids-comm")
;;   (file-error
;;    (princ 
;;     "KIDS not loaded:  couldn't load /kids/dev3/editor/kids-comm"
;;     t))))
