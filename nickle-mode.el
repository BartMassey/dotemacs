;; Nickle mode
(mapc
 (function
  (lambda (s)
    (load (concat "nickle-mode/cc-" s ".el"))))
 '("vars" "langs" "engine" "mode"))
(add-hook 'nickle-mode-hook (function (lambda () (setq c-basic-offset 4))))
(setq auto-mode-alist (cons '("\\.5c$" . nickle-mode) auto-mode-alist))
