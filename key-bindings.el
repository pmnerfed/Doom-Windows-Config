;;; key-bindings.el -*- lexical-binding: t; -*-

;; window movmement keys
(map! "C-w" nil)
(global-set-key  (kbd "C-<tab>") #'evil-window-next)
(global-set-key  (kbd "C-<iso-lefttab>") #'evil-window-prev)
(global-set-key  (kbd "C-w") #'ace-window)

(map! :nvig "C-<iso-lefttab>" #'evil-window-prev
      :nvig  "C-w" #'ace-window
      :nvig "C-<tab>" #'evil-window-next)

;; expand region
(map! :nvig "C-'" #'er/expand-region)
