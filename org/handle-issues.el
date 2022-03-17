;;; org/handle-issues.el -*- lexical-binding: t; -*-


;; When one of the src_elisp{org-mode-hook} functions errors, it halts the hook
;; execution. This is problematic, and there are two hooks in particular which
;; cause issues. Let's make their failure less eventful.

(defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
  :around #'org-fancy-priorities-mode
  :around #'org-superstar-mode
  (ignore-errors (apply orig-fn args)))
