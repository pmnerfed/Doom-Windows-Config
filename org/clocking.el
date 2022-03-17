;;; org/clocking.el -*- lexical-binding: t; -*-

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(defun org-pomodoro-prompt ()
(interactive)
(org-clock-goto)
(if (y-or-n-p "Start a new pomodoro?")
    (progn
      (org-pomodoro))))

(add-hook 'org-pomodoro-break-finished-hook 'org-pomodoro-prompt)
(setq org-pomodoro-keep-killed-time t
      org-pomodoro-manual-break t
      org-pomodoro-ticking-sound-p t
      org-pomodoro-ticking-frequency 300 ;; seconds
      org-pomodoro-format (concat (all-the-icons-material "timer"          :face 'all-the-icons-green     :v-adjust 0.01) "~%s"))
