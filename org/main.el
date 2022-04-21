;;; org/main.el -*- lexical-binding: t; -*-

(setq org-ellipsis " ▾ "
      org-use-property-inheritance t              ; it's convenient to have properties inherited
      org-log-done 'time                          ; having the time a item is done sounds convenient
      org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
      org-export-in-background t                  ; run export processes in external emacs process
      org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
        (?C . 'all-the-icons-yellow)
        (?D . 'all-the-icons-green)
        (?E . 'all-the-icons-blue))
      org-export-with-sub-superscripts '{}       ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-log-done 'note
      org-log-redeadline 'note
      org-log-reschedule 'note
      org-log-note-clock-out t
      org-log-into-drawer t
      org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈ now"

      ;; org-agenda-skip-scheduled-if-deadline-is- shown repeated-after-deadline
      org-treat-insert-todo-heading-as-state-change t
      org-log-refile 'note

)

;; Change list bullets with depth
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

(after! org
  (setq org-todo-keywords '((sequence "TODO(t/!)"  "NEXT(n/!)" "SOMEDAY(S@/!)" "STARTED(s/!)" "HOLD(h@/!)" "PROJ(p/!)" "|" "DONE(d@/!)" "KILL(k@/!)")
                            (sequence "[ ](T/!)" "[-](S/!)" "[?](W@/!)" "[!](I@/!)" "|" "[X](D@/!)")
                            (sequence "TO-READ(r/!)" "READING(/!)" "|" "READ(R@)")
                            (sequence "TO-WATCH(w/!)" "WATCHING(@/!)" "|" "WATCHED(W@)")
                            (sequence "|" "CANCELLED(c@)"))))



(load! "behaviour.el")
(load! "visuals.el")
(load! "src-blocks.el")
(load! "roam.el")
(load! "brain.el")
(load! "repetition.el")
(load! "agenda.el")
(load! "latex.el")
(load! "handle-issues.el")
(load! "clocking.el")
