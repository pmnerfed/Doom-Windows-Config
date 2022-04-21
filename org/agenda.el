;;; org/agenda.el -*- lexical-binding: t; -*-
(use-package! org-super-agenda
  :commands (org-super-agenda-mode))
(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-start-with-log-mode t
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t
      org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (todo . " %i %-12:c %s ")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c"))
      org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

(setq org-agenda-custom-commands
      '(("o" "Overview"
         (
          (agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-day "+0d")

                      (org-super-agenda-groups
                       '(
                         (:name "⚡ Today"
                          :time-grid t
                          :date today
                          ;; :todo "TODAY"
                          :scheduled today
                          :order 1)
                         (:discard (:anything t))))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          ;; (:name "Today"
                          ;;  :time-grid t
                          ;;        :date today
                          ;;        :scheduled today
                          ;;        :order 1)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 1)
                          (:discard
                           (:tag "GTD"
                           :deadline future))
                          (:name "Important"
                           :tag "Important"
                           :priority>= "B"
                           :order 2)
                          (:name "Due Today"
                           :deadline today
                           :order 3)
                          (:name "Quick Picks"
                           :effort< "0:30"
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 5)
                          (:name "Next to do"
                           :todo "NEXT"
                           :order 4)
                          (:name "Daily Rituals"
                           :tag "GTD"
                           :order 2)
                          (:name "Old Scheduled Items"
                           :scheduled past
                           :order 5)
                          (:name "Projects"
                           :children t
                           :todo "PROJ"
                           :order 9)
                          (:name "Work"
                           :tag ("office" "work" "@office" "@work")
                           :order 9)
                          (:name "Personal"
                           :and(
                                :todo "NEXT"
                                :tag ("personal" "@personal"))
                           :order 11)
                          ;; (:name "Mentor Actions"
                          ;;        :tag ("mentor" "#mentor")
                          ;;        :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag ("Emacs" "emacs" "#emacs" "org")
                           :order 13)
                          (:name "Project Items"
                           :and(:tag ("Project" "project")
                                :children t
                                :tag "PROJ")
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "Habits"
                           :habit t
                           :order 18)
                          (:name "Consume Media"
                           :and(:todo ("TO-WATCH" "WATCHING")
                                :tag ("video" "watch"))
                           :and(:todo ("TO-READ" "READING")
                                :tag("read" "Read"))
                           :order 30)
                          (:name "Waiting"
                           :todo "WAIT"
                           :order 20)
                          (:name "Learning Items"
                           :tag "learn"
                           :order 32)
                          (:discard(:tag "Project"))
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant" "tp")
                           :todo ("SOMEDAY")
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))
                          ))))))))



(setq org-agenda-category-icon-alist
      `(("Work" ,(list (all-the-icons-faicon "briefcase")) nil nil :ascent center)
        ("Personal" ,(list (all-the-icons-material "person")) nil nil :ascent center)
        ("GTD" ,(list (all-the-icons-material "timer")) nil nil :ascent center)
        ("Emacs" ,(list (all-the-icons-fileicon "emacs")) nil nil :ascent center)
        ("Habit" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)
        ("Home" ,(list (all-the-icons-faicon "home")) nil nil :ascent center)
        ("Programming" ,(list (all-the-icons-faicon "code")) nil nil :ascent center)
        ("Finance" ,(list (all-the-icons-material "local_atm")) nil nil :ascent center)
        ("Course" ,(list (all-the-icons-faicon "graduation-cap")) nil nil :ascent center)
        ("Project" ,(list (all-the-icons-material "archive")) nil nil :ascent center)
        ("Career" ,(list (all-the-icons-material "timeline")) nil nil :ascent center)
        ("todo" ,(list (all-the-icons-material "check")) nil nil :ascent center)
        ("Exercise" ,(list (all-the-icons-material "directions_run")) nil nil :ascent center)
        ("Book" ,(list (all-the-icons-octicon "book")) nil nil :ascent center)
        ("Video" ,(list (all-the-icons-material "ondemand_video")) nil nil :ascent center)))

(defun org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "o"))
(bind-key [f9] 'org-agenda-show-agenda-and-todo)

;; COLORIZE ORG AGENDA DAY VIEW
;; work with org-agenda dispatcher [c] "Today Clocked Tasks" to view today's clocked tasks.
(defun org-agenda-log-mode-colorize-block ()
  "Set different line spacing based on clock time duration."
  (save-excursion
    (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
                     ('light
                      (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
                     ('dark
                      (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue"))))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          ;; larger duration bar height
          (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors) :foreground "black"))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'org-agenda-log-mode-colorize-block)
