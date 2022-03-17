;;; org/agenda.el -*- lexical-binding: t; -*-
(use-package! org-super-agenda
    :commands (org-super-agenda-mode))
  (after! org-agenda
    (org-super-agenda-mode))

  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-tags-column 100 ;; from testing this seems to be a good value
        org-agenda-compact-blocks t
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
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          ;; (:name "Today"
                          ;;        :date today
                          ;;        :order 1)
                          (:name "Next to do"
                                 :todo "NEXT"
                                 :order 4)
                          (:name "Important"
                                 :tag "Important"
                                 :priority>= "B"
                                 :order 2)
                          ;; (:name "Due Today"
                          ;;        :deadline today
                          ;;        :order 3)
                          (:name "Daily Rituals"
                                 :tag "GTD"
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 5)
                          (:name "Overdue"
                                 :deadline past
                                 :face error
                                 :order 4)
                          (:name "Old Scheduled Items"
                                 :scheduled past
                                :order 5)
                          (:name "Quick Picks"
                           :effort< "0:30"
                           :order 8)
                          ;; (:name "Projects"
                          ;;  :children t
                          ;;  :todo "PROJ"
                          ;;  :order 9)
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
        ("Course" ,(list (all-the-icons-faicon "graduation-cap")) nil nil :ascent center)
        ("Project" ,(list (all-the-icons-material "archive")) nil nil :ascent center)
        ("Career Path" ,(list (all-the-icons-material "timeline")) nil nil :ascent center)
        ("todo" ,(list (all-the-icons-material "check")) nil nil :ascent center)
        ("Exercise" ,(list (all-the-icons-material "directions_run")) nil nil :ascent center)
        ("Book" ,(list (all-the-icons-octicon "book")) nil nil :ascent center)
        ("Video" ,(list (all-the-icons-material "ondemand_video")) nil nil :ascent center)))


;; (bind-key [f9] (org-agenda "o" "o"))
