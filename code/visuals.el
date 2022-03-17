(use-package! svg-tag-mode
  :hook ((prog-mode . svg-tag-mode)
         (org-mode . svg-tag-mode))
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ count total) nil
                                    :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag value nil
                           :stroke 0 :margin 0)) :ascent 'center)))

(setq svg-tag-tags
      `(
        ;; Org tags
        (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority
                                            :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))

        ;; TODO / DONE
        ("[^\\s\"]\\bTODO:?\\b" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
        ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


        ;; Citation of the form [cite:@Knuth:1984]
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))


        ;; Active date (without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s *\\)%s>" date-re time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s *\\(%s>\\)" date-re time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s *\\)%s\\]" date-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s *\\(%s\\]\\)" date-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))


  
  ;; (setq svg-tag-tags
  ;;       '(
  ;;         ("\\bDONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
  ;;         ("\\bFIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0))))
  ;;         ("\\/\\/\\W?MARK\\b:\\|MARK\\b:" . ((lambda (tag) (svg-tag-make "MARK" :face 'font-lock-doc-face :inverse t :margin 0 :crop-right t))))
  ;;         ("\\bMARK\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-doc-face :crop-left t))))

  ;;         ("\\/\\/\\W?swiftlint:disable" . ((lambda (tag) (svg-tag-make "swiftlint:disable" :face 'org-level-3 :inverse t :margin 0 :crop-right t))))
  ;;         ("\\bswiftlint:disable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-level-3 :crop-left t))))

  ;;         ("\\/\\/\\W?TODO\\b\\|TODO\\b" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0 :crop-right t))))
  ;;         ("\\bTODO\\b\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :crop-left t))))
  ;;         ))
  )
; //MARK: - Do something
; TODO fix me later
; //swiftlint:disable hello

;; To do:         TODO DONE
;; Tags:          :TAG1:TAG2:TAG3:
;; Priorities:    [#A] [#B] [#C]
;; Progress:      [1/3]
;;                [42%]
;; Active date:   <2021-12-24>
;;                <2021-12-24 14:00>
;; Inactive date: [2021-12-24]
;;                [2021-12-24 14:00]
;; Citation:      [cite:@Knuth:1984]
