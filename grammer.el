;;; ../../../mnt/c/Users/PUNEET MADAAN/.doom.d/grammer.el -*- lexical-binding: t; -*-

;; Disable flycheck in certain modes
(defun +markdown-flyspell-word-p ()
  "Return t if point is on a word that should be spell checked.

Return nil if on a link url, markup, html, or references."
  (let ((faces (doom-enlist (get-text-property (point) 'face))))
    (or (and (memq 'font-lock-comment-face faces)
             (memq 'markdown-code-face faces))
        (not (cl-loop with unsafe-faces = '(markdown-reference-face
                                            markdown-url-face
                                            markdown-markup-face
                                            markdown-comment-face
                                            markdown-html-attr-name-face
                                            markdown-html-attr-value-face
                                            markdown-html-tag-name-face
                                            markdown-code-face)
                      for face in faces
                      if (memq face unsafe-faces)
                      return t)))))


(setq langtool-language-tool-jar "/home/puneet/.tools/LanguageTool-5.7-stable/languagetool-commandline.jar")

(setq ispell-dictionary "en-custom")
(flycheck-define-checker textlint
  "A linter for textlint."
  :command ("npx" "textlint"
            "--config" "/home/puneet/.emacs.d/.textlintrc"
            "--format" "unix"
            "--rule" "write-good"
            "--rule" "no-start-duplicated-conjunction"
            "--rule" "max-comma"
            "--rule" "terminology"
            "--rule" "period-in-list-item"
            "--rule" "abbr-within-parentheses"
            "--rule" "alex"
            "--rule" "common-misspellings"
            "--rule" "en-max-word-count"
            "--rule" "diacritics"
            "--rule" "stop-words"
            "--plugin"
            (eval
             (if (derived-mode-p 'tex-mode)
                 "latex"
               "@textlint/text"))
            source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode latex-mode org-mode markdown-mode)
  )
