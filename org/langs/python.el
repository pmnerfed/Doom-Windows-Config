;;; ../../../mnt/c/Users/PUNEET MADAAN/.doom.d/org/langs/python.el -*- lexical-binding: t; -*-

(defun org-py-check ()
  "Run python check programs on a source block.
Opens a buffer with links to what is found."
  (interactive)
  (let ((eop (org-element-at-point))
        (temporary-file-directory ".")
        (cb (current-buffer))
        (n) ; for line number
        (content) ; error on line
        (pb "*org pycheck*")
        (pyflakes-status nil)
        (link)
        (tempfile))

    (unless (executable-find "pyflakes")
      (error "pyflakes is not installed."))

    (unless (executable-find "pycodestyle")
      (error "pycodestyle not installed"))

    (unless (executable-find "pylint")
      (error "pylint not installed"))

    ;; rm buffer if it exists
    (when (get-buffer pb) (kill-buffer pb))

    ;; only run if in a python code-block
    (when (and (eq 'src-block (car eop))
               (string= "python" (org-element-property :language eop)))

      ;; tempfile for the code
      (setq tempfile (make-temp-file "pychecker" nil ".py"))
      ;; create code file
      (with-temp-file tempfile
        (insert (org-element-property :value eop)))

      (let ((status (shell-command
                     (format "pyflakes %s" (file-name-nondirectory tempfile))))
            (output (delete "" (split-string
                                (with-current-buffer "*Shell Command Output*"
                                  (buffer-string)) "\n"))))
        (setq pyflakes-status status)
        (kill-buffer "*Shell Command Output*")
        (when output
          (set-buffer (get-buffer-create pb))
          (insert (format "\n* pyflakes output (status=%s)
pyflakes checks your code for errors. You should probably fix all of these.

" status))
          (dolist (line output)
            ;; get the line number
            (if
                (string-match (format "^%s:\\([0-9]*\\):\\(.*\\)"
                                      (file-name-nondirectory tempfile))
                              line)
                (progn
                  (setq n (match-string 1 line))
                  (setq content (match-string 2 line))
                  (setq link (format "[[elisp:(progn (switch-to-buffer-other-window \"%s\")(goto-char %s)(forward-line %s))][%s]]\n"
                                     cb
                                     (org-element-property :begin eop)
                                     n
                                     (format "Line %s: %s" n content))))
              ;; no match, just insert line
              (setq link (concat line "\n")))
            (insert link))))

      (let ((status (shell-command
                     (format "pycodestyle %s" (file-name-nondirectory tempfile))))
            (output (delete "" (split-string
                                (with-current-buffer "*Shell Command Output*"
                                  (buffer-string)) "\n"))))
        (kill-buffer "*Shell Command Output*")
        (when output
          (set-buffer (get-buffer-create pb))
          (insert (format "\n\n* Pycodestyle output (status = %s)\n" status))
          (insert "pep8 is the [[http://legacy.python.org/dev/peps/pep-0008][officially recommended style]] for writing Python code. Fixing these will usually make your code more readable and beautiful. Your code will probably run if you do not fix them, but, it will be ugly.

")
          (dolist (line output)
            ;; get the line number
            (if
                (string-match (format "^%s:\\([0-9]*\\):\\(.*\\)"
                                      (file-name-nondirectory tempfile))
                              line)
                (progn
                  (setq n (match-string 1 line))
                  (setq content (match-string 2 line))
                  (setq link (format "[[elisp:(progn (switch-to-buffer-other-window \"%s\")(goto-char %s)(forward-line %s))][%s]]\n"
                                     cb
                                     (org-element-property :begin eop)
                                     n
                                     (format "Line %s: %s" n content))))
              ;; no match, just insert line
              (setq link (concat line "\n")))
            (insert link))))

      ;; pylint
      (let ((status (shell-command
                     (format "pylint -r no %s" (file-name-nondirectory tempfile))))
            (output (delete "" (split-string
                                (with-current-buffer "*Shell Command Output*"
                                  (buffer-string)) "\n"))))
        (kill-buffer "*Shell Command Output*")
        (when output
          (set-buffer (get-buffer-create pb))
          (insert (format "\n\n* pylint (status = %s)\n" status))
          (insert "pylint checks your code for errors, style and convention. It is complementary to pyflakes and pep8, and usually more detailed.")

          (dolist (line output)
            ;; pylint gives a line and column number
            (if
                (string-match "\[a-zA-Z0-9.]+:\\([0-9]+\\):\\([0-9]+\\):\\s-\\(.*\\)"
                ;; (string-match "[A-Z]:\\s-*\\([0-9]*\\),\\s-*\\([0-9]*\\):\\(.*\\)"
                              line)
                (let ((line-number (match-string 1 line))
                      (column-number (match-string 2 line))
                      (content (match-string 3 line)))

                  (setq link (format "[[elisp:(progn (switch-to-buffer-other-window \"%s\")(goto-char %s)(forward-line %s)(forward-line 0)(forward-char %s))][%s]]\n"
                                     cb
                                     (org-element-property :begin eop)
                                     line-number
                                     column-number
                                     (format "≡ %02d⁙%02d ◆ %s" (string-to-number line-number) (string-to-number column-number) content))))
              ;; no match, just insert line
              (setq link (concat line "\n")))
            (insert link))))

      (when (get-buffer pb)
        (switch-to-buffer-other-window pb)
        (goto-char (point-min))
        (insert "Press q to close the window\n")
        (org-mode)
        (org-cycle '(64))
        ;; make read-only and press q to quit
        (setq buffer-read-only t)
        (use-local-map (copy-keymap org-mode-map))
        (local-set-key "q" #'(lambda () (interactive) (kill-buffer))))

      (unless (= 0 pyflakes-status)
        (forward-line 4)
        (error "pyflakes exited non-zero. please fix errors"))
      ;; final cleanup and delete file
      (delete-file tempfile)
      (switch-to-buffer-other-window cb))))


(defadvice org-babel-execute:python (before pychecker)
  (org-py-check))

(ad-activate 'org-babel-execute:python)
