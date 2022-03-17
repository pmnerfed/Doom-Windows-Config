;;; org/latex.el -*- lexical-binding: t; -*-


(setq org-highlight-latex-and-related '(native script entities))

;; (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; (setq org-format-latex-header "\\documentclass{article}
;; \\usepackage[usenames]{xcolor}

;; \\usepackage[T1]{fontenc}

;; \\usepackage{booktabs}

;; \\pagestyle{empty}             % do not remove
;; % The settings below are copied from fullpage.sty
;; \\setlength{\\textwidth}{\\paperwidth}
;; \\addtolength{\\textwidth}{-3cm}
;; \\setlength{\\oddsidemargin}{1.5cm}
;; \\addtolength{\\oddsidemargin}{-2.54cm}
;; \\setlength{\\evensidemargin}{\\oddsidemargin}
;; \\setlength{\\textheight}{\\paperheight}
;; \\addtolength{\\textheight}{-\\headheight}
;; \\addtolength{\\textheight}{-\\headsep}
;; \\addtolength{\\textheight}{-\\footskip}
;; \\addtolength{\\textheight}{-3cm}
;; \\setlength{\\topmargin}{1.5cm}
;; \\addtolength{\\topmargin}{-2.54cm}
;; % my custom stuff
;; \\usepackage[nofont,plaindd]{bmc-maths}
;; \\usepackage{arev}
;; ")
;; (setq org-format-latex-options
;;       (plist-put org-format-latex-options :background "Transparent"))
