;;; ../../../mnt/c/Users/PUNEET MADAAN/.doom.d/org/brain.el -*- lexical-binding: t; -*-


(use-package! polymode
  :defer t)
(use-package! org-brain
  :after org
  :init
  (setq org-brain-path (concat org-directory "org-notes/brain/"))
  ;; For Evil users
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  ;; (require 'polymode)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries t
        org-brain-file-entries-use-title t)
  (defun org-brain-insert-resource-icon (link)
  "Insert an icon, based on content of org-mode LINK."
  (insert (format "%s "
                  (cond ((string-prefix-p "brain:" link)
                         (all-the-icons-fileicon "brain"))
                        ((string-prefix-p "info:" link)
                         (all-the-icons-octicon "info"))
                        ((string-prefix-p "help:" link)
                         (all-the-icons-material "help"))
                        ((string-prefix-p "http" link)
                         (all-the-icons-icon-for-url link))
                        (t
                         (all-the-icons-icon-for-file link))))))

  ;; (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode)
  (add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)
  (setq org-agenda-category-icon-alist
      `(("computers" ,(list (all-the-icons-material "computer")) nil nil :ascent center)
        ("books" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
        ("learn" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
        ("concept" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
        ("finance" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
        ("path" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
        ))
  (add-hook 'org-noter-insert-heading-hook #'org-id-get-create)
(defun org-brain-open-org-noter (entry)
    "Open `org-noter' on the ENTRY.
If run interactively, get ENTRY from context."
    (interactive (list (org-brain-entry-at-pt)))
    (org-with-point-at (org-brain-entry-marker entry)
      (org-noter)))

 )

(use-package! polymode
  :hook (org-brain-visualize-mode-hook .  org-brain-polymode))


;; Setup Deft with brain
(defun org-brain-deft ()
  "Use `deft' for files in `org-brain-path'."
  (interactive)
  (let ((deft-directory org-brain-path)
        (deft-recursive t)
        (deft-extensions '("org")))
    (deft)))


(bind-key [f5] 'org-brain-visualize)
(bind-key [f6] 'org-brain-deft)
