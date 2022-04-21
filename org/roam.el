;;; org/roam.el -*- lexical-binding: t; -*-

(use-package! org-roam
  :defer 10
  :init
  (setq org-roam-directory "~/Dropbox/.org/org-notes/")
  :bind ([f8] . org-roam-node-find)
  :config
    (setq org-roam-capture-templates

      '(("d" "default" plain #'org-roam-capture--get-point :file-name "%<%Y-%m-%d>-${slug}" :head "#+title: ${title}\n#+ROAM_TAGS: %^{org-roam-tags}\n#+created: %u\n#+last_modified: %U\n%?" :unnarrowed t :jump-to-captured t)

                ("r" "ref" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "websites.org"
           :head "#+SETUPFILE:./hugo_setup.org
,#+ROAM_KEY: ${ref}
,#+HUGO_SLUG: ${slug}
,#+TITLE: ${title}
,#+added_on: %U

- source :: ${ref}"
           :unnarrowed t)

	("l" "clipboard" plain #'org-roam-capture--get-point "%i%a" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+ROAM_TAGS: %? \n" :unnarrowed t :prepend t :jump-to-captured t)))

(setq org-roam-completion-system 'ivy)
(setq org-roam-db-update-method 'immediate)
)

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :hook (org-roam . org-roam-ui-mode)
  :config
  (require 'org-roam) ; in case autoloaded
  (defun org-roam-ui-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (unless org-roam-ui-mode (org-roam-ui-mode 1))
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))
