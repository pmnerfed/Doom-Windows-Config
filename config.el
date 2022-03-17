;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; Resources that helped me
;;; Installing Emacs on Windows WSL 2 : https://github.com/hubisan/emacs-wsl#wsl-2-additional-firewall-configuration
;;;

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Puneet Madaan"
      user-mail-address "puneet.madaan@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq
       ;; doom-font (font-spec :family "LigaSrc Pro" :size 20 :weight 'Book)
       doom-font (font-spec :family "Liga Source Code Pro" :size 26)
       ;; doom-font (font-spec :family "BlexMono Nerd Font Mono" :size 20 :weight 'Book)
       ;; doom-variable-pitch-font (font-spec :family "BlexMono Nerd Font" :size 20 :weight 'light)
       doom-variable-pitch-font (font-spec :family "BlexMono Nerd Font" :size 26 :weight 'light)
       ;; doom-unicode-font (font-spec :family "SFMono Nerd Font")
       doom-unicode-font (font-spec :family "JuliaMono")
       ;; doom-unicode-font (font-spec :family "FuraCode Nerd Font Retina")
)


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/.org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative
      evil-vsplit-window-right t
      evil-split-window-below t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "news-feed.el")
(load! "./org/main.el")
(load! "key-bindings.el")
