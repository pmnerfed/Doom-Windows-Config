;;; news-feed.el -*- lexical-binding: t; -*-


(setq rmh-elfeed-org-files (list "~/Dropbox/.org/elfeed.org"))
;; Location:1 ends here

;; [[file:config.org::*Keybindings][Keybindings:1]]
(map! :leader :desc "News feed" :m "o n" #'=rss)
(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'+rss/quit
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "p" #'elfeed-show-pdf
      ;; :n "P" #'my/elfeed-search-open-enclosure'
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "y" #'elfeed-search-yank)
(map! :map elfeed-show-mode-map
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "q" #'+rss/delete-pane
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "N" #'elfeed-show-prev
      :nm "p" #'elfeed-show-pdf
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)
;; Keybindings:1 ends here

;; [[file:config.org::*Usability enhancements][Usability enhancements:1]]
(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))
(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode   'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))
;; Usability enhancements:1 ends here

;; [[file:config.org::*Visual enhancements][Visual enhancements:1]]
(after! elfeed

  (elfeed-org)
  (use-package! elfeed-link)
  ;; (elfeed-search-face-alist '((unread    elfeed-search-unread-title-face)
  ;;                           (star      elfeed-search-unread-count-face)))

  (setq elfeed-search-filter "@1-week-ago +unread"
        elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 1)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'nil
                      :weight 'light)

  (defadvice! +rss-elfeed-wrap-h-nicer ()
    "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
    :override #'+rss-elfeed-wrap-h
    (setq-local truncate-lines nil
                shr-width 160
                visual-fill-column-center-text t
                default-text-properties '(line-height 1.1))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (visual-fill-column-mode)
      (setq-local adaptive-wrap-prefix-mode -1)
      (setq-local shr-current-font '(:family "Work Sans" :height 1.2))
      (set-buffer-modified-p nil)))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/tag-column-width 30)
           (elfeed-goodies/feed-source-column-width 20)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))

           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))
      (setq-local line-spacing 0.3)))

  (defun +rss/elfeed-show-refresh--better-style ()
    "Update the buffer to match the selected entry, using a mail-style."
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (author (elfeed-meta elfeed-show-entry :author))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert "\n")
      (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
      (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
      (when (and author elfeed-show-entry-author)
        (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
      (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
      (when tags
        (insert (format "%s\n"
                        (propertize tagsstr 'face 'elfeed-search-tag-face))))
      ;; (insert (propertize "Link: " 'face 'message-header-name))
      ;; (elfeed-insert-link link link)
      ;; (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
               do (insert (propertize "Enclosure: " 'face 'message-header-name))
               do (elfeed-insert-link (car enclosure))
               do (insert "\n"))
      (insert "\n")
      (if content
          (if (eq type 'html)
              (elfeed-insert-html content base)
            (insert content))
        (insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min))))

  )
;; Visual enhancements:1 ends here

;; [[file:config.org::*Custom Tag faces][Custom Tag faces:1]]
(after! elfeed
  (defface elfeed-youtube
    '((t :foreground "#f9f"))
    "Marks YouTube videos in Elfeed."
    :group 'elfeed)

  (push '(youtube elfeed-youtube)
        elfeed-search-face-alist)

  (defface elfeed-comic
    '((t :foreground "#BFF"))
    "Marks comics in Elfeed."
    :group 'elfeed)

  (push '(comic elfeed-comic)
        elfeed-search-face-alist)

  (defface elfeed-audio
    '((t :foreground "#FA0"))
    "Marks podcasts in Elfeed."
    :group 'elfeed)

  (push '(audio elfeed-audio)
        elfeed-search-face-alist)

  (defface elfeed-important
    '((t :foreground "#E33"))
    "Marks important entries in Elfeed."
    :group 'elfeed)

  (push '(important elfeed-important)
        elfeed-search-face-alist)
  )
;; Custom Tag faces:1 ends here
;; [[file:config.org::*Star articles][Star articles:1]]
(after! elfeed





  ;; (defalias 'dan/elfeed-search-tag-all-star
;;         (elfeed-expose #'elfeed-search-tag-all 'star)
;;         "Add the `star' tag to all selected entries.")

;; (defalias 'dan/elfeed-search-untag-all-star
;;         (elfeed-expose #'elfeed-search-untag-all 'star)
;;         "Remove the `star' tag from all selected entries.")

;; (map! :map 'elfeed-search-mode-map :nv "x" #'dan/elfeed-search-tag-all-star
;;                                 :nv "X" #'dan/elfeed-search-untag-all-star)

;; below is a better way to just toggle it with one function.
        (defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))

;; (eval-after-load 'elfeed-search
;;   '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))
(map! :map elfeed-search-mode-map
        :after elfeed-search
        :n "m" 'elfeed-toggle-star)
  (map! :map elfeed-show-mode-map
        :after elfeed-show
        :n "m" 'elfeed-toggle-star)

)
;; Star articles:1 ends here

;; [[file:config.org::*PDF feeds][PDF feeds:1]]
(after! elfeed-show
  (require 'url)

  (defvar elfeed-pdf-dir
    (expand-file-name "pdfs/"
                      (file-name-directory (directory-file-name elfeed-enclosure-default-dir))))
  (setq elfeed-pdf-dir "/Volumes/Data/Reading/Computer Science/Research Papers/")

  (defvar elfeed-link-pdfs
    '(("https://www.jstatsoft.org/index.php/jss/article/view/v0\\([^/]+\\)" . "https://www.jstatsoft.org/index.php/jss/article/view/v0\\1/v\\1.pdf")
      ("http://arxiv.org/abs/\\([^/]+\\)" . "https://arxiv.org/pdf/\\1.pdf"))
    "List of alists of the form (REGEX-FOR-LINK . FORM-FOR-PDF)")

  (defun elfeed-show-pdf (entry)
    (interactive
     (list (or elfeed-show-entry (elfeed-search-selected :ignore-region))))
    (let ((link (elfeed-entry-link entry))
          (feed-name (plist-get (elfeed-feed-meta (elfeed-entry-feed entry)) :title))
          (title (elfeed-entry-title entry))
          (file-view-function
           (lambda (f)
             (when elfeed-show-entry
               (elfeed-kill-buffer))
             (pop-to-buffer (find-file-noselect f))))
          pdf)

      (let ((file (expand-file-name
                   (concat (subst-char-in-string ?/ ?, title) ".pdf")
                   (expand-file-name (subst-char-in-string ?/ ?, feed-name)
                                     elfeed-pdf-dir))))
        (if (file-exists-p file)
            (funcall file-view-function file)
          (dolist (link-pdf elfeed-link-pdfs)
            (when (and (string-match-p (car link-pdf) link)
                       (not pdf))
              (setq pdf (replace-regexp-in-string (car link-pdf) (cdr link-pdf) link))))
          (if (not pdf)
              (message "No associated PDF for entry")
            (message "Fetching %s" pdf)
            (unless (file-exists-p (file-name-directory file))
              (make-directory (file-name-directory file) t))
            (url-copy-file pdf file)
            (funcall file-view-function file))))))

  )
;; PDF feeds:1 ends here

;; [[file:config.org::*Play videos and audio with mpv][Play videos and audio with mpv:1]]
;; Original idea from
;; https://github.com/skeeto/.emacs.d/blob/985b68a0d10945e42ba4a0f7e429353c25b8add2/etc/feed-setup.el#L36-L48
(after! elfeed
  (defun elfeed-podcast-yank ()
  "Clean up and plays the first enclosure URL with mpv and copies the url into the clipboard."
  (interactive)
  (let* ((entry (elfeed-search-selected t))
         (url (caar (elfeed-entry-enclosures entry)))
         (fixed (replace-regexp-in-string "\\?.*$" "" url)))
    (if (fboundp 'gui-set-selection)
        (gui-set-selection elfeed-search-clipboard-type fixed)
      (with-no-warnings
        (x-set-selection elfeed-search-clipboard-type fixed)))
    (elfeed-untag entry 'unread)
    (message "Copied: %s" fixed)
    (if (eq (type-of fixed) 'string)
        (if (y-or-n-p (format "Found: %s ...play?" fixed))
        (call-process-shell-command
                 (format "mpv --force-window '%s'" fixed) nil 0)
        ))
    (unless (use-region-p) (forward-line))))

(defun my/elfeed-search-open-enclosure (&optional use-generic-p)
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
     (cl-loop for entry in entries
             when (elfeed-entry-link entry)
             do
             (call-process-shell-command
                 (format "mpv --force-window '%s'" (elfeed-entry-link entry)) nil 0))
    (mapc #'elfeed-search-update-entry entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))))
  (message "Opening...")
  (add-hook 'focus-out-hook (lambda () (message nil))))

  (map! :map elfeed-search-mode-map
        :after elfeed-search
        :n "P" 'elfeed-podcast-yank))
;; Play videos and audio with mpv:1 ends here

;; [[file:config.org::*Youtube Feeds][Youtube Feeds:1]]
(after! elfeed
  (defun std::elfeed::visit-entry-dwim (&optional arg)
    (interactive "P")
    (if arg
        (elfeed-search-browse-url)
      (-let [entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))]
        (if (s-matches? (rx "https://www.youtube.com/watch" (1+ any))
                        (elfeed-entry-link entry))
            (let* ((quality (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720" "1080")))
                   (format (if (= 0 (string-to-number quality)) "" (format "--ytdl-format=[height<=?%s]" quality))))
              (message "Opening %s with height ≤ %s with mpv..."
                       (elfeed-entry-link entry) quality)
              (elfeed-untag entry 'unread)
              (start-process "elfeed-mpv" nil "mpv" format (elfeed-entry-link entry))
              (elfeed-search-update :force))
          (if (eq major-mode 'elfeed-search-mode)
              (if (y-or-n-p "Not a youtube link. Want to open in browser?")
              (elfeed-search-browse-url) )
            (elfeed-show-visit))))))


  (map! :map elfeed-search-mode-map
        :after elfeed-search
        :n "v" 'std::elfeed::visit-entry-dwim)
  (map! :map elfeed-show-mode-map
        :after elfeed-show
        :n "v" 'std::elfeed::visit-entry-dwim)

(require 'youtube-dl)
(setq youtube-dl-directory "/Volumes/Data/Media/Videos/youtube_downloads")



(defun elfeed-show-youtube-dl ()
  "Download the current entry with youtube-dl."
  (interactive)
  (pop-to-buffer (youtube-dl (elfeed-entry-link elfeed-show-entry))))


(defun elfeed-show-youtube-dl ()
  "Download the current entry with youtube-dl."
  (interactive)
  (pop-to-buffer (youtube-dl (elfeed-entry-link elfeed-show-entry))))

(cl-defun elfeed-search-youtube-dl (&key slow)
  "Download the current entry with youtube-dl."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (dolist (entry entries)
      (if (null (youtube-dl (elfeed-entry-link entry)
                            :title (elfeed-entry-title entry)
                            :slow slow))
          (message "Entry is not a YouTube link!")
        (message "Downloading %s" (elfeed-entry-title entry)))
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (unless (use-region-p) (forward-line)))))

;; (defalias 'elfeed-search-youtube-dl-slow
  ;; (expose #'elfeed-search-youtube-dl :slow t))

(define-key elfeed-show-mode-map "d" 'elfeed-show-youtube-dl)
(define-key elfeed-search-mode-map "d" 'elfeed-search-youtube-dl)
;; (define-key elfeed-search-mode-map "D" 'elfeed-search-youtube-dl-slow)
(define-key elfeed-search-mode-map "L" 'youtube-dl-list)

  )
;; Youtube Feeds:1 ends here

;; [[file:config.org::*Auto add tags from categories metadata][Auto add tags from categories metadata:1]]
(after! elfeed
(defun tagize-for-elfeed (string)
  "Try to turn STRING into a reasonable Elfeed tag."
  (when (and (< (length string) 24)
             (string-match-p "^[/#]?[[:space:][:alnum:]]+$" string))
    (let* ((down (downcase string))
           (dashed (replace-regexp-in-string "[[:space:]]+" "-" down))
           (truncated (replace-regexp-in-string "^[/#]" "" dashed)))
      (intern truncated))))

(defun add-entry-categories-to-tags (entry)
  (dolist (category (elfeed-meta entry :categories) entry)
    (let ((tag (tagize-for-elfeed category)))
      (when tag
        (elfeed-tag entry tag)))))

(add-hook 'elfeed-new-entry-hook #'add-entry-categories-to-tags)
)
;; Auto add tags from categories metadata:1 ends here

;; [[file:config.org::*Open in Browser options][Open in Browser options:1]]
(after! elfeed
(defun elfeed-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (eww-browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-firefox-open (&optional use-generic-p)
  "open with firefox"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (browse-url-firefox it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-w3m-open (&optional use-generic-p)
  "open with w3m"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (ffap-w3m-other-window it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(define-key elfeed-search-mode-map (kbd "t") 'elfeed-w3m-open)
(define-key elfeed-search-mode-map (kbd "w") 'elfeed-eww-open)
;; (define-key elfeed-search-mode-map (kbd "f") 'elfeed-firefox-open)
)
;; Open in Browser options:1 ends here
