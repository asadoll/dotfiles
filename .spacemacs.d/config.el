;;; .spacemacs.d/config.el -*- lexical-binding: t; -*-
;;; package --- Summary
;;; Commentary:
;;; Code

(setq user-full-name "Asad. Gharighi"
      user-mail-address "a.ghrighi@gmail.com")

(fringe-mode '(0 . 4))
;; Platform Specific Configuration

;; Linux
;; (add-to-list 'load-path "~/.local/share/icons-in-terminal/")

;; System/Environment/Emacs/Runtime Configuration
(setq read-process-output-max (* 1024 1024 10)
      visual-fill-column-width 100)

;; Layer/Package level Configuration in order of Dependency

;; (require 'icons-in-terminal)

;; Preventing ligatures from scrambling
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

  ;;; Kawkab Mono
;; This works when using emacs --daemon + emacsclient
;; (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#X600 . #X6ff) "Kawkab Mono")))
;; This works when using emacs without server/client
;; (set-fontset-font t '(#X600 . #X6ff) "Kawkab Mono")

  ;;; Vazir
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#X600 . #X6ff) "Vazir") frame))
(set-fontset-font t '(#X600 . #X6ff) "Vazir")

;; (add-hook 'prog-mode-hook 'fira-code-mode)
(set-face-attribute 'link nil :bold nil)
(set-face-attribute 'button nil :bold nil)
(set-face-attribute 'font-lock-function-name-face nil :bold nil)
(set-face-attribute 'font-lock-constant-face nil :bold nil)
(set-face-attribute 'font-lock-keyword-face nil :bold nil)

(defun update-faces ()
  (message "Updating faces...")
  (when (bound-and-true-p lsp-ui-doc-mode)
    (message "Updating lsp-ui-doc faces...")
    (setq lsp-ui-doc-border (face-attribute 'highlight :background))

    (set-face-attribute 'lsp-ui-doc-header nil :background (face-attribute 'highlight :background))
    (set-face-attribute 'lsp-ui-doc-background nil :background (face-attribute 'hl-line :background))
    (message "Updating markdown faces...")
    (set-face-attribute 'markdown-code-face nil :background (face-attribute 'hl-line :background)))
  (when (bound-and-true-p lsp-mode)
    (message "Updating lsp faces...")
    (set-face-attribute 'lsp-face-highlight-read nil :background (face-attribute 'hl-line :background)))
  (when (bound-and-true-p which-key-posframe-mode)
    (message "Updating which-key-posframe faces...")
    (set-face-attribute 'which-key-posframe-border nil :background (face-attribute 'highlight :background)))
  )

(defun tweak-ui ()
  (message "Tweaking UI...")
  (setq doom-manegarm-darker-background nil
        doom-manegarm-muted-modeline nil
        doom-manegarm-padded-modeline 2)
  (setq doom-solarized-dark-brighter-modeline nil
        doom-solarized-dark-brighter-comments nil
        doom-solarized-dark-padded-modeline 2)
  (setq doom-solarized-light-brighter-modeline nil
        doom-solarized-light-brighter-comments nil
        doom-solarized-light-padded-modeline 2)
  (update-faces)
  )

(defun lsp-ui-setup ()
  (defun navigate-down ()
    (interactive)
    (ccls-navigate "D"))
  (defun navigate-left ()
    (interactive)
    (ccls-navigate "L"))
  (defun navigate-right ()
    (interactive)
    (ccls-navigate "R"))
  (defun navigate-up ()
    (interactive)
    (ccls-navigate "U"))
  (message "lsp-ui setup...")
  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/set-leader-keys "of" 'lsp-ui-doc-focus-frame)
  (spacemacs/set-leader-keys "od" 'navigate-down)
  (spacemacs/set-leader-keys "ol" 'navigate-left)
  (spacemacs/set-leader-keys "or" 'navigate-right)
  (spacemacs/set-leader-keys "ou" 'navigate-up)
  (update-faces)
  )
(add-hook 'lsp-ui-doc-mode-hook 'lsp-ui-setup)
(add-hook 'spacemacs-post-theme-change-hook 'tweak-ui)
(when (not spacemacs-post-user-config-hook-run)
  (message "Adding post user config hook...")
  (add-hook 'spacemacs-post-user-config-hook 'tweak-ui))

;; Enable ligatures
(use-package composite
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  ((prog-mode conf-mode nxml-mode markdown-mode help-mode
              lsp-ui-doc-frame-mode ;; org-mode
              company-mode company-quickhelp-mode company-box-mode
              ivy-posframe-mode company-posframe-mode
              haskell-doc-mode
              )
   . (lambda () (setq-local composition-function-table composition-ligature-table)))
  :config
  ;; support ligatures, some toned down to prevent hang
  (when (version<= "27.0" emacs-version)
    (let ((alist
           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36 . ".\\(?:\\(>\\)>?\\)")
             (37 . ".\\(?:\\(%\\)%?\\)")
             (38 . ".\\(?:\\(&\\)&?\\)")
             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94 . ".\\(?:\\(=\\)=?\\)")
             (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))

;; (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
(set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'prepend)
;; (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)

(setq company-tooltip-align-annotations t
      company-quickhelp-use-propertized-text t)

;; (use-package all-the-icons-ivy
;;   :after (all-the-icons ivy)
;;   :custom (all-the-icons-ivy-file-commands '(counsel-dired-jump
;;                                              counsel-find-file
;;                                              counsel-file-jump
;;                                              counsel-find-library
;;                                              counsel-git
;;                                              counsel-projectile-find-dir
;;                                              counsel-projectile-find-file
;;                                              counsel-recentf))
;;   :config (all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :ensure t
  :after (all-the-icons ivy)
  :init (all-the-icons-ivy-rich-mode 1)
  :config (setq all-the-icons-ivy-rich-icon-size 0.9))

(use-package ivy-rich
  :after (ivy all-the-icons-ivy-rich-mode)
  :ensure t
  :init (ivy-rich-mode 1)
  :config
  ;; (setq ivy-rich-path-style 'abbreviate)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package ivy-posframe
  :after (ivy ivy-rich)
  :config
  (setq ivy-posframe-border-width 10)
  (setq ivy-posframe-display-functions-alist
        '(;; (complete-symbol               . ivy-posframe-display-at-point)
          (swiper                        . ivy-posframe-display-at-window-center)
          (t                             . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 0)
          (right-fringe . 0)))
  (setq ivy-posframe-width 120)
  (ivy-posframe-mode))

(use-package which-key-posframe
  :config
  (setq which-key-posframe-border-width 1)
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (setq which-key-posframe-parameters
        '((left-fringe . 0)
          (right-fringe . 0)
          (internal-border-width . 1)
          ))
  (setq which-key-posframe-min-height 1)
  (setq which-key-posframe-min-width 200)
  (setq which-key-posframe-width 200)
  (which-key-posframe-mode))

(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

;; (use-package company-posframe
;;   :diminish company-posframe-mode
;;   :after company
;;   :config
;;   (company-posframe-mode))

;; (setq completion-styles `(basic partial-completion emacs22 initials
;;                                 ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))

(setq company-box-icons-unknown 'fa_question_circle
      company-box-icons-yasnippet 'fa_bookmark)
(setq company-box-icons-elisp
      '((fa_tag :face font-lock-function-name-face) ;; Function
        (fa_cog :face font-lock-variable-name-face) ;; Variable
        (fa_cube :face font-lock-constant-face) ;; Feature
        (md_color_lens :face font-lock-doc-face))) ;; Face
(setq company-box-icons-lsp
      '((1 . fa_text_height) ;; Text
        (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
        (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
        (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
        (5 . (fa_cog :foreground "#FF9800")) ;; Field
        (6 . (fa_cog :foreground "#FF9800")) ;; Variable
        (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
        (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
        (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
        (10 . (fa_cog :foreground "#FF9800")) ;; Property
        (11 . md_settings_system_daydream) ;; Unit
        (12 . (fa_cog :foreground "#FF9800")) ;; Value
        (13 . (md_storage :face font-lock-type-face)) ;; Enum
        (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
        (15 . md_closed_caption) ;; Snippet
        (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
        (17 . fa_file_text_o) ;; File
        (18 . md_refresh) ;; Reference
        (19 . fa_folder_open) ;; Folder
        (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
        (21 . (fa_square :face font-lock-constant-face)) ;; Constant
        (22 . (fa_cube :face font-lock-type-face)) ;; Struct
        (23 . fa_calendar) ;; Event
        (24 . fa_square_o) ;; Operator
        (25 . fa_arrows)) ;; TypeParameter
      )

;; Make current window shine
(use-package solaire-mode
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode))
  :config

  ;; (set-face-attribute 'solaire-default-face nil :background "#00212b")
  (solaire-global-mode)
  ;; (solaire-mode-swap-bg)
  )

;; Setup tabs
(use-package centaur-tabs
  :demand
  :init
  :config
  (setq centaur-tabs-set-bar 'under
        centaur-tabs-style "bar"
        centaur-tabs-close-button "" ;; 🅇
        centaur-tabs-height 28
        centaur-tabs-set-icons t
        centaur-tabs-plain-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker ""
        centaur-tabs-cycle-scope 'tabs)
  ;; (setq centaur-tabs--buffer-show-groups t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
  			                      magit-status-mode
  			                      magit-diff-mode
  			                      magit-log-mode
  			                      magit-file-mode
  			                      magit-blob-mode
  			                      magit-blame-mode
  			                      )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
  		                    help-mode))
       "Help")
      ((memq major-mode '(org-mode
  		                    org-agenda-clockreport-mode
  		                    org-src-mode
  		                    org-agenda-mode
  		                    org-beamer-mode
  		                    org-indent-mode
  		                    org-bullets-mode
  		                    org-cdlatex-mode
  		                    org-agenda-log-mode
  		                    diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-<iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ("C-c t" . centaur-tabs-counsel-switch-group)
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward)))

;; Starting with a decent wide window
;; (add-to-list 'default-frame-alist '(height . 35))
;; (add-to-list 'default-frame-alist '(width . 140))

(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)
;; (add-to-list 'exec-path "/home/asad/.local/bin")

;; Enabling vertical window divider
;; (window-divider-mode)

(display-time-mode t)
;; (display-battery-mode t)
(doom-themes-treemacs-config)
;; (doom-themes-visual-bell-config)
;; ivy will fuzz everywhere!
;; (with-eval-after-load 'ivy
;;   (push (cons #'swiper (cdr (assq t ivy-re-builders-alist)))
;;         ivy-re-builders-alist)
;;   (push (cons t #'ivy--regex-fuzzy) ivy-re-builders-alist))

;; Configuring hunspell as ispell executable
;; (add-to-list 'exec-path "c:/Users/aghar/scoop/apps/msys2/current/mingw64/bin")
;; (setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

;; Stop truncating lines by default
(setq-default truncate-lines t)

;; Org Mode
(use-package org-protocol
  :after org
  :config
  (append org-modules 'org-protocol))

(setq asad/org-agenda-directory "~/Dropbox/Personal/gtd/")
(require 'find-lisp)
(with-eval-after-load 'find-lisp
  (message "Populating agenda files...")
  (setq org-agenda-files
      (find-lisp-find-files asad/org-agenda-directory "\.org$")))
;; (setq org-projectile-file "/home/asad/Documents/agenda.org")
;; (setq org-agenda-files (list org-projectile-file))

(setq org-capture-templates
      `(("i" "inbox" entry (file ,(concat asad/org-agenda-directory "inbox.org"))
         "* TODO %?")
        ("e" "email" entry (file+headline ,(concat asad/org-agenda-directory "emails.org") "Emails")
         "* TODO [#A] Reply: %a :@home:@work:"
         :immediate-finish t)
        ("c" "org-protocol-capture" entry (file ,(concat asad/org-agenda-directory "inbox.org"))
         "* TODO [[%:link][%:description]]\n\n %i"
         :immediate-finish t)
        ("w" "Weekly Review" entry (file+olp+datetree ,(concat asad/org-agenda-directory "reviews.org"))
         (file ,(concat asad/org-agenda-directory "templates/weekly_review.org")))
        ;; TODO needs fix
        ("r" "Reading" todo ""
         ((org-agenda-files '(,(concat asad/org-agenda-directory "reading.org")))))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-bullets-bullet-list '("" "" "" "" "" "" "" ""))
;; (setq org-todo-keywords '((sequence " TODO(t)" "|"
;;                                     " DONE(d)")
;;                           (sequence " WAITING(w)" "|")
;;                           (sequence "|" " CANCELED(c)")))

(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(setq org-tag-alist (quote (("@errand" . ?e)
                            ("@home" . ?h)
                            ("@office" . ?o)
                            ("@work" . ?s)
                            (:newline)
                            ("WAITING" . ?w)
                            ("HOLD" . ?H)
                            ("CANCELLED" . ?c))))

(setq org-fast-tag-selection-single-key nil)
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("next.org" :level . 0)
                           ("someday.org" :level . 0)
                           ("reading.org" :level . 1)
                           ("projects.org" :maxlevel . 1)))

(with-eval-after-load 'ox
  (require 'ox-hugo)
  (defun asad/conditional-hugo-enable ()
    (save-excursion
      (if (cdr (assoc "SETUPFILE" (org-roam--extract-global-props '("SETUPFILE"))))
          (org-hugo-auto-export-mode +1)
        (org-hugo-auto-export-mode -1))))
  (add-hook 'org-mode-hook #'asad/conditional-hugo-enable))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "private-%Y-%m-%d.org"
        org-journal-dir "/home/asad/Dropbox/Personal/org"
        org-journal-carryover-items nil
        org-journal-date-format "%Y-%m-%d")
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(use-package org-download
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-dnd-base64
  :init
  ;; (map! :map org-mode-map
  ;;       "s-Y" #'org-download-screenshot
  ;;       "s-y" #'org-download-yank)
  ;; (pushnew! dnd-protocol-alist
  ;;           '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . +org-dragndrop-download-dnd-fn)
  ;;           '("^data:" . org-download-dnd-base64))
  (advice-add #'org-download-enable :override #'ignore)
  :config
  (defun +org/org-download-method (link)
    (let ((filename
           (file-name-nondirectory
            (car (url-path-and-query
                  (url-generic-parse-url link)))))
          ;; Create folder name with current buffer name, and place in root dir
          (dirname (concat "./images/"
                           (replace-regexp-in-string " " "_"
                                                     (downcase (file-name-base buffer-file-name)))))
          (filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename))))
      (make-directory dirname t)
      (expand-file-name filename-with-timestamp dirname)))
  ;; :config
  ;; (setq org-download-screenshot-method
  ;;       (cond (IS-MAC "screencapture -i %s")
  ;;             (IS-LINUX
  ;;              (cond ((executable-find "maim")  "maim -s %s")
  ;;                    ((executable-find "scrot") "scrot -s %s")))))
  ;; (if (memq window-system '(mac ns))
  ;;     (setq org-download-screenshot-method "screencapture -i %s")
  ;;   (setq org-download-screenshot-method "maim -s %s"))
  ;; (setq org-download-method 'my-org-download-method)
  )

;; (use-package org-ref-ox-hugo
;;   :ensure t
;;   :after (org org-ref ox-hugo)
;;   :config
;; (require 'org-ref-ox-hugo)
;; (add-to-list 'org-ref-formatted-citation-formats
;;              '("md"
;;                ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
;;                ("inproceedings" . "${author}, *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
;;                ("book" . "${author}, *${title}* (${year}), ${address}: ${publisher}.")
;;                ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
;;                ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
;;                ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
;;                ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
;;                ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
;;                ("misc" . "${author} (${year}). *${title}*. Retrieved from [${howpublished}](${howpublished}). ${note}.")
;;                (nil . "${author}, *${title}* (${year}).")))

(use-package password-store)
(use-package org-gcal
  :after (org password-store)
  :commands (org-gcal-fetch)
  :config
  (setq org-gcal-client-id (password-store-get "gmail/org-gcal-client")
        org-gcal-client-secret (password-store-get "gmail/org-gcal")
        org-gcal-fetch-file-alist '(("a.gharighi@gmail.com" . "~/Dropbox/Personal/gtd/calendars/personal.org")
                                    ;; ("dckbhpq9bq13m03llerl09slgo@group.calendar.google.com" . "~/.org/gtd/calendars/lab.org")
                                    )))

(use-package gif-screencast
  :bind
  ("<f12>" . gif-screencast-start-or-stop))

(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook (lambda () (toggle-word-wrap +1)))

(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(use-package elfeed-org
  :init
  (setq rmh-elfeed-org-files '("/home/asad/Dropbox/Personal/org/feeds.org"))
  :config
  (elfeed-org))

(use-package outshine
  :commands (outshine-mode))

(setq spaceline-org-clock-p t)
(doom-themes-org-config)


;; All languages
;; (add-hook 'prog-mode-hook #'lsp)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
;; (setq lsp-prefer-capf t)
(setq lsp-enable-on-type-formatting nil
      lsp-enable-indentation nil
      lsp-imenu-show-container-name t
      lsp-ui-doc-alignment 'frame
      lsp-ui-doc-delay 0.2
      lsp-ui-doc-frame-parameters '((left-fringe . 4) (right-fringe . 4) (internal-border-width . 1))
      lsp-ui-doc-header t
      lsp-ui-doc-include-signature t
      lsp-ui-doc-max-height 33
      lsp-ui-doc-max-width 120
      lsp-ui-doc-position 'top)
;; (setq lsp-document-sync-method 'full)

;; C-C++
(setq projectile-require-project-root t)
(setq cmake-ide-build-dir "./build")
(setq cmake-ide-header-search-other-file nil)
(setq cmake-ide-header-search-first-including nil)
(setq ccls-extra-init-params '(:index (:comments 2) :completion (:detailedLabel t)))
(setq ccls-sem-highlight-method nil)
;; (add-hooke 'WHATEVER-MODE-HOOK ((set-face-attribute 'ccls-sem-static-face nil :bold nil)))
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
;; (modern-c++-font-lock-global-mode t)
(setq gdb-many-windows t
      gdb-show-main t)

;; Haskell
;; (require 'lsp-haskell)
(setq lsp-haskell-process-path-hie "hie-wrapper")
(setq lsp-haskell-process-args-hie '("-d" "-l" "/home/asad/.temp/hie.log"))
;; (setq lsp-haskell-process-path-hie "cabal")
;; (setq lsp-haskell-process-args-hie '("new-exec" "ghcide" "--" "--lsp"))
;; (setq lsp-haskell-process-wrapper-function (lambda (argv) (cons (car argv) (cddr argv))))
(add-hook 'haskell-mode-hook 'lsp)
(add-hook 'haskell-mode-hook (lambda ()  (setq tab-width 2)))
;; Rust
(add-hook 'rust-mode-hook 'lsp)

;; Javascript
(add-hook 'js2-mode-hook 'lsp)
(add-hook 'typescript-mode-hook 'lsp)
;; (require 'dap-node)
;; (require 'dap-firefox)
;; (require 'dap-chrome)

;; Python
;; (add-hook 'python-mode-hook 'lsp)

;; Dart
;; (add-hook 'dart-mode-hook 'lsp)

(use-package dashboard
  :ensure t
  :config
  ;; (setq dashboard-buffer-name "*asad*")
  ;; Set the title
  (setq dashboard-banner-logo-title "")
  ;; Set the banner
  (setq dashboard-startup-banner "~/.spacemacs.d/spacemacs-icon-128.png")
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" which displays whatever image you would prefer

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  ;; (setq dashboard-show-shortcuts nil)

  (setq dashboard-items '((projects . 8)
                          (recents  . 10)
                          (bookmarks . 5)))

  ;; (setq show-week-agenda-p t)
  ;; (setq dashboard-org-agenda-categories '("Tasks" "Appointments"))

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)

  ;; (setq dashboard-set-footer nil)
  (setq dashboard-footer-messages '(""))
  (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face))

  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-octicon "package" :height 1 :v-adjust 0.0)
            "Update" "Update Packages" (lambda (&rest _) (configuration-layer/update-packages)) nil "" "")
           (,(all-the-icons-material "live_help" :height .9 :v-adjust -0.1)
            "Emacs Tutorial" "" (lambda (&rest _) (emacs-tutorial nil)) nil "" ""))
          ;; line 2
          ;; ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
          ;;   "Linkedin"
          ;;   ""
          ;;   (lambda (&rest _) (browse-url "homepage")))
          ;;  ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))
          ))

  (defun dashboard-setup ()
    "Setup post initialization hooks.
If a command line argument is provided,
assume a filename and skip displaying Dashboard."
    ;; Display useful lists of items
    (dashboard-insert-startupify-lists)
    (when (< (length command-line-args) 2 )
      (setq initial-buffer-choice #'(lambda ()
                                     (get-buffer dashboard-buffer-name)))
      (switch-to-buffer dashboard-buffer-name)
      (goto-char (point-min))
      (set-window-margins nil 1)
      (redisplay)))
  (dashboard-setup))

;; (use-package window-purpose)
;; (golden-ratio-mode 1)
(provide 'config)
