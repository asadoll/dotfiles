;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(fringe-mode '(0 . 0))

(setq read-process-output-max (eval-when-compile (* 1024 1024 10))
      visual-fill-column-width 100)
(setq gcmh-high-cons-threshold (eval-when-compile (* 1024 1024 1024)))

(setq user-full-name "Asad. Gharighi"
      user-mail-address "a.gharighi@gmail.com")

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
(setq inhibit-compacting-font-caches t)
;; (setq doom-font (font-spec :family "Cascadia Code" :size 16))
(setq doom-font (font-spec :family "Fira Code" :size 16))
(setq doom-variable-pitch-font (font-spec :family "ETBembo" :size 18))
;; (setq doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Personal/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#X600 . #X6ff) "Vazir") frame))
(set-fontset-font t '(#X600 . #X6ff) "Vazir")

(defun update-faces ()
  (message "Updating faces...")
  (set-face-attribute 'link nil :bold nil)
  (set-face-attribute 'button nil :bold nil)
  (set-face-attribute 'font-lock-function-name-face nil :bold nil)
  (set-face-attribute 'font-lock-constant-face nil :bold nil)
  (set-face-attribute 'font-lock-keyword-face nil :bold nil))

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
  (solaire-mode-reset)
  ;; (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (doom-themes-enable-org-fontification)
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (update-faces))

(add-hook 'doom-load-theme-hook #'tweak-ui)
;; (tweak-ui)

(use-package! composite
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

;; (use-package! all-the-icons-ivy-rich
;;   :after (all-the-icons ivy)
;;   :init (all-the-icons-ivy-rich-mode 1)
;;   :config (setq all-the-icons-ivy-rich-icon-size 0.9))

;; (use-package! ivy-rich
;;   :after (ivy all-the-icons-ivy-rich-mode)
;;   :init (ivy-rich-mode 1)
;;   :config
;;   (setq-default ivy-rich-path-style 'abbreviate)
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; (use-package! ivy-posframe
;;   :after (ivy ivy-rich)
;;   :config
;;   (setq ivy-posframe-border-width 10)
;;   (setq ivy-posframe-display-functions-alist
;;         '(;; (complete-symbol               . ivy-posframe-display-at-point)
;;           (swiper                        . ivy-posframe-display-at-window-center)
;;           (t                             . ivy-posframe-display-at-frame-top-center)))
;;   (setq ivy-posframe-parameters
;;         '((left-fringe . 0)
;;           (right-fringe . 0)))
;;   (setq ivy-posframe-width 120)
;;   (ivy-posframe-mode +1))

(after! ivy-posframe
  (setq ivy-posframe-border-width 10)
  (setq ivy-posframe-display-functions-alist
        '(;; (complete-symbol               . ivy-posframe-display-at-point)
          (swiper                        . ivy-posframe-display-at-window-center)
          (t                             . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 0)
          (right-fringe . 0)))
  (setq ivy-posframe-width 120))

(use-package! which-key-posframe
  :after (which-key posframe)
  :config
  (setq which-key-posframe-border-width 1)
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (setq which-key-posframe-parameters
        '((left-fringe . 0)
          (right-fringe . 0)
          (internal-border-width . 1)
          ))
  ;; (setq which-key-posframe-min-height 1)
  ;; (setq which-key-posframe-min-width 200)
  ;; (setq which-key-posframe-width 200)
  (which-key-posframe-mode +1))

(use-package! centaur-tabs
  :defer t
  :config
  (setq centaur-tabs-set-bar 'under
        centaur-tabs-style "bar"
        centaur-tabs-close-button "" ;; 🅇
        centaur-tabs-height 28
        centaur-tabs-set-icons t
        ;; centaur-tabs-plain-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker ""
        centaur-tabs-cycle-scope 'tabs)
  ;; (setq centaur-tabs--buffer-show-groups t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode +1)
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

;; Org Mode
(add-hook 'writeroom-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'org-mode-hook #'writeroom-mode)
(add-hook 'org-mode-hook #'+org-pretty-mode)
(add-hook 'org-mode-hook #'prettify-symbols-mode)
(use-package! org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :init
  (map! :leader
        :prefix "n"
        "c" #'org-capture)
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading)
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                         ("#+END_SRC" . "†")
                                         ("#+begin_src" . "†")
                                         ("#+end_src" . "†")))
  (setq org-src-window-setup 'current-window
        org-return-follows-link t
        org-babel-load-languages '((emacs-lisp . t)
                                   (dot . t)
                                   (R . t))
        org-confirm-babel-evaluate nil
        org-hide-emphasis-markers nil
        org-fontify-done-headline t
        org-hide-leading-stars t
        org-pretty-entities t
        org-odd-levels-only t
        org-use-speed-commands t
        org-catch-invisible-edits 'show
        org-preview-latex-image-directory "/tmp/ltximg/"
        org-structure-template-alist '(("a" . "export ascii")
                                       ("c" . "center")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "export latex")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("v" . "verse")
                                       ("el" . "src emacs-lisp")
                                       ("d" . "definition")
                                       ("t" . "theorem")))
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'proselint 'org-mode)))

(require 'org-capture)
(require 'org-protocol)
(add-to-list 'org-modules 'org-protocol)
;; (use-package! org-protocol
;;   :config
;;   (pushnew! org-modules 'org-protocol))

(after! org
  (defun asad/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))

  (setq asad/org-agenda-directory "~/Dropbox/Personal/gtd/")
  (message "Populating agenda files...")
  (require 'find-lisp)
  (setq org-agenda-files
        (find-lisp-find-files asad/org-agenda-directory "\.org$")))

;; (after! 'org-capture
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
  ;; )

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-ellipsis "  ⤵")
(setq org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
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


(defvar asad/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun asad/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (asad/bulk-process-entries))

(defvar asad/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun asad/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " asad/org-current-effort) nil nil asad/org-current-effort)))
  (setq asad/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil asad/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun asad/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'asad/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun asad/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'asad/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun asad/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(setq org-agenda-bulk-custom-functions `((,asad/org-agenda-bulk-process-key asad/org-agenda-process-inbox-item)))

(map! :map org-agenda-mode-map
      "i" #'org-agenda-clock-in
      "r" #'asad/org-process-inbox
      "R" #'org-agenda-refile
      "c" #'asad/org-inbox-capture)

(defun asad/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO"
  (org-todo "NEXT"))

(add-hook 'org-clock-in-hook 'asad/set-todo-state-next 'append)

(use-package! org-clock-convenience
  :defer t
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("o" . org-clock-convenience-fill-gap)
              ("e" . org-clock-convenience-fill-gap-both)))

(use-package! org-agenda
  :defer t
  :init
  (map! "<f1>" #'asad/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode +1)
  (defun asad/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  :config
  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands `((" " "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "To Refile")
                                              (org-agenda-files '(,(concat asad/org-agenda-directory "inbox.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Emails")
                                              (org-agenda-files '(,(concat asad/org-agenda-directory "emails.org")))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files '(,(concat asad/org-agenda-directory "someday.org")
                                                                  ,(concat asad/org-agenda-directory "projects.org")
                                                                  ,(concat asad/org-agenda-directory "next.org")))
                                              ))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Projects")
                                              (org-agenda-files '(,(concat asad/org-agenda-directory "projects.org")))
                                              ))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files '(,(concat asad/org-agenda-directory "next.org")))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))

(use-package! el-patch
  :defer t)
(eval-when-compile
  (require 'el-patch))

(add-hook 'deft-mode-hook #'hl-line-mode)
(add-hook 'deft-mode-hook #'display-line-numbers-mode)
(use-package! deft
  :after (org el-patch)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "/home/asad/Dropbox/Personal/org/")
  :config/el-patch
  (defun deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (el-patch-swap (if deft-use-filename-as-title
                       (deft-base-filename file)
                     (let ((begin (string-match "^.+$" contents)))
                       (if begin
                           (funcall deft-parse-title-function
                                    (substring contents begin (match-end 0))))))
                   (org-roam--get-title-or-slug file))))

(use-package! org-roam
  :defer t
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert)
  (setq org-roam-directory "/home/asad/Dropbox/Personal/org/"
        org-roam-db-location "/home/asad/org-roam.db")
  :config
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
- source :: ${ref}"
           :unnarrowed t))))

;; (use-package! company-org-roam
;;   :when (featurep! :completion company)
;;   :after org-roam
;;   :config
;;   (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(after! org-roam
  (message "Setting org links up...")
  (defun asad/org-roam--backlinks-list (file)
    (if (org-roam--org-roam-file-p file)
        (--reduce-from
         (concat acc (format "- [[file:%s][%s]]\n"
                             (file-relative-name (car it) org-roam-directory)
                             (org-roam--get-title-or-slug (car it))))
         "" (org-roam-sql [:select [file-from]
                                   :from file-links
                                   :where (= file-to $s1)
                                   :and file-from :not :like $s2] file "%private%"))
      ""))
  (defun asad/org-export-preprocessor (_backend)
    (let ((links (asad/org-roam--backlinks-list (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n" links))))))
  (add-hook 'org-export-before-processing-hook 'asad/org-export-preprocessor))

(after! org
  (message "Setting ox-hugo up...")
  (require 'ox-hugo)
  (defun asad/conditional-hugo-enable ()
    (save-excursion
      (if (cdr (assoc "SETUPFILE" (org-roam--extract-global-props '("SETUPFILE"))))
          (org-hugo-auto-export-mode +1)
        (org-hugo-auto-export-mode -1))))
  (add-hook 'org-mode-hook #'asad/conditional-hugo-enable))

(use-package! org-journal
  :defer t
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "private-%Y-%m-%d.org"
        org-journal-dir "/home/asad/Dropbox/Personal/org/"
        org-journal-carryover-items nil
        org-journal-date-format "%Y-%m-%d")
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(use-package! org-download
  :defer t
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-dnd-base64
  :init
  (map! :map org-mode-map
        "s-Y" #'org-download-screenshot
        "s-y" #'org-download-yank)
  (pushnew! dnd-protocol-alist
            '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . +org-dragndrop-download-dnd-fn)
            '("^data:" . org-download-dnd-base64))
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
  :config
  (setq org-download-screenshot-method
        (cond (IS-MAC "screencapture -i %s")
              (IS-LINUX
               (cond ((executable-find "maim")  "maim -s %s")
                     ((executable-find "scrot") "scrot -s %s")))))
  (if (memq window-system '(mac ns))
      (setq org-download-screenshot-method "screencapture -i %s")
    (setq org-download-screenshot-method "maim -s %s"))
  (setq org-download-method 'my-org-download-method))

(use-package! org-ref-ox-hugo
  :after (org org-ref ox-hugo)
  :config
  ;; (require 'org-ref-ox-hugo)
  (add-to-list 'org-ref-formatted-citation-formats
               '("md"
                 ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
                 ("inproceedings" . "${author}, *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("book" . "${author}, *${title}* (${year}), ${address}: ${publisher}.")
                 ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
                 ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
                 ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
                 ("misc" . "${author} (${year}). *${title}*. Retrieved from [${howpublished}](${howpublished}). ${note}.")
                 (nil . "${author}, *${title}* (${year})."))))

(use-package! org-gcal
  :after (org password-store)
  :commands (org-gcal-fetch)
  :config
  (setq org-gcal-client-id (password-store-get "gmail/org-gcal-client")
        org-gcal-client-secret (password-store-get "gmail/org-gcal")
        org-gcal-fetch-file-alist '(("a.gharighi@gmail.com" . "~/Dropbox/Personal/gtd/calendars/personal.org")
                                    ;; ("dckbhpq9bq13m03llerl09slgo@group.calendar.google.com" . "~/.org/gtd/calendars/lab.org")
                                    )))

(use-package! gif-screencast
  :defer t
  :bind
  ("<f12>" . gif-screencast-start-or-stop))

(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook (lambda () (toggle-word-wrap +1)))

(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(use-package! elfeed-org
  :defer t
  :init
  (setq rmh-elfeed-org-files '("/home/asad/Dropbox/Personal/org/feeds.org"))
  :config
  (elfeed-org))

(use-package! outshine
  :defer t
  :commands (outshine-mode))

;; (setq spaceline-org-clock-p t)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)

(after! which-key-posframe
  (defun asad/which-key-posframe-setup ()
    (message "Entering which-key-posframe-setup...")
    (when (and (facep 'which-key-posframe-border)
               (facep 'highlight))
      (message "Updating which-key-posframe-border...")
      (set-face-background 'which-key-posframe-border (face-background 'highlight) nil)))
  (add-hook 'which-key-posframe-mode-hook #'asad/which-key-posframe-setup)
  (add-hook 'doom-load-theme-hook #'asad/which-key-posframe-setup))
;; All languages

;; (setq lsp-prefer-capf t)
(after! lsp-mode
  (defun asad/lsp-setup ()
    (message "Entering lsp-setup...")
    (when (and (facep 'lsp-face-highlight-read)
               (facep 'hl-line))
      (message "Updating lsp faces...")
      (set-face-background 'lsp-face-highlight-read (face-background 'hl-line) nil))
    (setq lsp-enable-on-type-formatting nil
          lsp-enable-indentation nil
          lsp-imenu-show-container-name t))
  (asad/lsp-setup)
  (add-hook 'lsp-mode-hook #'asad/lsp-setup)
  (add-hook 'doom-load-theme-hook #'asad/lsp-setup))

(after! lsp-ui
  (defun asad/lsp-ui-doc-setup ()
    (message "Entering lsp-ui-doc-setup...")
    (when (facep 'highlight)
      (message "Setting lsp-ui-doc-border...")
      (setq lsp-ui-doc-border (face-background 'highlight)))
    (when (and (facep 'lsp-ui-doc-header)
               (facep 'highlight))
      (message "Updating lsp-ui-doc-header...")
      (set-face-background 'lsp-ui-doc-header (face-background 'highlight) nil))
    (when (and (facep 'lsp-ui-doc-background)
               (facep 'hl-line))
      (message "Updating lsp-ui-doc-background...")
      (set-face-background 'lsp-ui-doc-background (face-background 'hl-line) nil))
    (when (and (facep 'markdown-code-face)
               (facep 'hl-line))
      (message "Updating markdown-code-face...")
      (set-face-background 'markdown-code-face (face-background 'hl-line) nil))

    (setq lsp-ui-doc-alignment 'frame
          lsp-ui-doc-delay 0.2
          lsp-ui-doc-frame-parameters '((left-fringe . 4) (right-fringe . 4) (internal-border-width . 1))
          lsp-ui-doc-header t
          lsp-ui-doc-include-signature t
          lsp-ui-doc-max-height 33
          lsp-ui-doc-max-width 120
          lsp-ui-doc-position 'top))
  ;; (setq lsp-document-sync-method 'full)
  (asad/lsp-ui-doc-setup)
  (add-hook 'lsp-ui-doc-mode-hook #'asad/lsp-ui-doc-setup)
  (add-hook 'doom-load-theme-hook #'asad/lsp-ui-doc-setup)
  (lsp-ui-doc-mode +1))

;; C-C++
(after! 'projectile
  (setq projectile-require-project-root t))

(after! 'cmake-ide
  (setq cmake-ide-build-dir "./build")
  (setq cmake-ide-header-search-other-file nil)
  (setq cmake-ide-header-search-first-including nil))

(after! 'ccls
  (setq ccls-extra-init-params '(:index (:comments 2) :completion (:detailedLabel t)))
  (setq ccls-sem-highlight-method nil))
;; (add-hook 'WHATEVER-MODE-HOOK ((set-face-attribute 'ccls-sem-static-face nil :bold nil)))
;; (modern-c++-font-lock-global-mode t)

(after! 'gdb-mi
  (setq gdb-many-windows t
        gdb-show-main t))

;; Haskell
;; (require 'lsp-haskell)
(after! 'lsp-haskell
  ;; (add-hook 'haskell-mode-hook (lambda ()  (setq tab-width 2)))
  (setq haskell-process-type 'cabal-new-repl)
  (setq lsp-haskell-process-args-hie '("-d" "-l" "/home/asad/.temp/hie.log")))
;; (setq lsp-haskell-process-path-hie "cabal")
;; (setq lsp-haskell-process-args-hie '("new-exec" "ghcide" "--" "--lsp"))
;; (setq lsp-haskell-process-wrapper-function (lambda (argv) (cons (car argv) (cddr argv))))

