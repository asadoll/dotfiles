;;; package --- Summary
;;; Commentary:
;;; Code

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(treemacs-root-face ((t (:inherit (variable-pitch font-lock-string-face) :weight normal :height 1.1)))))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(package-selected-packages
   '(all-the-icons-ivy-rich zenburn-theme zen-and-art-theme zeal-at-point yasnippet-snippets yaml-mode xterm-color x86-lookup ws-butler writeroom-mode winum white-sand-theme which-key-posframe wgrep web-mode web-beautify vterm volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme typo twittering-mode twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-evil toxi-theme toml-mode toc-org tide terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection spray spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme solaire-mode soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slime-company slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme restart-emacs rebecca-theme realgud ranger rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme racket-mode racer purple-haze-theme pug-mode professional-theme prettier-js powershell popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el password-store password-generator paradox pandoc-mode ox-twbs ox-pandoc ox-hugo ox-gfm overseer outshine orgit organic-green-theme org-roam org-ref org-projectile org-present org-pomodoro org-mime org-journal org-download org-cliplink org-bullets org-brain open-junk-file omtose-phellack-theme omnisharp oldlace-theme occidental-theme obsidian-theme nodejs-repl noctilux-theme nginx-mode nasm-mode naquadah-theme nameless mustang-theme multi-term mu4e-maildirs-extension mu4e-alert move-text monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-svn magit-section magit-gitflow madhat2r-theme lush-theme lsp-ui lsp-haskell lorem-ipsum livid-mode link-hint light-soap-theme kaolin-themes json-navigator js2-refactor js-doc jbeans-theme jazz-theme ivy-yasnippet ivy-xref ivy-rtags ivy-rich ivy-purpose ivy-posframe ivy-hydra ir-black-theme intero insert-shebang inkpot-theme indent-guide impatient-mode hybrid-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helpful helm-make hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme graphviz-dot-mode grandshell-theme gotham-theme google-translate google-c-style golden-ratio gnuplot gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gif-screencast gh-md gandalf-theme fuzzy fsharp-mode forge font-lock+ flyspell-correct-ivy flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-haskell flycheck-elsa flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-snipe evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks engine-mode emojify emoji-cheat-sheet-plus emmet-mode elisp-slime-nav elfeed-org editorconfig dumb-jump dracula-theme dotenv-mode doom-themes doom-modeline dockerfile-mode docker django-theme disaster diminish diff-hl devdocs deft define-word dashboard darktooth-theme darkokai-theme darkmine-theme darkburn-theme dap-mode dante dakrone-theme cyberpunk-theme csv-mode cquery cpp-auto-include counsel-projectile counsel-dash counsel-css company-ycmd company-web company-tern company-statistics company-shell company-rtags company-reftex company-quickhelp company-posframe company-lsp company-ghci company-ghc company-emoji company-cabal company-c-headers company-box company-auctex common-lisp-snippets command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode cmm-mode cmake-mode cmake-ide clues-theme clean-aindent-mode clang-format chocolate-theme cherry-blossom-theme centered-cursor-mode centaur-tabs ccls cargo busybee-theme bubbleberry-theme browse-at-remote birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile attrap apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme all-the-icons-ivy alect-themes aggressive-indent afternoon-theme add-node-modules-path ace-link ac-ispell))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((eval asad/conditional-hugo-enable)
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(treemacs-root-face ((t (:inherit (variable-pitch font-lock-string-face) :weight normal :height 1.1)))))
