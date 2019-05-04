;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; if non-nil then spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; if non-nil layers with lazy install support are lazy installed.
   ;; list of additional paths where to look for configuration layers.
   ;; paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; list of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; example of useful layers you may want to use right away.
     ;; uncomment some layer names and press `spc f e r' (vim style) or
     ;; `m-m f e r' (emacs style) to install them.
     ;; ----------------------------------------------------------------
     asm
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-help-tooltip t
                      :disabled-for org erc)
     ;; better-defaults
     bibtex
     (c-c++ :variables
            c-c++-backend 'lsp-ccls
            c-c++-enable-c++11 t
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-google-newline t
            c-c++-enable-google-style t)
     (cmake :variables cmake-enable-cmake-ide-support t)
     (colors :variables
             colors-colorize-identifiers 'variables)
     command-log
     common-lisp
     csharp
     csv
     dap
     dash
     (debug :variables debug-additional-debuggers '("node-inspect"))
     deft
     django
     docker
     elixir
     emacs-lisp
     erc
     erlang
     ess
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     fsharp
     git
     github
     go
     graphviz
     (gtags :varibales gtags-enable-by-default t)
     (haskell :variables
              haskell-completion-backend 'ghci
              haskell-enable-hindent-style "chris-done"
              haskell-enable-ghc-mod-support nil
              haskell-process-type 'stack-ghci
              haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans") ;; "--with-ghc=intero")
              haskell-stylish-on-save t)
     helm
     html
     imenu-list
     ipython-notebook
     ;; ivy
     (javascript :variables
                 javascript-backend 'lsp
                 node-add-modules-path t)
     kotlin
     latex
     lsp
     lua
     markdown
     multiple-cursors
     ;; neotree
     nginx
     nim
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-projectile-file "c:/users/aghar/documents/todos.org")
     ;; pandoc
     (python :variables
             python-backend 'lsp
             python-test-runner 'pytest)
     racket
     (ranger :varibales
             ranger-override-dired t
             ranger-show-preview t)
     ;; restclient
     react
     rust
     search-engine
     semantic
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     spacemacs-layouts
     speed-reading
     (spell-checking :variables spell-checking-enable-by-default nil)
     sql
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     tern
     theming
     treemacs
     twitter
     (typescript :variables typescript-backend 'lsp)
     typography
     (version-control :variables version-control-diff-tool 'diff-hl)
     vinegar
     windows-scripts
     yaml
    )

   ;; list of additional packages that will be installed without being
   ;; wrapped in a layer. if you need some configuration for these
   ;; packages, then consider creating a layer. you can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; to use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      ;; pretty-mode
                                      (lsp-haskell :location (recipe :fetcher github :repo "emacs-lsp/lsp-haskell"))
                                      (modern-cpp-font-lock :location (recipe :fetcher github :repo "ludwigpacifici/modern-cpp-font-lock" :min-version "1"))
                                      solaire-mode
                                     )

   ;; a list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; a list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(exec-path-from-shell)

   ;; defines the behaviour of spacemacs when installing packages.
   ;; possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "initialization:
this function is called at the very beginning of spacemacs startup,
before layer configuration.
it should only modify the values of spacemacs settings."
  ;; this setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; if non-nil then enable support for the portable dumper. you'll need
   ;; to compile emacs 27 from source following the instructions in file
   ;; experimental.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; file path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; name of the spacemacs dump file. this is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; to load it when starting emacs add the parameter `--dump-file'
   ;; when invoking emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; if non-nil elpa repositories are contacted via https whenever it's
   ;; possible. set it to nil if you have no way to use https in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; this variable has no effect if emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; maximum allowed time in seconds to contact an elpa repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; this is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(500000000 0.1)

   ;; if non-nil then spacelpa repository is the primary source to install
   ;; a locked version of packages. if nil then spacemacs will install the
   ;; latest version of packages from melpa. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; if non-nil then verify the signature for downloaded spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; if non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. note that checking for
   ;; new versions works via git commands, thus it calls github services
   ;; whenever you start emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; if non-nil, a form that evaluates to a package directory. for example, to
   ;; use different package directories for different emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; one of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. the value can also be a list
   ;; with `:variables' keyword (similar to layers). check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; if non-nil output loading progress in `*messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; specify the startup banner. default value is `official', it displays
   ;; the official spacemacs logo. an integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. a string value must be a path to an image format supported
   ;; by your emacs build.
   ;; if the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil ;; 'official

   ;; list of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. if nil then it is disabled.
   ;; possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; list sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; true if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; initial message in the scratch buffer, such as "welcome to spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; list of themes, the first of the list is loaded when spacemacs starts.
   ;; press `spc t n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-dracula
                         ;; doom-one
                         solarized-light
                         ;; solarized-dark
                         ;; spacemacs-dark
                         ;; spacemacs-light
                        )

   ;; set the theme for the spaceline. supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. the
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default emacs mode-line. `custom' is a user defined themes,
   ;; refer to the documentation.org for more info on how to create your own
   ;; spaceline theme. value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(all-the-icons :separator none :separator-scale 1)
   ;; dotspacemacs-mode-line-theme '(doom :separator none :separator-scale 1)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state nil

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(("Fira Code"
                               :size 20
                               :weight normal
                               :width normal)
                               ;; ("Fira Code Symbol"
                               ;; :size 18
                               ;; :weight normal
                               ;; :width normal)
                              )
   ;; dotspacemacs-default-font (let ((fonts '("Not a font!" "Ubuntu Mono" "Source Code Pro")))
   ;;                             (mapcar (lambda (font)
   ;;                                       `(,font
   ;;                                         :size 12
   ;;                                         :weight normal
   ;;                                         :width normal
   ;;                                         :powerline-scale 1.5))
   ;;                                     fonts))
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil ;; t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t ;; nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t ;; nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq inhibit-compacting-font-caches t))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Starting with a decent wide window
  (add-to-list 'default-frame-alist '(height . 35))
  (add-to-list 'default-frame-alist '(width . 140))

  (add-to-list 'exec-path "/home/asad/.local/bin")
  ;; Enabling vertical window divider
  (window-divider-mode)

  ;; Preventing ligatures from scrambling
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)

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
  ;; (setq-default truncate-lines t)

  ;; Org Mode
  (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))
  (setq spaceline-org-clock-p t)
  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (push (org-projectile:todo-files) org-agenda-files))

  ;; C-C++
  ;; (require 'ccls)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (modern-c++-font-lock-global-mode t)

  ;; Haskell
  (require 'lsp-haskell)
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  (setq lsp-haskell-process-args-hie '("--vomit" "-d" "-l" "/home/asad/.temp/hie.log"))
  (add-hook 'haskell-mode-hook #'lsp)

  ;; Rust
  (add-hook 'rust-mode-hook #'lsp)

  ;; Javascript
  (add-hook 'js2-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)

  ;; Python
  (add-hook 'python-mode-hook #'lsp)

  ;; Make fill-column visible
  ;; (setq-default fci-rule-color "#ff0000")
  ;; (setq-default fci-rule-use-dashes t)
  ;; (add-hook 'prog-mode-hook #'fci-mode)

  ;; Enable ligatures

  ;;; Kawkab Mono
  ;; This works when using emacs --daemon + emacsclient
  (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#X600 . #X6ff) "Kawkab Mono")))
  ;; This works when using emacs without server/client
  (set-fontset-font t '(#X600 . #X6ff) "Kawkab Mono")

  ;;; Fira code
  (defun fira-code-mode--make-alist (list)
    "Generate prettify-symbols alist from LIST."
    (let ((idx -1))
      (mapcar
      (lambda (s)
        (setq idx (1+ idx))
        (let* ((code (+ #Xe100 idx))
          (width (string-width s))
          (prefix ())
          (suffix '(?\s (Br . Br)))
          (n 1))
    (while (< n width)
      (setq prefix (append prefix '(?\s (Br . Bl))))
      (setq n (1+ n)))
    (cons s (append prefix suffix (list (decode-char 'ucs code))))))
      list)))

  (defconst fira-code-mode--ligatures
    '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
      "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
      "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
      "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
      ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
      "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
      "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
      "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
      ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
      "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
      "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
      "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
      "x" ":" "+" "+" "*"))

  (defvar fira-code-mode--old-prettify-alist)

  (defun fira-code-mode--enable ()
    "Enable Fira Code ligatures in current buffer."
    (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
    (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
    (prettify-symbols-mode t))

  (defun fira-code-mode--disable ()
    "Disable Fira Code ligatures in current buffer."
    (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
    (prettify-symbols-mode -1))

  (define-minor-mode fira-code-mode
    "Fira Code ligatures minor mode"
    :lighter " FiraCode"
    (setq-local prettify-symbols-unprettify-at-point 'right-edge)
    (if fira-code-mode
        (fira-code-mode--enable)
      (fira-code-mode--disable)))

  (defun fira-code-mode--setup ()
    "Setup Fira Code Symbols"
    (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
    (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

  (fira-code-mode--setup)
  (add-hook 'prog-mode-hook 'fira-code-mode)

  ;; Make current window shine
  (use-package solaire-mode
    :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
    :config
    (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
    (solaire-mode-swap-bg)
  )
)
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
 '(package-selected-packages
   '(writeroom-mode tide dumb-jump doom-modeline ace-link counsel swiper ess helm ivy lsp-mode treemacs hydra zeal-at-point yasnippet-snippets yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights visual-fill-column vi-tilde-fringe uuidgen use-package typescript-mode treemacs-projectile treemacs-evil toml-mode toc-org tagedit symon string-inflection sql-indent spray spaceline-all-the-icons solarized-theme solaire-mode smex smeargle slim-mode shrink-path shell-pop scss-mode sass-mode restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode prettier-js powershell popwin pony-mode pippel pipenv pip-requirements pfuture persp-mode pcre2el password-generator paradox ox-twbs ox-gfm overseer orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file omnisharp ob-ipython nginx-mode nameless multi-term move-text modern-cpp-font-lock mmm-mode markdown-toc magithub magit-svn magit-gitflow macrostep lsp-ui lsp-haskell lorem-ipsum livid-mode live-py-mode link-hint julia-mode json-navigator js2-refactor js-doc ivy-yasnippet ivy-xref ivy-rtags ivy-purpose ivy-hydra insert-shebang indent-guide importmagic impatient-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-make helm-core haskell-snippets google-translate google-c-style golden-ratio gnuplot gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md ggtags fuzzy forge font-lock+ flyspell-correct-ivy flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-haskell flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu ess-R-data-view eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav eldoc-eval ein editorconfig dotenv-mode doom-themes dockerfile-mode docker disaster diminish diff-hl deft define-word cython-mode csv-mode cquery counsel-projectile counsel-gtags counsel-dash counsel-css company-web company-tern company-statistics company-shell company-rtags company-quickhelp company-lsp company-ghci company-cabal company-c-headers company-anaconda column-enforce-mode color-identifiers-mode cmm-mode cmake-mode cmake-ide clean-aindent-mode clang-format centered-cursor-mode ccls cargo browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ace-window ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zeal-at-point yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tide typescript-mode tagedit sql-indent spray spaceline powerline solaire-mode smex smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode powershell popwin pony-mode pip-requirements persp-mode pcre2el paradox ox-twbs ox-gfm orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file omnisharp nginx-mode neotree multi-term move-text modern-cpp-font-lock mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lsp-haskell lsp-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint js2-refactor multiple-cursors js-doc ivy-hydra intero insert-shebang indent-guide imenu-list hydra hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-make haskell-snippets haml-mode google-translate golden-ratio gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md ggtags fuzzy flyspell-correct-ivy flyspell-correct flycheck-rust flycheck-pos-tip flycheck-haskell flycheck flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit transient git-commit with-editor lv evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu ess-smart-equals ess-R-data-view ctable ess julia-mode eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav ein skewer-mode deferred request websocket js2-mode simple-httpd dumb-jump dockerfile-mode docker json-mode tablist magit-popup docker-tramp json-snatcher json-reformat disaster diminish diff-hl deft define-word cython-mode csv-mode csharp-mode counsel-projectile projectile pkg-info epl counsel-dash helm-dash helm helm-core counsel swiper ivy company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-quickhelp pos-tip company-ghci company-ghc ghc haskell-mode company-cabal company-c-headers company-anaconda company column-enforce-mode color-identifiers-mode coffee-mode cmm-mode cmake-mode clean-aindent-mode clang-format cargo markdown-mode rust-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed async anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link avy ac-ispell auto-complete popup doom-dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
