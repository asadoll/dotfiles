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

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; example of useful layers you may want to use right away.
     ;; uncomment some layer names and press `spc f e r' (vim style) or
     ;; `m-m f e r' (emacs style) to install them.
     ;; ----------------------------------------------------------------
     ansible
     asm
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-return-key-behavior nil
                      auto-completion-enable-help-tooltip t
                      auto-completion-use-company-box t
                      :disabled-for org erc)
     ;; better-defaults
     bibtex
     (c-c++ :variables
            c-c++-adopt-subprojects t
            c-c++-backend 'lsp-ccls
            c-c++-enable-c++11 t
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-google-newline t
            c-c++-enable-google-style t
            c-c++-lsp-args '("-v=2")
            c-c++-lsp-cache-dir ".ccls-cache"
            c-c++-lsp-extra-init-params '(:completion (:detailedLabel t) :index (:comments 2))
            c-c++-lsp-sem-highlight-method 'font-lock
            c-c++-lsp-sem-highlight-rainbow nil)
     ;; cascadia-code-ligatures
     (cmake :variables cmake-enable-cmake-ide-support t)
     (colors :variables
             colors-colorize-identifiers 'variables)
     command-log
     common-lisp
     csharp
     csv
     dap
     dart
     dash
     (debug :variables debug-additional-debuggers '("node-inspect"))
     deft
     django
     docker
     elixir
     emacs-lisp
     emoji
     erc
     erlang
     ess
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     ;; fira-code-ligatures
     fsharp
     git
     github
     go
     gpu
     graphviz
     ;; (gtags :varibales gtags-enable-by-default t)
     (haskell :variables
              haskell-completion-backend 'lsp
              haskell-enable-hindent t
              haskell-enable-hindent-style "johan-tibell"
              haskell-enable-ghc-mod-support nil
              haskell-process-type 'cabal-new-repl
              haskell-stylish-on-save t)
     ;; helm
     html
     imenu-list
     ipython-notebook
     ivy
     (javascript :variables
                 javascript-backend 'lsp
                 node-add-modules-path t)
     json
     kotlin
     latex
     (lsp :variables
          lsp-prefer-capf nil
          lsp-ui-doc-enable t
          lsp-ui-sideline-enable nil)
     lua
     markdown
     multiple-cursors
     nginx
     nim
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t)
          ;; org-projectile-file "/home/asad/Documents/agenda.org")
     pandoc
     pdf
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
     ;; semantic
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
     themes-megapack
     theming
     (treemacs :variables
               ;; treemacs-use-follow-mode 'tag
               treemacs-use-filewatch-mode t
               treemacs-use-git-mode 'deferred)
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
                                      company-posframe
                                      ivy-posframe
                                      exec-path-from-shell
                                      centaur-tabs
                                      ;; (lsp-haskell :location (recipe :fetcher github :repo "emacs-lsp/lsp-haskell"))
                                      (modern-cpp-font-lock :location (recipe :fetcher github :repo "ludwigpacifici/modern-cpp-font-lock" :min-version "1"))
                                      solaire-mode
                                     )

   ;; a list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; a list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    ;; exec-path-from-shell
                                    window-purpose)

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

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

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
   dotspacemacs-startup-lists '((projects . 7)
                                (recents . 15)
                                (todos . 5))

   ;; true if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; initial message in the scratch buffer, such as "welcome to spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; list of themes, the first of the list is loaded when spacemacs starts.
   ;; press `spc t n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         doom-solarized-dark
                         doom-manegarm
                         doom-solarized-light
                        )

   ;; set the theme for the spaceline. supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. the
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default emacs mode-line. `custom' is a user defined themes,
   ;; refer to the documentation.org for more info on how to create your own
   ;; spaceline theme. value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   ;; dotspacemacs-mode-line-theme '(all-the-icons :separator none :separator-scale 1)
   dotspacemacs-mode-line-theme '(doom :separator none :separator-scale 0.5)

   ;; if non-nil the cursor color matches the state color in gui emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(
                               ;; ("Hurmit Nerd Font Mono"
                               ;;  :size 20
                               ;;  :weight normal
                               ;;  :width normal)

                               ("Cascadia Code"
                                :size 20
                                :weight normal)

                               ;; ("icons-in-terminal"
                               ;;  :size 20
                               ;;  :weight normal
                               ;;  :width normal)

                               ;; ("kawkab mono"
                               ;; :size 14
                               ;; :weight normal
                               ;; :width normal)
                              )
   ;; the leader key (default "spc")
   dotspacemacs-leader-key "SPC"

   ;; the key used for emacs commands `M-x' (after pressing on the leader key).
   ;; (default "spc")
   dotspacemacs-emacs-command-key "SPC"

   ;; the key used for vim ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; the leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "c-m-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; these variables control whether separate commands are bound in the gui to
   ;; the key pairs `c-i', `tab' and `c-m', `ret'.
   ;; setting it to a non-nil value, allows for separate commands under `c-i'
   ;; and tab or `c-m' and `ret'.
   ;; in the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the gui. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; if non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; if non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; if non-nil, auto-generate layout name when creating new layouts. only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; size (in mb) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; location where to auto-save files. possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; if non-nil, the paste transient-state is enabled. while enabled, after you
   ;; paste something, pressing `c-j' and `c-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; which-key delay in seconds. the which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; which-key frame position. possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; control where `switch-to-buffer' displays the buffer. if nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. if non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; if non-nil a progress bar is displayed when spacemacs is loading. this
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil ;; t

   ;; if non-nil the frame is fullscreen when emacs starts up. (default nil)
   ;; (emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; if non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; use to disable fullscreen animations in osx. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; a value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 20

   ;; a value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; if non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; if non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; if non-nil unicode symbols are displayed in the mode line.
   ;; if you use emacs as a daemon and wants unicode characters only in gui set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; if non-nil smooth scrolling (native-scrolling) is enabled. smooth
   ;; scrolling overrides the default behavior of emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers t ;; nil

   ;; code folding method. possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; if non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; if non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; this can be temporary disabled by pressing `c-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; select a scope to highlight delimiters. possible values are `any',
   ;; `current', `all' or `nil'. default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; if non-nil, start an emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t ;; nil

   ;; set the emacs server socket location.
   ;; if nil, uses whatever the emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". it has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; if non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; list of search tool executable names. spacemacs uses the first installed
   ;; tool of the list. supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; format specification for setting the frame title.
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
   ;; dotspacemacs-frame-title-format "Emacs: %a"

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
  ;; (require 'rtags)
  (setq inhibit-compacting-font-caches t)
)

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
  (setq read-process-output-max (* 1024 1024))

  (add-to-list 'load-path "~/.local/share/icons-in-terminal/")
  (require 'icons-in-terminal)

  (setq doom-manegarm-darker-background t
        doom-manegarm-muted-modeline t)

  (use-package ivy-posframe
    :after ivy
    :config
    (setq ivy-posframe-border-width 10)
    (setq ivy-posframe-display-functions-alist
          '((complete-symbol               . ivy-posframe-display-at-point)
            (t                             . ivy-posframe-display-at-frame-top-center)))
    (setq ivy-posframe-parameters
          '((left-fringe . 0)
            (right-fringe . 0)))
    (ivy-posframe-mode))
  ;;   (set-face-attribute 'ivy-posframe-border nil :background "black")

  (use-package company-posframe
    :diminish company-posframe-mode
    :after company
    :config
    (company-posframe-mode))

  ;; (setq completion-styles `(basic partial-completion emacs22 initials
  ;;                                 ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))

  (setq company-box-icons-unknown 'fa_question_circle)
  (setq company-box-icons-elisp
    '((fa_tag :face font-lock-function-name-face) ;; Function
      (fa_cog :face font-lock-variable-name-face) ;; Variable
      (fa_cube :face font-lock-constant-face) ;; Feature
      (md_color_lens :face font-lock-doc-face))) ;; Face
  (setq company-box-icons-yasnippet 'fa_bookmark)
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
    :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
           (minibuffer-setup . solaire-mode-in-minibuffer))
    :config
    (solaire-global-mode)
    (solaire-mode-swap-bg))

  ;; Setup tabs
  (use-package centaur-tabs
    :demand
    :init
    :config
    (setq centaur-tabs-set-bar 'under)
    (setq centaur-tabs-style "chamfer")
    (setq centaur-tabs-close-button "") ;; 🅇
    (setq centaur-tabs-height 32)
    (setq centaur-tabs-set-icons t)
    (setq centaur-tabs-gray-out-icons 'buffer)
    (setq centaur-tabs-set-modified-marker t)
    ;; (setq centaur-tabs-modified-marker "")
    (setq centaur-tabs-cycle-scope 'tabs)
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
  (window-divider-mode)

  ;; Preventing ligatures from scrambling
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)

  (display-time-mode t)
  ;; (display-battery-mode t)
  ;; (doom-themes-treemacs-config)
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
  (with-eval-after-load 'org
    (setq org-projectile-file "/home/asad/Documents/agenda.org")
    (setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "▶" "◇" "■" "◆"))
    (setq org-todo-keywords '((sequence "+ TODO(t)" "|" " DONE(d)")
                              (sequence "- WAITING(w)" "|")
                              (sequence "|" " CANCELED(c)")))
    (setq spaceline-org-clock-p t)
    ;; (doom-themes-org-config)
    )

  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (push org-projectile-file org-agenda-files))

  ;; All languages
  ;; (add-hook 'prog-mode-hook #'lsp)

  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  ;; (setq lsp-prefer-capf t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-imenu-show-container-name t)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 23)
  ;; (setq lsp-document-sync-method 'full)

  ;; C-C++
  (setq projectile-require-project-root t)
  (setq cmake-ide-build-dir "./build")
  (setq cmake-ide-header-search-other-file nil)
  (setq cmake-ide-header-search-first-including nil)
  (setq ccls-extra-init-params '(:index (:comments 2) :completion (:detailedLabel t)))
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (modern-c++-font-lock-global-mode t)
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
  (require 'dap-node)
  (require 'dap-firefox)
  (require 'dap-chrome)

  ;; Python
  (add-hook 'python-mode-hook 'lsp)

  ;; Dart
  (add-hook 'dart-mode-hook 'lsp)

  ;;; Kawkab Mono
  ;; This works when using emacs --daemon + emacsclient
  ;; (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#X600 . #X6ff) "Kawkab Mono")))
  (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#X600 . #X6ff) "Vazir")))
  ;; This works when using emacs without server/client
  ;; (set-fontset-font t '(#X600 . #X6ff) "Kawkab Mono")
  (set-fontset-font t '(#X600 . #X6ff) "Vazir")

  ;; (add-hook 'prog-mode-hook 'fira-code-mode)
  ;; Enable ligatures
  (use-package composite
    :defer t
    :init
    (defvar composition-ligature-table (make-char-table nil))
    :hook
    (((prog-mode conf-mode nxml-mode markdown-mode help-mode
                 lsp-ui-doc-frame-mode org-mode
                 company-mode company-box-mode
                 ivy-posframe-mode company-posframe-mode
                 )
      . (lambda () (setq-local composition-function-table composition-ligature-table))))
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
      (set-char-table-parent composition-ligature-table composition-function-table))
    )
  (require 'window-purpose)
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
 '(package-selected-packages '(ansi package-build shut-up epl git commander f dash s))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(treemacs-root-face ((t (:inherit (variable-pitch font-lock-string-face) :weight normal :height 1.1)))))
)
