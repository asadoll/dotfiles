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
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; example of useful layers you may want to use right away.
     ;; uncomment some layer names and press `spc f e r' (vim style) or
     ;; `m-m f e r' (emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; ansible
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
            c-c++-lsp-enable-semantic-highlight t
            c-c++-lsp-semantic-highlight-method 'overlay
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
     ;; dart
     dash
     (debug :variables debug-additional-debuggers '("node-inspect"))
     deft
     ;; django
     docker
     ;; elixir
     emacs-lisp
     emoji
     erc
     ;; erlang
     ;; ess
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     ;; fira-code-ligatures
     fsharp
     git
     github
     ;; go
     ;; gpu
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
     helpful
     html
     imenu-list
     ;; ipython-notebook
     (ivy :variables
          ivy-enable-advanced-buffer-information t)
     (javascript :variables
                 javascript-backend 'lsp
                 node-add-modules-path t)
     json
     ;; kotlin
     latex
     (lsp :variables
          lsp-prefer-capf nil
          lsp-ui-doc-enable t
          lsp-ui-sideline-enable nil)
     ;; lua
     markdown
     mu4e
     multiple-cursors
     nginx
     ;; nim
     (org :variables
          org-directory "~/Dropbox/Personal/org/"
          org-enable-github-support t
          org-enable-hugo-support t
          org-enable-org-journal-support t
          org-enable-bootstrap-support t)
     org-roam
     pandoc
     pdf
     ;; (python :variables
     ;;         python-backend 'lsp
     ;;         python-test-runner 'pytest)
     racket
     (ranger :varibales
             ranger-override-dired t
             ranger-show-preview t)
     ;; restclient
     ;; react
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
     ;; sql
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
     (typography :variables typography-enable-typographic-editing t)
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
                                      ;; (mathpix :location (recipe :fetcher github :repo "jethrokuan/mathpix.el"))
                                      ;; (org-ref-ox-hugo :location (recipe :fetcher github :repo "jethrokuan/org-ref-ox-hugo"))
                                      ;; ox-hugo
                                      all-the-icons-ivy
                                      all-the-icons-ivy-rich
                                      centaur-tabs
                                      company-posframe
                                      dashboard
                                      elfeed-org
                                      exec-path-from-shell
                                      gif-screencast
                                      ivy-posframe
                                      outshine
                                      password-store
                                      solaire-mode
                                      which-key-posframe
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
  "Initialization:
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
   dotspacemacs-emacs-pdumper-executable-file "emacs-28.0.50"

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
                                (recents . 15))

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
                                :size 16
                                :weight light)

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
   dotspacemacs-line-numbers '(:relative t) ;; nil

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
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
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
  (load (expand-file-name "config.el" dotspacemacs-directory))
  (load custom-file))
