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

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence (kbd "jk")
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-minimum-prefix-length 2
                      auto-completion-idle-delay 0.2
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-use-company-box t
                      auto-completion-enable-sort-by-usage t)
     better-defaults
     (chinese :variables
              chinese-conv-backend "cconv"
              chinese-enable-youdao-dict t)
     ;; emacs-lisp
     ;;helm
     ;; lsp
     (compleseus :variables
                 compleseus-engine 'vertico
                 compleseus-consult-preview-keys '("M-." "C-SPC" :debounce 0.5 "<up>" "<down>")
                 )
     markdown
     ;;multiple-cursors
     (org :variables
          org-enable-notifications t
          org-start-notification-daemon-on-startup t
          org-enable-reveal-js-support t
          org-enable-transclusion-support t
          org-enable-roam-support t
          org-enable-roam-ui t
          org-enable-valign t
          org-enable-appear-support t
          )
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     pdf
     syntax-checking
     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk t
          git-enable-magit-todos-plugin t)
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     (unicode-fonts)
     treemacs
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      (pyim-greatdict :location (recipe :fetcher github :repo "tumashu/pyim-greatdict")) ; Large dictionary
                                      org-daily-reflection
                                      org-drill
                                      deft
vulpea
   )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "nerd-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light). A theme from external
   ;; package can be defined with `:package', or a theme can be defined with
   ;; `:location' to download the theme package, refer the themes section in
   ;; DOCUMENTATION.org for the full theme specifications.
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 12.0
                               :weight normal
                               :width normal)

   ;; Default icons font, it can be `all-the-icons' or `nerd-icons'.
   dotspacemacs-default-icons-font 'all-the-icons

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
   ;; (default "C-M-m" for terminal mode, "M-<return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "M-<return>" "C-M-m")

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
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

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

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
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
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

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

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup `trailing

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line

(setq evil-want-keybinding nil)

  ;; remove startup auto-evilification messages:
  (with-eval-after-load 'org-agenda
    (dolist (key '("f" "C-n" "G" "\\" "|")) (define-key org-agenda-mode-map (kbd key) nil))
    (evil-define-key 'evilified org-agenda-mode-map (kbd "fc") #'org-agenda-filter-by-category)
    (evil-define-key 'evilified org-agenda-mode-map (kbd "fd") #'org-agenda-filter-remove-all)
    (evil-define-key 'evilified org-agenda-mode-map (kbd "fe") #'org-agenda-filter-by-effort)
    (evil-define-key 'evilified org-agenda-mode-map (kbd "fh") #'org-agenda-filter-by-top-headline)
    (evil-define-key 'evilified org-agenda-mode-map (kbd "ft") #'org-agenda-filter-by-tag)
    (evil-define-key 'evilified org-agenda-mode-map (kbd "fx") #'org-agenda-filter-by-regexp)
    )
  )


(defun dotspacemacs/user-config ()
  "用户自定义配置函数。
  此函数在 Spacemacs 启动的最后阶段，即层配置完成后调用。
  在此处放置你的配置代码，但那些需要在包加载前设置的变量除外。"

  ;;------------------------------------------------------------------
  ;;  通用设置
  ;;------------------------------------------------------------------
  ;; 设置连续按下 'fd' 等同于 'ESC'，用于快速退出编辑模式。
  (setq evil-escape-key-sequence "fd")

  ;; 全局启用视觉换行和单词换行，使文本在窗口边缘自动换行，并尽量保持单词完整。
  (global-visual-line-mode 1)
  (global-word-wrap-whitespace-mode 1)

  ;;------------------------------------------------------------------
  ;;  Org Mode 与 Org-roam 配置
  ;;------------------------------------------------------------------
  ;; 指定 Org 文件的根目录和 Org-roam 笔记的存储目录。
  ;; 注意：`org-directory` 通常应在 Org 包加载前设置，此处是重申。
  (setq org-directory "~/org/")
  (setq org-roam-directory (file-truename "~/org/roam/"))
  (setq org-daily-reflection-dailies-directory "~/org/roam/daily")

  ;; 自动创建 Org 和 Org-roam 目录（如果不存在），并提示用户。
  (unless (file-directory-p org-directory)
    (make-directory org-directory t)
    (message "Org 目录 '%s' 已创建。" org-directory))

  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t)
    (message "Org-roam 目录 '%s' 已创建。" org-roam-directory))

  ;; 确保 'roam-agenda' 标签不会被子标题继承，这是 Org Agenda 的重要设置。
  (add-to-list 'org-tags-exclude-from-inheritance "roam-agenda")

  ;; Org-roam UI 配置
  ;; 启用 Org-roam UI 的相关功能，如全局补全、主题同步、跟随当前笔记等。
  (setq org-roam-completion-everywhere t)
  (use-package websocket :after org-roam) ; Org-roam UI 的依赖
  (use-package org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

  ;; Org-ql 搜索配置：确保搜索目录递归。
  (setq org-ql-search-directories-files-recursive t)

  ;; Org-drill 修复：调整时间戳格式，确保与 Org-mode 兼容。
  (with-eval-after-load 'org-drill
    (defun org-drill-time-to-inactive-org-timestamp (time)
      "将 TIME 转换为 Org-mode 时间戳格式。"
      (format-time-string (concat "[" (cdr org-time-stamp-formats) "]") time)))

  ;; --- Vulpea 包及其动态 Agenda 配置 ---
  ;; 使用 `use-package` 管理 Vulpea，并配置其核心功能。
  (use-package vulpea
    :config
    ;; 辅助函数：判断当前 buffer 是否为 Org-roam 笔记。
    (defun vulpea-buffer-p ()
      "如果当前访问的 buffer 是 Org-roam 笔记，则返回非 nil。"
      (and buffer-file-name
           (string-prefix-p
            (expand-file-name (file-name-as-directory org-roam-directory))
            (file-name-directory buffer-file-name))))

    ;; 改进版函数：判断当前 buffer 是否包含活跃的 TODO 或未来的时间戳。
    ;; 忽略已完成/取消的 TODO 和过去的时间戳。
    (defun vulpea-has-active-entry-p ()
      "如果当前 buffer 包含任何未完成的 TODO 或未来的时间戳，则返回非 nil。"
      (save-excursion
        (goto-char (point-min))
        (let ((has-active-todo nil)
              (has-future-timestamp nil))
          (org-element-map (org-element-parse-buffer 'headline) 'headline
            (lambda (headline)
              (let* ((todo-type (org-element-property :todo-type headline))
                     (todo-state (org-element-property :todo-keyword headline))
                     (timestamps (org-element-property :timestamps headline)))
                ;; 检查活跃的 TODO 状态
                (when (and todo-type
                           (not (member todo-state org-done-keywords)))
                  (setq has-active-todo t))
                ;; 检查未来 DEADLINE 或 SCHEDULED 时间戳
                (when timestamps
                  (dolist (ts timestamps)
                    (when (member (org-element-property :type ts) '(deadline scheduled))
                      (let* ((date-list (org-element-property :date ts))
                             (time (when date-list (apply #'encode-time date-list)))
                             (in-future (and time (time-less-p (current-time) (org-clear-time time)))))
                        (when in-future
                          (setq has-future-timestamp t)))))))))
          (or has-active-todo has-future-timestamp))))

    ;; 更新笔记文件标签的函数：根据活跃条目添加或移除 'roam-agenda' 标签。
    (defun vulpea-project-update-tag (&optional arg)
      "根据当前 buffer 是否包含活跃条目，更新 'roam-agenda' 标签。"
      (interactive "P")
      (when (and (not (active-minibuffer-window))
                 (vulpea-buffer-p))
        (save-excursion
          (goto-char (point-min))
          (let* ((tags (vulpea-buffer-tags-get))
                 (original-tags tags))
            (if (vulpea-has-active-entry-p)
                (setq tags (cons "roam-agenda" tags))
              (setq tags (remove "roam-agenda" tags)))
            (setq tags (seq-uniq tags))
            ;; 如果标签有变化，则更新文件的 `#+filetags:` 属性
            (when (or (seq-difference tags original-tags)
                      (seq-difference original-tags tags))
              (apply #'vulpea-buffer-tags-set tags))))))

    ;; 钩子：在保存 Org 文件前自动调用 `vulpea-project-update-tag` 更新标签。
    (add-hook 'before-save-hook #'vulpea-project-update-tag))

  ;; Org-roam 动态 agenda 辅助函数：用于从 Org-roam 数据库中获取文件。
  (defun my/org-roam-filter-by-tag (tag-name)
    "返回一个 lambda 函数，用于通过 TAG-NAME 过滤 Org-roam 节点。"
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (defun my/org-roam-list-notes-by-tag (tag-name)
    "列出带有 TAG-NAME 的 Org-roam 笔记文件。"
    (mapcar #'org-roam-node-file
            (seq-filter (my/org-roam-filter-by-tag tag-name)
                        (org-roam-node-list))))

  ;; 过滤 `org-agenda-files` 的 advice：将带有 `roam-agenda` 标签的文件添加到 Agenda 列表。
  (defun dynamic-agenda-files-advice (orig-val)
    "动态添加带有 'roam-agenda' 标签的 Org-roam 笔记到 `org-agenda-files`。"
    (let ((roam-agenda-files (delete-dups (my/org-roam-list-notes-by-tag "roam-agenda"))))
      (cl-union orig-val roam-agenda-files :test #'equal)))

  ;; 钩子和 advice：将动态文件列表注入 `org-agenda-files` 的关键步骤。
  (advice-add 'org-agenda-files :filter-return #'dynamic-agenda-files-advice)

  ;;------------------------------------------------------------------
  ;;  编码设置 (尤其适用于 Windows 环境)
  ;;------------------------------------------------------------------
  ;; 设置语言环境为 UTF-8。
  (set-language-environment "UTF-8")
  ;; 将 ripgrep 进程的编码设置为 UTF-8 输入和 GBK-DOS 输出，用于解决 Windows 下中文搜索乱码。
  (add-to-list 'process-coding-system-alist
               '("[rR][gG]" . (utf-8 . gbk-dos)))
  ;; 默认文件编码为 UTF-8 (Unix 风格换行符)。
  (setq-default buffer-file-coding-system 'utf-8-unix)
  ;; 优先使用 Unicode 字符集。
  (set-charset-priority 'unicode)
  ;; 优先使用 UTF-8 编码系统。
  (prefer-coding-system 'utf-8)
  ;; 设置系统时间区域为 "C" (通用语言环境)，避免某些中文显示问题。
  (setq system-time-locale "C")

  ;; 解决 `find note` (Projectile) 出现文件名乱码的问题。
  ;; 通过 advice 强制 Projectile 以 UTF-8 解码外部命令的输出。
  (defun projectile-files-via-ext-command@decode-utf-8 (root command)
    "Override `projectile-files-via-ext-command' 以 UTF-8 解码 shell 输出。"
    (when (stringp command)
      (let ((default-directory root))
        (with-temp-buffer
          (shell-command command t "*projectile-files-errors*")
          (decode-coding-region (point-min) (point-max) 'utf-8) ;; 关键的解码行
          (let ((shell-output (buffer-substring (point-min) (point-max))))
            (split-string (string-trim shell-output) "\0" t))))))

  (advice-add 'projectile-files-via-ext-command
              :override 'projectile-files-via-ext-command@decode-utf-8)

  ;;------------------------------------------------------------------
  ;;  Deft 笔记管理配置
  ;;------------------------------------------------------------------
  ;; Deft 笔记目录和文件类型设置。
  (setq deft-directory "~/org/")
  (setq deft-recursive t)      ; 递归扫描子目录
  (setq deft-extensions '("txt" "md" "org")) ; 支持的文件扩展名
  (setq deft-parse-org t)      ; 启用 Org 文件解析

  ;; 优化 Deft 搜索结果显示：优先使用 Org 文件的 #+TITLE: 作为标题。
  (defun cm/deft-parse-title (file contents)
    "解析 FILE 和 CONTENTS，确定笔记标题。优先使用 #+TITLE:，否则使用文件名。"
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)

  ;; 配置 Deft 摘要排除的正则表达式，以更好地显示笔记内容。
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; 空行或制表符
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; Org-mode 元数据
                "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n" ;; Org-mode PROPERTIES 抽屉
                "\\)"))

  ;;------------------------------------------------------------------
  ;;  输入法 (PyIM) 配置
  ;;------------------------------------------------------------------
  (use-package pyim
    :init
    ;; 确保 PyIM 在需要时加载并激活为默认输入法。
    (setq default-input-method "pyim")
    :config
    ;; 加载 PyIM 相关模块。
    (require 'pyim-greatdict)
    (require 'pyim-cregexp-utils)
    (require 'pyim-cstring-utils)

    ;; 设置默认的 PyIM 方案为微软双拼。
    (pyim-default-scheme 'microsoft-shuangpin)

    ;; 启用基础词库和大词库。
    (pyim-basedict-enable)
    (pyim-greatdict-enable)

    ;; 仅启用百度云词库，谷歌云词库保持注释状态。
    (setq pyim-cloudim 'baidu)
    ;;(setq pyim-cloudim 'google) ;; 谷歌云词库已注释

    ;; 启用词库缓存。
    (require 'pyim-dregcache)
    (setq pyim-dcache-backend 'pyim-dregcache)

    ;; 在 Evil Insert 模式下绑定 'SPC o j' 键以切换输入法。
    (with-eval-after-load 'evil-maps
      (define-key evil-insert-state-map (kbd "M-m o j") #'toggle-input-method))

    ;; 使用 posframe 显示候选词界面，提升视觉效果。
    (require 'posframe)
    (setq pyim-page-tooltip 'posframe)

    ;; 取消模糊音功能。
    (setq pyim-pinyin-fuzzy-alist nil)

    ;; 启用代码搜索中的中文支持（如拼音、五笔码等）。
    (pyim-isearch-mode 1))

  ;; 让 Vertico, Selectrum 等补全框架通过 Orderless 支持拼音搜索候选项。
  (use-package orderless
    :config
    (defun my-orderless-regexp (orig-func component)
      "将 Orderless 的正则组件转换为 PyIM 可理解的正则。"
      (let ((result (funcall orig-func component)))
        (pyim-cregexp-build result)))
    (advice-add 'orderless-regexp :around #'my-orderless-regexp))

  ;; PyIM 翻页和 Minibuffer 显示样式优化。
  (with-eval-after-load 'pyim-page
    ;; 使用 '.' 和 ',' 键进行翻页。
    (define-key pyim-mode-map "." 'pyim-page-next-page)
    (define-key pyim-mode-map "," 'pyim-page-previous-page)

    ;; 确保 PyIM 在 Minibuffer 中显示时，候选词显示在下一行。
    (defun my-pyim-page-info-format-minibuffer-advice (original-function style page-info)
      "修改 `pyim-page-info-format`，使 Minibuffer 中的候选词分行显示。"
      (if (eq style 'minibuffer)
          ;; 自定义格式：拼音在第一行，候选词在第二行
          (format "%s%s:\n%s(%s/%s)"
                  (pyim-page-preview-create
                   (plist-get page-info :scheme))
                  (if (plist-get page-info :assistant-enable) " (辅)" "")
                  (pyim-page-menu-create
                   (plist-get page-info :candidates)
                   (plist-get page-info :position)
                   nil
                   (plist-get page-info :hightlight-current))
                  (plist-get page-info :current-page)
                  (plist-get page-info :total-page))
        ;; 其他样式则调用原始函数
        (funcall original-function style page-info)))

    ;; 将自定义的 advice 添加到 `pyim-page-info-format` 函数上。
    (advice-add 'pyim-page-info-format :around #'my-pyim-page-info-format-minibuffer-advice)

    ;; 可选：如果两行显示后 Minibuffer 高度不够，可以尝试增加 Minibuffer 的最大高度。
    ;; (setq max-mini-window-height 5) ; 根据需要调整此值
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
 '(line-number-mode t)
 '(org-agenda-files '("~/org/todo.org"))
 '(package-selected-packages
   '(ace-jump-helm-line ace-link aggressive-indent all-the-icons auto-compile
                        auto-highlight-symbol auto-yasnippet browse-at-remote
                        centered-cursor-mode clean-aindent-mode closql
                        code-review column-enforce-mode company-quickhelp
                        company-statistics consult-notes consult-org-roam
                        define-word devdocs diff-hl diminish dired-quick-sort
                        disable-mouse dotenv-mode drag-stuff dumb-jump
                        edit-indirect elisp-def elisp-demos elisp-slime-nav
                        emacsql emr eval-sexp-fu evil-anzu evil-args
                        evil-cleverparens evil-collection evil-easymotion
                        evil-escape evil-evilified-state evil-exchange
                        evil-goggles evil-iedit-state evil-indent-plus evil-lion
                        evil-lisp-state evil-matchit evil-mc evil-multiedit
                        evil-nerd-commenter evil-numbers evil-org evil-surround
                        evil-textobj-line evil-tutor evil-unimpaired
                        evil-visual-mark-mode evil-visualstar expand-region
                        eyebrowse fancy-battery flycheck-elsa flycheck-package
                        flycheck-pos-tip gh-md git-link git-messenger git-modes
                        git-timemachine gitignore-templates gnuplot golden-ratio
                        google-translate helm-ag helm-c-yasnippet helm-comint
                        helm-company helm-descbinds helm-git-grep helm-ls-git
                        helm-make helm-mode-manager helm-org helm-org-rifle
                        helm-projectile helm-purpose helm-swoop helm-themes
                        helm-xref hide-comnt highlight-indentation
                        highlight-numbers highlight-parentheses hl-todo
                        holy-mode htmlize hungry-delete hybrid-mode indent-guide
                        info+ inspector link-hint lorem-ipsum macrostep magit
                        markdown-toc multi-line mwim nameless open-junk-file
                        org-agenda-files-track org-cliplink org-contrib
                        org-download org-drill org-mime org-modern org-pomodoro
                        org-present org-projectile org-ql org-rich-yank
                        org-roam-ql org-super-agenda org-superstar orgit
                        overseer page-break-lines paradox password-generator
                        pcre2el popwin pyim quickrun rainbow-delimiters
                        restart-emacs smeargle space-doc spaceline
                        spacemacs-purpose-popwin spacemacs-whitespace-cleanup
                        string-edit-at-point string-inflection symbol-overlay
                        symon term-cursor toc-org transient treemacs-evil
                        treemacs-icons-dired treemacs-magit treemacs-persp
                        treemacs-projectile undo-fu undo-fu-session unfill
                        uuidgen vi-tilde-fringe volatile-highlights vundo wgrep
                        winum with-editor writeroom-mode ws-butler
                        yasnippet-snippets))
 '(safe-local-variable-values
   '((eval require 'magit-utils nil t) (toc-org-max-depth . 2)
     (org-hide-macro-markers . t) (buffer-file-coding-system . utf-8-unix)
     (eval auto-fill-mode t) (eval require 'ox-texinfo+ nil t)
     (eval require 'ol-info) (org-src-preserve-indentation . t)
     (org-src-preserve-indentation) (eval require 'ol-man nil t)
     (eval require 'magit-base nil t) (eval require 'org-make-toc)
     (eval when (featurep 'toc-org) (toc-org-mode)) (org-list-indent-offset . 1)
     (toc-org-max-depth . 4)))
 '(warning-suppress-log-types
   '((package reinitialization) (package reinitialization)
     (files missing-lexbind-cookie
            "~/.emacs.d/elpa/pyim-greatdict-20170725.62510/pyim-greatdict.el")
     (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
