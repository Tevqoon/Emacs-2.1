;;; init.el --- Jure Smolar's Emacs config ;;; -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Jure Smolar's rewritten Emacs config, made to maximize power and extensibility through understanding.
;;;
;;; Using [[https://github.com/d12frosted/homebrew-emacs-plus][Emacs plus]]
;;;
;;; Build commands:
;;;
;;; brew tap d12frosted/emacs-plus
;;; brew install emacs-plus@30 --with-native-comp --with-imagemagick --with-xwidgets
;;;
;;;
;;; Code:
;;; --> Initialization
;;;
;;; -> Initialization -> Package initialization

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/") :append)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(setq use-package-always-ensure t)
;; (setq package-native-compile t)
(setq package-native-compile nil)
;; (setq package-check-signature nil)

(require 'use-package)
;; (setq use-package-vc-prefer-newest nil)

(use-package exec-path-from-shell
  :functions exec-path-from-shell-initialize
  :init (exec-path-from-shell-initialize))

;;; -> Initialization -> Basic setup
(use-package emacs
  :init
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 5)
  (column-number-mode)
  (delete-selection-mode nil) ; For lispy
  (global-auto-revert-mode 1)
  (savehist-mode +1)
  (when (eq system-type 'darwin)
    (pixel-scroll-precision-mode 1)
    (setq pixel-scroll-precision-use-momentum t))
  :custom
  (inhibit-startup-message t)
  (enable-recursive-minibuffers t)
  (frame-resize-pixelwise t)
  (cursor-type 'bar)
  (echo-keystrokes .01)
  (confirm-kill-emacs #'y-or-n-p)
  (global-auto-revert-non-file-buffers t)
  (delete-by-moving-to-trash t)
  (sentence-end-double-space nil)

  (auto-hscroll-mode t)
  (mouse-wheel-scroll-amount '(1 ((meta))
                                 ((control meta) . global-text-scale)
                                 ((control) . text-scale)))

  :bind (("C-h" . delete-backward-char)
	 ("<pinch>" . 'ignore)
	 ("<C-wheel-up>" . 'ignore)
	 ("<C-wheel-down>" . 'ignore)
	 ("C-s-l" . copy-current-line)
	 )

  :config
  ;; weird keys ;;
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "s-p"))
  (setq disabled-command-function nil)

  ;; utf-8 ;;
  (setq locale-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  (define-advice keyboard-quit
      (:around (quit) quit-current-context)
    "Quit the current context.

When there is an active minibuffer and we are not inside it close
it.  When we are inside the minibuffer use the regular
`minibuffer-keyboard-quit' which quits any active region before
exiting.  When there is no minibuffer `keyboard-quit' unless we
are defining or executing a macro."
    (if (active-minibuffer-window)
	(if (minibufferp)
            (minibuffer-keyboard-quit)
          (abort-recursive-edit))
      (unless (or defining-kbd-macro
                  executing-kbd-macro)
	(funcall-interactively quit))))

  (server-start)
  )

(use-package files
  :ensure nil
  :custom
  ;; Backup settings
  (make-backup-files t)
  (backup-inhibited nil)
  (vc-make-backup-files t) ; Even for files in git!
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions -1)
  (kept-new-versions 20)
  (kept-old-versions 10)
  (backup-directory-alist
   `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))

  ;; Auto-save settings
  (auto-save-default t)
  (auto-save-interval 50)           ; Every 50 keystrokes
  (auto-save-timeout 10)            ; Every 10 seconds
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

  ;; Create the directories if they don't exist
  :config
  (make-directory (expand-file-name "backups/" user-emacs-directory) t)
  (make-directory (expand-file-name "auto-saves/" user-emacs-directory) t)

  (setq-default backup-inhibited nil)

  ;; Auto-save visited files
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 30)  ; Save every 30 seconds
  )

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package bug-hunter)

(use-package emacs-everywhere
  :bind
  ("<f16>" . js/goto-finder)
  :config
  ;; TODO: Linux version?
  (defun js/goto-finder ()
    (interactive)
    (shell-command "open .")))

(use-package backup-walker
  :vc (:url "https://github.com/lewang/backup-walker")
  :commands backup-walker-start
  :init
  (defalias 'string-to-int 'string-to-number)  ; removed in 26.1
  (defalias 'display-buffer-other-window 'display-buffer))


;;; -> Initialization -> Browser Integration

(use-package browser-setup
  :no-require t
  :ensure nil
  :if (eq system-type 'darwin)
  :init
  (defun browse-url-safari (url &optional _new-window)
    "Open URL in Safari."
    (interactive (browse-url-interactive-arg "URL: "))
    (shell-command-to-string (format "open -a Safari %s" (shell-quote-argument url))))
  :custom
  (browse-url-browser-function 'browse-url-safari)
  (browse-url-secondary-browser-function 'browse-url-firefox)

  (browse-url-handlers
   '(("\\`https?://\\(?:www\\.\\)?youtube\\.com" . browse-url-firefox)
     ("\\`https?://\\(?:www\\.\\)?youtu\\.be" . browse-url-firefox))
   )
  )

;;; -> Initialization -> Misc

;;; -> Initialization -> Misc -> Persistent Variables Utility
(use-package persistent-vars
  :ensure nil
  :commands (dump-vars-to-file dump-closing-variables)
  :defines (closing-variables)
  :hook (kill-emacs . dump-closing-variables) ;; Fixed typo here
  :init
  (defvar closing-variables nil
    "Variables to dump to a file upon closing emacs.")
  (defvar closing-variables-filename "~/.emacs.d/variables.el"
    "File path where persistent variables will be saved when Emacs closes.
This file stores variables specified in `closing-variables` to maintain state
between Emacs sessions.")

  (defun dump-vars-to-file (varlist filename)
    "Simplistic dumping of variables in VARLIST to a file FILENAME"
    (save-excursion
      (let ((buf (find-file-noselect filename t)))
	(with-current-buffer buf
          (erase-buffer)
          (dump-varlist varlist buf)
          (save-buffer)
          (kill-buffer)))))
  (defun dump-varlist (varlist buffer)
    "Insert into buffer the setq statement to recreate the variables in VARLIST"
    (mapc (lambda (var)
            (print (list 'setq var (list 'quote (symbol-value var)))
                   buffer))
          varlist))

  ;; Create directory if it doesn't exist
  (make-directory (file-name-directory closing-variables-filename) t)

  ;; Ensure the file exists
  (unless (file-exists-p closing-variables-filename)
    (with-temp-file closing-variables-filename
      (insert ";; Persistent variables\n")))

  ;; Load existing variables - with error handling
  (condition-case nil
      (load closing-variables-filename t t t)
    (error (message "Could not load %s, creating new file" closing-variables-filename)))

  (defun dump-closing-variables ()
    "Writes all of the variables in the list closing-variables to the file closing-variables-filename"
    (interactive)
    (dump-vars-to-file closing-variables closing-variables-filename))
  )

;;; --> Projects
(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :custom
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (project-find-regexp "Find regexp" ?g)
     (project-dired "Dired" ?d)
     (project-eshell "Eshell" ?e)
     (magit-project-status "Magit" ?m)
     (agent-shell "Agent shell" ?a)))
  (project-vc-extra-root-markers
   '(".dir-locals.el" "flake.nix" "package.json" "Cargo.toml" "pyproject.toml"))
  (project-kill-buffers-display-buffer-list t)
  :config
  (project-forget-zombie-projects)

  :hook (find-file . (lambda ()
		       (when-let ((pr (project-current)))
			 (project-remember-project pr))))
  )

;;; --> Look and feel

(use-package doom-themes
  :functions my/apply-theme
  :init
  ;; Theme definitions
  (defvar my/light-theme 'doom-solarized-light)
  (defvar my/dark-theme 'doom-solarized-dark)

  (defvar my/redshift? nil)
  (defvar my/light-theme-redshift 'modus-operandi)
  (defvar my/dark-theme-redshift 'modus-vivendi)

  (defun my/apply-theme (appearance)
    "Load theme based on appearance and redshift? status."
    (interactive)
    (mapc #'disable-theme custom-enabled-themes)
    (pcase `(,appearance ,my/redshift?)
      ('(light nil) (load-theme my/light-theme t))
      ('(dark nil)  (load-theme my/dark-theme t))
      ('(light t)   (load-theme my/light-theme-redshift t))
      ('(dark t)    (load-theme my/dark-theme-redshift t))))
  (my/apply-theme 'light)

  (defun toggle-redshift ()
    (interactive)
    (setq my/redshift? (not my/redshift?))
    (message "Retoggled redshift")))

(use-package nerd-icons
  :defer t)

(use-package doom-modeline
  :functions doom-modeline-mode
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 24)
  (doom-modeline-icon t)
  :config
  (if (eq system-type 'gnu/linux)
      (setq doom-modeline-height 12)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.01))

(use-package paren
  :init
  (electric-pair-mode 1)
  (show-paren-mode 1)
  :config
  (defun js/fix-angle-bracket-syntax ()
    "Make < and > punctuation instead of paired delimiters."
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?> "."))
  :custom
  (show-paren-delay 0)
  :hook
  (LaTeX-mode . js/fix-angle-bracket-syntax)
  (org-mode . js/fix-angle-bracket-syntax))

(use-package surround
  :ensure t
  :bind-keymap ("M-'" . surround-keymap))

(use-package rainbow-delimiters
  :defer t
  :hook prog-mode)

(use-package alert
  :defer nil
  :config
  (if (eq system-type 'darwin)
      (setq
       ;; alert-default-style 'notifier
       alert-default-style 'osx-notifier
       ))
  ;; (alert "This is an alert" :severity 'high)
  ;; (alert "This is an alert" :title "My Alert" :category 'debug)
  )

(use-package helpful
  :bind (("<f1> f" . helpful-callable)
         ("<f1> v" . helpful-variable)
         ("<f1> k" . helpful-key)
         :map help-map
         ("p" . helpful-at-point)
	 :map helpful-mode-map
	 ("q" . quit-window--and-kill))
  :custom
  (helpful-switch-buffer-function #'switch-to-buffer))

(use-package devdocs
  :bind
  (("<f1> D" . devdocs-lookup)))

;;; -> Look and feel -> Tabs, frames, windows

(use-package tab-bar
  :custom
  (tab-bar-new-tab-choice 'scratch-buffer)
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-last-tab-choice 'delete-frame)
  (tab-bar-auto-width nil)
  (tab-bar-select-tab-modifiers '(hyper))

  :bind (("s-t" . tab-bar-new-tab)
	 ("s-T" . tab-undo)
	 ("s-w" . tab-close)
	 ("C-s-f" . toggle-frame-fullscreen)
	 ("s-o" . other-window)
	 ("M-s-d" . tab-bar-duplicate-tab))
  :config
  (tab-bar-mode 1))

(use-package visual-fill-column
  :defines
  visual-fill-column-width
  visual-fill-column-center-text
  :hook
  (visual-fill-column-mode . efs/org-mode-visual-fill)
  (text-mode . visual-line-mode)
  (visual-line-mode . visual-wrap-prefix-mode)
  text-mode
  :config
  (defun efs/org-mode-visual-fill ()
    "Sets the width just so that there's a little bit
     of space on the left and right."
    (interactive)
    (setq visual-fill-column-width 160)
    (setq visual-fill-column-center-text t)
    )
  )

;; Also includes config from
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(use-package ace-window
  :defines aw-dispatch-alist
  :functions aw-switch-to-window
  :bind
  ([remap other-window] . ace-window)
  :custom
  (aw-scope 'frame)
  (aw-background nil)
  (switch-to-buffer-obey-display-actions t))

(use-package transpose-frame
  :bind ("s-i" . transpose-frame))

(use-package symbol-overlay
  :hook prog-mode)

;; Keeps windows still when opening minibuffers
(use-package sinister
  :vc (:url "https://github.com/positron-solutions/sinister")
  :config
  (sinister-stillness-mode 1)
  )

(use-package winpulse
  :vc (:url "https://github.com/xenodium/winpulse"
	    :rev :newest)
  :config
  (winpulse-mode +1))

;;; -> OS specific configuration

(pcase system-type
  ;;; MacOS Config
  ('darwin
   (setq ns-alternate-modifier 'meta)
   (setq ns-right-alternate-modifier 'meta)
   (setq ns-right-control-modifier 'control)
   (setq ns-control-modifier 'control)
   (define-key key-translation-map (kbd "ESC") (kbd "C-g")) ; works in gui only

   (setq ns-control-modifier 'control)
   (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

   (setq ns-pop-up-frames nil)

   (defvar org-roam-directory "~/Documents/org")
   ;;; MacOS Autocommit:
   ;;; launchctl load ~/Library/LaunchAgents/com.jure.org-git-autocommit.plist

   (defvar org-directory "~/Documents/org")
   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
   (setq ring-bell-function 'ignore)
   (global-set-key (kbd "C-s-<tab>") #'ns-next-frame)
   (defvar yt-dlp-folder "~/Movies/Youtube"
     "Main directory for downloading videos using yt-dlp.")
   ;; Macos Registers ;;
   (set-register ?r '(file . "~/.emacs.d/init.el"))
   (set-register ?t `(file . ,(concat org-directory "/tasks.org")))
   (set-register ?j `(file . ,(concat org-directory "/journals/Journelly.org")))
   (set-register ?p `(file . ,(concat org-directory "/20250823160311-software.org")))

   ;;; https://www.reddit.com/r/emacs/comments/1qlnde7/comment/o1fq5lj/
   (defun start-process@use-pipe (fn &rest args)
     ;; checkdoc-params: (fn args)
     "Advice to ensure that `start-process' uses a pipe rather than
a pty for the compilation command. This increases performance on OSX
by a factor of 10, as the default pty size is a pitiful 1024 bytes."
     (let ((process-connection-type nil))
       (apply fn args)))



;;; NOTE: Might work as well: https://github.com/d12frosted/homebrew-emacs-plus/tree/master/community/patches/aggressive-read-buffering

   )
  ;;; Android configuration
  ('android
   (defvar org-roam-directory "~/Documents/org")
   (defvar org-directory "~/Documents/org")
   (defvar yt-dlp-folder "~/Movies/Youtube"
     "Main directory for downloading videos using yt-dlp.")
   (defvar elfeed-db-directory "~/Documents/elfeed")
   ;; Android Registers ;;
   (set-register ?r '(file . "~/.emacs.d/init.el"))
   (set-register ?t `(file . ,(concat org-directory "/tasks.org")))

   (my/apply-theme 'dark)
   ;; Bars ;;
   (menu-bar-mode -1)

   (tool-bar-mode 1)
   (setq tool-bar-position 'bottom)
   (setq tool-bar-button-margin 26)
   (setq touch-screen-word-select t)
   (setq touch-screen-extend-selection t)
   (setq touch-screen-display-keyboard nil)
   (setq touch-screen-enable-hscroll nil)

   (defun android-display-keyboard ()
     "Displays the Android on-screen keyboard for the current frame."
     (interactive)
     (android-toggle-on-screen-keyboard (selected-frame) nil))

   (tool-bar-add-item
    "spell" ; icon
    'android-display-keyboard  ; function
    'android-keyboard              ; property
    :help "Display Android keyboard")
   )

  ('gnu/linux
   (menu-bar-mode -1)
   (scroll-bar-mode -1)
   (setq ring-bell-function 'ignore)

   (define-key key-translation-map (kbd "ESC") (kbd "C-g")) ; works in gui only
   (global-set-key (kbd "s-k") #'kill-current-buffer)
   (global-set-key (kbd "A-k") #'kill-current-buffer)
   (global-set-key (kbd "s-s") #'save-buffer)
   (global-set-key (kbd "s-z") #'undo)
   (global-set-key (kbd "s-v") #'yank)
   (global-set-key (kbd "s-a") #'mark-whole-buffer)
   (global-set-key (kbd "s-c") #'kill-ring-save)

   (defvar org-roam-directory "~/org")
   (defvar org-directory "~/org")


   (set-register ?r '(file . "~/.emacs.d/init.el"))
   (set-register ?t `(file . ,(concat org-directory "/tasks.org")))
   (set-register ?p `(file . ,(concat org-directory "/journals/Journelly.org")))
   (set-register ?l '(file . "~/.stumpwm.d/init.lisp")) ;; StumpWM config
   (set-register ?n '(file . "/etc/nixos/configuration.nix")) ;; StumpWM config
   )

  (_ (display-warning 'os "Unhandled operating system %s" system-type :warning))
  )

;;; -> Look and feel -> Fonts

;; ===================================
;; Common Font Configuration (All Platforms)
;; ===================================

;; Define platform-specific fixed-pitch font
(defvar fixed-pitch-font
  (pcase system-type
    ('darwin "Menlo")
    ('android "JetBrains Mono")
    ('gnu/linux "JetBrains Mono")
    (t "Monospace")))

;; Define variable-pitch font - same across platforms
(defvar variable-pitch-font "Brygada 1918")
(defvar org-heading-font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1")

;; ===================================
;; Platform-Specific Font Configuration
;; ===================================

(pcase system-type
  ;; ===================================
  ;; macOS Configuration
  ;; ===================================
  ('darwin
   ;; Define fallback fonts for macOS
   (defvar fallback-font-families
     '(("Apple Color Emoji" . 140)            ; Apple Emojis
       ("Noto Sans" . 140)                    ; General Unicode coverage
       ("Noto Sans Egyptian Hieroglyphs" . 140) ; Hieroglyphs
       ("Noto Sans Cuneiform" . 140)          ; Cuneiform
       ("Noto Sans Linear B" . 140)           ; Linear B
       ("Noto Sans Old Persian" . 140)        ; Old Persian
       ("Noto Sans Phoenician" . 140)         ; Phoenician
       ("Noto Sans Brahmi" . 140)             ; Brahmi
       ("Noto Sans Gothic" . 140)             ; Gothic
       ("Noto Sans Old Turkic" . 140)         ; Old Turkic
       ("Noto Sans Imperial Aramaic" . 140)   ; Imperial Aramaic
       ("Symbola" . 140)))                    ; General symbol fallback

   ;; Set up emoji and special character fonts
   (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
   (set-fontset-font t '(#x1F300 . #x1FAD6) "Apple Color Emoji") ;; Emoji range
   (set-fontset-font t '(#x1F600 . #x1F64F) "Apple Color Emoji") ;; Emoticons
   (set-fontset-font t '(#x1F900 . #x1F9FF) "Apple Color Emoji") ;; Supplemental Symbols and Pictographs
   (set-fontset-font t '(#x2600 . #x26FF) "Apple Color Emoji")   ;; Miscellaneous Symbols

   ;; Create the fontset with correct XLFD syntax
   (create-fontset-from-fontset-spec
    (concat
     "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-fontset-unicode,"
     (mapconcat
      (lambda (font-spec)
        (format "%s:-*-%s-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1"
                (car font-spec)
                (car font-spec)))
      fallback-font-families
      ",")))

   (custom-set-faces
    `(fixed-pitch ((t (:family ,fixed-pitch-font :height 140))))
    '(org-block ((t (:inherit fixed-pitch))))
    '(org-code ((t (:inherit (shadow fixed-pitch)))))
    '(org-document-info ((t (:foreground "dark orange"))))
    '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
    `(org-document-title ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.5 :underline nil))))
    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
    `(org-level-1 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.5))))
    `(org-level-2 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.25))))
    `(org-level-3 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.1))))
    `(org-level-4 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.1))))
    `(org-level-5 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    `(org-level-6 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    `(org-level-7 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    `(org-level-8 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    '(org-link ((t (:foreground "royal blue" :underline t))))
    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
    '(org-property-value ((t (:inherit fixed-pitch))))
    '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
    '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
    '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
    `(variable-pitch ((t (:family ,variable-pitch-font :height 155)))))

   ;; macOS-specific mixed-pitch setup
   (use-package mixed-pitch
     :init
     (set-face-attribute 'default nil :height 140)
     (set-face-attribute 'default nil :family fixed-pitch-font :height 140)
     (set-face-attribute 'variable-pitch nil :family variable-pitch-font :height 160)
     ;; Add fallback fonts using set-fontset-font
     (dolist (font fallback-font-families)
       (set-fontset-font t nil (font-spec :family (car font)) nil 'append))
     :custom
     (mixed-pitch-set-height 160)
     :hook text-mode))

  ;; ===================================
  ;; android Configuration
  ;; ===================================
  ('android
   (defvar fallback-font-families
     '(("Symbola" . 140)                    ; Symbol coverage
       ("Noto Color Emoji" . 140)))         ; Emoji support

   ;; Set up symbol ranges to use Symbola
   (set-fontset-font t 'symbol "Symbola" nil 'prepend)
   (set-fontset-font t '(#x2600 . #x27BF) "Symbola")  ;; Misc Symbols range
   (set-fontset-font t '(#x2500 . #x259F) "Symbola")  ;; Box Drawing range

   ;; Add fallback fonts
   (dolist (font fallback-font-families)
     (set-fontset-font t nil (font-spec :family (car font)) nil 'append))

   (custom-set-faces
    `(fixed-pitch ((t (:family ,fixed-pitch-font))))
    '(org-block ((t (:inherit fixed-pitch))))
    '(org-code ((t (:inherit (shadow fixed-pitch)))))
    '(org-document-info ((t (:foreground "dark orange"))))
    '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
    `(org-document-title ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.5 :underline nil))))
    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
    `(org-level-1 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.5))))
    `(org-level-2 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.25))))
    `(org-level-3 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.1))))
    `(org-level-4 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.1))))
    `(org-level-5 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    `(org-level-6 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    `(org-level-7 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    `(org-level-8 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    '(org-link ((t (:foreground "royal blue" :underline t))))
    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
    '(org-property-value ((t (:inherit fixed-pitch))))
    '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
    '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
    '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
    `(variable-pitch ((t (:family ,variable-pitch-font)))))

   ;; Android-specific mixed-pitch setup
   (use-package mixed-pitch
     :init
     (set-face-attribute 'default nil :family fixed-pitch-font)
     (set-face-attribute 'variable-pitch nil :family variable-pitch-font)
     :hook text-mode))

  ;; ===================================
  ;; Linux Font Configuration
  ;; ===================================
  ('gnu/linux
   (defvar fallback-font-families
     '(("Symbola" . 140)                    ; Symbol coverage
       ("Noto Color Emoji" . 140)))         ; Emoji support

   ;; Set up symbol ranges to use Symbola
   (set-fontset-font t 'symbol "Symbola" nil 'prepend)
   (set-fontset-font t '(#x2600 . #x27BF) "Symbola")  ;; Misc Symbols range
   (set-fontset-font t '(#x2500 . #x259F) "Symbola")  ;; Box Drawing range

   ;; Add fallback fonts
   (dolist (font fallback-font-families)
     (set-fontset-font t nil (font-spec :family (car font)) nil 'append))

   (custom-set-faces
    `(fixed-pitch ((t (:family ,fixed-pitch-font))))
    '(org-block ((t (:inherit fixed-pitch))))
    '(org-code ((t (:inherit (shadow fixed-pitch)))))
    '(org-document-info ((t (:foreground "dark orange"))))
    '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
    `(org-document-title ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.5 :underline nil))))
    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
    `(org-level-1 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.5))))
    `(org-level-2 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.25))))
    `(org-level-3 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.1))))
    `(org-level-4 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.1))))
    `(org-level-5 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    `(org-level-6 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    `(org-level-7 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    `(org-level-8 ((t (:inherit default :weight bold :foreground "#556b72" :font ,org-heading-font :height 1.0))))
    '(org-link ((t (:foreground "royal blue" :underline t))))
    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
    '(org-property-value ((t (:inherit fixed-pitch))))
    '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
    '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
    '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
    `(variable-pitch ((t (:family ,variable-pitch-font)))))

   ;; Linux-specific mixed-pitch setup
   (use-package mixed-pitch
     :init
     (set-face-attribute 'default nil :family fixed-pitch-font)
     (set-face-attribute 'variable-pitch nil :family variable-pitch-font)
     :hook text-mode
     )
   (set-face-attribute 'default nil :height 165))

  )

;;; --> Searching and navigation

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (isearch-allow-scroll t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " [%s/%s]")
  :bind
  ("M-s i" . isearch-forward)
  )

(use-package counsel
  :defines
  ivy-minibuffer-map
  ivy-re-builders-alist
  swiper-use-visual-line-p
  :functions
  ivy-mode
  counsel-mode
  :defer 0.1
  :custom
  (ivy-use-virtual-buffers nil)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  ;; (ivy-dynamic-exhibit-delay-ms 250)
  :bind (("s-b" . counsel-switch-buffer)
	 ("C-s" . swiper)
	 ("M-s s" . swiper-isearch)
	 ("M-s a" . swiper-all)
	 :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1)
  (counsel-mode)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq swiper-use-visual-line-p #'ignore)

  (defun js/ivy-org-node-mtime-compare (a b)
    "Compare org-node candidates A and B by file mtime, most recent first."
    (let* ((node-a (gethash a org-node--candidate<>entry))
           (node-b (gethash b org-node--candidate<>entry))
           (mtime-a (and node-a (ignore-errors (org-mem-file-mtime node-a))))
           (mtime-b (and node-b (ignore-errors (org-mem-file-mtime node-b)))))
      (cond ((null mtime-a) nil)
            ((null mtime-b) t)
            (t (time-less-p mtime-b mtime-a)))))

  (add-to-list 'ivy-sort-functions-alist
               '(org-node-collection . js/ivy-org-node-mtime-compare))
  )

(use-package ibuffer
  :functions ibuffer-switch-to-saved-filter-groups
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-saved-filter-groups
   (quote (("default"
	    ("dired" (mode . dired-mode))
	    ("org" (mode . org-mode))
	    ("magit" (name . "^magit"))
	    ("planner" (or
			(name . "^\\*Calendar\\*$")
			(name . "^\\*Org Agenda\\*")))
	    ("emacs" (or
		      (name . "^\\*scratch\\*$")
		      (name . "^\\*Messages\\*$")))))))
  :config
  (defun ibuffer-switch-to-default ()
    (ibuffer-switch-to-saved-filter-groups "default"))
  :hook
  (ibuffer-mode . ibuffer-switch-to-default)
  (ibuffer-mode . ibuffer-auto-mode)
  )

(use-package ivy-rich
  :functions ivy-rich-mode
  :after ivy
  :config (ivy-rich-mode 1))

(use-package marginalia
  :functions marginalia-mode
  :init (marginalia-mode))

(use-package company
  :hook (prog-mode text-mode))

(use-package company-box
  :hook company-mode)

(use-package hydra)

(use-package smerge-mode
  :bind
  (:map smerge-mode-map
	("C-c ^ ^" . unpackaged/smerge-hydra/body))
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue)))

;; (use-package expand-region
;;   :bind ("s-f" . er/expand-region))

(use-package expreg
  :custom
  (expreg-restore-point-on-quit t)
  :bind ("s-f" . expreg-expand)

  :config

  ;; -- helpers -------------------------------------------------------------

  (defun js/expreg--paragraph ()
    "Paragraph expansion without text-mode gate."
    (ignore-errors
      (save-excursion
	(backward-paragraph)
	(skip-syntax-forward "-")
	(let ((beg (point)))
          (forward-paragraph)
          `((paragraph . ,(cons beg (point))))))))

  (defun js/expreg--org-block ()
    "Select contents then full org block."
    (when (derived-mode-p 'org-mode)
      (ignore-errors
	(save-excursion
          (let* ((block-begin
                  (save-excursion
                    (re-search-backward "^[ \t]*#\\+begin[_:]" nil t)
                    (point)))
		 (block-end
                  (save-excursion
                    (goto-char block-begin)
                    (re-search-forward "^[ \t]*#\\+end[_:].*$" nil t)
                    (point)))
		 (inner-beg
                  (save-excursion
                    (goto-char block-begin)
                    (end-of-line) (forward-char 1)
                    (point)))
		 (inner-end
                  (save-excursion
                    (goto-char block-end)
                    (re-search-backward "^[ \t]*#\\+end[_:]")
                    (point)))
		 (preamble-beg
                  (save-excursion
                    (goto-char block-begin)
                    (while (and (not (bobp))
				(progn (forward-line -1)
                                       (looking-at "^[ \t]*#\\+"))))
                    (point))))
            (when (and block-begin block-end
                       (<= block-begin (point))
                       (<= (point) block-end))
              `((org-block-inner   . ,(cons inner-beg inner-end))
		(org-block         . ,(cons block-begin block-end))
		(org-block-preamble . ,(cons preamble-beg block-end)))))))))

  (defun js/expreg--org-heading ()
    "Expand through org structure: heading body → subtree → parent subtree → ...
Produces multiple regions so expreg can step through them."
    (when (derived-mode-p 'org-mode)
      (ignore-errors
	(let (regions)
          (save-excursion
            ;; 1. Body of current heading (heading line to first child/end, no subheadings)
            (org-back-to-heading t)
            (let ((heading-start (point))
                  (body-end (save-excursion
                              (outline-next-heading)
                              (point))))
              ;; body = from after the heading line to the first subheading
              (forward-line 1)
              (let ((body-start (point)))
		(when (< body-start body-end)
                  (push `(org-heading-body . ,(cons body-start body-end)) regions)))
              ;; 2. Full subtree at this heading
              (goto-char heading-start)
              (let ((sub-beg heading-start)
                    (sub-end (save-excursion
                               (org-end-of-subtree t t)
                               (point))))
		(push `(org-subtree . ,(cons sub-beg sub-end)) regions))
              ;; 3. Walk up to each ancestor, collecting their subtrees
              (goto-char heading-start)
              (while (org-up-heading-safe)
		(let ((parent-beg (point))
                      (parent-end (save-excursion
                                    (org-end-of-subtree t t)
                                    (point))))
                  (push `(org-subtree-parent . ,(cons parent-beg parent-end)) regions)))))
          (nreverse regions)))))

  (defun js/expreg--latex ()
    "Expand through LaTeX constructs."
    (ignore-errors
      (let (regions)
	;; --- inline fragments via org-element-context (org-mode only) ---
	(when (derived-mode-p 'org-mode)
          (let* ((ctx (org-element-context))
		 (type (org-element-type ctx)))
            (when (eq type 'latex-fragment)
              (let* ((beg (org-element-property :begin ctx))
                     (end (save-excursion
                            (goto-char (org-element-property :end ctx))
                            (skip-chars-backward " \t")
                            (point)))
                     (inner-beg
                      (save-excursion
			(goto-char beg)
			(cond ((looking-at "\\\\\\[") (+ beg 2))
                              ((looking-at "\\\\(")   (+ beg 2))
                              ((looking-at "\\$\\$")  (+ beg 2))
                              ((looking-at "\\$")     (+ beg 1))
                              (t beg))))
                     (inner-end
                      (save-excursion
			(goto-char end)
			(cond ((looking-back "\\\\\\]" 3) (- end 2))
                              ((looking-back "\\\\)" 3)   (- end 2))
                              ((looking-back "\\$\\$" 3)  (- end 2))
                              ((looking-back "\\$" 2)     (- end 1))
                              (t end)))))
		(when (< inner-beg inner-end)
                  (push `(latex-fragment-inner . ,(cons inner-beg inner-end)) regions))
		(push `(latex-fragment . ,(cons beg end)) regions)))))

	;; --- \begin{env}...\end{env} ---
	(let* ((env-begin
		(or
		 ;; 1. point is somewhere on a \begin{...} line
		 (save-excursion
		   (goto-char (line-beginning-position))
		   (when (looking-at org-element--latex-begin-environment)
		     (cons (point) (match-string 1))))
		 ;; 2. point is inside the environment body
		 (save-excursion
		   (when (re-search-backward
			  org-element--latex-begin-environment nil t)
		     (cons (point) (match-string 1))))))
	       (env-name  (cdr env-begin))
	       (env-start (car env-begin)))
	  (when (and env-begin env-name)
	    (save-excursion
	      (goto-char env-start)
	      (when (re-search-forward
		     (format "\\\\end{%s}" (regexp-quote env-name)) nil t)
		(let* ((full-end  (point))
		       (full-beg  env-start)
		       (inner-beg (save-excursion
				    (goto-char full-beg)
				    (end-of-line) (forward-char 1)
				    (point)))
		       (inner-end (save-excursion
				    (re-search-backward
				     (format "\\\\end{%s}"
					     (regexp-quote env-name)))
				    (point))))
		  (when (< inner-beg inner-end)
		    (push `(latex-env-inner . ,(cons inner-beg inner-end)) regions))
		  (push `(latex-env . ,(cons full-beg full-end)) regions))))))

	(nreverse regions))))
  ;; -- wiring --------------------------------------------------------------

  (defun js/expreg-setup-text ()
    (add-to-list 'expreg-functions #'expreg--sentence t)
    (add-to-list 'expreg-functions #'js/expreg--paragraph t))

  (defun js/expreg-setup-org ()
    (add-to-list 'expreg-functions #'expreg--sentence t)
    (add-to-list 'expreg-functions #'js/expreg--paragraph t)
    (add-to-list 'expreg-functions #'js/expreg--latex t)
    (add-to-list 'expreg-functions #'js/expreg--org-block t)
    (add-to-list 'expreg-functions #'js/expreg--org-heading t))

  (defun js/expreg-setup-elfeed ()
    (add-to-list 'expreg-functions #'expreg--sentence t)
    (add-to-list 'expreg-functions #'js/expreg--paragraph t))

  (defun js/expreg-setup-latex ()
    (add-to-list 'expreg-functions #'expreg--sentence t)
    (add-to-list 'expreg-functions #'js/expreg--paragraph t)
    (add-to-list 'expreg-functions #'js/expreg--latex t))


  :hook
  (text-mode      . js/expreg-setup-text)
  (org-mode       . js/expreg-setup-org)   ; org derives text-mode, both fire — ok, add-to-list dedupes
  (latex-mode       . js/expreg-setup-latex)
  (LaTeX-mode       . js/expreg-setup-latex)   ; AUCTeX uses capital-L
  (elfeed-show-mode . js/expreg-setup-elfeed))

(use-package phi-search)

(use-package multiple-cursors
  :defines mc/keymap
  :defer nil
  :bind (("s-<down>" . mc/mark-next-like-this)
	 ("s-<up>" . mc/mark-previous-like-this)
	 ("s-M-<up>" . mc/unmark-next-like-this)
	 ("s-M-<down>" . mc/unmark-previous-like-this)
	 ("s-d" . mc/mark-all-dwim)
	 ("s-r" . mc/edit-lines)
	 ("s-<mouse-1>" . mc/add-cursor-on-click)
	 :map mc/keymap
	 ("<return>" . nil))
  )

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package imenu-list
  :bind
  ("C-c n h" . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (use-package-enable-imenu-support t)
  :hook (org-mode . (lambda () (imenu-add-to-menubar "Imenu")))
  )

(use-package yasnippet
  :functions yas-global-mode
  :init (yas-global-mode 1)
  :custom
  (yas-fallback-behavior 'return-nil)
  :config
  (defun js/inside-prooftree-p ()
    "Return non-nil if point is inside a prooftree or bprooftree environment."
    (let ((pos (point)))
      (or
       (save-excursion
	 (and (re-search-backward "\\\\begin{prooftree}" nil t)
              (re-search-forward "\\\\end{prooftree}" nil t)
              (> (point) pos)))
       (save-excursion
	 (and (re-search-backward "\\\\begin{bprooftree}" nil t)
              (re-search-forward "\\\\end{bprooftree}" nil t)
              (> (point) pos))))))

  (defun js/inside-logicproof-p ()
    "Return non-nil if point is inside a logicproof environment."
    (save-excursion
      (let ((pos (point)))
	(and (re-search-backward "\\\\begin{\\(logicproof\\|subproof\\)}" nil t)
             (let ((begin-pos (point)))
               (goto-char pos)
               (not (re-search-backward "\\\\end{logicproof}" begin-pos t)))))))
  )

(use-package undo-fu
  :functions
  undo-fu-only-undo
  undo-fu-only-redo
  :bind (("s-z" . undo-fu-only-undo)
	 ("s-Z" . undo-fu-only-redo)))

(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :bind
  ("C-x u" . vundo))

(use-package deadgrep
  :defines
  deadgrep-mode-map
  org-roam-dailies
  s-directory
  deadgrep-project-root-function
  org-roam-directory
  :functions
  deadgrep-search-directory
  :after org-roam
  :hook (deadgrep-finished . my/deadgrep-activate-org-links)
  :bind (("<f5>" . deadgrep)
	 ("C-c n e d" . deadgrep-search-org-roam-dailies)
	 ("C-c n e n" . deadgrep-search-org-roam)
	 :map deadgrep-mode-map
	 ("q" . quit-window--and-kill)
	 ("M-o" . ace-link-org)
	 ())
  :config
  (defun deadgrep-search-directory (dir)
    "Search a specific directory using deadgrep."
    (let ((deadgrep-project-root-function
	   (lambda () dir)))
      (call-interactively #'deadgrep)))

  (defun deadgrep-search-org-roam ()
    "Search all org-roam files."
    (interactive)
    (deadgrep-search-directory org-roam-directory))

  (defun deadgrep-search-org-roam-dailies ()
    "Search only org-roam daily journal entries."
    (interactive)
    (deadgrep-search-directory
     (expand-file-name org-roam-dailies-directory org-roam-directory)))

  (defun my/deadgrep-activate-org-links ()
    "Activate Org links in deadgrep results buffer."
    (interactive)
    (when (eq major-mode 'deadgrep-mode)
      (let ((inhibit-read-only t))
	;; Make sure org is loaded
	(require 'org)

	;; Find and process links
	(save-excursion
          (goto-char (point-min))
          (while (re-search-forward org-link-bracket-re nil t)
            (let* ((start (match-beginning 0))
                   (end (match-end 0))
                   (link-text (buffer-substring-no-properties start end))
                   (path (match-string-no-properties 1))
                   (desc (or (match-string-no-properties 2) path))
                   (overlay (make-overlay start end)))

              ;; Apply the link face
              (overlay-put overlay 'face 'org-link)
              (overlay-put overlay 'mouse-face 'highlight)

              ;; Make it clickable using org's machinery
              (overlay-put overlay 'help-echo
                           (concat "Link: " path "\nMouse-1: Open link"))
              (overlay-put overlay 'keymap
                           (let ((map (make-sparse-keymap)))
                             (define-key map [mouse-1]
					 (lambda ()
					   (interactive)
					   (save-excursion
					     (org-link-open-from-string link-text))))
                             (define-key map (kbd "RET")
					 (lambda ()
					   (interactive)
					   (save-excursion
					     (org-link-open-from-string link-text))))
                             map))

              ;; Only display the description if different from path
              (when (and desc (not (string= desc path)))
		(overlay-put overlay 'display desc))))))))
  )

(use-package wgrep
  :ensure t)

(use-package flash
  :commands (flash-jump flash-jump-continue
			flash-treesitter)
  :bind ("s-j" . flash-jump)
  :custom
  (flash-multi-window t)
  (flash-rainbow t)
  (flash-rainbow-shade 8)
  (flash-autojump t))

(use-package ace-link
  :functions ace-link-setup-default
  :hook
  (prog-mode . goto-address-prog-mode)
  :bind (("s-u" . counsel-ace-link)
         :map prog-mode-map
         ("M-o" . ace-link-addr)

         )
  :init
  (ace-link-setup-default)
  :config
  (defun ace-link-safari ()
    "Ace jump to safari link with C-u prefix by default."
    (interactive)
    (let ((current-prefix-arg '(4)))  ; Simulate C-u
      (call-interactively 'ace-link-eww)))

  (defun my/ace-link-org-agenda-urls ()
    "Open visible links (not just agenda items) in the Org Agenda buffer."
    (interactive)
    (let (link-candidates pt)
      ;; Collect visible links in the agenda buffer
      (save-excursion
	(goto-char (window-start))
	(while (re-search-forward org-link-any-re (window-end) t)
          (push (cons (match-string-no-properties 0)
                      (match-beginning 0))
		link-candidates)))

      ;; Use avy to let the user choose a link
      (setq pt (avy-with custom-ace-link-org-agenda
		 (avy-process
                  (mapcar #'cdr (nreverse link-candidates))
                  (avy--style-fn avy-style))))

      ;; Open the selected link
      (when pt
	(goto-char pt)
	(org-open-at-point))))

  (defun my/ace-link-agenda-current-line ()
    "Open the link in the current Org agenda line directly, if one exists."
    (interactive)
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
          (link-regex org-link-any-re)
          link-start)
      ;; Check if there's a link in the current line
      (save-excursion
	(goto-char (line-beginning-position))
	(setq link-start (and (re-search-forward link-regex (line-end-position) t)
                              (match-beginning 0))))

      (if link-start
          (progn
            (goto-char link-start)
            (org-open-at-point))
	(message "No link found in the current agenda line"))))
  )

(use-package crux
  :bind (([remap kill-line] . crux-smart-kill-line)
	 ([remap move-beginning-of-line] . crux-move-beginning-of-line))
  )

;;; -> Searching and navigation -> Move text

(use-package move-text
  :bind
  ("M-<up>" . move-text-up)
  ("M-<down>" . move-text-down))

;;; -> Searching and navigation -> Casual suite

(use-package casual-suite
  :ensure t)

(use-package casual-editkit
  :ensure nil
  :bind (("s-e" . casual-editkit-main-tmenu)))

(use-package casual-calc
  :defines calc-mode-map
  :ensure nil
  :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))

(use-package casual-info
  :defines Info-mode-map
  :ensure nil
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))

;;; Dired
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-a" . js/dired-smart-bol)
              ("C-e" . js/dired-smart-eol)
	      ("F" . js/dired-find-marked-files-in-tabs)
	      :map wdired-mode-map
	      ("C-a" . js/dired-smart-bol)
              ("C-e" . js/dired-smart-eol)
	      )
  :config
  (when (eq system-type 'darwin)
    (setq dired-listing-switches "-lagGFDh")
    (setq insert-directory-program "gls")
    (setq dired-use-ls-dired t))

  (defun js/dired-find-marked-files-in-tabs ()
    "Open each marked file in a new tab."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (dolist (file files)
	(tab-bar-new-tab)
	(find-file file))))

  (defun js/dired-smart-bol ()
    "Toggle point between beginning of filename and beginning of line."
    (interactive)
    (let ((old (point))
          (filename-pos (dired-move-to-filename)))
      (if (or (null filename-pos) (<= old filename-pos))
          (move-beginning-of-line 1))))

  (defun js/dired-smart-eol ()
    "Cycle point: before filename → filename start, before filename end → filename end, else → eol."
    (interactive)
    (let ((old (point))
          (fn-start (save-excursion (dired-move-to-filename))))
      (cond
       ((or (null fn-start) (< old fn-start))
	(goto-char fn-start))
       (t
	(dired-move-to-filename)
	(let ((fn-end (dired-move-to-end-of-filename t)))
          (if (or (null fn-end) (>= old fn-end))
              (move-end-of-line 1)))))))

  ;; Handle video files with external viewer via file-name-handler-alist
  (defun video-external-handler (operation &rest args)
    "Handle video files by opening them externally."
    (cond
     ((eq operation 'insert-file-contents)
      (let ((filename (car args)))
        (start-process "open-video" nil "open" filename)
        (message "Opening %s externally..." (file-name-nondirectory filename))
        (error "Video file opened externally")))
     (t
      (let ((inhibit-file-name-handlers
             (cons 'video-external-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args)))))

  (add-to-list 'file-name-handler-alist
               '("\\.\\(mp4\\|avi\\|mkv\\|mov\\|webm\\|flv\\|wmv\\|m4v\\|3gp\\|ogv\\|mpg\\|mpeg\\|vob\\)\\'" . video-external-handler)))

(use-package casual-dired
  :defines dired-mode-map
  :ensure nil
  :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu))
  )

(use-package dired-preview
  :bind (:map dired-mode-map ("P" . dired-preview-mode)))

(use-package casual-avy
  :ensure nil
  :bind ("s-g" . casual-avy-tmenu)
  :hook
  (markdown-mode . imenu-add-menubar-index)
  (makefile-mode . imenu-add-menubar-index)
  ;; (prog-mode . imenu-add-menubar-index)
  (org-mode . imenu-add-menubar-index)
  )

(use-package casual-symbol-overlay
  :functions
  mc/remove-fake-cursors
  symbol-overlay-get-list
  mc/save-excursion
  mc/create-fake-cursor-at-point
  mc/maybe-multiple-cursors-mode
  :ensure nil
  :bind	("s-." . casual-symbol-overlay-tmenu)
  :config

  ;;; Add multiple cursors to the casual-symbol-overlay
  (transient-append-suffix
    'casual-symbol-overlay-tmenu
    '(0 2 0)
    '("h" "Multiple-cursors" ar/mc-mark-all-symbol-overlays))

  ;; Adapted from https://lmno.lol/alvaro/its-all-up-for-grabs-and-it-compounds
  (defun ar/mc-mark-all-symbol-overlays ()
    "Mark all symbol overlays using multiple cursors."
    (interactive)
    (mc/remove-fake-cursors)
    (when-let* ((overlays (symbol-overlay-get-list 0))
		(point (point))
		(point-overlay (seq-find
				(lambda (overlay)
                                  (and (<= (overlay-start overlay) point)
                                       (<= point (overlay-end overlay))))
				overlays))
		(offset (- point (overlay-start point-overlay))))
      (setq deactivate-mark t)
      (mapc (lambda (overlay)
              (unless (eq overlay point-overlay)
		(mc/save-excursion
		 (goto-char (+ (overlay-start overlay) offset))
		 (mc/create-fake-cursor-at-point))))
            overlays)
      (mc/maybe-multiple-cursors-mode)))
  )

;; (use-package casual-isearch
;;   :ensure nil
;;   :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu)))

(use-package casual-ibuffer
  :ensure nil
  :bind (:map
         ibuffer-mode-map
         ("C-o" . casual-ibuffer-tmenu)
         ("F" . casual-ibuffer-filter-tmenu)
         ("s" . casual-ibuffer-sortby-tmenu)
         ("<double-mouse-1>" . ibuffer-visit-buffer) ; optional
         ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window) ; optional
         ("{" . ibuffer-backwards-next-marked) ; optional
         ("}" . ibuffer-forward-next-marked)   ; optional
         ("[" . ibuffer-backward-filter-group) ; optional
         ("]" . ibuffer-forward-filter-group)  ; optional
         ("$" . ibuffer-toggle-filter-group))  ; optional
  :after (ibuffer))

;;; TODO: Figure out re-builder
;; (use-package re-builder
;;   :defer t)
;; (use-package casual-re-builder
;;   :ensure nil
;;   :bind (:map
;;          reb-mode-map ("C-o" . casual-re-builder-tmenu)
;;          :map
;;          reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu))
;;   :after (re-builder))

(use-package bookmark
  :ensure nil
  :defer t)

(use-package casual-bookmarks
  :ensure nil
  :bind (:map
	 bookmark-bmenu-mode-map
         ("C-o" . casual-bookmarks-tmenu)
         ("S" . casual-bookmarks-sortby-tmenu)
         ("J" . bookmark-jump))
  :after (bookmark))

(use-package casual-agenda
  :defines org-agenda-mode-map
  :ensure nil
  :bind (:map
	 org-agenda-mode-map
         ("C-o" . casual-agenda-tmenu)
         ("M-j" . org-agenda-clock-goto) ; optional
         ("J" . bookmark-jump))) ; optional

;;; -> Searching and navigation -> Additional keybinds

(use-package key-chord
  :config
  ;; (key-chord-define-global "hj" 'undo)
  (key-chord-mode 1)
  )

;;; -> Searching and navigation -> Controller support
;;;
;;; Note: On the 8bitdo in switch mode on android, AB and XY are swapped between each other.
;;;

(use-package controller-bindings
  :ensure nil  ;; Not a real package, just for organization
  :if (eq system-type 'android)
  :bind (("<KEYCODE_BUTTON_MODE>" . elfeed)
	 )
  )

(use-package controller-bindings--elfeed
  :ensure nil  ;; Not a real package, just for organization
  :if (eq system-type 'android)
  :after elfeed
  :bind (;; Elfeed search mode bindings
         :map elfeed-search-mode-map
         ("<KEYCODE_BUTTON_B>" . elfeed-search-show-entry)       ; A
         ("<KEYCODE_BUTTON_A>" . elfeed-search-untag-all-unread) ; B
         ("<KEYCODE_BUTTON_Y>" . js/log-elfeed-process)          ; X
	 ("<KEYCODE_BUTTON_X>" . elfeed-search-trash)            ; Y
         ("<KEYCODE_BUTTON_R1>" . elfeed-search-browse-url)
	 ("<KEYCODE_BUTTON_R2>" . elfeed-search-tag-all-unread)
         ("<KEYCODE_BUTTON_L1>" . my/elfeed-show-non-trash--no-search)
	 ("<KEYCODE_BUTTON_L2>" . set-mark-command)
         ("<KEYCODE_BUTTON_START>" . elfeed-search-fetch)
         ("<KEYCODE_BUTTON_SELECT>" . elfeed-search-clear-filter)
	 ("<KEYCODE_BUTTON_MODE>" . elfeed-search-quit-window)

         ;; Elfeed show mode bindings
         :map elfeed-show-mode-map
         ("<KEYCODE_BUTTON_B>" . elfeed-show-visit)  ; A
         ("<KEYCODE_BUTTON_A>" . elfeed-show-next)  ; B
	 ("<KEYCODE_BUTTON_Y>" . elfeed-show-prev) ; X
         ("<KEYCODE_BUTTON_X>" . elfeed-show-trash) ; Y
         ("<KEYCODE_BUTTON_SELECT>" . nil)
         ("<KEYCODE_BUTTON_START>" .  js/log-elfeed-process)
         ("<KEYCODE_BUTTON_L1>" . elfeed-scroll-down-command)
         ("<KEYCODE_BUTTON_R1>" . elfeed-scroll-up-command)
         ("<KEYCODE_BUTTON_L2>" . beginning-of-buffer)
         ("<KEYCODE_BUTTON_R2>" . end-of-buffer)
	 ("<KEYCODE_BUTTON_MODE>" . elfeed-kill-buffer)

         ;; Org-agenda bindings
         ;; :map org-agenda-mode-map
         ;; ("<KEYCODE_BUTTON_A>" . org-agenda-goto)
         ;; ("<KEYCODE_BUTTON_B>" . org-agenda-quit)
         ;; ("<KEYCODE_BUTTON_X>" . org-agenda-todo)
         ;; ("<KEYCODE_BUTTON_Y>" . org-agenda-schedule)
         ;; ("<KEYCODE_BUTTON_L1>" . org-agenda-earlier)
         ;; ("<KEYCODE_BUTTON_R1>" . org-agenda-later)
         ;; ("<KEYCODE_BUTTON_L2>" . org-agenda-backward-block)
         ;; ("<KEYCODE_BUTTON_R2>" . org-agenda-forward-block)
         ;; ("<KEYCODE_BUTTON_SELECT>" . org-agenda-filter-by-tag)
         ;; ("<KEYCODE_BUTTON_START>" . org-agenda-redo)

         ;; Dired bindings
         ;; :map dired-mode-map
         ;; ("<KEYCODE_BUTTON_A>" . dired-find-file)
         ;; ("<KEYCODE_BUTTON_B>" . dired-up-directory)
         ;; ("<KEYCODE_BUTTON_X>" . dired-mark)
         ;; ("<KEYCODE_BUTTON_Y>" . dired-do-flagged-delete)
         ;; ("<KEYCODE_BUTTON_L1>" . beginning-of-buffer)
         ;; ("<KEYCODE_BUTTON_R1>" . end-of-buffer)
         ;; ("<KEYCODE_BUTTON_SELECT>" . dired-toggle-marks)
         ;; ("<KEYCODE_BUTTON_START>" . dired-do-shell-command)
	 )
  )

;;; --> Misc helper packages

(use-package function-groups
  :load-path "~/.emacs.d/lisp/function-groups/")

(use-package vterm
  :if (eq system-type 'darwin))

(use-package stripspace
  :ensure t
  :hook ((prog-mode . stripspace-local-mode)
         ;; (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))

  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean t)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))

;;; -> Misc -> PDFView

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; Build/install the helper binary on first run (quiet)
  (unless (featurep 'pdf-tools)
    (pdf-tools-install :noquery)))

;;; -> Misc -> uptime

(use-package js-uptime
  :load-path "~/.emacs.d/lisp"
  :config
  (add-hook 'kill-emacs-hook #'js/uptime-log-session))

;;; --> AI configuration
;;; -> AI configuration -> GPTel

(use-package gptel
  :functions
  org/enable-gptel-for-chatlog-buffer
  :defer t
  :after org
  :bind
  (("C-c g k" . gptel-abort)
   ("C-c g c" . gptel-add)
   ("C-c g r" . gptel-rewrite)
   ("C-c g s" . gptel-send)
   ("C-c g /" . gptel-menu)
   ("C-c g w" . org/save-gptel-chat-as-node)
   ("C-c g g" . gptel)
   ("C-c g C" . copilot-mode)
   (:map gptel-mode-map
	 ("C-c g t o" . gptel-org-set-topic))
   )
  :hook
  (org-mode . org/enable-gptel-for-chatlog-buffer)
  :custom
  (gptel-default-mode 'org-mode)

  :config
  (setq gptel-model 'gpt-5-mini
	gptel-backend (gptel-make-gh-copilot "Copilot"))
  (require 'gptel-org)
  (defvar org-roam-chatlogs-directory "chatlogs/"
    "The directory to save gptel chatlogs in.")

  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key 'gptel-api-key-from-auth-source
    :models '(openai/gpt-5-mini
	      openai/gpt-4o
	      openai/o4-mini-deep-research
	      anthropic/claude-sonnet-4.6)
    ;; :request-params '(:provider (:only ["openai"]))
    )

  ;;; -> GPTel -> Tool use
  ;; Don't include tool and reasoning blocks by default
  (setq-default gptel-include-reasoning nil)
  (setq-default gptel-include-tool-results 'auto)

  (require 'gptel-integrations) ; MCP

  ;;; -> GPTel -> Saving
  (defun org/enable-gptel-for-chatlog-buffer ()
    "Enable gptel-mode if the current buffer has the `chatlog' tag."
    (when (and (buffer-file-name)
               (member "chatlog" (vulpea-buffer-tags-get)))
      (gptel-mode 1)))

  (defun org/save-gptel-chat-as-node ()
    "Save the current gptel chat buffer as an org-roam node and link it in today's journal."
    (interactive)
    (let* ((existing-id (org-id-get))
           (title (read-string "Title for chat node: " (format-time-string "Chat %Y-%m-%d %H:%M")))
           (chatlog-dir (expand-file-name org-roam-chatlogs-directory org-roam-directory))
           (file-name (format-time-string "Chat-%Y-%m-%d_%H-%M.org"))
           (full-path (expand-file-name file-name chatlog-dir)))

      (if existing-id
          (save-buffer)

	(unless (file-directory-p chatlog-dir)
          (make-directory chatlog-dir t))

	(write-file full-path)

	(goto-char (point-min))
	(org-id-get-create)
	(unless (save-excursion (re-search-forward "^#\\+title:" nil t))
          (goto-char (point-min))
          (when (re-search-forward "^:END:$" nil t)
            (forward-line 1)
            (insert (format "#+title: %s\n" title))))

	(save-buffer)
	(org-roam-db-sync)

	(let* ((node-id (org-id-get))
               (link (org-roam-link-make-string node-id title)))
          (org-roam-dailies-autocapture-today "c" link)
          (message "Chat saved as '%s' and linked in today's journal." title)))))

  )

;;; End of GPTel package block

;;; -> AI Configuration -> Copilot

(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :custom
  (copilot-idle-delay 0)
  :bind (:map copilot-completion-map
	      ("C-f" . copilot-accept-completion)
	      ("M-f" . copilot-accept-completion-by-word)
	      ("C-e" . copilot-accept-completion-by-line)
	      ("M-n" . copilot-next-completion)
	      ("M-p" . copilot-previous-completion))
  :config
  (add-to-list 'warning-suppress-types '(copilot)))

;;; -> AI configuration -> Emacs MCP

(use-package mcp-server-lib)

;; Synchronous tool server for direct MCP tool calls
(use-package emacs-mcp-tool-server
  :load-path "~/.emacs.d/lisp/emacs-mcp-tool-server"
  :ensure mcp-server-lib
  :bind
  (("C-c g t i" . emacs-mcp-tool-install-stdio-script)
   ("C-c g t s" . emacs-mcp-tool-start-server)
   ("C-c g t k" . emacs-mcp-tool-stop-server)
   ("C-c g t r" . emacs-mcp-tool-restart-server)
   ("C-c g t ?" . emacs-mcp-tool-server-status))
  :config
  (require 'emacs-mcp-tool-server)
  )

(use-package mcp
  :ensure t
  :custom (mcp-hub-servers
           `(
             ;; ("fetch" . (:command "uvx" :args ("mcp-server-fetch"))) ; Seems like DDG has its own fetch tool.
	     ("duckduckgo" . (:command "uvx" :args ("duckduckgo-mcp-server")))
	     ("emacs-mcp-tool-server" .
	      (:command ,(expand-file-name "~/.emacs.d/emacs-mcp-stdio.sh")
			:args ("--init-function=emacs-mcp-tool-start-server"
			       "--stop-function=emacs-mcp-tool-stop-server"
			       "--server-id=default")))
             ))
  :config
  (require 'mcp-hub)
  (mcp-hub-start-all-server)
  )

;;; -> AI configuration -> aidermacs

;; (use-package aidermacs
;;   :if (eq system-type 'darwin)
;;   :after gptel vterm
;;   :bind ("C-c g a" . aidermacs-transient-menu)
;;   :custom
;;   (aidermacs-backend 'vterm)
;;   (aidermacs-use-architect-mode t)
;;   (aidermacs-default-model "gpt-4o-mini")
;;   ;; (aidermacs-extra-args '("--edit-format editor-diff" "--copy-paste"))
;;   ;; (aidermacs-architect-model "your-architect-model")  ;; Optional
;;   ;; (aidermacs-editor-model "your-editor-model")        ;; Optional
;;   :config
;;   ;; Set API keys from the auth source once for both OpenAI and Claude
;;   (setenv "OPENAI_API_KEY" (gptel-api-key-from-auth-source "api.openai.com"))
;;   (setenv "CLAUDE_API_KEY" (gptel-api-key-from-auth-source "api.anthropic.com"))
;;   )

;;; End of aidermacs package block

;;; -> AI Configuration -> Xenodium Agent shell

(use-package agent-shell
  :ensure t
  :ensure-system-package
  ((opencode . "npm install -g opencode-ai"))
  :custom
  (agent-shell-preferred-agent-config 'opencode)
  (agent-shell-opencode-default-model-id "github-copilot/gpt-5-mini")
  (agent-shell-prefer-viewport-interaction t)
  (agent-shell-session-strategy 'new)
  (agent-shell-show-usage-at-turn-end t)
  :bind
  ("C-c g a" . agent-shell)
  (:map agent-shell-mode-map
	("M-<tab>" . agent-shell-cycle-session-mode)
	("C-c C-f" . agent-shell-prompt-compose)))

(use-package agent-shell-manager
  :vc (:url "https://github.com/jethrokuan/agent-shell-manager"
	    :rev :newest)
  :after agent-shell
  :bind ("C-c g m" . agent-shell-manager-toggle)
  :custom
  (agent-shell-manager-side 'bottom))

(use-package agent-shell-attention
  :vc (:url "https://github.com/ultronozm/agent-shell-attention.el"
	    :rev :newest)
  :after agent-shell
  :demand t
  :bind ("C-c g j" . agent-shell-attention-jump)
  :custom
  (agent-shell-attention-render-function #'agent-shell-attention-render-active)
  (agent-shell-attention-indicator-location 'global-mode-string)
  (agent-shell-attention-notify-function
   (lambda (_buffer title body)
     (alert body :title title :category 'agent-shell)))

  :config
  (agent-shell-attention-mode 1))

;;; End of Agent shell block

;;; End of AI configuration block


;;; --> Markdown

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

;;; --> Org-mode

(defvar-local js/org-rename-buffer-enabled nil
  "Buffer-local variable to enable/disable tag updating.")

(use-package org
  :functions
  org-collect-keywords
  js/org-rename-buffer-to-title
  org-element-context
  org-element-property
  dom-text
  dom-by-tag
  org-roam-node-from-id
  org-roam-node-title
  elfeed-db-get-entry
  elfeed-entry-title
  get-elfeed-entry-author
  js/get-link-title
  org-link-make-string
  org-in-regexp
  js/format-link
  browse-url-safari
  :custom
  (org-hide-emphasis-markers t)
  (org-image-actual-width nil)
  (org-startup-with-inline-images t)
  (calendar-week-start-day 1)
  (org-insert-heading-respect-content t)
  (org-export-with-broken-links t)
  (org-loop-over-headlines-in-active-region start-level)

  (org-special-ctrl-a/e t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-insert-heading-respect-content t)

  (org-goto-auto-isearch nil)
  (org-M-RET-may-split-line nil)

  (org-default-notes-file (concat org-directory "/inbox.org"))
  (org-capture-templates
   `(("t" "Task" entry (file ,org-default-notes-file)
      "* TODO %?\n" :empty-lines 1)
     ("l" "Link at point - immediate finish" entry (file ,org-default-notes-file)
      "* PROCESS %a\n%i" :empty-lines 1 :immediate-finish t)
     ("L" "Link at point - bring up the thingie" entry (file ,org-default-notes-file)
      "* PROCESS %a\n%i" :empty-lines 1)
     ("c" "Capture" entry (file ,org-default-notes-file)
      ("* %?\n" :empty-lines 1))
     ))

  ;; Org-refile customization
  (org-refile-use-outline-path t)
  ;; (org-refile-allow-creating-parent-nodes ')
  (org-refile-targets '((nil :maxlevel . 5)))
  (org-outline-path-complete-in-steps nil)

  :hook
  (org-mode . js/org-rename-buffer-to-title-enable)

  :bind
  (("C-c l" . org-store-link)
   ("C-c C-l" . ar/org-insert-link-dwim)
   :map org-mode-map
   ("M-o" . ace-link-org)
   ("C-c C-l" . ar/org-insert-link-dwim)
   ("C-'" . nil)
   ("C-," . nil)

   )

  :config
  ;; Org Exporting
  (setq org-latex-classes
	'(("article" "\\documentclass[11pt,a4paper]{article}"
	   ("\\section{%s}" . "\\section*{%s}")
	   ("\\subsection{%s}" . "\\subsection*{%s}")
	   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	   ("\\paragraph{%s}" . "\\paragraph*{%s}")
	   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	  ("report" "\\documentclass[11pt,a4paper]{report}"
	   ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}")
	   ("\\section{%s}" . "\\section*{%s}")
	   ("\\subsection{%s}" . "\\subsection*{%s}")
	   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	  ("book" "\\documentclass[11pt,a4paper]{book}"
	   ("\\part{%s}" . "\\part*{%s}")
	   ("\\chapter{%s}" . "\\chapter*{%s}")
	   ("\\section{%s}" . "\\section*{%s}")
	   ("\\subsection{%s}" . "\\subsection*{%s}")
	   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

  (defun js/org-export-configure-numbering (backend)
    (pcase backend
      ('latex (setq-local org-export-with-section-numbers t))
      ('html  (setq-local org-export-with-section-numbers nil))))
  (setq org-export-with-section-numbers nil)

  (add-hook 'org-export-before-processing-hook #'js/org-export-configure-numbering)
  ;; Open links in the same window
  (setf (alist-get 'file org-link-frame-setup) #'find-file)

  (defun js/org-rename-buffer-to-title ()
    "Rename buffer to value of #+TITLE:."
    (interactive)
    ;; Don't rename temp/hidden buffers (those starting with space)
    (unless (string-prefix-p " " (buffer-name))
      (let ((title (cadar (org-collect-keywords '("title")))))
	(when title (rename-buffer title)))))


  (defun js/org-rename-buffer-to-title-enable ()
    "Enable buffer renaming for the current buffer.
This function is expected to be hooked in org-mode."
    (setq-local js/org-rename-buffer-enabled t)
    (add-hook 'before-save-hook #'js/org-rename-buffer-to-title nil t)
    (js/org-rename-buffer-to-title))

  (defun get-org-link-at-point ()
    "Get URL and description of Org link at point."
    (interactive)
    (let* ((context (org-element-context)))
      (when (equal (car context) 'link)
	(list (org-element-property :type context)
	      (org-element-property :raw-link context)
	      (org-element-property :description context)))))

  (defun js/get-link-title (url)
    "Get the title for a given `url' based on its type."
    (cond
     ;; ((string-prefix-p "http" url t)
     ;;  (with-current-buffer (url-retrieve-synchronously url)
     ;; 	(let ((dom (libxml-parse-html-region (point-min) (point-max))))
     ;; 	  (string-trim (dom-text (car (dom-by-tag dom 'title)))))))

     ((string-prefix-p "http" url t)
      (condition-case nil
          (let ((buffer (url-retrieve-synchronously url t t 3))) ; Add timeout of 3 seconds
            (when buffer
	      (unwind-protect
                  (with-current-buffer buffer
                    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
		      (string-trim (dom-text (car (dom-by-tag dom 'title))))))
		(kill-buffer buffer))))
	(error nil)))

     ((string-prefix-p "id:" url t)
      (let* ((id (string-remove-prefix "id:" url))
             (node (org-roam-node-from-id id)))
	(org-roam-node-title node)))

     ((string-prefix-p "elfeed:" url t)
      (let* ((link (string-remove-prefix "elfeed:" url))
             (entry (when (string-match "\\([^#]+\\)#\\(.+\\)" link)
		      (elfeed-db-get-entry (cons (match-string 1 link)
						 (match-string 2 link))))))
	(when entry
          (let ((title (elfeed-entry-title entry))
		(author (get-elfeed-entry-author entry)))
            (if author
		(format "%s - %s" title author)
	      title)))))

     (t nil)))  ; Return nil for unrecognized URL types

  (defun js/format-link (url)
    "Return the current `url' formatted as an org link with its title."
    (let ((title (js/get-link-title url)))
      (if title
          (org-link-make-string url title)
	(org-link-make-string url))))  ; Just create a plain link if no title

  ;; https://xenodium.com/emacs-dwim-do-what-i-mean/
  (defun ar/org-insert-link-dwim ()
    "Like `org-insert-link' but with personal dwim preferences."
    (interactive)
    (let* ((point-in-link (org-in-regexp org-link-any-re 1))
           (clipboard-url (when (string-match-p "^http" (current-kill 0))
                            (current-kill 0)))
           (region-content (when (region-active-p)
                             (buffer-substring-no-properties (region-beginning)
                                                             (region-end)))))
      (cond ((and region-content clipboard-url (not point-in-link))
             (delete-region (region-beginning) (region-end))
             (insert (org-link-make-string clipboard-url region-content)))
	    ;; New URLs default to title
            ((and clipboard-url (not point-in-link))
             (insert (js/format-link clipboard-url)))
            (t (call-interactively 'org-insert-link)))))

  ;; Browser integration

  (defun open-urls-at-point-or-region (&optional arg)
    "Open links in region or at point using configured browsers.
If region is active, extract and open all URLs found in the region.
Otherwise, open the URL at point.
With prefix ARG, use secondary browser."
    (interactive "P")
    (let ((browse-url-browser-function (if arg
                                           browse-url-secondary-browser-function
					 browse-url-browser-function)))
      (if (use-region-p)
          ;; If we have a region, extract ALL links regardless of format
          (let ((text (buffer-substring-no-properties (region-beginning) (region-end)))
		(urls '()))
            (with-temp-buffer
	      (insert text)

	      ;; 1. Collect org-mode links [[url][description]] and mark them
	      (goto-char (point-min))
	      (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\[[^]]*\\]\\]" nil t)
		(let ((url (match-string 1)))
                  (when (string-match-p "^\\(https?://\\|www\\.\\)" url)
                    (push url urls))
                  ;; Replace the entire org link with a placeholder to avoid re-matching
                  (replace-match "" nil nil)))

	      ;; 2. Collect Markdown links [description](url)
	      (goto-char (point-min))
	      (while (re-search-forward "\\[.*?\\](\\([^)]*\\))" nil t)
		(let ((url (match-string 1)))
                  (when (string-match-p "^\\(https?://\\|www\\.\\)" url)
                    (push url urls))
                  (replace-match "" nil nil)))

	      ;; 3. Collect HTML href attributes
	      (goto-char (point-min))
	      (while (re-search-forward "href=[\"']\\([^\"']+\\)[\"']" nil t)
		(let ((url (match-string 1)))
                  (when (string-match-p "^\\(https?://\\|www\\.\\)" url)
                    (push url urls))
                  (replace-match "" nil nil)))

	      ;; 4. Collect plain text URLs (now that structured links are removed)
	      (goto-char (point-min))
	      (while (re-search-forward "\\(https?://\\|www\\.\\)[^\s\n\"]+" nil t)
		(let ((url (match-string 0)))
                  (when (string-match "^www\\." url)
                    (setq url (concat "https://" url)))
                  (when (string-match "\\([.,:;\"']+\\)$" url)
                    (setq url (substring url 0 (match-beginning 1))))
                  (push url urls))))

            ;; Remove duplicates and open all collected URLs
            (setq urls (delete-dups (nreverse urls)))
            (dolist (url urls)
	      (browse-url url))
            (message "Opened %d URLs in browser" (length urls)))

	;; Otherwise just use the built-in browse-url for single URL
	(save-excursion
          (cond
           ;; First try org links
           ((get-org-link-at-point)
            ;; Move to beginning of link to ensure org-open-at-point works
            (when (org-in-regexp org-link-any-re)
	      (goto-char (match-beginning 0)))
            (org-open-at-point arg))
           ;; Then try plain URLs
           ((thing-at-point 'url)
            (browse-url (thing-at-point 'url)))
           ;; Try moving forward one word and recursing
           ((and (< (point) (line-end-position))
		 (forward-word 1)
		 (< (point) (line-end-position)))
            (open-urls-at-point-or-region arg))
           ;; Finally, give up
           (t (message "No URL at point")))))))

  (defun my/org-wrap-or-toggle (n)
    "If region is active, wrap/unwrap it with the typed char `last-command-event'.
Handles both |*abc*| and *|abc|* cases. Otherwise behave like self-insert."
    (interactive "p")
    (let ((c last-command-event))
      (if (use-region-p)
          (let* ((beg (region-beginning))
		 (end (region-end))
		 (mbeg (copy-marker beg))
		 (mend (copy-marker end))
		 (before-beg (and (> (marker-position mbeg) (point-min))
                                  (char-before (marker-position mbeg))))
		 (after-end (and (< (marker-position mend) (point-max))
				 (char-after (marker-position mend))))
		 (at-beg (char-after (marker-position mbeg)))
		 (before-end (char-before (marker-position mend))))
            (save-excursion
              (cond
               ;; case: *|abc|*  (markers just outside region)
               ((and before-beg after-end (eq before-beg c) (eq after-end c))
		;; delete the after-end marker first, then the before-beg marker
		(goto-char (marker-position mend)) (delete-char 1)
		(goto-char (marker-position mbeg)) (delete-char -1))
               ;; case: |*abc*|  (markers inside region)
               ((and (> (- (marker-position mend) (marker-position mbeg)) 1)
                     (eq at-beg c) (eq before-end c))
		;; delete the end marker (char before end) first, then the beginning marker
		(goto-char (marker-position mend)) (delete-char -1)
		(goto-char (marker-position mbeg)) (delete-char 1))
               ;; otherwise wrap the region
               (t
		(goto-char (marker-position mend)) (insert (char-to-string c))
		(goto-char (marker-position mbeg)) (insert (char-to-string c))))))
	(self-insert-command n))))

  (defun my/org-setup-wrap-keys ()
    "Bind common Org emphasis keys to `my/org-wrap-or-toggle' in org-mode."
    (dolist (k '("*" "/" "_" "=" "~" "+"))
      (local-set-key (kbd k) #'my/org-wrap-or-toggle)))

  (add-hook 'org-mode-hook #'my/org-setup-wrap-keys)

  ;;; -> Org mode -> Navigation
  (defun my/org-narrow-to-heading-content ()
    "Narrow to current heading's content, excluding subheadings.
Automatically expands the heading if it's folded."
    (interactive)
    (save-excursion
      (org-back-to-heading t)
      ;; Ensure the heading is expanded
      (org-show-entry)
      (let ((start (point))
            (end (save-excursion
                   (outline-next-heading)
                   (point))))
	(narrow-to-region start end))))

  (defun my/org-next-heading-narrow ()
    "Move to next visible heading and narrow to its content.
Automatically expands the heading if it's folded."
    (interactive)
    (widen)
    (org-next-visible-heading 1)
    (my/org-narrow-to-heading-content))

  (defun my/org-previous-heading-narrow ()
    "Move to previous visible heading and narrow to its content.
Automatically expands the heading if it's folded."
    (interactive)
    (widen)
    (org-previous-visible-heading 1)
    (my/org-narrow-to-heading-content))

  (defun js/org-goto-first-sibling ()
    "Move to first sibling at current level."
    (interactive)
    (org-back-to-heading t)
    (if (org-up-heading-safe)
	(org-goto-first-child)
      (goto-char (point-min))
      (unless (org-at-heading-p) (outline-next-heading))))

  (defun js/org-goto-last-sibling ()
    "Move to last sibling at current level."
    (interactive)
    (org-back-to-heading t)
    (let ((pos (point)))
      (while (org-get-next-sibling)
	(setq pos (point)))
      (goto-char pos)))

  (defun js/org-sort-siblings-by-todo ()
    "Sort sibling entries by todo state order."
    (interactive)
    (save-excursion
      (if (org-up-heading-safe)
          (org-sort-entries nil ?o)
	;; At top level, sort the whole file's top-level headings
	(goto-char (point-min))
	(org-sort-entries nil ?o))))
  )

;;; -> Org mode -> spliced exporting

(use-package js-ox-strip-heading
  :load-path "~/.emacs.d/lisp/"
  :after ox
  )

(use-package ox
  :ensure nil
  :custom
  (org-export-allow-bind-keywords t)
  :config
  (defcustom js/ox-html-collapsible-headlines t
    "If non-nil, export org headlines as collapsible <details> elements."
    :type 'boolean
    :group 'js-ox)

  (defcustom js/ox-html-collapsible-blocks nil
    "If non-nil, export theorem-style special blocks as collapsible <details> elements."
    :type 'boolean
    :group 'js-ox)

  (defcustom js/ox-html-headlines-open-by-default t
    "If non-nil, collapsible headlines start in the open state."
    :type 'boolean
    :group 'js-ox)

  (defcustom js/ox-html-blocks-open-by-default nil
    "If non-nil, collapsible theorem blocks start in the open state."
    :type 'boolean
    :group 'js-ox)

  (defun js/ox-html-headline (headline contents info)
    "Transcode HEADLINE to HTML, optionally wrapping in <details>."
    (unless (org-element-property :footnote-section-p headline)
      (let* ((numberedp  (org-export-numbered-headline-p headline info))
             (numbers    (org-export-get-headline-number headline info))
             (level      (+ (org-export-get-relative-level headline info)
                            (1- (plist-get info :html-toplevel-hlevel))))
             (todo       (and (plist-get info :with-todo-keywords)
                              (let ((todo (org-element-property :todo-keyword headline)))
                                (and todo (org-export-data todo info)))))
             (todo-type  (and todo (org-element-property :todo-type headline)))
             (priority   (and (plist-get info :with-priority)
                              (org-element-property :priority headline)))
             (text       (org-export-data (org-element-property :title headline) info))
             (tags       (and (plist-get info :with-tags)
                              (org-export-get-tags headline info)))
             (full-text  (funcall (plist-get info :html-format-headline-function)
                                  todo todo-type priority text tags info))
             (contents   (or contents ""))
             (id         (org-html--reference headline info))
             (formatted-text
              (if (plist-get info :html-self-link-headlines)
                  (format "<a href=\"#%s\">%s</a>" id full-text)
                full-text)))
        (if (org-export-low-level-p headline info)
            ;; Deep subtree: delegate to default list-item rendering
            (org-html-headline headline contents info)
          ;; Standard headline
          (let* ((extra-class   (org-element-property :HTML_CONTAINER_CLASS headline))
                 (headline-class (org-element-property :HTML_HEADLINE_CLASS headline))
                 (first-content (car (org-element-contents headline)))
                 (section-contents
                  (if (org-element-type-p first-content 'section)
                      contents
                    (concat (org-html-section first-content "" info) contents)))
                 (open-attr (if js/ox-html-headlines-open-by-default " open" "")))
            (if js/ox-html-collapsible-headlines
                (format "<%s id=\"%s\" class=\"%s\">\n<details%s>\n<summary>\n<h%d id=\"%s\"%s>%s</h%d>\n</summary>\n%s\n</details>\n</%s>\n"
                        (org-html--container headline info)
                        (format "outline-container-%s" id)
                        (concat (format "outline-%d" level)
                                (and extra-class " ")
                                extra-class)
                        open-attr
                        level
                        id
                        (if headline-class (format " class=\"%s\"" headline-class) "")
                        (concat
                         (and numberedp
                              (format "<span class=\"section-number-%d\">%s</span> "
                                      level
                                      (concat (mapconcat #'number-to-string numbers ".") ".")))
                         formatted-text)
                        level
                        section-contents
                        (org-html--container headline info))
              ;; Collapsible disabled: fall through to default
              (org-html-headline headline contents info)))))))

  (defun js/ox-html-special-block (special-block contents info)
    "Transcode SPECIAL-BLOCK to HTML, injecting a title from :parameters if present."
    (let* ((type  (downcase (org-element-property :type special-block)))
           (title (org-element-property :parameters special-block))
           (id    (org-export-get-reference special-block info))
           (inner (or contents "")))
      (format "<div class=\"%s\" id=\"%s\">\n%s%s</div>"
              type id
              (if title (format "<span class=\"block-title\">%s</span>\n" title) "")
              inner)))

  (defun js/ox-latex-special-block (special-block contents info)
    "Transcode SPECIAL-BLOCK to LaTeX, using :parameters for the optional title.
Falls back to #+attr_latex :options for backwards compatibility."
    (let* ((type  (org-element-property :type special-block))
           (params (org-element-property :parameters special-block))
           (opt   (or (and params (format "[%s]" params))
                      (org-export-read-attribute :attr_latex special-block :options)
                      ""))
           (caption (org-latex--caption/label-string special-block info))
           (caption-above-p (org-latex--caption-above-p special-block info)))
      (concat (format "\\begin{%s}%s\n" type opt)
              (and caption-above-p caption)
              contents
              (and (not caption-above-p) caption)
              (format "\\end{%s}" type))))



  (with-eval-after-load 'ox-html
    (setf (alist-get 'special-block
                     (org-export-backend-transcoders
                      (org-export-get-backend 'html)))
          #'js/ox-html-special-block)
    (setf (alist-get 'headline
                     (org-export-backend-transcoders
                      (org-export-get-backend 'html)))
          #'js/ox-html-headline)
    )

  (with-eval-after-load 'ox-latex
    (setf (alist-get 'special-block
                     (org-export-backend-transcoders
                      (org-export-get-backend 'latex)))
          #'js/ox-latex-special-block))

  )

;;; End of org-mode package block

(use-package org-download)

(use-package org-mac-link
  :if (eq system-type 'darwin)
  :ensure t)

(use-package xenops
  :after org
  :defer nil
  :bind
  (:map org-mode-map
	("C-c n !" . xenops-mode))
  :config
  (setq xenops-reveal-on-entry t)
  (pcase system-type
    ('gnu/linux (setq xenops-math-image-scale-factor 2.0))
    ('darwin (setq xenops-math-image-scale-factor 1.6)))
  (setq xenops-math-latex-process 'imagemagick)
  (defun xenops-src-parse-at-point ()
    "Parse 'src element at point."
    (-if-let* ((element (xenops-parse-element-at-point 'src))
	       (org-babel-info
		(xenops-src-do-in-org-mode
		 (org-babel-get-src-block-info 'light (org-element-context)))))
	(xenops-util-plist-update
	 element
	 :type 'src
	 :language (nth 0 org-babel-info)
	 :org-babel-info org-babel-info)))

  ;; :hook org-mode
  )

(use-package org-fragtog
  ;; :disabled ;; Seems like xenops is working again
  ;; Not disabled but unhooked - I set some C-c C-x C-l stuff here
  :defer nil
  :after org
  :custom
  (org-latex-create-formula-image-program 'imagemagick-lualatex)
  (org-latex-packages-alist
   (pcase system-type
     ('gnu/linux '(("" "/home/jure/.emacs.d/defaults/js" t)))
     ('darwin '(("" "/Users/jure/.emacs.d/defaults/js" t)))))
  (org-export-in-background nil)

  :config
  (add-to-list
   'org-preview-latex-process-alist
   '(imagemagick-lualatex
     :programs ("lualatex" "convert") :description "pdf > png"
     :message
     "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf" :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler
     ("lualatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -verbose -density %D -background none -trim -antialias %f -quality 100 %O")
     ))

  ;; In some rare cases, the argument that gets passed to the density makes convert unable to trim.
  ;; 1.6 fucks randy up
  (pcase system-type
    ('gnu/linux (plist-put org-format-latex-options :scale 2.0))
    ('darwin (plist-put org-format-latex-options :scale 1.6)))
  )

(use-package org-appear
  :hook org-mode)

(use-package org-superstar
  :custom
  (org-superstar-leading-bullet ?\u2002)
  :hook org-mode)

(use-package org-pretty-table
  :vc (:url "https://github.com/fuco1/org-pretty-table")
  :hook org-mode)

(use-package org-edna
  :after org org-roam
  :hook
  org-mode
  (org-after-todo-state-change . mm/org-insert-trigger)
  (org-after-todo-statistics . org-summary-todo)
  :config

  (defun js/org-log-processed-today ()
    "Log a DONE link to the current node under today's 'Processed today'."
    (interactive)
    (save-excursion
      (org-back-to-heading t)
      ;; If there's no id or content, don't capture
      (if-let* ((title (js/org--derive-title))
		(id    (js/org-current-node-id))
		(link (org-roam-link-make-string id title)))
	  (org-roam-dailies-autocapture-today "x" link))))

  (defun mm/org-insert-trigger ()
    "An org-edna handler which adds TRIGGER properties."
    (cond ((equal org-state "NEXT")
           (org-set-property "TRIGGER" "next-sibling(todo-only) todo!(NEXT) chain!(\"TRIGGER\") self delete-property!(\"TRIGGER\")"))

	  ;; Changing from PROCESS logs the file to today
	  ((equal org-last-state "PROCESS")
	   (js/org-log-processed-today))))

  ;; Automatically complete the parent with a statistics cookie when all children are complete
  (defun org-summary-todo (_n-done n-not-done)
    "Switch entry to DONE when all subentries are done"
    (let (org-log-done org-todo-log-states) ; turn off logging
      (when (= n-not-done 0)
	(org-todo "DONE"))))
  )

;;; -> Org mode -> Org-roam

(use-package org-roam
  :ensure t
  :functions
  org-roam-node-from-ref
  :bind (("C-c n n b " . org-roam-buffer-toggle)
         ;; ("C-c n f" . js/org-roam-node-find)
         ;; ("C-c n i" . js/org-roam-node-insert)
         ("C-c n d" . org-roam-dailies-map)
         ("C-c n n r" . org-roam-refile)
         ("C-c n n g" . org-id-get-create)
         ("C-c n n t" . js/org-roam-extract-subtree)
         ("C-c n n a" . org-roam-alias-add)
         ("C-c n c" . org-capture-task)
         ("C-c n n u" . org-roam-ui-open)
	 ("C-c n o" . open-urls-at-point-or-region)
	 ("C-c n r" . js/roamify-url-at-point)
	 ("C-c n t" . org-roam-tag-add)
	 ("C-c n n s" . org-roam-db-sync)

	 ;; Blog
	 ("C-c n b b" . org-static-blog-publish)
	 ("C-c n b p" . js/sync-blog)
	 ("C-c n b o" . js/blog-open-in-browser)
	 :map org-roam-dailies-map
	 ("m" . js/org-roam-monthlies-goto-today)
	 ("M" . js/org-roam-monthlies-goto-date)
	 ("l" . js/org-roam-monthlies-capture-today)

         :map org-mode-map
         ("C-M-i" . completion-at-point)


	 ;; Narrowing and movement
	 ;; ("C-c n ." . my/org-narrow-to-heading-content) ;; current position
	 ;; ("C-c n >" . my/org-next-heading-narrow)       ;; move forward
	 ;; ("C-c n <" . my/org-previous-heading-narrow)   ;; move backward

	 ("C-c n >" . js/org-goto-last-sibling)
	 ("C-c n <" . js/org-goto-first-sibling)
	 ("C-c n s d" . js/org-sort-siblings-by-todo)
         )
  :hook (org-roam-mode . visual-line-mode)

  :custom

  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "journals/")
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:40}" 'face 'org-tag)))

  (org-archive-file-header-format nil)

  (org-id-link-to-org-use-id 'nil)
  (org-roam-mode-section-functions
   (list (lambda (node) (org-roam-backlinks-section
			 node
			 :show-backlink-p (lambda (backlink) ; Add the negation of all refinements
					    (and (not (archived-backlink-p backlink))
						 ))
			 :section-heading "Backlinks: "))
         #'org-roam-reflinks-section
         ;; #'org-roam-unlinked-references-section
	 (lambda (node) (org-roam-backlinks-section
			 node
			 :show-backlink-p #'archived-backlink-p
			 :section-heading "Archived backlinks: "))
	 ))



  :config
  (defun js/org-roam-node-not-archived-p (node)
    "Return non-nil if NODE should be shown.
Filters out nodes with ARCHIVE tag."
    (not (member "ARCHIVE" (org-roam-node-tags node))))

  (defun js/org-roam-node-find (&optional arg)
    "Find and open an Org-roam node, hiding archived by default.
With C-u prefix, show all nodes including archived."
    (interactive "P")
    (let ((filter-fn (if arg nil #'js/org-roam-node-not-archived-p)))
      (org-roam-node-find nil nil filter-fn)))

  (defun js/org-roam-node-insert (&optional arg)
    "Insert a link to an Org-roam node, hiding archived by default.
With C-u prefix, show all nodes including archived."
    (interactive "P")
    (let ((filter-fn (if arg nil #'js/org-roam-node-not-archived-p)))
      (org-roam-node-insert filter-fn)))

  (add-to-list 'org-roam-file-exclude-regexp ".stversions/" t)

  (defun js/org-roam-extract-subtree (&optional no-link)
    "Extract subtree to org-roam node.
If heading contains a non-id link, adds it as ROAM_REFS.
By default, replaces the heading with a link to the new node.
With prefix arg NO-LINK, leave nothing behind (original behavior)."
    (interactive "P")
    (org-back-to-heading-or-point-min t)
    (when (bobp) (user-error "Already a top-level node"))
    (let* ((heading-text (org-get-heading t t t t))
           (link-parts (js/extract-org-link heading-text))
           (url (car link-parts))
           (is-ref-link (and url (not (string-prefix-p "id:" url))))
           (title (if (and is-ref-link (cadr link-parts)
                           (not (string-empty-p (cadr link-parts))))
		      (cadr link-parts)
                    heading-text))
           (level (org-current-level))
           (marker (point-marker))
           (id (org-id-get-create)))
      (when is-ref-link
	(org-edit-headline title))
      (save-buffer)
      (org-roam-extract-subtree)
      ;; Add ref in the new file
      (when is-ref-link
	(when-let* ((node (org-roam-node-from-id id))
                    (file (org-roam-node-file node)))
          (with-current-buffer (find-file-noselect file)
            (goto-char (org-roam-node-point node))
            (org-roam-ref-add url)
            (save-buffer))))
      ;; Insert link at original position (unless suppressed)
      (unless no-link
	(with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (set-marker marker nil)
          (insert (make-string level ?*) " "
                  (org-link-make-string (concat "id:" id) title)
                  "\n")
          (forward-line -1)))))

;;; -> org-roam -> Aesthetics

  ;; Color roam links differently
  (defface org-roam-link
    '((t :foreground "orange" :underline t))
    "Face for Org-roam links."
    :group 'org-roam-faces)

  (org-link-set-parameters "id" :face 'org-roam-link)

  ;; Fonfify links in filetags
  (defun js/org-activate-keyword-links (limit)
    "Activate links within org keyword values up to LIMIT.
This extends normal org link fontification to work inside keyword lines
like #+CATEGORY:, #+FILETAGS:, etc., allowing all configured link types
and faces to work normally."
    (let ((case-fold-search t))
      (when (re-search-forward "^[ \t]*#\\+\\w+:[ \t]*\\(.*\\)$" limit t)
	(let ((keyword-value-start (match-beginning 1))
	      (keyword-value-end (match-end 1)))
          (save-excursion
            (goto-char keyword-value-start)
            ;; Use org's built-in link activation within the keyword value
            (let ((old-limit limit))
	      (org-activate-links keyword-value-end))))
	;; Continue searching for more keywords
	t)))

  (defcustom js/org-keywords-with-links '("category" "filetags" "archive")
    "list of org keywords that should have their values scanned for links."
    :group 'org-appearance
    :type '(repeat string))

  (defun js/org-activate-specific-keyword-links (limit)
    "activate links within specific org keyword values up to limit.
only processes keywords listed in `js/org-keywords-with-links'."
    (let ((case-fold-search t)
          (keyword-regexp (concat "^[ \t]*#\\+\\("
				  (mapconcat #'regexp-quote
                                             js/org-keywords-with-links "\\|")
				  "\\):[ \t]*\\(.*\\)$")))
      (when (re-search-forward keyword-regexp limit t)
	(let ((keyword-value-start (match-beginning 2))
	      (keyword-value-end (match-end 2)))
          (save-excursion
            (goto-char keyword-value-start)
            (org-activate-links keyword-value-end)))
	t)))

  (defun js/setup-specific-keyword-link-fontification ()
    "add specific keyword link fontification to org-mode.
only processes keywords listed in `js/org-keywords-with-links'."
    (font-lock-add-keywords
     nil
     '((js/org-activate-specific-keyword-links))
     'append))

  (add-hook 'org-mode-hook #'js/setup-specific-keyword-link-fontification)

  ;; Folded backlink buffer
  (add-to-list 'magit-section-initial-visibility-alist (cons 'org-roam-node-section 'hide))

;;; -> org-roam -> Dailies
  (setq org-extend-today-until 4)

  ;; Fuck it, when nothing else works, bring in a global variable
  (defvar org-roam-capture-content nil
    "Variable to pass content to capture templates.")

  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+startup: show2levels")
           :unnarrowed t)
          ))

  (defvar org-roam-autocapture-templates
    '(("r" "reference" plain "%?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
       :unnarrowed t))
    "A list of templates to use for automatic capture."
    )

  (setq org-roam-dailies-capture-templates
	`(("d" "default" plain "* %?"
	   :target (file+head "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n#+startup: show2levels"))
	  ))

  (defun org-capture-task ()
    (interactive)
    "A function to automatically capture content into a daily template."
    (let (;(org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory))
					;(org-roam-dailies-directory "./")
					;(org-roam-capture-content contents)
	  )
      (org-roam-capture- :keys "t"
			 :node (org-roam-node-create)
			 :templates '(("t" "task" plain "** TODO %?"
				       :target (node "C6C9881B-7EF4-4DAF-A502-84D396372A68")
				       :unnarrowed nil))
					;:props (list :override-default-time (current-time))
			 )
      ))

  ;;; -> org-roam -> Autocapture -> dailies

  (defvar org-roam-capture--browser nil
    "Variable to pass current browser to capture templates.")

  (defvar org-roam-capture-body nil
    "Variable to pass body content to capture templates.")

  (defvar org-roam-dailies-autocapture-templates
    '(("w" "url capture" plain "%(eval (or org-roam-capture-body \"\"))"
       :target (file+head+olp "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n#+startup: show2levels" ("Web" "%(eval (concat org-roam-capture-content))"))
       :immediate-finish t)
      ("r" "url reading capture" plain "%(eval (or org-roam-capture-body \"\"))"
       :target (file+head+olp "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n#+startup: show2levels" ("Processing" "%(eval (concat \"PROCESS \" org-roam-capture-content))"))
       :immediate-finish t)
      ("e" "elfeed link capture" plain "%(eval (or org-roam-capture-body \"\"))"
       :target (file+head+olp "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n#+startup: show2levels" ("Elfeed" "%(eval (concat org-roam-capture-content))"))
       :immediate-finish t)
      ("p" "process capture" plain "%(eval (or org-roam-capture-body \"\"))"
       :target (file+head+olp "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n#+startup: show2levels" ("Processing" "%(eval (concat \"PROCESS \" org-roam-capture-content))"))
       :immediate-finish t)
      ("c" "chatlog capture" plain "%(eval (or org-roam-capture-body \"\"))"
       :target (file+head+olp "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n#+startup: show2levels" ("Chats" "%(eval (concat org-roam-capture-content))"))
       :immediate-finish t)
      ("x" "processed log" plain "%(eval (or org-roam-capture-body \"\"))"
       :target (file+head+olp "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n#+startup: show2levels" ("Processed today" "%(eval (concat \"DONE \" org-roam-capture-content))"))
       :immediate-finish t)
      )
    "A list of templates to use for automatic daily capture."
    )

  (defun org-roam-dailies-autocapture-today (keys &optional contents body)
    "A function to automatically capture content into a daily template."
    (let* ((org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory))
           (org-roam-dailies-directory "./")
	   (org-roam-capture-content (or contents org-roam-capture-content))
	   (daily-node nil)
	   (org-roam-capture-body (or body org-roam-capture-body))
	   )
      (org-roam-capture- :keys keys
			 :node (org-roam-node-create)
			 :templates org-roam-dailies-autocapture-templates
			 :props (list :override-default-time (current-time)))
      ))

  ;;; -> Org-roam -> Autocapture -> Browser Integration

  (defun org-roam-link-make-string (id &optional description)
    "Makes an org-roam link string pointing to the given id.
     Assumes one actually exists."
    (let* ((node (org-roam-node-from-id id))
	   (description (or description
			    (org-roam-node-title node))))
      (org-link-make-string (concat "id:" id) description)))

  (defvar js/browsers
    (pcase system-type
      ('darwin
       '((Safari . org-mac-link-safari-get-frontmost-url)
	 (Firefox . org-mac-link-firefox-get-frontmost-url)))
      (_ '(NoBrowser . (lambda () "No browser function configured!"))))
    "Available browsers with their corresponding URL retrieval functions."
    )

  (defun js/retrieve-url (&optional browser)
    "Retrieve the URL of the given browser page as a string.
Using the org-mac-link, this comes pre-formatted with the url title."
    (let* ((browser (or browser (caar js/browsers))) ;; Default to first browser in list
           (url-function (cdr (assoc browser js/browsers))))
      (if url-function
          (funcall url-function)
	(user-error "Browser %s not supported" browser))))

  (defun js/extract-org-link (org-link-string)
    "Return (URL DESCRIPTION) if ORG-LINK-STRING contains an Org link, else nil."
    (with-temp-buffer
      (insert org-link-string)
      (org-mode)
      (goto-char (point-min))
      (let ((ctx (ignore-errors (org-element-link-parser))))
	(when ctx
          (let ((raw (org-element-property :raw-link ctx))
		(cb  (org-element-property :contents-begin ctx))
		(ce  (org-element-property :contents-end   ctx)))
            (list raw (if (and cb ce)
                          (buffer-substring-no-properties cb ce)
			"")))))))

  (defvar js/url-targets
    '((Log . js/url-target-log)
      (Process . js/url-target-process)
      (Wallabag . js/url-target-wallabag))
    "Alist of available URL handling targets and their handler functions.")

  ;; Handler functions for each target
  (defun js/url-target-log (url-source)
    "Log URL-SOURCE to the daily journal."
    (let ((org-roam-capture-content url-source))
      (org-roam-dailies-autocapture-today "w"))
    "logged")

  (defun js/url-target-process (url-source)
    "Log URL-SOURCE to the processing section."
    (let ((org-roam-capture-content url-source))
      (org-roam-dailies-autocapture-today "r"))
    "processed")

  (defun js/url-target-wallabag (url-source)
    "Add URL-SOURCE to wallabag."
    (let* ((url-parts (js/extract-org-link url-source))
           (url (when url-parts (car url-parts))))
      (when url
	(message "Adding to wallabag: %s" url)
	;; (run-with-timer 2 nil #'wallabag-request-and-synchronize-entries)
	(run-with-timer 2 nil #'wallabag-request-token)
	(wallabag-add-entry url "")
	;; Sync wallabag changes to server
	"added to wallabag")))

  (cl-defun js/log-page (&key url browser clipboard (targets '(Log)) interactive)
    "Process a URL according to specified targets.
Keyword arguments:
:url         - direct URL string
:browser     - browser symbol (e.g., 'Safari)
:clipboard   - non-nil to check clipboard
:interactive - non-nil to prompt for browser
:targets     - list of targets (Log, Process, Wallabag)
               default is '(Log)

Examples:
  (js/log-page :browser 'Safari)
  (js/log-page :browser 'Safari :targets '(Process Wallabag))
  (js/log-page :clipboard t :targets '(Process))
  (js/log-page :url \"https://example.com\" :targets '(Log Wallabag))

For emacsclient:
  emacsclient --eval \"(js/log-page :browser 'Safari)\"
  emacsclient --eval \"(js/log-page :browser 'Safari :targets '(Process))\"
  emacsclient --eval \"(js/log-page :targets '(Log Wallabag))\"
"
    (interactive)

    ;; Get URL from specified source
    (let* ((url-source (or url
                           (when browser (js/retrieve-url browser))
                           (when (or clipboard (not (or url browser interactive)))
                             (let ((clip (current-kill 0 t)))
			       (when (and clip (string-match-p "^https?://" clip))
				 clip)))
                           (when (or interactive (not (or url browser)))
                             (js/retrieve-url (intern (completing-read "Select browser: " js/browsers nil t 'Safari))))))
           (results '()))

      (when url-source
	;; Apply each requested target
	(dolist (target targets)
          (when-let* ((handler-fn (cdr (assq target js/url-targets)))
		      (result (funcall handler-fn url-source)))
            (push result results)))

	;; Alert based on actions taken
	(alert (format "URL %s!" (string-join results " and "))
	       :title "URL Handler" :category 'debug)

	;; Return the URL source for potential chaining
	url-source)))

  (defun js/roamify-url-at-point (&optional node-id post-cleanup-functions)
    "Convert a URL at point into an org-roam node and replace the link.

With C-u prefix, prompt for existing node to add URL as ref to.
Can optionally pass in your own `NODE-ID' which will get used as the target node.

`POST-CLEANUP-FUNCTIONS' will be executed upon completion of the finalizer."
    (interactive
     (when current-prefix-arg
       (list (org-roam-node-id (org-roam-node-read nil nil nil 'require-match)))))
    (let* ((context (org-element-context))
           (type (org-element-type context))
           (link-type (org-element-property :type context))
           (url (org-element-property :raw-link context))
           (end (org-element-property :end context))
           (beg (org-element-property :begin context))
           (title-beg (org-element-property :contents-begin context))
           (title-end (org-element-property :contents-end context))
           (working-title (or (buffer-substring-no-properties title-beg title-end)
			      (js/get-link-title url)))
           (capture-node nil)
           (target-id nil)
           (message-text nil))
      (pcase (list type link-type)
	(`(link "id")
	 (message "Is already roam link.")
	 (return))
	(`(link ,_)
	 ;; Check if ref already exists first - this takes precedence
	 (if-let ((existing-ref-node (org-roam-node-from-ref url)))
             ;; Ref already exists - just replace link, no capture
             (let ((existing-id (org-roam-node-id existing-ref-node))
                   (existing-title (org-roam-node-title existing-ref-node)))
	       (delete-region beg end)
	       (insert (org-roam-link-make-string existing-id working-title))
	       (message "Using existing node with this ref: %s" existing-title))
           ;; No existing ref - proceed with capture logic
           (pcase node-id
             (`nil
	      ;; Default behavior: create new node
	      (setq capture-node (org-roam-node-create :title working-title))
	      (setq target-id 'from-ref)
	      (setq message-text (format "Created org-roam node: %s" working-title)))
             ((and id (guard (org-roam-node-from-id id)))
	      ;; Add ref to existing node - use the actual existing node object
	      (let ((existing-node (org-roam-node-from-id id)))
		(setq capture-node existing-node)
		(setq target-id id)
		(setq message-text (format "Added ref to existing node: %s" (org-roam-node-title existing-node)))))
             (id
	      ;; Create new node with specified ID
	      (setq capture-node (org-roam-node-create :title working-title :id id))
	      (setq target-id 'from-ref)
	      (setq message-text (format "Created org-roam node: %s" working-title))))))
	(_
	 (message "No link found at point.")
	 (return)))

      ;; Single org-roam-capture- call (always runs when we have a capture-node)
      (when capture-node
	(defun roamify-finalizer ()
          "Replace URL with roam link after capture completes."
          (let ((final-id (cond
                           ((eq target-id 'from-ref)
                            (when-let* ((full-node (org-roam-node-from-ref url)))
			      (org-roam-node-id full-node)))
                           (target-id
                            ;; Add ref to existing node
                            (let ((existing-node (org-roam-node-from-id target-id)))
			      (with-current-buffer (find-file-noselect (org-roam-node-file existing-node))
				(goto-char (org-roam-node-point existing-node))
				(org-roam-ref-add url))
			      target-id)))))
            (when final-id
	      (delete-region beg end)
	      (insert (org-roam-link-make-string final-id working-title))
	      (message message-text)
	      )))
	(org-roam-capture-
	 :keys "d"
	 :node capture-node
	 :info (list :ref url)
	 :props (list :finalize #'roamify-finalizer)))))

  ;;; -> org-roam -> Helper functions

  (defun js/org--derive-title ()
    "Return the first link's description in the heading, or the whole heading, as a plain string."
    (save-excursion
      (org-back-to-heading t)
      (let* ((raw  (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position)))
             (head (if (string-match "^\\*+ \\(?:[A-Z]+ \\)?\\(.*\\)$" raw)
		       (match-string 1 raw)
                     raw))
             (parsed (js/extract-org-link head))
             (desc   (and parsed (cadr parsed)))
             (res    (if (and desc (not (string-empty-p desc))) desc head)))
	(substring-no-properties res))))

  (defun js/org-current-node-id ()
    "Return org-roam node ID at point, nil if nonexistent."
    (when-let* ((node (ignore-errors (org-roam-node-at-point)))
		(id (org-roam-node-id node)))
      id))

  ;;; -> org-roam -> Archiving
  (defun archived-backlink-p (backlink)
    "Checks whether the backlink lives under the Archived today heading."
    (let* ((properties (org-roam-backlink-properties backlink))
	   (outline (plist-get properties :outline)))
      (equal "Archived today" (car outline))))

  ;; https://freerangebits.com/posts/2024/01/archiving-in-org-mode/
  (defun js/org-archive-subtree-to-daily (&optional _find-done)
    "Archive the current subtree to the roam daily file."
    (interactive "P")
    (require 'org-roam)
    (require 'org-archive)
    (let* ((daily-file (expand-file-name
			(format-time-string "%Y-%m-%d.org")
			(expand-file-name org-roam-dailies-directory org-roam-directory)))
           (today (if (string= (buffer-file-name) daily-file)
		      daily-file
                    (save-window-excursion
		      (save-excursion
			(org-roam-dailies-goto-today "d")
			(buffer-file-name)))))
           (org-archive-location (concat today "::* Archived today :ARCHIVE:"))
           (file-id (save-excursion
		      (goto-char (point-min))
		      (org-roam-id-at-point)))
           (heading-title (org-get-heading t t t t)))
      (when file-id
	(org-set-property "ARCHIVE_NODE" (org-roam-link-make-string file-id)))
      (org-archive-subtree 0)
      ;; Set TODO state to CANCELLED in the archived entry
      (with-current-buffer (find-file-noselect today)
	(save-excursion
          (goto-char (point-min))
          ;; Find the "Archived today" heading
          (when (re-search-forward "^\\* Archived today" nil t)
            (let ((archive-end (save-excursion (org-end-of-subtree t t))))
	      ;; Search for our heading within this subtree
	      (when (re-search-forward
                     (format "^\\*\\* .*%s" (regexp-quote heading-title))
                     archive-end t)
		(org-back-to-heading t)
		(when (org-get-todo-state)
                  (org-todo "CANCELLED"))))))
	(save-buffer))
      (save-buffer)))

  (custom-set-variables
   '(org-archive-default-command #'js/org-archive-subtree-to-daily))

  (setq org-archive-subtree-save-file-p t)

  (defun js/org-unarchive-subtree-from-daily ()
    "Unarchive the current subtree back to its original location.
Uses ARCHIVE_NODE and ARCHIVE_OLPATH properties to restore the entry.
Restores the original TODO state from ARCHIVE_TODO."
    (interactive)
    (require 'org-roam)

    ;; Ensure we're at a heading
    (unless (org-at-heading-p)
      (org-back-to-heading t))

    ;; Verify we're in an archive section
    (unless (member "ARCHIVE" (org-get-tags))
      (user-error "Current heading doesn't appear to be in an archive section"))

    ;; Get archive metadata
    (let* ((archive-node-link (org-entry-get nil "ARCHIVE_NODE"))
           (archive-file (org-entry-get nil "ARCHIVE_FILE"))
           (archive-olpath (org-entry-get nil "ARCHIVE_OLPATH"))
           (archive-todo (org-entry-get nil "ARCHIVE_TODO")))

      (unless archive-node-link
	(user-error "No ARCHIVE_NODE property found. This doesn't appear to be an archived entry"))

      ;; Extract node ID from the org-roam link
      (let* ((node-id (when (string-match "\\[\\[id:\\([^]]+\\)\\]" archive-node-link)
			(match-string 1 archive-node-link)))
             (target-node (when node-id (org-roam-node-from-id node-id)))
             (target-file (or (when target-node (org-roam-node-file target-node))
			      archive-file)))

	(unless target-file
          (user-error "Could not determine target file from node %s or archive file %s"
		      archive-node-link archive-file))

	(unless (file-exists-p target-file)
          (user-error "Target file does not exist: %s" target-file))

	;; Store the current buffer and position
	(let ((archive-buffer (current-buffer))
	      (archive-point (point-marker))
	      paste-level)

          ;; Copy the subtree content (without cutting yet)
          (org-copy-subtree 1 nil)

          ;; Navigate to target and paste
          (save-window-excursion
            (with-current-buffer (find-file-noselect target-file)
	      (save-excursion
		(goto-char (point-min))

		;; Navigate to the correct location
		(if (and node-id (org-roam-id-find node-id))
                    (progn
		      (org-id-goto node-id)
		      (setq paste-level 1)

		      ;; If we have an OLPATH, navigate through it
		      (when (and archive-olpath (not (string-empty-p archive-olpath)))
			(let ((path-components (split-string archive-olpath "/")))
                          (goto-char (org-roam-capture-find-or-create-olp path-components))
                          (setq paste-level (+ 1 (length path-components))))))
                  ;; Fallback: if no node ID, try to use file and OLPATH
                  (when (and archive-olpath (not (string-empty-p archive-olpath)))
                    (let ((path-components (split-string archive-olpath "/")))
		      (goto-char (org-roam-capture-find-or-create-olp path-components))
		      (setq paste-level (+ 1 (length path-components))))))

		;; Move to end of current subtree for insertion
		(org-end-of-subtree t t)

		;; Paste the subtree at the correct level
		(org-paste-subtree paste-level)

		;; Move to the pasted subtree and clean up
		(org-back-to-heading t)

		;; Restore TODO state before deleting properties
		(when archive-todo
                  (org-todo archive-todo))

		;; Delete archive properties
		(mapc #'org-delete-property
		      '("ARCHIVE_NODE" "ARCHIVE_TIME" "ARCHIVE_FILE"
			"ARCHIVE_OLPATH" "ARCHIVE_CATEGORY" "ARCHIVE_ITAGS"
			"ARCHIVE_TODO"))

		(save-buffer))))

          ;; Now remove the original from archive buffer
          (goto-char archive-point)
          (org-back-to-heading t)
          (org-cut-subtree 1)
          (save-buffer)

          ;; Success message
          (message "Unarchived subtree to %s%s"
                   target-file
                   (if archive-olpath
		       (format " under %s" archive-olpath)
                     ""))))))

  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . fit-window-to-buffer)))
  (org-roam-db-autosync-mode)

  (advice-add 'org-roam-db-update-file :around
	      (defun +org-roam-db-update-file (fn &rest args)
                (emacsql-with-transaction (org-roam-db)
                  (apply fn args))))

  ;; Fixes a bug in the capture templates using a heading outline path
  ;; https://github.com/org-roam/org-roam/pull/2336
  (defun org-roam-capture-find-or-create-olp (olp)
    "Return a marker pointing to the entry at OLP in the current buffer.
If OLP does not exist, create it. If anything goes wrong, throw
an error, and if you need to do something based on this error,
you can catch it with `condition-case'."
    (let* ((level 1)
           (lmin 1)
           (lmax 1)
           (start (point-min))
           (end (point-max))
           headings
           found flevel)
      (unless (derived-mode-p 'org-mode)
	(error "Buffer %s needs to be in Org mode" (current-buffer)))
      (org-with-wide-buffer
       (setq headings (mapcar #'org-roam-capture--fill-template olp))
       (goto-char start)
       (dolist (heading headings)
	 (let ((re (format org-complex-heading-regexp-format
                           (regexp-quote heading)))
	       (cnt 0))
           (while (re-search-forward re end t)
             (setq level (- (match-end 1) (match-beginning 1)))
             (when (and (>= level lmin) (<= level lmax))
	       (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
           (when (> cnt 1)
             (error "Heading not unique on level %d: %s" lmax heading))
           (when (= cnt 0)
             ;; Create heading if it doesn't exist
             (goto-char end)
             (unless (bolp) (newline))
             (let (org-insert-heading-respect-content)
	       (org-insert-heading nil nil t))
             (unless (= lmax 1)
	       (dotimes (_ level) (org-do-demote)))
             (insert heading)
             (setq end (point))
             (goto-char start)
             (while (re-search-forward re end t)
	       (setq level (- (match-end 1) (match-beginning 1)))
	       (when (and (>= level lmin) (<= level lmax))
		 (setq found (match-beginning 0) flevel level cnt (1+ cnt))))))
	 (goto-char found)
	 (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
	 (setq start found
	       end (save-excursion (org-end-of-subtree t t))))
       (point-marker))))

  ;;; -> org-roam -> dailies -> monthlies
  ;;; Monthly professional log
  ;; journals/monthlies/%Y-%m-monthly.org
  ;; * %Y-%m-%d %A         <- day heading, no ID
  ;;   ** ...               <- log entries captured here

  (defvar org-roam-monthlies-capture-templates
    '(("m" "monthly log goto" plain ""
       :target (file+head+olp "%<%Y-%m>-monthly.org"
                              "#+title: %<%Y-%m>\n#+startup: show2levels\n"
                              ("%<%Y-%m-%d %A>"))
       :unnarrowed t
       :immediate-finish t)
      ("e" "monthly log entry" plain "** %?"
       :target (file+head+olp "%<%Y-%m>-monthly.org"
                              "#+title: %<%Y-%m>\n#+startup: show2levels\n"
                              ("%<%Y-%m-%d %A>"))
       :unnarrowed t))
    "Capture templates for monthly professional log files.")

  (defun js/org-roam-monthlies--directory ()
    "Return the absolute path to the monthlies directory, creating it if needed."
    (let ((dir (expand-file-name
		"monthlies/"
		(expand-file-name org-roam-dailies-directory org-roam-directory))))
      (make-directory dir t)
      dir))

  (defun js/org-roam-monthlies--file-for-time (time)
    "Return the absolute path to the monthly file for TIME."
    (expand-file-name (format-time-string "%Y-%m-monthly.org" time)
                      (js/org-roam-monthlies--directory)))

  (defun js/org-roam-monthlies--day-heading-re (time)
    "Return a regexp matching the day heading for TIME."
    (concat "^\\* " (regexp-quote (format-time-string "%Y-%m-%d %A" time))))

  (defun js/org-roam-monthlies--capture (time &optional goto keys)
    "Core capture dispatcher for monthly logs.
TIME is an Emacs time value.  GOTO and KEYS as in `org-roam-capture-'."
    (let ((org-roam-directory (js/org-roam-monthlies--directory))
          (org-roam-dailies-directory "./"))
      (org-roam-capture- :goto (when goto '(4))
			 :keys keys
			 :node (org-roam-node-create)
			 :templates org-roam-monthlies-capture-templates
			 :props (list :override-default-time time))))

;;;###autoload
  (defun js/org-roam-monthlies-goto-today ()
    "Go to today's heading in the monthly log, creating it if absent."
    (interactive)
    (js/org-roam-monthlies--capture (current-time) 'goto "m"))

;;;###autoload
  (defun js/org-roam-monthlies-goto-date ()
    "Prompt for a date and navigate to its monthly log.
If the day heading exists, jump to it.  Otherwise open the file at the top.
Never creates a heading for the chosen date."
    (interactive)
    (let* ((time (org-read-date nil t nil "Monthly log for date: "))
           (file (js/org-roam-monthlies--file-for-time time)))
      (if (not (file-exists-p file))
          ;; File doesn't exist yet — open/create it via capture (writes the
          ;; #+title header and registers the org-id) then bail to top.
          (js/org-roam-monthlies--capture time 'goto "m")
	(find-file file)
	(widen)
	(goto-char (point-min))
	(re-search-forward (js/org-roam-monthlies--day-heading-re time) nil t)
	;; re-search-forward leaves point after the match (end of heading line).
	;; Move to the beginning of the line whether found or not.
	(beginning-of-line))))

;;;###autoload
  (defun js/org-roam-monthlies-capture-today ()
    "Capture a new subheading entry under today's day heading in the monthly log."
    (interactive)
    (js/org-roam-monthlies--capture (current-time) nil "e"))


  )
;;; End of org-roam package block

;;; -> org-roam -> org-transclusion

(use-package org-transclusion
  :ensure t
  :after org
  :bind
  (("<f12>" . org-transclusion-add)
   ("s-<f12>" . org-transclusion-deactivate)
   ("C-c n x" . org-transclusion-transient-menu) ;; Xanadu
   )
  :custom
  (org-transclusion-include-first-section nil)
  :config
  (set-face-attribute
   'org-transclusion-fringe nil
   :foreground "green"
   :background "green")
  )

(use-package org-transclusion-font-lock
  :ensure nil
  :after org-transclusion
  :config (org-transclusion-font-lock-mode +1))

;;; -> Org-roam -> org-node

(use-package org-mem
  :defer
  :custom
  (org-mem-do-sync-with-org-id t)
  :config
  (org-mem-updater-mode))

(use-package org-node
  :after org-roam
  :bind
  (("C-c n f" . js/org-node-find)
   ("C-c n i" . js/org-node-insert))

  :custom
  (org-node-alter-candidates t) ; OLP support

  ;; Performance tuning
  (org-node-perf-keep-file-name-handlers nil)  ; Max speed

  ;; Org-roam compatibility
  (org-node-creation-fn #'org-node-new-via-roam-capture)
  (org-node-file-slug-fn #'org-node-slugify-like-roam-default)
  (org-node-file-timestamp-format "%Y%m%d%H%M%S-")

  ;; Directory configuration
  (org-id-locations-file-relative t)
  (org-node-extra-id-dirs
   (list org-roam-directory
         (expand-file-name org-roam-dailies-directory org-roam-directory)))

  ;; Customs
  (org-node-display-sort-fn #'org-node-sort-by-file-mtime)

  :config
  ;; Your custom filtering logic
  (defun js/org-node-not-archived-p (node)
    "Return t if NODE should be shown (not archived)."
    (not (member "ARCHIVE" (org-mem-tags node))))

  (defun js/org-node-find (&optional arg)
    "Find and open an org-node, hiding archived by default.
With C-u prefix, show all nodes including archived."
    (interactive "P")
    (let ((org-node-filter-fn
           (if arg nil #'js/org-node-not-archived-p)))
      (org-node-find)))

  (defun js/org-node-insert (&optional arg)
    "Insert a link to an org-node, hiding archived by default.
With C-u prefix, insert a transclusion instead."
    (interactive "P")
    (if arg
	(org-node-insert-transclusion)
      (let ((org-node-filter-fn #'js/org-node-not-archived-p))
	(org-node-insert-link*))))

  (defun org-node-insert-link (&optional region-as-initial-input novisit)
    "Insert a link to one of your ID nodes.

To behave exactly like org-roam\\='s `org-roam-node-insert',
see `org-node-insert-link*', or pass REGION-AS-INITIAL-INPUT t.

Argument NOVISIT for use by `org-node-insert-link-novisit'."
    (interactive "@*" org-mode)

    ;; (unless (derived-mode-p 'org-mode)
    ;;   (user-error "Only works in org-mode buffers"))
    (org-node-cache-ensure)
    (let* ((beg nil)
           (end nil)
           (region-text (when (region-active-p)
                          (setq end (region-end))
                          (goto-char (region-beginning))
                          (skip-chars-forward "\n[:space:]")
                          (setq beg (point))
                          (goto-char end)
                          (skip-chars-backward "\n[:space:]")
                          (setq end (point))
                          (org-link-display-format
                           (buffer-substring-no-properties beg end))))
           (initial (if (or region-as-initial-input
                            (and region-text
				 (try-completion region-text
						 org-node--title<>affixations)))
			region-text
		      nil))
           (_ (when (eq t initial)
		;; Guard against `try-completion' returning t instead of a string
		;; (who knew?!)
		(setq initial nil)))
           (input (if (and novisit initial)
		      initial
                    (org-node-read-candidate nil t initial)))
           (_ (when (string-blank-p input)
		(setq input (funcall org-node-blank-input-title-generator))))
           (node (gethash input org-node--candidate<>entry))
           (id (if node (org-mem-id node) (org-id-new)))
           (link-desc (or region-text
                          (and node
			       org-node-custom-link-format-fn
			       (funcall org-node-custom-link-format-fn node))
                          (and (not org-node-alter-candidates) input)
                          (and node (seq-find (##string-search % input)
					      (org-mem-entry-roam-aliases node)))
                          (and node (org-mem-entry-title node))
                          input)))
      (atomic-change-group
	(when region-text
          (delete-region beg end))
	;; TODO: When inserting a citation, insert a [cite:] instead of a normal
	;;       link
	;; (if (string-prefix-p "@" input))
	(insert (org-link-make-string (concat "id:" id) link-desc)))
      (run-hooks 'org-node-insert-link-hook)
      ;; TODO: Delete the link if a node was not created
      ;;       See `org-node-insert-transclusion'
      ;; TODO: Respect `org-node-stay-in-source-buffer'
      (unless node
	(org-node-create input id))))

  ;; Initialize the cache
  (org-node-cache-mode)
  (org-node-cache-ensure)
  (org-node-roam-accelerator-mode -1)
  )

;;; -> Org-roam -> org-roam-ui

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org_roam-ui-follow nil)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

;;; TODO -> Org mode -> Citation

;;; https://github.com/org-roam/org-roam-bibtex
;;; -> Org mode -> Tag management

;; The tag handling code adapted from:
;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
(use-package vulpea
  :functions
  tags/org-update-all-tags
  vulpea-buffer-p
  vulpea-buffer-tags-get
  vulpea-buffer-tags-set
  tags/maybe-update-tags
  :defines
  tags/update-tags-enabled
  :preface
  (setq prune/ignored-files '("tasks.org" "inbox.org")) ; These should always have project tags.
  (setq tag-checkers '(("project" . org/project-p)
		       ("flashcards" . org/has-anki-flashcards-p)
		       ("chatlog" . org/has-gptel-chatlog-p)))
  (setq tags/updating-tags (mapcar #'car tag-checkers))

  :commands (tags/make-db-searcher)
  :config

  (defun org/project-p ()
    "Return non-nil if current buffer has a todo entry.
Ignores headlines under ARCHIVE-tagged ancestors."
    (org-element-map
	(org-element-parse-buffer 'headline)
	'headline
      (lambda (h)
	;; Skip if this headline or any ancestor has :ARCHIVE: tag
	(unless (org-element-lineage-map h
                    (lambda (ancestor)
		      (member "ARCHIVE" (org-element-property :tags ancestor)))
                  'headline 'with-self 'first-match)
          (eq (org-element-property :todo-type h) 'todo)))
      nil 'first-match))

  (defun org/has-anki-flashcards-p ()
    "Return non-nil if current buffer has ANKI-related properties in actual drawers.
Ignores headlines under ARCHIVE-tagged ancestors."
    (org-element-map
	(org-element-parse-buffer 'headline)
	'headline
      (lambda (h)
	;; Skip if this headline or any ancestor has :ARCHIVE: tag
	(unless (org-element-lineage-map h
                    (lambda (ancestor)
		      (member "ARCHIVE" (org-element-property :tags ancestor)))
                  'headline 'with-self 'first-match)
          (or (org-element-property :ANKI_NOTE_TYPE h)
	      (org-element-property :ANKI_DECK h)
	      (org-element-property :ANKI_NOTE_ID h)
	      (org-element-property :ANKI_TAGS h))))
      nil 'first-match))

  (defun org/has-gptel-chatlog-p ()
    "Return non-nil if current buffer has GPTEL-related properties in actual drawers."
    (org-element-map
	(org-element-parse-buffer 'headline)
	'headline
      (lambda (h)
	(or (org-element-property :GPTEL_TOPIC h)
            (org-element-property :GPTEL_MESSAGES h)
            (org-element-property :GPTEL_MODEL h)
            (org-element-property :GPTEL_CONTEXT h)))
      nil 'first-match))

  ;; Exclude the relevant tags from inheritance
  (dolist (tag (cons "summary" tags/updating-tags))
    (add-to-list 'org-tags-exclude-from-inheritance tag))

  (add-to-list 'org-tags-exclude-from-inheritance "interesting")

  (defvar tags/tag-added-hook nil
    "Hook run when a tag is added to a file.
Each function is called with two arguments: the tag and the buffer.")

  (defvar tags/tag-removed-hook nil
    "Hook run when a tag is removed from a file.
Each function is called with two arguments: the tag and the buffer.")

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
	 (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun tags/org-update-tag (tcpair)
    "Update \\='(tag . checker) tag in the current buffer."
    (when (and (not (member (buffer-name) prune/ignored-files))
	       (not (active-minibuffer-window))
	       (vulpea-buffer-p))
      (save-excursion
	(goto-char (point-min))
	(let* ((tag-name (car tcpair))
	       (tags (vulpea-buffer-tags-get))
	       (original-tags tags)
	       (had-tag (member tag-name tags)))

          ;; Run checker and modify tags
          (if (funcall (cdr tcpair))
	      (setq tags (cons tag-name tags))
            (setq tags (remove tag-name tags)))

          ;; Cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; Update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags)

            ;; Run appropriate hooks
            (let ((now-has-tag (member tag-name tags)))
	      (cond
	       ;; Tag was added
	       ((and (not had-tag) now-has-tag)
		(run-hook-with-args 'tags/tag-added-hook tag-name (current-buffer)))
	       ;; Tag was removed
	       ((and had-tag (not now-has-tag))
		(run-hook-with-args 'tags/tag-removed-hook tag-name (current-buffer))))))))))

  (defun tags/org-update-all-tags ()
    (mapc #'tags/org-update-tag tag-checkers))

  (defmacro tags/make-db-searcher (tag)
    "Define the function to return a list of note files containing the specified TAG."
    (let ((func-name (intern (format "org-%s-files" tag))))
      `(defun ,func-name ()
	 ,(format "Return a list of note files containing the '%s' tag." tag)
	 (seq-uniq
	  (seq-map
	   #'car
	   (org-roam-db-query
	    [:select [nodes:file]
		     :from tags
		     :left-join nodes
		     :on (= tags:node-id nodes:id)
		     :where (like tag (quote ,(format "%%\"%s\"%%" tag)))]))))))

  (defvar-local tags/update-tags-enabled nil
    "Buffer-local variable to enable/disable tag updating.")

  (defvar tags/tag-pause nil
    "Global flag to pause tag updating during certain operations.")

  (defun tags/maybe-update-tags ()
    "Update tags if enabled for the current buffer."
    (when (and tags/update-tags-enabled
	       (not tags/tag-pause)
	       (not (member (buffer-name) prune/ignored-files))
	       (not (active-minibuffer-window))
	       (vulpea-buffer-p))
      (message "Updating tags!")
      (tags/org-update-all-tags)
      ))

  (defun tags/enable-tag-updating ()
    "Enable tag updating for the current buffer."
    (setq-local tags/update-tags-enabled t)
    (add-hook 'before-save-hook #'tags/maybe-update-tags nil t)
    (tags/maybe-update-tags))

  ;; Fixes weird tag insertion on extracting heading with `TODO' subheadings
  (defun tags/extract-subtree-with-tag-pause (orig-fun &rest args)
    "Pause tag updating during extraction, then update tags after."
    (let ((tags/tag-pause t))
      (apply orig-fun args))
    ;; Now we're in the new buffer, tags/tag-pause is nil again
    (message (concat "The current value is: " tags/tag-pause))
    (save-buffer))

  (advice-add 'org-roam-extract-subtree :around #'tags/extract-subtree-with-tag-pause)

  :hook
  (org-mode . tags/enable-tag-updating)

  ) ;;
;;; End of vulpea package block

;;; -> Org mode -> Anki

(use-package anki-editor
  :if (not (eq system-type 'android))
  :bind
  (:map org-mode-map
        ("C-c n p" . my/anki-flashcard-push-current-buffer)
        ("C-c n n p" . my/anki-flashcard-push-all))
  :after org
  :defer nil
  :vc (:url "https://github.com/anki-editor/anki-editor" :rev :newest)
  :custom
  (anki-editor-latex-style 'mathjax)
  (anki-editor-ignored-org-tags '("project" "flashcards" "ex-flashcards"))

  :config
  (defvar anki-tag-list '()
    "Keeps track of the most recently used flashcard tags.")

  (add-to-list 'closing-variables 'anki-tag-list)

  (defun anki/my/after-snippet-tag-handler ()
    "Select or create an Anki tag, prioritizing recent tags."
    (let* ((tag (completing-read "Enter tag: "
                                 (delete-dups (cons "" anki-tag-list))
                                 nil nil
                                 (car anki-tag-list))))
      (when (not (string-empty-p tag))
        (setq anki-tag-list (delete nil (cons tag (remove tag anki-tag-list)))))
      tag))

  (tags/make-db-searcher "flashcards")

  ;;; -> Org mode -> Anki -> Overrides

  (defcustom anki-editor-builtin-latex-environments '("tikzcd" "bprooftree" "prooftree" "logicproof")
    "LaTeX environments that will always be translated using Anki's built-in LaTeX.
This is useful for environments not supported by MathJax."
    :type '(repeat string))

  (defun anki-editor--contains-builtin-env (latex-code)
    "Check if LATEX-CODE contains any environment that should use builtin LaTeX."
    (cl-some (lambda (env)
               (string-match-p (format "\\\\begin{%s}" env) latex-code))
             anki-editor-builtin-latex-environments))

  (defun anki-editor--ox-latex (latex _contents _info)
    "Transcode LATEX from Org to HTML.
CONTENTS is nil. INFO is a plist holding contextual information."
    (let* ((code (org-remove-indentation (org-element-property :value latex)))
           (original-style anki-editor-latex-style)
           (contains-special-env (anki-editor--contains-builtin-env code)))
      (when (and (eq original-style 'mathjax) contains-special-env)
        (setq anki-editor-latex-style 'builtin))
      (setq code (cl-ecase (org-element-type latex)
                   (latex-fragment (anki-editor--translate-latex-fragment code))
                   (latex-environment (anki-editor--translate-latex-env code))))
      (setq anki-editor-latex-style original-style)
      (if anki-editor-break-consecutive-braces-in-latex
          (replace-regexp-in-string "}}" "} } " code)
        code)))

  (require 'js-anki-body-converter
           (expand-file-name "lisp/js-anki-body-converter.el" user-emacs-directory))

  (defun anki-editor-note-at-point ()
    "Make a note struct from current entry using block-aware field extraction.

Field sources, in priority order:

  Front : '** Front' subheading > #+attr_latex :options on primary block > heading
  Hint  : '** Hint' subheading  > first definition block > first theorem/lemma/etc block
  Back  : '** Back' subheading  > proof/solution blocks + residual text > raw body

See `js/anki-derive-fields' for full hierarchy details."
    ()
    (let* ((deck      (org-entry-get-with-inheritance anki-editor-prop-deck))
           (note-id   (org-entry-get nil anki-editor-prop-note-id))
           (hash      (org-entry-get nil anki-editor-prop-note-hash))
           (note-type (or (org-entry-get nil anki-editor-prop-note-type)
                          anki-editor-default-note-type))
           (tags      (cl-set-difference (anki-editor--get-tags)
                                         anki-editor-ignored-org-tags
                                         :test #'string=))
           (heading   (substring-no-properties (org-get-heading t t t t)))
           (body      (anki-editor--note-contents-before-subheading))
           (explicit-front nil)
           (explicit-hint  nil)
           (explicit-back  nil))
      (save-excursion
        (when (org-goto-first-child)
          (cl-loop
           for element    = (org-element-at-point)
           for subheading = (substring-no-properties
                             (org-element-property :raw-value element))
           for begin      = (save-excursion (anki-editor--skip-drawer element))
           for end        = (org-element-property :contents-end element)
           for content    = (and begin end
                                 (buffer-substring-no-properties
                                  begin (min (point-max) end)))
           when (string= subheading "Front") do (setq explicit-front content)
           when (string= subheading "Hint")  do (setq explicit-hint  content)
           when (string= subheading "Back")  do (setq explicit-back  content)
           while (org-get-next-sibling))))
      (let* ((derived (js/anki-derive-fields
                       heading body explicit-front explicit-hint explicit-back))
             (fields (sort (list (cons "Front" (plist-get derived :front))
                                 (cons "Hint"  (plist-get derived :hint))

                                 (cons "Back"  (plist-get derived :back)))
                           (lambda (a b) (string< (car a) (car b))))))
        (unless deck      (user-error "Missing deck"))
        (unless note-type (user-error "Missing note type"))
        (make-anki-editor-note :id     note-id
                               :model  note-type
                               :deck   deck
                               :tags   tags
                               :fields fields
                               :hash   hash
                               :marker (point-marker)))))

  ;;; -> Org mode -> Anki -> Push functions

  (defvar anki-flashcard-error-buffer "*Anki Flashcard Errors*"
    "Buffer name for displaying Anki flashcard push errors.")

  (defun anki-flashcard-clear-error-buffer ()
    "Clear the error buffer or create it if it doesn't exist."
    (with-current-buffer (get-buffer-create anki-flashcard-error-buffer)
      (erase-buffer)
      (insert "Anki Flashcard Push Results:\n\n")))

  (defun anki-flashcard-report-error (file error-msg)
    "Report ERROR-MSG for FILE in the error buffer."
    (with-current-buffer (get-buffer-create anki-flashcard-error-buffer)
      (goto-char (point-max))
      (insert (format "ERROR pushing %s:\n%s\n\n" file error-msg))))

  (defun my/anki-flashcard-push-current-buffer ()
    "Push current buffer's flashcards to Anki."
    (interactive)
    (if (buffer-file-name)
        (progn
          (anki-flashcard-clear-error-buffer)
          (save-buffer)
          (condition-case err
              (progn
                (save-excursion
		  (org-transclusion-mode +1)
		  (anki-editor-push-notes 'file))
                (message "Successfully pushed %s to Anki" (buffer-file-name)))
            (error
             (anki-flashcard-report-error (buffer-file-name) (error-message-string err))
             (message "Failed to push %s to Anki — see %s"
                      (buffer-file-name) anki-flashcard-error-buffer))))
      (message "Buffer is not visiting a file")))

  (defun my/anki-flashcard-push-all ()
    "Push all flashcard files (via org-roam tag index) to Anki."
    (interactive)
    (anki-flashcard-clear-error-buffer)
    (let* ((files (org-flashcards-files))
           (total (length files))
           (success 0))
      (dolist (file files)
        (condition-case err
            (with-current-buffer (find-file-noselect file)
	      (org-transclusion-mode +1)
              (save-excursion (anki-editor-push-notes 'file))
              (cl-incf success))
          (error (anki-flashcard-report-error file (error-message-string err)))))
      (if (< success total)
          (progn
            (message "Pushed %d/%d, %d errors — see %s"
                     success total (- total success) anki-flashcard-error-buffer)
            (display-buffer anki-flashcard-error-buffer))
        (message "Pushed all %d flashcard files" total)))))

;;; -> Org mode -> Agenda

(use-package org-agenda
  :ensure org
  :defer t
  :custom
  (org-todo-keywords
   '((sequence "NEXT(n)" "ACTIVE(a)" "COURSE(C)" "EXAM(E)" "PROJECT(P)"
	       "TODO(t)" "FINISH(f)" "PROCESS(p)" "EXPLORE(e)" "IDEA(I)" "HOLD(h)" "PACT(A)"
	       "|"
	       "DONE(d)" "CANCELLED(c)" "FAILED(F)" "NAREDU(N)")))

  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer nil)
  (org-agenda-start-on-weekday nil)
  (org-reverse-note-order nil)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-todo-list-sublevels t)
  (org-enforce-todo-dependencies t)
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-sticky t)
  (org-priority-default ?C)
  (org-agenda-bulk-persistent-marks t) ;; Process -> Refile
  (org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
  (org-agenda-hide-tags-regexp
   (regexp-opt '("lecture-notes" "noexport")))

  :bind (("C-c a" . open-org-agenda)
	 :map org-agenda-mode-map
	 ("o" . my/ace-link-agenda-current-line)
	 ("M-o" . my/ace-link-org-agenda-urls)
	 ;; This one doesn't change the view
	 ("g" . org-agenda-redo)
	 ("r" . roam-agenda-files-update)
	 ("<tab>" . outline-toggle-children)
	 ("<backtab>" . outline-cycle-buffer)
	 ("<return>" . org-agenda-goto)
	 ("S-<return>" . org-agenda-switch-to)
	 ("R" . js/agenda-refile)
	 ("O" . js/agenda-roamify)
	 )

  :config
  (add-to-list 'warning-suppress-types '(org-element))
  (tags/make-db-searcher "project")

  (defun roam-agenda-files-update (&rest _)
    (interactive)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (org-project-files))
    (message "Updated agenda files."))

  ;; Automatically update the agenda files to those roam entries with the `project' tag.
  (advice-add 'org-agenda :before #'roam-agenda-files-update)

  (defun open-org-agenda (&optional arg)
    (interactive "P")
    (if arg
	(call-interactively 'org-agenda)
      (org-agenda nil "d")))

  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.
     PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
	nil)))

  (defun air-org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
          subtree-end
	nil)))

  (defun air-org-skip-subtree-if-blocked ()
    "Skip an agenda subtree if the task is blocked by an incomplete child task."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (org-entry-blocked-p)
          subtree-end
	nil)))

  (defun air-org-skip-if-blocked ()
    "Skip the current task if it is currently blocked"
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max)))))
      (if (org-entry-blocked-p)
	  next-headline
	nil)))

  ;; I'll help you complete this function. Let me first check the Emacs org-mode functions for skipping and scheduling:Now let me look at the relevant functions to understand how to implement this:Now I understand. Here's the completed function:

  (defun js/org-skip-if-future ()
    "Skip the current task if it is scheduled in the future."
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max))))
          (scheduled (org-get-scheduled-time (point))))
      (if (and scheduled (> (time-to-seconds scheduled) (time-to-seconds (current-time))))
          next-headline
	nil)))

  (defvar js/org-ancestor-block-states '("HOLD" "DONE" "CANCELLED" "FAILED" "PROCESS" "EXPLORE")
    "TODO states that hide their descendants from agenda views.")

  (defun js/org-skip-if-ancestor-blocked (&optional extra-states)
    "Skip subtree if any ancestor has a state in `js/org-ancestor-block-states'.
EXTRA-STATES is an optional list of additional states to block on."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (block-states (append js/org-ancestor-block-states extra-states)))
      (save-excursion
	(while (and (not (bobp))
                    (not (org-at-heading-p)))
          (outline-previous-heading))
	(if (cl-loop while (and (not (bobp)) (org-up-heading-safe))
                     thereis (member (org-get-todo-state) block-states))
            subtree-end
          nil))))

  (setq org-agenda-custom-commands
	(quote (("u" alltodo ""
		 ((org-agenda-skip-function
		   (lambda nil
                     (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
					       (quote regexp) "\n]+>")))
		  (org-agenda-overriding-header "* Unscheduled TODO entries: ")))
		("p" "Process Items"
		 ((todo "PROCESS" ((org-agenda-overriding-header "* To process:  ")
				   (org-agenda-sorting-strategy '(category-up alpha-up))))))


		("d" "Daily agenda and all TODOs"
		 ((tags "PRIORITY=\"A\""
			((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			 (org-agenda-overriding-header "* High-priority:")))
		  (todo "NEXT" ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
							       (air-org-skip-subtree-if-priority ?B)
							       (air-org-skip-if-blocked)
							       (js/org-skip-if-ancestor-blocked)))
				(org-agenda-overriding-header "* Up next: ")))
		  (todo "ACTIVE" ((org-agenda-overriding-header "* Active projects: ")
				  (org-agenda-sorting-strategy '(deadline-up))))
		  (todo "PACT" ((org-agenda-overriding-header "* Pacts: ")
				(org-agenda-sorting-strategy '(deadline-up))
				(org-agenda-skip-function '(or (js/org-skip-if-ancestor-blocked)))))
		  (tags "PRIORITY=\"B\""
			((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			 (org-agenda-overriding-header "* Lower-priority:")))
		  (todo "COURSE" ((org-agenda-overriding-header "* Active courses: ")))
		  (todo "EXAM" ((org-agenda-overriding-header "* Looming exams: ")
				(org-agenda-sorting-strategy '(deadline-up))))

		  (todo "PROJECT" ((org-agenda-overriding-header "* Projects: ")))

		  ;; (agenda "" ((org-agenda-span 'week)))
		  (agenda "" ((org-agenda-span 'week)
			      (org-agenda-skip-function
			       '(or (org-agenda-skip-entry-if 'todo 'done)
				    (org-agenda-skip-entry-if 'todo '("PROCESS"))))))
		  (todo "FINISH" ((org-agenda-overriding-header "* Items to finish up:  ")
				  (org-agenda-sorting-strategy '(scheduled-up category-up alpha-up))))
		  (todo "PROCESS" ((org-agenda-overriding-header "* To process:  ")
				   (org-agenda-sorting-strategy '(scheduled-up category-up alpha-up))
				   (org-agenda-skip-function '(or (js/org-skip-if-future)))
				   ))
		  (alltodo "" ((org-agenda-skip-function
				'(or (air-org-skip-subtree-if-habit)
				     (air-org-skip-subtree-if-priority ?A)
				     (air-org-skip-subtree-if-priority ?B)
                                     (air-org-skip-if-blocked)
				     (org-agenda-skip-entry-if 'todo '("NEXT" "ACTIVE" "HOLD" "PROCESS" "EXPLORE" "PROJECT" "COURSE" "EXAM" "IDEA" "FINISH" "PACT"))
				     (js/org-skip-if-ancestor-blocked '("ACTIVE" "PROJECT"))
				     ))
			       (org-agenda-overriding-header "* All normal priority tasks:")))
		  (todo "IDEA" ((org-agenda-overriding-header "* Ideas: ")))
		  (todo "HOLD" ((org-agenda-overriding-header "* Currently on hold: ")))
		  (todo "EXPLORE" ((org-agenda-overriding-header "* Things to explore: ")))
		  )))))

  ;;; -> Org mode -> Agenda -> Refiling integration

  (defun js/agenda-refile ()
    "From org-agenda, refile the subtree into the selected org-roam node."
    (interactive)
    (let ((dest-node (org-roam-node-read nil nil nil 'require-match)))
      (org-agenda-with-point-at-orig-entry nil
	(org-roam-refile dest-node)))
    (next-line))

  (defun js/agenda-refile ()
    "Refile marked entries or the entry at point into the selected org-roam node.

If there are marked entries, refile all of them. Otherwise, refile
the current entry at point and move to the next line."
    (interactive)
    ;; If no marks exist, mark the current entry for single-entry refile
    (if (not org-agenda-bulk-marked-entries)
	(save-excursion (org-agenda-bulk-mark)))

    (let ((dest-node (org-roam-node-read nil nil nil 'require-match)))
      (dolist (marker (reverse org-agenda-bulk-marked-entries))
	;; Navigate to each marked entry and refile it
	(when (and (markerp marker)
                   (marker-buffer marker)
                   (buffer-live-p (marker-buffer marker))
                   (marker-position marker))
          (with-current-buffer (marker-buffer marker)
            (goto-char (marker-position marker))
            (org-roam-refile dest-node)))))

    ;; Clear marks and move cursor
    (org-agenda-bulk-unmark-all)
    (next-line))

  (defun js/agenda-roamify ()
    "Roamify URL at point from agenda."
    (interactive)
    (org-agenda-with-point-at-orig-entry nil
      (end-of-line)
      (call-interactively #'js/roamify-url-at-point)))

  )

;; In init.el with use-package:
(use-package js-triage-session
  :after org-agenda
  :load-path "~/.emacs.d/lisp"
  :bind
  (("C-c n j b p" . js/session-process)
   ("C-c n j b P" . js/session-project)
   ;; TODO: Keywords don't work since they have the keybind
   ;; ("C-c n j b k" . js/session-keyword)
   ("C-c n j b r" . js/session-review)
   ("C-c n j b a" . js/session-all)
   ("C-c n j b b" . js/session-buffer)
   ("C-c n j b s" . js/session-subtree-notodo)

   ("C-c n j n" . js/triage-next)
   ("C-c n j p" . js/triage-goto-prev)
   ("C-c n j d" . js/triage-done)
   ("C-c n j c" . js/triage-cancel)
   ("C-c n j s" . js/triage-snooze)
   ("C-c n j S" . js/triage-manual)
   ("C-c n j q" . js/triage-quit)
   ("C-c n j ." . js/triage-goto-current)
   ("C-c n j ," . js/triage-status)

   ("C-c n j j" . js/triage-hydra/body)
   )
  :config
  (defhydra js/triage-hydra
    (:color pink :hint nil)
    "
^Move^              ^Action^             ^Meta^
^^-----------------^^------------------^^-----------
_n_ext              _t_odo               _q_uit
_p_rev              _o_pen URLs          _._ current
_d_one              _r_oam refile
_c_ancel            _R_oamify URL
_s_ooner            _w_org refile
_l_ater
_S_manual
"
    ("." js/triage-goto-current)
    ("," js/triage-status)
    ("n" js/triage-next)
    ("p" js/triage-goto-prev)
    ("d" js/triage-done)
    ("c" js/triage-cancel)
    ("s" js/triage-snooze-soon)
    ("l" js/triage-snooze-later)
    ("S" js/triage-manual)
    ("t" org-todo)
    ("r" org-roam-refile)
    ("w" org-refile)
    ("R" js/roamify-url-at-point :exit t)
    ("o" open-urls-at-point-or-region)
    ("q" js/triage-quit :color blue))
  )


;;; End of org agenda package block

;;; -> Org mode -> Babel

(use-package org ;; babel
  :defer t
  :custom
  (org-babel-python-command "python3")
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  :config
  (require 'org-tempo)
  (require 'ob-haskell)



  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (C . t)
     (haskell . t)
     (ocaml . t)
     (octave . t)
     (awk . t)
     (shell . t)
     ))


  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("hl" . "src haskell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python :results output"))
  (add-to-list 'org-structure-template-alist '("oc" . "src ocaml"))
  (add-to-list 'org-structure-template-alist '("cs" . "src C"))
  (add-to-list 'org-structure-template-alist '("md" . "src markdown"))
  (add-to-list 'org-structure-template-alist '("defa" . "definicija"))
  (add-to-list 'org-structure-template-alist '("def" . "definition"))
  (add-to-list 'org-structure-template-alist '("izr" . "izrek"))
  (add-to-list 'org-structure-template-alist '("thm" . "theorem"))
  (add-to-list 'org-structure-template-alist '("prop" . "proposition"))
  (add-to-list 'org-structure-template-alist '("pf" . "proof"))
  (add-to-list 'org-structure-template-alist '("trd" . "trditev"))
  (add-to-list 'org-structure-template-alist '("lem" . "lema"))
  (add-to-list 'org-structure-template-alist '("lemm" . "lemma"))
  (add-to-list 'org-structure-template-alist '("abst" . "abstract"))
  (add-to-list 'org-structure-template-alist '("ex" . "lexample"))
  (add-to-list 'org-structure-template-alist '("item" . "itemize"))
  (add-to-list 'org-structure-template-alist '("cor" . "corollary"))
  (add-to-list 'org-structure-template-alist '("def" . "definition"))
  (add-to-list 'org-structure-template-alist '("rem" . "remark"))
  (add-to-list 'org-structure-template-alist '("ax" . "axiom"))  ; you have several axioms
  )

;;; -> Org Mode -> org-static-blog
;;; Blog + Personal website configuration

(use-package htmlize
  :ensure t
  :custom
  (org-html-htmlize-output-type 'css))

(use-package org-static-blog
  :after org-roam
  :custom
  (org-static-blog-publish-title "My Blog")
  (org-static-blog-publish-url "https://www.jure-smolar.com/")
  (org-static-blog-publish-directory "~/Documents/blog/")
  (org-static-blog-posts-directory org-roam-directory)
  (org-static-blog-drafts-directory org-roam-directory)
  (org-static-blog-enable-tags t)
  (org-export-with-toc nil)

  ;; This is destructive.
  ;; (org-export-with-section-numbers nil)

  (org-static-blog-use-preview t)
  (org-static-blog-enable-tag-rss t)
  (org-static-blog-enable-og-tags t)

  (org-static-blog-page-header
   "<meta name=\"author\" content=\"Jure Smolar\">
    <meta name=\"referrer\" content=\"no-referrer\">
    <meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">
    <link href=\"static/sakura.css\" rel=\"stylesheet\" type=\"text/css\" />
    <link href=\"static/custom.css\" rel=\"stylesheet\" type=\"text/css\" />
    <link rel=\"icon\" href=\"static/favicon.ico\">
    <script src=\"static/mathjax-config.js\"></script>
    <script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script>

    <script>
    document.addEventListener('DOMContentLoaded', function () {
						  const btn = document.createElement('button');
						  btn.className = 'js-toggle-headings';
						  btn.textContent = 'expand all';

						  let allOpen = false;
						  btn.addEventListener('click', function () {
										allOpen = !allOpen;
										document.querySelectorAll('details').forEach(function (d) {
																      allOpen ? d.setAttribute('open', '') : d.removeAttribute('open');
																      });
										btn.textContent = allOpen ? 'collapse all' : 'expand all';
										});

						  // Insert after the first h1 on the page
						  const h1 = document.querySelector('h1');
						  if (h1) h1.insertAdjacentElement('afterend', btn);
						  });
    </script>
    ")

  (org-static-blog-page-preamble
   "<div style=\"display:none\">\\(\\newenvironment{bprooftree}{\\begin{prooftree}}{\\end{prooftree}}\\)</div>
    <nav class=\"header\">
    <div class=\"header-left\">
    <a href=\"/\">Home</a>
    <a href=\"/archive.html\">Archive</a>
    <a href=\"/tags.html\">Tags</a>
    <a href=\"/rss.xml\">RSS</a>
    <a href=\"/lecture-notes.html\">Lecture Notes</a>
    </div>
    <div class=\"header-right\">
    <a href=\"/about.html\">About</a>
    </div>
    </nav>")

  (org-static-blog-page-postamble nil)

  (org-static-blog-index-front-matter "Recent posts.")

  :config
  (defvar orb-ignored-tags '("blog" "note" "project" "flashcards" "blog-static-page" "draft")
    "Tags used for file management that shouldn't appear on the blog.")

  (defvar blog-tags '("blog")
    "Org-roam tags that mark nodes as published blog posts.")

  (defvar static-tags '("blog-static-page" "draft" "lecture-notes")
    "Org-roam tags that mark nodes as static pages, drafts, or lecture notes.")

  ;; These tags should not be inherited to facilitate future subtree publishing
  (add-to-list 'org-tags-exclude-from-inheritance "blog")
  (add-to-list 'org-tags-exclude-from-inheritance "note")
  (add-to-list 'org-tags-exclude-from-inheritance "lecture-notes")

  (defun js/tags->or-clause (tags)
    "Build an emacsql :where clause matching tags:tag against any tag in TAGS.
Returns e.g. (or (= tags:tag \"blog\") (= tags:tag \"note\"))."
    (cons 'or (mapcar (lambda (tag) `(= tags:tag ,tag)) tags)))

  ;; Override to use org-roam query instead of subfolders
  (defun org-static-blog-get-post-filenames ()
    "Get blog posts from org-roam nodes tagged with any tag in `blog-tags'."
    (delete-dups
     (mapcar #'car
             (org-roam-db-query
              `[:select :distinct [nodes:file] :from nodes
                        :inner-join tags :on (= tags:node-id nodes:id)
                        :where ,(js/tags->or-clause blog-tags)]))))

  (defun org-static-blog-get-draft-filenames ()
    "Get static pages from org-roam nodes tagged with any tag in `static-tags'."
    (mapcar #'car
            (org-roam-db-query
             `[:select :distinct [nodes:file] :from nodes
                       :inner-join tags :on (= tags:node-id nodes:id)
                       :where ,(js/tags->or-clause static-tags)])))

  (defun org-static-blog-get-tags (post-filename)
    "Extract tags from POST-FILENAME, excluding management tags."
    (let ((case-fold-search t)
          (all-tags nil))
      (with-temp-buffer
        (insert-file-contents post-filename)
        (goto-char (point-min))
        (when (or (search-forward-regexp "^\\#\\+filetags:[ ]*:\\(.*\\):$" nil t)
                  (search-forward-regexp "^\\#\\+filetags:[ ]*\\(.+\\)$" nil t))
          (setq all-tags (if (match-string 1)
                             (split-string (match-string 1) ":")
                           (split-string (match-string 1))))))
      (cl-remove-if (lambda (tag)
                      (member (downcase tag) orb-ignored-tags))
                    all-tags)))

  (defun my/org-static-blog-link (link desc info)
    "Transcode ID links to proper blog post URLs.
Falls back to standard org-html-link for other link types."
    (if (not (string= (org-element-property :type link) "id"))
        (org-html-link link desc info)
      (let* ((id (org-element-property :path link))
             (node (org-roam-node-from-id id))
             (tags (and node (org-roam-node-tags node)))
             (published-p (and tags (seq-intersection tags (append blog-tags static-tags))))
             (fallback-desc (if node (org-roam-node-title node) id)))
        (if published-p
            (format "<a href=\"/%s\">%s</a>"
                    (org-static-blog-get-post-public-path (org-roam-node-file node))
                    (or desc (org-roam-node-title node)))
          (format "<a href=\"broken-link.html\" class=\"broken-link\">%s</a>"
                  (or desc fallback-desc))))))

  ;; Redefine the backend every time before rendering
  (defun my/setup-blog-backend (&rest _args)
    "Ensure our custom link and tikzcd handlers are in the backend."
    (org-export-define-derived-backend 'org-static-blog-post-bare 'html
      :translate-alist '((template . (lambda (contents info) contents))
                         (link . my/org-static-blog-link))))

  ;; Turn on transclusions before exporting
  (defun org-static-blog-render-post-content (post-filename)
    "Render blog content as bare HTML without header."
    (let ((org-html-doctype "html5")
          (org-html-html5-fancy t))
      (save-excursion
	(let ((current-buffer (current-buffer))
              (buffer-exists (org-static-blog-file-buffer post-filename))
              (result nil))
          (with-temp-buffer
            (if buffer-exists
		(insert-buffer-substring buffer-exists)
              (insert-file-contents post-filename))
            (org-mode)
	    (org-transclusion-mode +1)
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (setq org-map-continue-from (point))
               (org-cut-subtree))
             org-static-blog-no-post-tag)
            (setq result
                  (org-export-as 'org-static-blog-post-bare nil nil nil nil))
            (switch-to-buffer current-buffer)
            result)))))

  ;; Hook into the render function
  (advice-add 'org-static-blog-render-post-content :before #'my/setup-blog-backend)

  (defvar js/tikzcd-svg-directory "diagrams/"
    "Directory for tikzcd SVG files, relative to org file.")

  (defun js/tikzcd-to-svg ()
    "Render tikzcd environment at point to SVG and insert link.
Expects cursor to be inside a \\begin{tikzcd}...\\end{tikzcd} block."
    (interactive)
    (save-excursion
      (let* ((start (progn (search-backward "\\begin{tikzcd}") (point)))
             (end (progn (search-forward "\\end{tikzcd}") (point)))
             (tikzcd-code (buffer-substring-no-properties start end))
             (filename (read-string "SVG filename (without extension): "))
             (svg-file (concat filename ".svg"))
             (temp-dir (make-temp-file "tikzcd" t))
             (temp-tex (expand-file-name "diagram.tex" temp-dir))
             (temp-pdf (expand-file-name "diagram.pdf" temp-dir))
             (temp-svg (expand-file-name "diagram.svg" temp-dir))
             (org-dir (file-name-directory (buffer-file-name)))
             (svg-dir (expand-file-name js/tikzcd-svg-directory org-dir))
             (output-path (expand-file-name svg-file svg-dir))
             (relative-link (concat js/tikzcd-svg-directory svg-file)))

        ;; Create output directory if it doesn't exist
        (unless (file-exists-p svg-dir)
          (make-directory svg-dir t))

        ;; Write LaTeX file
        (with-temp-file temp-tex
          (insert "\\documentclass[border=2pt]{standalone}\n")
          (insert "\\usepackage{tikz-cd}\n")
          (insert "\\usepackage{amsmath}\n")
          (insert "\\usepackage{amssymb}\n")
          (insert "\n\\begin{document}\n")
          (insert tikzcd-code)
          (insert "\n\\end{document}\n"))

        ;; Compile to PDF in temp directory
        (message "Compiling LaTeX...")
        (shell-command (format "cd %s && pdflatex -interaction=nonstopmode diagram.tex"
                               temp-dir))

        ;; Convert to SVG and copy to destination
        (if (file-exists-p temp-pdf)
            (progn
              (message "Converting to SVG...")
              (shell-command (format "pdf2svg %s %s" temp-pdf temp-svg))

              (if (file-exists-p temp-svg)
                  (progn
                    (copy-file temp-svg output-path t)
                    (delete-directory temp-dir t)
                    (goto-char end)
                    (insert (format "\n\n[[file:%s]]\n" relative-link))
                    (message "Created %s" output-path))
                (progn
                  (delete-directory temp-dir t)
                  (error "SVG conversion failed"))))
          (progn
            (delete-directory temp-dir t)
            (error "PDF compilation failed"))))))

  (defun js/sync-blog (arg)
    "Sync blog to muffalo server.

No prefix: do not update the 'static/' directory on the remote.
With C-u: include the 'static/' directory (push local static to remote).
With C-u C-u: pull the 'static/' directory from the remote to local."
    (interactive "P")
    (let* ((local (expand-file-name "~/Documents/blog/"))
           (remote "jure@muffalo:~/blog/")
           (cmd (cond
                 ((null arg)
                  (format "rsync -avz --delete --exclude 'static/' %s %s" local remote))
                 ((and arg (= (prefix-numeric-value arg) 16))
                  (format "rsync -avz --delete %s %s" (concat remote "static/") (concat local "static/")))
                 (t
                  (format "rsync -avz --delete %s %s" local remote)))))
      (compile cmd)))

  (defun js/blog-open-in-browser ()
    "Open the current org-roam file as its published blog URL."
    (interactive)
    (let* ((file (buffer-file-name))
           (public-path (org-static-blog-get-post-public-path file))
           (url (concat org-static-blog-publish-url public-path)))
      (browse-url url))))

;;; End of org-static-blog code block.

;;; -> Org Mode -> Open Street map

(use-package osm
  :bind-keymap ("C-c n m" . osm-prefix-map) ;; Alternatives: `osm-home' or `osm'
  :bind (:map osm-prefix-map
	      ("o" . my/open-heading-coords-in-osm))
  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information

  :config
  ;; Add custom servers, see also https://github.com/minad/osm/wiki
  ;; (osm-add-server 'myserver
  ;;   :name "My tile server"
  ;;   :group "Custom"
  ;;   :description "Tiles based on aerial images"
  ;;   :url "https://myserver/tiles/%z/%x/%y.png?apikey=%k")

  (defun my/open-heading-coords-in-osm ()
    "Extract coordinates from heading properties and open in OSM."
    (interactive)
    (save-excursion
      (while (and (not (org-at-heading-p)) (not (bobp)))
	(outline-previous-heading))
      (when (org-at-heading-p)
	(let ((lat nil)
	      (lon nil))
          ;; Search for latitude and longitude in properties
          (let ((props (org-entry-properties nil 'standard)))
            (setq lat (cdr (assoc "LATITUDE" props)))
            (setq lon (cdr (assoc "LONGITUDE" props))))

          (if (and lat lon)
	      (progn
		;; Convert to numbers if they contain commas (European format)
		(when (string-match "," lat)
                  (setq lat (replace-regexp-in-string "," "." lat)))
		(when (string-match "," lon)
                  (setq lon (replace-regexp-in-string "," "." lon)))

		(message "Opening coordinates: %s, %s" lat lon)
		(osm-goto (string-to-number lat) (string-to-number lon) 15))
            (message "No coordinates found in heading properties"))))))
  )

;;; --> Elfeed

(use-package elfeed
  :defines
  elfeed-search-mode-map
  elfeed-show-mode-map
  :defer t
  :bind (("C-x w" . elfeed)
	 :map elfeed-search-mode-map
         ("SPC" . elfeed-search-show-entry)
	 ("t" . elfeed-search-trash)
         ("T" . elfeed-filter-trash)
	 ("A" . elfeed-filter-asmr)
	 ("P" . elfeed-filter-papers)
         ("i" . open-downloaded-youtube-in-iina)
         ("I" . download-selected-youtube-videos)
         ("D" . elfeed-filter-downloaded)
	 ("s" . my/elfeed-show-default)
	 ("U" . js/log-elfeed-process)
	 ("B" . elfeed-browse-with-secondary-browser)
	 ("W" . js/elfeed-entries-to-wallabag)
	 ("O" . js/elfeed-entries-to-podcastify)
	 ("<wheel-up>" . previous-line)
	 ("<wheel-down>" . next-line)

	 ("y" . nil)
	 ("y y" . elfeed-search-yank)
	 ("y g" . js/elfeed-yank-entry-content)

	 ("V" . elpapers-ingest-full)
	 ("K" . elpapers-semantic-search)

         :map elfeed-show-mode-map
         ("SPC" . elfeed-scroll-up-command)
         ("S-SPC" . elfeed-scroll-down-command)
	 ("U" . js/log-elfeed-process)
         ;; If called with C-u then bring up the capture buffer
	 ("t" . elfeed-show-trash)
         ("i" . open-downloaded-youtube-in-iina)
	 ("M-o" . ace-link-safari)
	 ("B" . elfeed-browse-with-secondary-browser)
	 ("W" . js/elfeed-entries-to-wallabag)
	 ("O" . js/elfeed-entries-to-podcastify)
	 ("V" . elpapers-ingest-full)

	 ("y" . nil)
	 ("y y" . elfeed-show-yank)
	 ("y g" . js/elfeed-yank-entry-content)
         )
  :hook
  (elfeed-show-mode . mixed-pitch-mode)
  (elfeed-show-mode . visual-line-mode)
  (elfeed-show-mode . efs/org-mode-visual-fill)
  (elfeed-search-mode . my/setup-elfeed-scroll)
  ;; (elfeed-search-mode . my/elfeed-setup-local-activation-hooks)
  :config
  ;; Note: the following function looks useless since deleting an item will just refetch it.
  ;; I'm using it for testing some RSS feeds and such.
  (defun js/elfeed-delete-selected-entries ()
    "Delete selected elfeed entries from the database."
    (interactive)
    (elfeed-db-ensure)
    (let ((entries (elfeed-search-selected)))
      (dolist (entry entries)
	(let ((id (elfeed-entry-id entry)))
          ;; avl-tree-delete first: its comparator (elfeed-db-compare)
          ;; calls elfeed-db-get-entry, so the hash must still have the entry
          (avl-tree-delete elfeed-db-index id)
          (remhash id elfeed-db-entries)))
      (elfeed-db-save)
      (elfeed-search-update :force)
      (message "Deleted %d entries" (length entries))))

  (defun js/elfeed-yank-entry-content ()
    "Yank the full content of the current entry or selected entries.
    In elfeed-search-mode, works on selected entries (or entry at point).
    In elfeed-show-mode, works on the current entry.
    If multiple entries are selected, concatenates their content."
    (interactive)
    nil
    )

  ;; Variables
  (setq-default elfeed-search-filter "-trash -asmr @7-days-ago +unread")

  ;; Functions
  (defun elfeed-scroll-up-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-up-command arg)
        (error (elfeed-show-next)))))

  (defun elfeed-scroll-down-command (&optional arg)
    "Scroll down or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-down-command arg)
        (error (elfeed-show-prev)))))

  ;; Elfeed mouse support
  (defun my/setup-elfeed-scroll ()
    "Set up scroll bindings for elfeed-show-mode with adjusted sensitivity."
    (let ((map (make-sparse-keymap)))
      ;; Create custom scroll functions with reduced sensitivity
      (defun my/elfeed-previous-line (event)
	(interactive "e")
	;; Extract the delta from the event
	(when-let* ((delta (and (nth 4 event)
				(round (cdr (nth 4 event)))))
                    ;; Reduce sensitivity by dividing the delta
                    (adjusted-delta (/ delta 5)))  ;; Adjust this divisor as needed
          (unless (zerop adjusted-delta)
            (previous-line (max 1 (abs adjusted-delta))))))

      (defun my/elfeed-next-line (event)
	(interactive "e")
	;; Extract the delta from the event
	(when-let* ((delta (and (nth 4 event)
				(round (cdr (nth 4 event)))))
                    ;; Reduce sensitivity by dividing the delta
                    (adjusted-delta (/ delta 4)))  ;; Adjust this divisor as needed
          (unless (zerop adjusted-delta)
            (next-line (max 1 (abs adjusted-delta))))))

      ;; Define our local wheel bindings with adjusted sensitivity
      (define-key map (kbd "<wheel-up>") #'my/elfeed-previous-line)
      (define-key map (kbd "<wheel-down>") #'my/elfeed-next-line)
      (define-key map (kbd "<mouse-2>") #'elfeed-search-browse-url)

      ;; Override the mode-specific map
      (setq-local minor-mode-overriding-map-alist
                  (cons (cons pixel-scroll-precision-mode map)
			minor-mode-overriding-map-alist))))

  ;; Give a visual indicator of the filter being cleared.
  ;; Helps with an inbox zero approach to elfeed.
  (advice-add 'elfeed-search-clear-filter
	      :after (lambda () (message "Clearing filter.")))

  (defun elfeed-filter-maker (filter &optional message)
    "Sets the elfeed search filter and displays a message if there is one."
    (elfeed-search-set-filter filter)
    (when message (message message)))

  ;; Trash
  (defun my/elfeed-show-default () ; s
    "Set Elfeed search filter to exclude 'trash' tagged entries and start live filtering."
    (interactive)
    (setq elfeed-search-filter "-trash @7-days-ago ")
    (elfeed-search-update :force)
    (elfeed-search-live-filter))

  (defun my/elfeed-show-non-trash--no-search ()
    (interactive)
    (setq elfeed-search-filter "-trash @7-days-ago ")
    (elfeed-search-update :force))

  (defun elfeed-filter-trash ()
    (interactive)
    (elfeed-filter-maker "+trash @7-days-ago" "Showing trashed."))

  (defun elfeed-search-trash ()
    "Tags the selection as trash in search mode."
    (interactive)
    (elfeed-search-toggle-all 'trash))

  (defun elfeed-show-trash ()
    "Tags the item as trash and moves on to the next item in show mode."
    (interactive)
    (elfeed-show-tag 'trash)
    (elfeed-show-next))

  ;; ASMR
  (defun elfeed-filter-asmr ()
    "Open up the asmr tagged feed."
    (interactive)
    (elfeed-filter-maker "-trash +asmr @1-months-ago" "Showing ASMR."))

  (defun elfeed-filter-papers ()
    "Open up the papers tagged feed."
    (interactive)
    (elfeed-filter-maker "-trash +papers @1-months-ago" "Showing papers."))

  ;;; Modified so we can search both by feed name and author.
  (defun elfeed-search-compile-filter (filter)
    "Compile FILTER into a lambda function for `byte-compile'.

    Executing a filter in bytecode form is generally faster than
    \"interpreting\" the filter with `elfeed-search-filter'."
    (cl-destructuring-bind (&key after     before
				 must-have must-not-have
				 matches   not-matches
				 feeds     not-feeds
				 limit &allow-other-keys)
	filter
      `(lambda (,(if (or after matches not-matches must-have must-not-have feeds not-feeds)
                     'entry
                   '_entry)
		,(if (or feeds not-feeds)
                     'feed
                   '_feed)
		,(if limit
                     'count
                   '_count))
	 (let* (,@(when after
                    '((date (elfeed-entry-date entry))
		      (age (- (float-time) date))))
		,@(when (or must-have must-not-have)
                    '((tags (elfeed-entry-tags entry))))
		,@(when (or matches not-matches)
                    '((title (or (elfeed-meta entry :title)
				 (elfeed-entry-title entry)))
		      (link (elfeed-entry-link entry))))
		,@(when (or feeds not-feeds)
                    '((feed-id (elfeed-feed-id feed))
		      (feed-title (or (elfeed-meta feed :title)
				      (elfeed-feed-title feed)
				      ""))
		      (author-names (mapconcat (lambda (au) (plist-get au :name))
					       (elfeed-meta entry :authors)
					       " "))
		      )))
           ,@(when after
	       `((when (> age ,after)
                   (elfeed-db-return))))
           ,@(when limit
	       `((when (>= count ,limit)
                   (elfeed-db-return))))
           (and ,@(cl-loop for forbid in must-not-have
                           collect `(not (memq ',forbid tags)))
		,@(cl-loop for forbid in must-have
                           collect `(memq ',forbid tags))
		,@(cl-loop for regex in matches collect
                           `(or (string-match-p ,regex title)
				(string-match-p ,regex link)))
		,@(cl-loop for regex in not-matches collect
                           `(not
                             (or (string-match-p ,regex title)
				 (string-match-p ,regex link))))

		;; Every = entry must be matched by either feed or title.
		,@(when feeds
		    `((and
		       ,@(cl-loop for regex in feeds
				  collect `(or (string-match-p ,regex author-names)
					       (string-match-p ,regex feed-id)
					       (string-match-p ,regex feed-title))))))
		,@(when not-feeds
                    `((not
		       (or ,@(cl-loop
			      for regex in not-feeds
			      collect `(string-match-p ,regex feed-id)
			      collect `(string-match-p ,regex feed-title)
			      collect `(string-match-p ,regex author-names))))))
		,@(when before
                    `((> age ,before))))))))

  (defun elfeed-browse-with-secondary-browser ()
    "Visit the current entry in the secondary browser."
    (interactive)
    (let ((browse-url-browser-function browse-url-secondary-browser-function))
      (if (derived-mode-p 'elfeed-show-mode)
          (browse-url (elfeed-entry-link elfeed-show-entry))
	(elfeed-search-browse-url))))


;;; -> Elfeed -> Wallabag integration
  (defun js/elfeed-entries-to-wallabag (&optional entries)
    "Add elfeed entries to wallabag and sync to server.
    If ENTRIES is provided, use those instead of the selected entries.
    In show mode, adds the current entry; in search mode, adds all selected entries."
    (interactive)
    (let ((entries
           (cond
            (entries entries)
            ((derived-mode-p 'elfeed-show-mode) (list elfeed-show-entry))
            ((derived-mode-p 'elfeed-search-mode) (elfeed-search-selected))
            (t (user-error "Not in an Elfeed buffer or no entries provided"))))
          (added-count 0))
      (when entries
	(wallabag-request-token))
      ;; Process each entry
      (dolist (entry entries)
	(let ((url (elfeed-entry-link entry))
	      (title (elfeed-entry-title entry)))
          (if url
	      (progn
		(message "Adding to wallabag: %s" title)
		(wallabag-add-entry url "")
		(cl-incf added-count))
            (message "No URL found for entry: %s" title))))

      ;; Sync changes to server if we added anything
      (when (> added-count 0)
	(message "Syncing %d entries to wallabag server..." added-count)
	(run-with-timer 2 nil #'wallabag-request-and-synchronize-entries))

      (message "Added %d entries to wallabag" added-count)))

  ;;; -> Elfeed -> Podcastify integration
  (defvar my/elfeed-podcastify-feed-rules nil
    "List of (feed-name . predicate-function) pairs for automatic feed classification.
Each predicate function should take an elfeed entry and return non-nil if the entry
should go to that feed. Rules are checked in order, first match wins.
Falls back to 'default' feed if no rules match.

Example configuration:
  (setq my/elfeed-podcastify-feed-rules
    '((\"asmr\" . my/elfeed-entry-has-asmr-tag-p)
      (\"tech-channels\" . (lambda (entry)
                            (my/elfeed-entry-from-channel-p entry '(\"TechChannel\" \"CodeTube\"))))
      (\"music\" . (lambda (entry)
                     (string-match-p \"music\\\\|song\\\\|audio\"
                                   (downcase (elfeed-entry-title entry)))))))")

  (defun my/elfeed-entry-has-asmr-tag-p (entry)
    "Return t if elfeed ENTRY has asmr tag."
    (member 'asmr (elfeed-entry-tags entry)))

  (setq my/elfeed-podcastify-feed-rules
	'(("asmr" . my/elfeed-entry-has-asmr-tag-p)
	  ))


  (defun my/podcastify-determine-feed (entry)
    "Determine which feed an elfeed ENTRY should go to based on feed rules.
Returns the feed name, defaulting to 'default' if no rules match."
    (or (cl-loop for (feed-name . predicate-fn) in my/elfeed-podcastify-feed-rules
                 when (funcall predicate-fn entry)
                 return feed-name)
        "default"))

  ;;   (defun my/elfeed-entry-from-channel-p (entry channel-patterns)
  ;;     "Return t if elfeed ENTRY is from a channel matching any of CHANNEL-PATTERNS.
  ;; CHANNEL-PATTERNS should be a list of strings or regexps to match against feed title."
  ;;     (let ((feed-title (elfeed-feed-title (elfeed-entry-feed entry))))
  ;;       (cl-some (lambda (pattern)
  ;;                  (string-match-p pattern (or feed-title "")))
  ;;                channel-patterns)))

  (defun js/elfeed-entries-to-podcastify (&optional prompt-for-feed entries feed-name)
    "Send elfeed entries to podcastify and mark as read.
PROMPT-FOR-FEED when non-nil (or called with C-u), prompts for feed selection.
If ENTRIES is provided, use those instead of the selected entries.
FEED-NAME specifies which feed to add videos to.
In show mode, adds the current entry; in search mode, adds all selected entries."
    (interactive "P")
    (let* ((entries
            (cond
             (entries entries)
             ((derived-mode-p 'elfeed-show-mode) (list elfeed-show-entry))
             ((derived-mode-p 'elfeed-search-mode) (elfeed-search-selected))
             (t (user-error "Not in an Elfeed buffer or no entries provided"))))
           ;; Handle feed selection logic
           (feed (cond
                  ;; If prompt-for-feed (C-u was used), prompt user
                  (prompt-for-feed
                   (let* ((rule-feeds (mapcar 'car my/elfeed-podcastify-feed-rules))
                          (all-feeds (cl-remove-duplicates
				      (append rule-feeds '("default"))
				      :test 'string=)))
                     (completing-read "Podcastify feed: " all-feeds nil nil nil nil "default")))
                  ;; If feed-name was explicitly provided, use it
                  (feed-name feed-name)
                  ;; Otherwise, use automatic classification for first entry
                  (t (my/podcastify-determine-feed (car entries)))))
           (added-count 0)
           (failed-count 0))

      ;; Show which feed is being used
      (unless prompt-for-feed
        (message "Using podcastify feed: %s" feed))

      ;; Process each entry
      (dolist (entry entries)
	(let ((url (elfeed-entry-link entry))
	      (title (elfeed-entry-title entry)))
          (if (and url (string-match-p "youtube\\.com\\|youtu\\.be" url))
	      (condition-case err
                  (progn
                    (message "Adding to podcastify: %s" title)
                    ;; Send to podcastify API
                    (url-retrieve
                     (format "http://localhost:8081/add?url=%s&feed=%s"
                             (url-encode-url url)
                             (url-encode-url feed))
                     (lambda (status)
		       (if (plist-get status :error)
                           (message "Failed to add %s to podcastify: %s"
                                    title (plist-get status :error))
			 (message "Successfully added %s to podcastify" title)))
                     nil nil t)

                    ;; Mark entry as read in elfeed
                    (elfeed-untag entry 'unread)
                    (elfeed-tag entry 'podcastify)

                    ;; Move point to next entry in search mode
                    (when (derived-mode-p 'elfeed-search-mode)
		      (forward-line 1))

                    (cl-incf added-count))
		(error
		 (message "Error processing %s: %s" title (error-message-string err))
		 (cl-incf failed-count)))
            (message "Skipping non-YouTube entry: %s" title))))

      ;; Update elfeed display
      (when (derived-mode-p 'elfeed-search-mode)
	(elfeed-search-update--force))

      ;; Show summary
      (if (> failed-count 0)
          (message "Added %d entries to podcastify (feed: %s), %d failed"
                   added-count feed failed-count)
	(message "Added %d entries to podcastify (feed: %s)" added-count feed))))

  (defun my/podcastify-add-link ()
    "Interactively add a link to podcastify.
Prompts for a URL and feed name, then adds the link to the specified podcastify feed."
    (interactive)
    (let* ((url (read-string "Enter URL to add to podcastify: "))
           (rule-feeds (mapcar 'car my/elfeed-podcastify-feed-rules))
           (all-feeds (cl-remove-duplicates
		       (append rule-feeds '("default"))
		       :test 'string=))
           (feed (completing-read "Podcastify feed: " all-feeds nil nil nil nil "default")))

      (if (string-empty-p url)
          (message "URL cannot be empty")
        (condition-case err
            (progn
	      (message "Adding %s to podcastify feed: %s" url feed)
	      (url-retrieve
	       (format "http://localhost:8081/add?url=%s&feed=%s"
		       (url-encode-url url)
		       (url-encode-url feed))
	       (lambda (status)
                 (if (plist-get status :error)
                     (message "Failed to add to podcastify: %s"
			      (plist-get status :error))
                   (message "Successfully added to podcastify")))
	       nil nil t))
          (error
           (message "Error adding to podcastify: %s" (error-message-string err)))))))

;;; -> Elfeed -> Deluge integration

  (defvar deluge-watch-dir (expand-file-name "~/Downloads/")
    "The folder into which to download the .torrent files.")

  (defun js/elfeed-entries-to-deluge (&optional entries)
    "Download .torrent files from elfeed entries to Deluge watch folder.
If ENTRIES is provided, use those instead of the selected entries.
In show mode, adds the current entry; in search mode, adds all selected entries."
    (interactive)
    (let ((entries
           (cond
            (entries entries)
            ((derived-mode-p 'elfeed-show-mode) (list elfeed-show-entry))
            ((derived-mode-p 'elfeed-search-mode) (elfeed-search-selected))
            (t (user-error "Not in an Elfeed buffer or no entries provided"))))
          (total-count (length entries))
          (completed-count 0))

      ;; Ensure the watch directory exists
      (unless (file-directory-p deluge-watch-dir)
	(user-error "Deluge watch directory does not exist: %s" deluge-watch-dir))

      (message "Starting download of %d torrent(s)..." total-count)

      ;; Process each entry asynchronously
      (dolist (entry entries)
	(let ((url (elfeed-entry-link entry))
	      (title (elfeed-entry-title entry)))
          (if url
	      (let* ((safe-title (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" title))
                     (filename (concat safe-title ".torrent"))
                     (filepath (expand-file-name filename deluge-watch-dir)))

		(message "Starting download: %s" title)

		;; Use url-retrieve for non-blocking download
		(url-retrieve
		 url
		 (lambda (status entry title filepath completed-count total-count)
                   (let ((error (plist-get status :error)))
                     (if error
			 (message "Failed to download torrent for %s: %s" title error)
		       ;; Success - write the torrent file
		       (progn
			 ;; Move past HTTP headers
			 (goto-char (point-min))
			 (re-search-forward "\n\n" nil t)

			 ;; Write the torrent data to file
			 (write-region (point) (point-max) filepath nil 'quiet)
			 (message "Downloaded torrent: %s" title)

			 ;; Mark as read in elfeed
			 (elfeed-untag entry 'unread)))

                     ;; Update completion counter
                     (cl-incf completed-count)
                     (when (= completed-count total-count)
		       (message "Completed downloading %d torrent(s) to Deluge watch folder" total-count)
		       ;; Update elfeed display if in search mode
		       (when (and (get-buffer "*elfeed-search*")
                                  (with-current-buffer "*elfeed-search*"
                                    (derived-mode-p 'elfeed-search-mode)))
			 (with-current-buffer "*elfeed-search*"
                           (elfeed-search-update--force))))))
		 ;; Pass variables to the callback
		 (list entry title filepath completed-count total-count)
		 nil t))  ; silent, no-cookies
            (message "No URL found for entry: %s" title))))))

;;; -> Elfeed -> Backups
;;; From https://punchagan.muse-amuse.in/blog/elfeed-db-back-up-hooks/

  (defvar pc/elfeed-db-save-timer nil
    "Timer for debounced elfeed database saves.")

  (defun pc/elfeed-db-save-and-backup ()
    "Save the elfeed database and commit to git."
    (when (and (boundp 'elfeed-db) elfeed-db)
      (elfeed-db-save)
      (let ((default-directory elfeed-db-directory))
	(when (file-exists-p ".git")
          (call-process "git" nil "*elfeed-db-backup*" nil "add" "-A")
          (call-process "git" nil "*elfeed-db-backup*" nil "commit" "-m" "auto-backup")
          (call-process "git" nil "*elfeed-db-backup*" nil "push" "origin" "main")))))

  (defun pc/elfeed-db-save-soon ()
    "Schedule a database save after 10 seconds of idle."
    (interactive)
    (when pc/elfeed-db-save-timer
      (cancel-timer pc/elfeed-db-save-timer))
    (setq pc/elfeed-db-save-timer
          (run-with-idle-timer 10 nil #'pc/elfeed-db-save-and-backup)))

  ;; Save and backup when tags change (elfeed-web usage)
  (add-hook 'elfeed-tag-hooks   (lambda (&rest _) (pc/elfeed-db-save-soon)))
  (add-hook 'elfeed-untag-hooks (lambda (&rest _) (pc/elfeed-db-save-soon)))

  ;; Save and backup when new entries are added
  (add-hook 'elfeed-db-update-hook #'pc/elfeed-db-save-soon)

;;; -> Elfeed -> Multi-Device Syncing

;;; Core Variables
  (defvar my/elfeed-db-modified nil
    "When non-nil, indicates the Elfeed database has unsaved modifications.")

  (defvar my/elfeed-save-timer nil
    "Timer for deferred database saving.")

  (defvar my/elfeed-save-delay 15
    "Seconds to wait after modification before saving database.")

  (defvar my/elfeed-debug nil
    "When non-nil, enable verbose debugging messages for Elfeed sync.")

;;; Core Functions
  (defun my/elfeed-mark-db-modified (&rest _)
    "Mark database as modified and schedule a save.
This is attached directly to database modification functions."
    (when my/elfeed-debug
      (message "Elfeed: Database modification detected!"))
    (setq my/elfeed-db-modified t)
    (when my/elfeed-debug
      (message "Elfeed: Modified flag set to %s" my/elfeed-db-modified))
    (my/elfeed-schedule-save))

  (defun my/elfeed-schedule-save ()
    "Schedule a database save after inactivity period."
    (when my/elfeed-debug
      (message "Elfeed: Scheduling save in %s seconds" my/elfeed-save-delay))

    (unless my/elfeed-save-timer
      (message "Elfeed: Timer started."))

    (when my/elfeed-save-timer
      (when my/elfeed-debug
	(message "Elfeed: Cancelling existing save timer"))
      (cancel-timer my/elfeed-save-timer)
      (setq my/elfeed-save-timer nil))

    (setq my/elfeed-save-timer
          (run-with-timer my/elfeed-save-delay nil #'my/elfeed-save-if-modified))
    (when my/elfeed-debug
      (message "Elfeed: Save timer scheduled")))

  (defun my/elfeed-save-if-modified ()
    "Save the database if it has unsaved changes."
    (when my/elfeed-debug
      (message "Elfeed: Save timer triggered. Modified: %s" my/elfeed-db-modified))

    (when my/elfeed-db-modified
      (message "Elfeed: Saving database changes...")
      (elfeed-db-save)
      (setq my/elfeed-db-modified nil)
      (setq my/elfeed-save-timer nil)
      (message "Elfeed: Database saved.")))

  (defun my/elfeed-load-db ()
    "Load the database from disk if no unsaved changes exist."
    (when my/elfeed-debug
      (message "Elfeed: Load DB called. Modified: %s" my/elfeed-db-modified))

    (unless my/elfeed-db-modified
      (message "Elfeed: Loading database from disk...")
      (elfeed-db-load)
      (when my/elfeed-debug
	(message "Elfeed: Database loaded, updating search buffer..."))

      (when-let ((buffer (get-buffer "*elfeed-search*")))
	(with-current-buffer buffer
          (elfeed-search-update t))) ; Force update

      (message "Elfeed: Database loaded.")))

;;; Setup hooks for buffer activation
  (defun my/elfeed-setup-local-activation-hooks ()
    "Set up buffer-local hooks for database reloading on activation."
    (when my/elfeed-debug
      (message "Elfeed: Setting up local activation hooks in buffer: %s"
	       (current-buffer)))

    (add-hook 'focus-in-hook #'my/elfeed-load-db nil t)
    (add-hook 'tab-bar-tab-post-select-functions
	      (lambda (&rest _)
		(when my/elfeed-debug
                  (message "Elfeed: Tab selection triggered db load"))
		(my/elfeed-load-db))
	      nil t)

    (when my/elfeed-debug
      (message "Elfeed: Local activation hooks installed.")))

;;; Setup and Hooks
  (defun my/elfeed-setup-sync ()
    "Setup database synchronization by attaching to core DB functions."
    (when my/elfeed-debug
      (message "Elfeed: Setting up sync system..."))

    ;; Monitor actual database modification functions
    (advice-add 'elfeed-tag :after #'my/elfeed-mark-db-modified)
    (advice-add 'elfeed-untag :after #'my/elfeed-mark-db-modified)
    (advice-add 'elfeed-db-add :after #'my/elfeed-mark-db-modified)

    ;; Load on entering/focusing Elfeed
    (advice-add 'elfeed :before #'my/elfeed-load-db)

    ;; Add the local hooks to elfeed modes
    (add-hook 'elfeed-search-mode-hook #'my/elfeed-setup-local-activation-hooks)
    (add-hook 'elfeed-show-mode-hook #'my/elfeed-setup-local-activation-hooks)

    ;; Ensure save on exit
    (advice-add 'elfeed-search-quit-window :before #'my/elfeed-save-if-modified)
    (add-hook 'kill-emacs-hook #'my/elfeed-save-if-modified)

    (when my/elfeed-debug
      (message "Elfeed: Setup complete!")))

  (defun my/elfeed-disable-sync ()
    "Disable database synchronization by removing hooks and advice."
    (when my/elfeed-debug
      (message "Elfeed: Disabling sync system..."))

    ;; Remove advice from core functions
    (advice-remove 'elfeed-tag #'my/elfeed-mark-db-modified)
    (advice-remove 'elfeed-untag #'my/elfeed-mark-db-modified)
    (advice-remove 'elfeed-db-add #'my/elfeed-mark-db-modified)

    ;; Remove load on entering/focusing Elfeed
    (advice-remove 'elfeed #'my/elfeed-load-db)

    ;; Remove the local hooks from elfeed modes
    (remove-hook 'elfeed-search-mode-hook #'my/elfeed-setup-local-activation-hooks)
    (remove-hook 'elfeed-show-mode-hook #'my/elfeed-setup-local-activation-hooks)

    ;; Remove save on exit
    (advice-remove 'elfeed-search-quit-window #'my/elfeed-save-if-modified)
    (remove-hook 'kill-emacs-hook #'my/elfeed-save-if-modified)

    (when my/elfeed-debug
      (message "Elfeed: Sync system disabled!")))

  ;; Initialize the system
  ;; (my/elfeed-setup-sync)

  ;; Command to manually trigger a save
  (defun my/elfeed-manual-save ()
    "Manually save the Elfeed database, regardless of the modified flag."
    (interactive)
    (message "Elfeed: Manual save requested")
    (elfeed-db-save)
    (setq my/elfeed-db-modified nil)
    (message "Elfeed: Database manually saved."))

  ;; Command to show sync status
  (defun my/elfeed-sync-status ()
    "Display the current status of the Elfeed sync system."
    (interactive)
    (message "Elfeed Status: Modified: %s, Timer: %s"
             my/elfeed-db-modified
             (if my/elfeed-save-timer "Active" "Inactive")))

  ;; Command to toggle debug mode
  (defun my/elfeed-toggle-debug ()
    "Toggle verbose debug messages for Elfeed sync."
    (interactive)
    (setq my/elfeed-debug (not my/elfeed-debug))
    (message "Elfeed: Debug mode %s"
             (if my/elfeed-debug "enabled" "disabled")))


  ) ;;; End of elfeed use-package block

;;; The abstracted out package has some weird issues
(use-package elfeed-sync
  :disabled
  :load-path "~/.emacs.d/lisp/elfeed-sync"
  ;; NOTE TO SELF: Defer after pattern
  :defer nil
  :after elfeed
  :init
  ;; Make sure timeout.el is in the load path
  (add-to-list 'load-path "~/.emacs.d/lisp")
  ;; :config
  ;; Optional: Adjust timing parameters if default values don't suit your workflow
  ;; (setq my/elfeed-save-delay 10)      ;; Save after 10 seconds of inactivity
  ;; (setq my/elfeed-load-throttle 3)    ;; Allow loading at most once every 3 seconds

  ;; Optional: Add keybindings for the utility functions
  :bind (:map elfeed-search-mode-map
	      ;; ("s" . my/elfeed-manual-save)
	      ;; ("r" . my/elfeed-force-reload)
	      ("?" . my/elfeed-sync-status)
	      ("D" . my/elfeed-toggle-debug)
	      )
  )

(use-package cuckoo-search
  ;; :vc (:url "https://github.com/rtrppl/cuckoo-search" :rev :newest)
  :after (elfeed)
  :bind
  (:map elfeed-search-mode-map
	("C" . cuckoo-search)
	;; ("x" . cuckoo-search-saved-searches)
	))

(use-package elfeed-org
  :defer nil
  :after elfeed
  :custom
  (rmh-elfeed-org-files (list (concat org-roam-directory "/elfeed.org")))

  :config
  (elfeed-org)

  (require 'elfeed-link)

  (org-link-set-parameters "elfeed" :export #'elfeed-link-export-link)

  ;; This allows conversion to links to underlying content when exporting
  ;; https://takeonrules.com/2024/08/11/exporting-org-mode-elfeed-links/
  (defun elfeed-link-export-link (link desc format _protocol)
    "Export `org-mode' `elfeed' LINK with DESC for FORMAT."
    (if (string-match "\\([^#]+\\)#\\(.+\\)" link)
	(if-let* ((entry
                   (elfeed-db-get-entry
                    (cons (match-string 1 link)
			  (match-string 2 link))))
		  (url
                   (elfeed-entry-link entry))
		  (title
                   (elfeed-entry-title entry)))
	    (pcase format
	      ('html (format "<a href=\"%s\">%s</a>" url desc))
	      ('md (format "[%s](%s)" desc url))
	      ('latex (format "\\href{%s}{%s}" url desc))
	      ('texinfo (format "@uref{%s,%s}" url desc))
	      (_ (format "%s (%s)" desc url)))
	  (format "%s (%s)" desc url))
      (format "%s (%s)" desc link)))


  ;;; Elfeed autologging
  (advice-add 'elfeed-search-browse-url :before #'js/log-elfeed-entries)
  (advice-add 'elfeed-search-show-entry :after #'js/elfeed-search-logger)
  (advice-add 'open-youtube-in-iina :before #'js/log-elfeed-entries)
  (advice-add 'js/elfeed-entries-to-wallabag :before #'js/log-elfeed-entries)
  (advice-add 'js/elfeed-entries-to-podcastify :before #'js/log-elfeed-entries)
  (advice-add 'js/elfeed-entries-to-deluge :before #'js/log-elfeed-entries)
  ;; (advice-add 'open-downloaded-youtube-in-iina :before #'js/log-elfeed-entries)

  (defun js/elfeed-search-logger (entry)
    "Wrapper for elfeed entry logger for elfeed-search-show-entry"
    (interactive (list (elfeed-search-selected :ignore-region)))
    (js/log-elfeed-entries nil (list entry)))

  (defun get-elfeed-entry-author (entry)
    "Extract the (first) author name from an Elfeed entry."
    (let* ((meta (elfeed-entry-meta entry))
           (authors (plist-get meta :authors))
           (first-author (car authors)))
      (plist-get first-author :name)))

  (defun js/make-elfeed-entry-link (entry)
    "Returns the `org' link string to the given Elfeed entry."
    (org-link-make-string
     (format "elfeed:%s#%s"
             (car (elfeed-entry-id entry))
             (cdr (elfeed-entry-id entry)))
     (let ((author (get-elfeed-entry-author entry))
	   (title (elfeed-entry-title entry)))
       (if author
	   (format "%s - %s" title author)
	 title))))

  ;; Define variables for skipped tags and feed IDs
  (defvar js/elfeed-skipped-tags '(logged asmr papers github trash)
    "Tags for Elfeed entries that should not be logged automatically.")

  (defvar js/elfeed-skipped-feed-ids '("xkcd.com")
    "Feed IDs for Elfeed entries that should not be logged automatically.")

  (defun js/elfeed-entry-should-be-logged-p (entry)
    "Return non-nil if the Elfeed ENTRY should be logged."
    (let ((tags (elfeed-entry-tags entry))
          (feed-id (car (elfeed-entry-id entry))))
      (and (not (seq-intersection tags js/elfeed-skipped-tags))
           (not (member feed-id js/elfeed-skipped-feed-ids)))))

  (defun js/log-elfeed-entries (&optional arg r keys)
    "Log unlogged elfeed entries to the daily file and mark them as logged.
R can be a list of entries to log.
With prefix ARG, log entries regardless of filters.
If a key is provided, use it instead of the default capture template."
    (interactive "P")
    (let ((entries
           (cond
            ((and r (consp (car r))) (car r))
            ((derived-mode-p 'elfeed-show-mode) (list elfeed-show-entry))
            ((derived-mode-p 'elfeed-search-mode) (elfeed-search-selected))
            (t (user-error "Not in an Elfeed buffer or no entries provided")))))
      (dolist (entry entries)
	(when (or arg (js/elfeed-entry-should-be-logged-p entry))
          (let ((link (js/make-elfeed-entry-link entry)))
            (org-roam-dailies-autocapture-today (or keys "e") link)
            (elfeed-tag entry 'logged))))
      (elfeed-db-save)))

  (defun js/log-elfeed-process ()
    (interactive)
    (js/log-elfeed-entries 1 nil "p"))
  ) ;;
;;; End of elfeed-org package block

;;; -> Elfeed -> Elfeed-tube
;;; TODO: Rewrite the elfeed downloader

(use-package elfeed-tube
  :defer nil
  :after elfeed
  :bind
  (:map elfeed-show-mode-map
	("F" . elfeed-tube-fetch)
	([remap save-buffer] . elfeed-tube-save)
	:map elfeed-search-mode-map
	("F" . elfeed-tube-fetch)
	([remap save-buffer] . elfeed-tube-save))
  :custom (elfeed-tube-auto-save-p t)
  :config
  (elfeed-tube-setup)
  (defvar yt-dlp-priority-tags '(asmr chess osrs gaming essays)
    "List of tags to prioritize when determining download subfolder.
     The first matching tag in this list determines the subfolder.")

  ;; Old version with combined streaming downloaded behavior
  ;; (defun open-youtube-in-iina ()
  ;;   "Create a playlist with selected elfeed entries and open it in IINA."
  ;;   (interactive)
  ;;   (let* ((entries (elfeed-search-selected))
  ;;          (iina-command "open -a IINA")
  ;;          (playlist-file (make-temp-file "emacs-iina-playlist" nil ".m3u8")))
  ;;     (with-temp-file playlist-file
  ;; 	(dolist (entry entries)
  ;;         (if (member 'downloaded (elfeed-entry-tags entry))
  ;;             (let* ((relative-filename (elfeed-meta entry :filename))
  ;;                    (filename (expand-file-name relative-filename yt-dlp-folder)))
  ;; 		(if (and filename (file-exists-p filename))
  ;;                   (insert filename)
  ;;                 (insert (elfeed-entry-link entry))
  ;;                 (elfeed-untag entry 'downloaded))
  ;; 		(insert "\n")))))
  ;;     (start-process-shell-command "iina" nil (concat iina-command " \"" playlist-file "\""))
  ;;     (message "Opening YouTube playlist in IINA...")))

  (defun open-youtube-in-iina (&optional _arg)
    "Create a playlist with selected elfeed entries and open it in IINA."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (iina-command "open -a IINA")
           (playlist-file (make-temp-file "emacs-iina-playlist" nil ".m3u8")))
      (with-temp-file playlist-file
	(dolist (entry entries)
	  (elfeed-untag entry 'unread)
          (insert (elfeed-entry-link entry))
          (insert "\n")))
      (start-process-shell-command "iina" nil (concat iina-command " \"" playlist-file "\""))
      (message "Opening YouTube playlist in IINA...")))

  (defun open-downloaded-youtube-in-iina ()
    "Open downloaded files for selected elfeed entries as a playlist in IINA.
For each entry with the +downloaded tag, opens the local file.
If the file doesn't exist, removes the +downloaded tag and skips it."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (iina-command "open -a IINA")
           (playlist-file (make-temp-file "emacs-iina-playlist" nil ".m3u8"))
           (any-added nil))
      (with-temp-file playlist-file
	(dolist (entry entries)
          (if (not (member 'downloaded (elfeed-entry-tags entry)))
              (message "Skipping \"%s\": no +downloaded tag." (elfeed-entry-title entry))
            (let* ((relative-filename (elfeed-meta entry :filename))
                   (filename (and relative-filename
                                  (expand-file-name relative-filename yt-dlp-folder))))
              (if (and filename (file-exists-p filename))
                  (progn
                    (elfeed-untag entry 'unread)
                    (insert filename)
                    (insert "\n")
                    (setq any-added t))
		(elfeed-untag entry 'downloaded)
		(message "File not found for \"%s\"; removed +downloaded tag."
			 (elfeed-entry-title entry)))))))
      (if any-added
          (progn
            (start-process-shell-command "iina" nil
					 (concat iina-command " \"" playlist-file "\""))
            (message "Opening downloaded videos in IINA..."))
	(message "No downloaded files found to open."))))

  (defun determine-subfolder (entry)
    "Determine the subfolder for the video based on its tags and yt-dlp-priority-tags."
    (let ((tags (elfeed-entry-tags entry)))
      (catch 'found
	(dolist (tag yt-dlp-priority-tags)
          (when (member tag tags)
            (throw 'found (symbol-name tag)))
          nil))))

  (defun determine-file-extension (base-name possible-extensions)
    "Determine the actual file extension for BASE-NAME by checking which one exists."
    (catch 'found
      (dolist (ext possible-extensions)
	(let ((full-name (expand-file-name (concat base-name "." ext) yt-dlp-folder)))
          (when (file-exists-p full-name)
            (throw 'found ext))))))

  (defun yt-dlp-sentinel (process event entry base-filename)
    "Sentinel for handling yt-dlp process completion for a single entry."
    (when (memq (process-status process) '(exit signal))
      (if (= (process-exit-status process) 0)
          (progn
            (message "yt-dlp download for %s completed successfully!" (elfeed-entry-title entry))
            ;; Determine the actual file extension
            (let ((ext (determine-file-extension base-filename '("mp4" "mkv" "webm" "flv"))))
	      (elfeed-meta--put entry :filename
				(if ext
				    (concat base-filename "." ext)
				  base-filename)))
            ;; Tag the entry as "downloaded"
            (elfeed-tag entry 'downloaded))
	(message "yt-dlp encountered an error for %s. Check *yt-dlp-output* for details." (elfeed-entry-title entry)))))

  (defun create-single-entry-sentinel (entry base-filename)
    `(lambda (proc event)
       (yt-dlp-sentinel proc event ',entry ',base-filename)))

  (defun escape-single-quotes (str)
    "Escape single-quote characters in STR."
    (replace-regexp-in-string "'" "'\"'\"'" str))

  (defun escape-slashes (str)
    "Escape characters in STR to make it safe for a filename."
    (replace-regexp-in-string "/" "_" str))

  (defun download-selected-youtube-videos (entries)
    "Download selected YouTube videos using yt-dlp."
    (interactive (list (elfeed-search-selected)))
    (dolist (entry entries)
      (let* ((subfolder (or (determine-subfolder entry) ""))
             (author-plist (car (elfeed-meta entry :authors)))
             (author-name (plist-get author-plist :name))
             (upload-date (elfeed-search-format-date (elfeed-entry-date entry)))
             (elfeed-title (escape-slashes (elfeed-entry-title entry)))

             ;;; The base filename is stored relative to the Youtube folder
             ;;; It is later reconstructed with the yt-dlp-folder variable
             (base-filename (concat (file-name-as-directory subfolder)
                                    (file-name-as-directory author-name)
                                    upload-date
                                    " - "
                                    elfeed-title))
             (output-template (concat (file-name-as-directory yt-dlp-folder)
				      (escape-single-quotes base-filename)
				      ".%(ext)s"))
             (yt-dlp-command (format "yt-dlp --cookies-from-browser firefox --no-progress -S 'res:1080,ext,vcodec:h265,h264' --embed-subs --sub-lang 'en.*' --sponsorblock-mark all --sponsorblock-remove 'sponsor' -o '%s' '%s'" output-template (elfeed-entry-link entry)))
             (process (start-process-shell-command "yt-dlp" "*yt-dlp-output*" yt-dlp-command)))
        ;; Use the constructed sentinel for each entry
        (set-process-sentinel process (eval (create-single-entry-sentinel entry base-filename)))))
    (message "Downloading selected YouTube videos..."))

  (defun js/elfeed-delete-downloaded-videos ()
    "Delete downloaded video files for selected entries from the filesystem.
Works on the region if active, otherwise the entry at point.
Entries without the `downloaded' tag are silently skipped.
If none of the selected entries are downloaded, a message is shown."
    (interactive)
    (let* ((entries (cond
                     ((derived-mode-p 'elfeed-show-mode)
                      (list elfeed-show-entry))
                     ((derived-mode-p 'elfeed-search-mode)
                      (elfeed-search-selected))
                     (t (user-error "Not in an Elfeed buffer"))))
           (downloaded (seq-filter (lambda (e)
                                     (member 'downloaded (elfeed-entry-tags e)))
                                   entries)))
      (if (null downloaded)
          (message "No downloaded items found in selection.")
	(when (yes-or-no-p (format "Delete %d downloaded video file(s)? "
                                   (length downloaded)))
          (dolist (entry downloaded)
            (let* ((base-filename (elfeed-meta entry :filename)))
              (if (null base-filename)
                  (message "Warning: entry \"%s\" tagged `downloaded' but has no :filename metadata; skipping."
                           (elfeed-entry-title entry))
		(let ((full-path (expand-file-name base-filename yt-dlp-folder)))
                  (if (not (file-exists-p full-path))
                      (message "Warning: file not found: %s; skipping." full-path)
                    (delete-file full-path)
                    (elfeed-untag entry 'downloaded)
                    (elfeed-meta--put entry :filename nil)
                    (elfeed-search-update-entry entry))))))
          (message "Deleted %d video file(s)." (length downloaded))
          (elfeed-db-save)))))

  (defun elfeed-filter-downloaded ()
    (interactive)
    (elfeed-filter-maker "+downloaded" "Showing previously downloaded items."))

  ;;; Manually add items
  (defun js/elfeed-fetch-youtube-metadata (url)
    "Synchronously fetch title and channel info for a YouTube video at URL.
Returns a plist with :title, :channel-name, :channel-id, or nil on failure."
    (condition-case err
	(with-current-buffer (url-retrieve-synchronously url t t 10)
          (goto-char (point-min))
          (let ((title      (when (re-search-forward "<title>\\(.*?\\) - YouTube</title>" nil t)
                              (decode-coding-string (match-string 1) 'utf-8)))
		(ch-name    nil)
		(ch-id      nil))
            (goto-char (point-min))
            (when (re-search-forward "\"channelId\":\"\\([^\"]+\\)\"" nil t)
              (setq ch-id (match-string 1)))
            (goto-char (point-min))
            (when (re-search-forward "\"ownerChannelName\":\"\\([^\"]+\\)\"" nil t)
              (setq ch-name (match-string 1)))
            (list :title       (or title url)
                  :channel-name (or ch-name "Unknown Channel")
                  :channel-id   ch-id)))
      (error
       (message "js/elfeed-fetch-youtube-metadata: %s" err)
       nil)))

  ) ;;
;;; End of elfeed-tube package block

;;; -> Elfeed -> Elfeed-score

(use-package elfeed-score
  :ensure t
  :config
  (progn
    (setq elfeed-search-print-entry-function #'elfeed-score-print-entry)

    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map))

  )

;;; -> Elfeed -> Elpapers

;;; Works for Emacs 30+
(use-package elpapers
  :if (eq system-type 'darwin)
  :after elfeed
  :demand t
  :load-path "~/Documents/repos/elpapers/lisp/"
  :init
  ;; Explicitly load the API module since it's a separate file
  (require 'elpapers)
  :config
  (elpapers-enable-auto-ingest))

;;; -> Elfeed -> End

;;; --> Readwise

;; (use-package org-readwise
;;   :vc (:url "https://github.com/Tevqoon/org-readwise" :rev :newest)
;;   :custom
;;   ;; Output location - single file in readwise subfolder
;;   (org-readwise-output-directory
;;    (expand-file-name "readwise" org-roam-directory))

;;   ;; Sync both classic Readwise highlights and Reader documents
;;   (org-readwise-sync-highlights t)
;;   (org-readwise-sync-reader t)

;;   ;; Track last sync time
;;   (org-readwise-last-sync-time-file
;;    (expand-file-name ".org-readwise-last-sync" user-emacs-directory))

;;   ;; Debug level (set to 0 once working)
;;   (org-readwise-debug-level 1)

;;   :config
;;   ;; Ensure the readwise subdirectory exists
;;     (unless (file-exists-p org-readwise-output-directory)
;;       (make-directory org-readwise-output-directory t)))

;;; --> Wallabag annotation importer

(use-package org-roam-annotation-import
  :vc (:url "https://github.com/Tevqoon/org-roam-annotation-import" :rev :newest)
  :init
  (require 'wallabag-backend)
  )

;;; --> Wallabag

(use-package request
  :ensure t)
(use-package emacsql
  :ensure t)

(use-package wallabag
  :defer t
  :after request emacsql
  :bind (("C-x W" . wallabag)
         :map wallabag-search-mode-map
         ;; Basic navigation and viewing
         ("SPC" . wallabag-view)
         ("b" . wallabag-browse-url)
         ("B" . wallabag-browse-url-firefox)
         ("n" . wallabag-next-entry)
         ("p" . wallabag-previous-entry)
         ("q" . wallabag-search-quit)

         ;; Filtering and display options
         ("c" . my/wallabag-show-unarchived)          ; Mirror elfeed's clear filter - show default view
         ("s" . my/wallabag-show-all)                 ; Show all entries including archived
         ("S" . wallabag-search-live-filter)          ; Search functionality

         ;; Tag and status management
         ("+" . wallabag-add-tags)
         ("-" . wallabag-remove-tag)
         ("t" . wallabag-delete-entry)
         ("f" . wallabag-update-entry-starred)

         ;; Other functions
         ("y" . wallabag-org-link-copy)
         ("i" . wallabag-add-entry)
         ("g" . wallabag-search-refresh-and-clear-filter)
         ("G" . wallabag-search-update-and-clear-filter)
	 ("R" . wallabag-search-synchronize-and-clear-filter)
	 ("Y" . wallabag-full-update)
         ("r" . wallabag-update-entry-archive)
	 ("R" . js/wallabag-roamify-entry)

         :map wallabag-entry-mode-map
         ;; Entry mode keys (same as before)
         ("SPC" . scroll-up-command)
         ("S-SPC" . scroll-down-command)
         ("M-o" . ace-link-safari)
         ("b" . wallabag-browse-url)
         ("+" . wallabag-add-tags)
         ("-" . wallabag-remove-tag)
         ("q" . wallabag-entry-quit)
         ("n" . wallabag-next-entry)
         ("p" . wallabag-previous-entry)
         ("g" . wallabag-view)
         ("t" . wallabag-delete-entry)
         ("<" . beginning-of-buffer)
         (">" . end-of-buffer)
         ("y" . wallabag-org-link-copy)
         ("f" . wallabag-update-entry-starred)
         ("x" . wallabag-update-entry-archive)
	 ("r" . wallabag-update-entry-archive)
	 ("R" . js/wallabag-roamify-entry))
  :init
  ;; contains the wallabag info
  (load "~/.emacs.d/private-config.el")

  :custom
  (wallabag-search-print-items '("title" "domain" "tag" "reading-time" "date"))
  (wallabag-search-page-max-rows 32)
  (url-automatic-caching t) ;; for image caching

  ;; Set default filter to unarchived only
  (wallabag-search-filter "Unread")

  :hook
  (wallabag-after-render-hook . my/wallabag-initialize-view)

  :config
  (setq wallabag-show-entry-switch #'switch-to-buffer)

  ;; Set up our custom parsers
  (advice-add 'wallabag-parse-entry-as-string :override
	      #'my/wallabag-parse-entry-as-string-with-archive-status)

  ;; Initialize with unarchived view
  (advice-add 'wallabag :after #'my/wallabag-initialize-view)

  ;; Define a custom face for archived entries
  (defface my/wallabag-archived-face
    '((t :inherit wallabag-title-face :foreground "#888888" :slant italic))
    "Face for archived wallabag entries.")

  (defun my/wallabag-show-unarchived ()
    "Show only unarchived wallabag entries (default view)."
    (interactive)
    (setq wallabag-group-filteringp t)
    (wallabag-search-update-buffer-with-keyword "Unread")
    (message "Showing unarchived entries only"))

  (defun my/wallabag-show-all ()
    "Show all wallabag entries, including archived ones."
    (interactive)
    (setq wallabag-group-filteringp t)
    (wallabag-search-update-buffer-with-keyword "All")
    (message "Showing all entries (including archived)"))

  (defun my/wallabag-initialize-view ()
    "Initialize wallabag to show only unarchived entries by default."
    (my/wallabag-show-unarchived))

  (defun wallabag-get-item-value (item entry)
    "Get the formatted value for ITEM from ENTRY."
    (pcase item
      ("date" (propertize
	       (let* ((created-at (alist-get 'created_at entry))
		      (created-at-days (string-to-number
					(format-seconds "%d" (+ (float-time
								 (time-subtract (current-time)
										(encode-time (parse-time-string created-at))))
								86400)))))
		 (cond ((< created-at-days 7)
			(format "%sd" created-at-days))
		       ((< created-at-days 30)
			(format "%sw" (/ created-at-days 7)))
		       ((< created-at-days 365)
			(format "%sm" (/ created-at-days 30)))
		       (t
			(format "%sy" (/ created-at-days 365)))))
	       'face 'wallabag-date-face))
      ("domain" (propertize (or (alist-get 'domain_name entry) "")
                            'face 'wallabag-domain-name-face))
      ("tag" (let ((tag (alist-get 'tag entry)))
	       (format (if (string-empty-p tag) "" "(%s)" )
		       (propertize tag 'face 'wallabag-tag-face))))
      ("reading-time" (propertize (concat (number-to-string (alist-get 'reading_time entry)) " min")
				  'face 'wallabag-reading-time-face))
      ("seperator" (format "\n%s" (make-string (window-width) ?-)))
      (_ item)))

  (defun my/wallabag-parse-entry-as-string-with-archive-status (entry)
    "Parse wallabag ENTRY and return as string with archive status indicator."
    (let* ((title (or (alist-get 'title entry) "NO TITLE"))
           (is-archived (alist-get 'is_archived entry))
           (is-starred (alist-get 'is_starred entry))
           (star (if (= is-starred 0)
                     ""
                   (format "%s " (propertize wallabag-starred-icon
                                             'face 'wallabag-starred-face
                                             'mouse-face 'wallabag-mouse-face
                                             'help-echo "Filter the favorite items"))))
           ;; Add archive indicator
           (archive-indicator (if (= is-archived 0)
				  ""
				(format "%s " (propertize "✓"
							  'face 'shadow
							  'mouse-face 'wallabag-mouse-face
							  'help-echo "Archived entry")))))

      ;; Concatenate items with their formatted values
      (mapconcat #'identity
		 (cl-loop for item in wallabag-search-print-items
                          collect (pcase item
                                    ("title" (format "%s%s%s"
                                                     star
                                                     archive-indicator
                                                     (if (= is-archived 0)
							 (propertize title 'face 'wallabag-title-face)
						       (propertize title 'face 'my/wallabag-archived-face))))
                                    (_ (wallabag-get-item-value item entry))))
		 " ")))

  (defun wallabag-add-entry (&optional url tags)
    "Add a new entry by URL and TAGS."
    (interactive)
    (let* ((url (pcase major-mode
                  ('elfeed-show-mode
                   (if (and (boundp 'elfeed-show-entry)
                            (fboundp 'elfeed-entry-link))
		       (elfeed-entry-link elfeed-show-entry) ""))
                  ('eaf-mode
                   (if (boundp 'eaf--buffer-url) (abbreviate-file-name eaf--buffer-url) ""))
                  ('eww-mode
                   (if (boundp 'eww-data) (plist-get eww-data :url) ""))
                  (_ (if url url (read-from-minibuffer "What URL do you want to add? ")))))
           ;; FIXME if no tags pull before, it will return empty string
           (tags (or tags (wallabag-get-tag-name)))
           (host (wallabag-host))
           (token (or wallabag-token (wallabag-request-token))))
      (request (format "%s/api/entries.json" host)
	:parser 'json-read
	:type "POST"
	:data `(("url" . ,url)
		("archive" . 0)
		("starred" . 0)
		("tags" . ,tags)
		("access_token" . ,token))
	:headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
	:error
	(cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
		       (message "Wallaget request error: %S" error-thrown)))
	:status-code `((401 . ,(wallabag-request-token-retry #'wallabag-add-entry url)))
	:success (cl-function
                  (lambda (&key data &allow-other-keys)
                    ;; convert tags array to tag comma seperated string
                    (setq data
                          (progn
                            (setf
                             (alist-get 'tag data)
                             (if (stringp (alist-get 'tag data))
                                 (alist-get 'tag data)
			       (wallabag-convert-tags-to-tag data)))
                            data))
                    (let ((inhibit-read-only t)
                          (id (alist-get 'id data)))
		      ;; check id exists or not
		      (if (eq 1 (caar (wallabag-db-sql
				       `[:select :exists
						 [:select id :from items :where (= id ,id)]])))
                          (progn
                            (message "Entry Already Exists")
                            (goto-char (wallabag-find-candidate-location id))
                            (wallabag-flash-show (line-beginning-position) (line-end-position) 'highlight 0.5))
                        (wallabag-db-insert (list data))
                        (if (buffer-live-p (get-buffer wallabag-search-buffer-name))
                            (with-current-buffer (get-buffer wallabag-search-buffer-name)
			      (save-excursion
                                (goto-char (point-min))
                                (funcall wallabag-search-print-entry-function data))) )
                        (message "Add Entry: %s" id)
                        (if wallabag-show-entry-after-creation
                            (wallabag-show-entry (car (wallabag-db-select :id id))) ))))))))

  (defun js/wallabag-roamify-entry ()
    "Convert current wallabag entry into an org-roam node.
Creates a new node with:
- Wallabag link as a ref (wallabag:ID)
- Source URL as a ref
- Ready for note-taking

In search mode, operates on the entry at point.
In entry mode, operates on the current entry."
    (interactive)

    ;; Get the current entry depending on the mode
    (let ((entry
           (pcase major-mode
             ('wallabag-entry-mode
	      ;; In entry mode, get the entry from the title's text property
	      (get-text-property (point-min) 'wallabag-entry))
             ('wallabag-search-mode
	      ;; In search mode, get the entry at point
	      (wallabag-find-candidate-at-point))
             (_ (user-error "Not in a wallabag buffer")))))

      (unless entry
	(user-error "No wallabag entry found"))

      (let* ((id (alist-get 'id entry))
             (title (alist-get 'title entry))
             (source-url (alist-get 'url entry))
             (domain (alist-get 'domain_name entry))

             ;; Create the wallabag link with ID format
             (wallabag-link (format "wallabag:%s" id))

             ;; Create descriptive title with domain if available
             (working-title (if domain
				(format "%s (%s)" title domain)
			      title))

             ;; Create a new node
             (capture-node (org-roam-node-create :title working-title)))

	;; Define the finalizer function
	(defun wallabag-roamify-finalizer ()
          "Add both wallabag link and source URL as refs after capture."
          (let ((node (org-roam-node-at-point)))
            (when node
	      (let ((node-file (org-roam-node-file node)))
		;; Add both refs to the created node
		(with-current-buffer (find-file-noselect node-file)
                  (goto-char (org-roam-node-point node))
                  ;; Add wallabag reference
                  (org-roam-ref-add wallabag-link)
                  ;; Add source URL as reference
                  (org-roam-ref-add source-url)
                  (save-buffer)
                  (message "Created node: %s" working-title))))))

	;; Create the node via org-roam-capture
	(org-roam-capture-
	 :keys "d"
	 :node capture-node
	 :props (list :finalize #'wallabag-roamify-finalizer)))))

  ;; Raw import
  (defun my/escape-html (s)
    "Minimal HTML-escape for S."
    (let ((s (replace-regexp-in-string "&" "&amp;" s)))
      (setq s (replace-regexp-in-string "<" "&lt;" s))
      (setq s (replace-regexp-in-string ">" "&gt;" s)))
    (replace-regexp-in-string "\"" "&quot;" s))

  (defun my/wallabag-buffer-to-html-string ()
    "Return region or buffer as an HTML string.
If in org-mode use org export; if in markdown-mode try pandoc; otherwise wrap in <pre>."
    (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
           (end (if (use-region-p) (region-end) (point-max)))
           (txt (buffer-substring-no-properties beg end)))
      (cond
       ((derived-mode-p 'org-mode)
	(require 'ox)
	(org-export-string-as txt 'html t nil))
       ((derived-mode-p 'markdown-mode)
	(if (executable-find "pandoc")
            (with-temp-buffer
              (insert txt)
              (call-process-region (point-min) (point-max) "pandoc" t t nil "-f" "markdown" "-t" "html")
              (buffer-string))
          (concat "<pre>" (my/escape-html txt) "</pre>")))
       (t
	(concat "<pre>" (my/escape-html txt) "</pre>")))))

  (defun my/wallabag-add-buffer-or-region-as-entry (&optional url title)
    "Add current region (or whole buffer) to wallabag as CONTENT (HTML).
Prompts for optional URL and TITLE; falls back to buffer name as title."
    (interactive
     (list (read-string "URL (optional): " "")
           (read-string "Title: " (buffer-name))))
    (let ((content (my/wallabag-buffer-to-html-string)))
      (wallabag-insert-entry (unless (string= url "") url)
                             (unless (string= title "") title)
                             content)))
  )

;;; -> Verb for HTTP Requests
(use-package verb
  :ensure t
  :defer t
  :commands (verb-send-request-on-point verb-mode))

;;; --> Programming

(use-package magit
  :defer t
  :bind
  (("C-x g" . magit-status)
   ("C-x f" . magit-file-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-process-connection-type nil)
  :config
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/opt/homebrew/bin/git"))
  )

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode 1))

(use-package magit-todos
  :after (magit)
  :config
  (magit-todos-mode 1))

(use-package crdt)

(use-package quickrun
  :bind (("C-c C-r" . js/quickrun-dwim))
  :config
  (quickrun-add-command "python"
    '((:command . "python3")
      (:compile-only . "pyflakes %s")
      (:description . "Run Python script"))
    :override t)

  (defun js/quickrun-dwim ()
    "Run quickrun on region if active, otherwise on buffer."
    (interactive)
    (if (region-active-p)
        (call-interactively #'quickrun-region)
      (call-interactively #'quickrun)))
  )

(use-package git-timemachine
  :defer t)

(use-package dumb-jump
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package flymake
  :defer t
  :ensure nil  ; built-in package
  :hook
  (prog-mode . flymake-mode)
  (prog-mode . subword-mode)
  (lisp-interaction-mode . (lambda () (flymake-mode -1)))
  :config
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error)
	      ("M-l" . flymake-show-buffer-diagnostics)
	      ))

(use-package flyover
  :ensure t
  :hook ((flycheck-mode . flyover-mode)
         (flymake-mode . flyover-mode))
  :custom
  ;; Checker settings
  (flyover-checkers '(flycheck flymake))
  (flyover-levels '(error))

  ;; Appearance
  (flyover-use-theme-colors t)
  (flyover-background-lightness 45)

  ;; Text tinting
  (flyover-text-tint 'lighter)
  (flyover-text-tint-percent 50)

  ;; Icon tinting (foreground and background)
  (flyover-icon-tint 'lighter)
  (flyover-icon-tint-percent 50)
  (flyover-icon-background-tint 'darker)
  (flyover-icon-background-tint-percent 50)

  ;; Icons
  (flyover-info-icon " ")
  (flyover-warning-icon " ")
  (flyover-error-icon " ")

  ;; Border styles: none, pill, arrow, slant, slant-inv, flames, pixels
  (flyover-border-style 'pill)
  (flyover-border-match-icon t)

  ;; Display settings
  (flyover-hide-checker-name t)
  (flyover-show-virtual-line t)
  (flyover-virtual-line-type 'curved-dotted-arrow)
  (flyover-line-position-offset 1)

  ;; Message wrapping
  (flyover-wrap-messages t)
  (flyover-max-line-length 80)

  ;; Performance
  (flyover-debounce-interval 0.2)
  (flyover-cursor-debounce-interval 0.3)

  ;; Display mode (controls cursor-based visibility)
  (flyover-display-mode 'always)

  ;; Completion integration
  (flyover-hide-during-completion t))

(use-package aggressive-indent
  :diminish aggressive-indent-mode)

;;; -> Programming -> Tree-sitter

;; (use-package treesit
;;   :ensure nil
;;   :custom
;;   (treesit-language-source-alist
;;    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;      (c "https://github.com/tree-sitter/tree-sitter-c")
;;      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;      (json "https://github.com/tree-sitter/tree-sitter-json")
;;      (python "https://github.com/tree-sitter/tree-sitter-python")
;;      (rust "https://github.com/tree-sitter/tree-sitter-rust")))

;;   (major-mode-remap-alist
;;    '((python-mode . python-ts-mode)
;;      (c-mode . c-ts-mode)
;;      (c++-mode . c++-ts-mode)
;;      (css-mode . css-ts-mode)
;;      (js-mode . js-ts-mode)
;;      (json-mode . json-ts-mode)
;;      (rust-mode . rust-ts-mode)
;;      (bash-mode . bash-ts-mode)))

;;   :config
;;   (customize-set-variable 'treesit-font-lock-level 4)
;;   )

;; (use-package treesit-auto
;;   :demand t
;;   :config
;;   (global-treesit-auto-mode))

;;; -> Programming -> apheleia

(use-package apheleia
  :config
  ;; -- Formatter definitions for tools not built into apheleia --

  ;; Nix: nixfmt (or alejandra if you prefer)
  (setf (alist-get 'nixfmt apheleia-formatters)
        '("nixfmt" "-"))

  ;; Clojure: cljfmt via clj-kondo's --lint or zprint standalone
  ;; zprint is simpler - single binary, no project config needed
  (setf (alist-get 'zprint apheleia-formatters)
        '("zprint" "{:style [:community] :map {:comma? false}}"))

  ;; Emacs Lisp: use the built-in lisp-indent formatter
  ;; (this is an elisp function formatter bundled with apheleia)

  ;; -- Mode associations --
  ;; These override/extend apheleia's defaults

  ;; Python: black is already the default, but chain with isort
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--profile" "black" "--stdout" "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(isort black))

  ;; C/C++: clang-format (already built-in to apheleia)
  ;; These are already mapped by default, but being explicit:
  (setf (alist-get 'c-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode apheleia-mode-alist) 'clang-format)

  ;; Rust: rustfmt (already built-in)
  ;; rust-mode -> rustfmt is already the default

  ;; Haskell: ormolu (or fourmolu if you prefer)
  ;; Both are already defined in apheleia-formatters
  (setf (alist-get 'haskell-mode apheleia-mode-alist) 'ormolu)

  ;; Nix
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'nixfmt)

  ;; OCaml: ocamlformat (already built-in)
  ;; tuareg-mode -> ocamlformat is already the default

  ;; Clojure
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'zprint)

  ;; Emacs Lisp: opt-in to lisp-indent
  (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'lisp-indent)

  ;; YAML: prettier handles this (already built-in)
  ;; yaml-mode -> prettier is already the default

  ;; Dockerfile: no good standalone formatter, skip

  ;; LaTeX: latexindent (already built-in)
  ;; latex-mode -> latexindent is already the default

  ;; Shell: shfmt (disabled by default upstream due to zsh issues)
  ;; Re-enable for bash specifically
  (setf (alist-get 'sh-mode apheleia-mode-alist) 'shfmt)

  ;; -- Inhibit in modes where formatting is unwanted --
  ;; Org, restclient, octave - no formatters, apheleia will
  ;; silently skip these since they have no mode-alist entry

  ;; Enable globally
  (apheleia-global-mode +1))

;;; -> Programming -> LSP Eglot

(use-package eglot
  :functions jsonrpc--log-event
  :ensure nil  ; Eglot is built-in from Emacs 29
  :defer t
  :hook ((python-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         ;; (haskell-ts-mode . eglot-ensure)
	 )

  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setf (plist-get eglot-events-buffer-config :size) 0)
  )

;;; -> Programming -> LaTeX

(use-package tex
  :ensure auctex
  :custom
  (font-latex-fontify-script nil)
  (latex-run-command "lualatex")
  (TeX-engine 'luatex)
  (TeX-source-correlate-method 'synctex)
  (TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g -b %n %o %b")))
  (TeX-view-program-selection '((output-pdf "Skim")))
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-master 'dwim)
  (reftex-plug-into-AUCTeX t)
  (LaTeX-electric-left-right-brace t)
  (TeX-command-extra-options " --shell-escape ")
  (texmathp-tex-commands
   '(("prooftree" env-on)
     ("tikzcd" env-on)
     ("logicproof" env-on)
     ("forest" env-on)))

  :config
  (setq-default TeX-master nil)


  :hook
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . turn-on-reftex)
  )

;;; -> Programming -> LaTeX -> CDLaTeX
(use-package cdlatex
  :config
  (defun js/cdlatex-sub-superscript ()
    "Insert ^{} or _{} unless the number of backslashes before point is odd.
When not in LaTeX math environment, insert raw ^ or _ character.
When pressed twice, make the sub/superscript roman."
    (interactive)
    (if (and cdlatex-make-sub-superscript-roman-if-pressed-twice
             (equal this-command last-command))
	(progn
          (insert "\\mathrm{}")
          (backward-char 1))
      (if (cdlatex-number-of-backslashes-is-odd)
          ;; Quoted - insert raw character
          (insert (event-basic-type last-command-event))
	;; Check if we are in math mode (ignore the outside-math-mode setting)
	(if (cdlatex--texmathp)
            ;; In math mode - do the full template
            (progn
	      (cdlatex-ensure-math)
	      (insert (event-basic-type last-command-event))
	      (insert "{}")
	      (forward-char -1))
          ;; Not in math mode - just insert raw character
          (insert (event-basic-type last-command-event))))))

  (defun js/yas-cdlatex-tab ()
    "Try yasnippet expansion first, then fall back to cdlatex-tab."
    (interactive)
    (unless (yas-expand)
      (cdlatex-tab)))

  ;; Replace cdlatex's TAB binding with our integrated version
  (define-key cdlatex-mode-map (kbd "TAB") #'js/yas-cdlatex-tab)

  ;; cdlatex backtab support
  (defvar-local js/cdlatex-tab-history nil
    "Stack of positions before cdlatex-tab jumps, for backtab navigation.")

  (defun js/cdlatex-tab-record-and-jump ()
    "Push current position, then run cdlatex-tab."
    (push (point-marker) js/cdlatex-tab-history))

  (advice-add 'cdlatex-tab :before #'js/cdlatex-tab-record-and-jump)

  (defun js/cdlatex-backtab ()
    "Jump to the previous position visited by cdlatex-tab."
    (interactive)
    (if js/cdlatex-tab-history
	(let ((marker (pop js/cdlatex-tab-history)))
          (goto-char marker)
          (set-marker marker nil))
      (message "No previous cdlatex-tab position")))

  (defun js/org-cdlatex-backtab ()
    "In a LaTeX fragment with history, go back. Otherwise, org-shifttab."
    (interactive)
    (if (and org-cdlatex-mode
             (org-inside-LaTeX-fragment-p)
             js/cdlatex-tab-history)
	(js/cdlatex-backtab)
      (org-shifttab)))

  (define-key org-cdlatex-mode-map (kbd "<backtab>") #'js/org-cdlatex-backtab)
  ;; Also bind in cdlatex-mode-map for .tex buffers if you want parity:
  (define-key cdlatex-mode-map (kbd "<backtab>") #'js/cdlatex-backtab)


  ;; Bind the custom function to ^ and _
  (define-key cdlatex-mode-map (kbd "^") 'js/cdlatex-sub-superscript)
  (define-key cdlatex-mode-map (kbd "_") 'js/cdlatex-sub-superscript)

  :custom
  (cdlatex-takeover-parenthesis nil)
  (cdlatex-takeover-dollar nil)

  (cdlatex-command-alist
   '(("al" "Insert aligned environment" "" cdlatex-environment ("aligned") nil t)
     ("bm" "Insert bmatrix environment" "" cdlatex-environment ("bmatrix") nil t)

     ("se" "Insert a nice subseteq" "\\subseteq" nil nil nil t)
     ("sse" "Insert a nice supseteq" "\\supseteq" nil nil nil t)
     ("sne" "Insert a nice subsetneq" "\\subsetneq" nil nil nil t)
     ("ssne" "Insert a nice supsetneq" "\\supsetneq" nil nil nil t)
     ("tl" "Insert a nice triangle left" "\\lhd" nil nil nil t)
     ("tse" "Insert a nice triangle sub left" "\\unlhd" nil nil nil t)
     ("tsne" "Insert a nice triangle sub left" "\\unlhd" nil nil nil t)
     ("tr" "Insert a nice triangle right" "\\rhd" nil nil nil t)
     ("tsse" "Insert a nice triangle sub right" "\\unrhd" nil nil nil t)
     ("imp" "implies" "\\implies" nil nil nil t)
     ("imb" "Implied" "\\impliedby" nil nil nil t)
     ("le" "leq" "\\leq" nil nil nil t)
     ("ge" "geq" "\\geq" nil nil nil t)
     ("ne" "neq" "\\neq" nil nil nil t)
     ("int" "int" "\\int" nil nil nil t)

     ("or" "text or in math" "\\text{ or }" nil nil nil t)
     ("and" "text and in math" "\\text{ and }" nil nil nil t)


     ("capd" "Inserts cap dots" "\\cap \\cdots \\cap " nil nil nil t)
     ("cupd" "Inserts cup dots" "\\cup \\cdots \\cup " nil nil nil t)
     ("plusd" "Inserts plus dots" "+ \\cdots + " nil nil nil t)
     ("oplusd" "Inserts oplus dots" "\\oplus \\cdots \\oplus " nil nil nil t)
     ("timesd" "Inserts times dots" "\\times \\cdots \\times " nil nil nil t)
     ("otimesd" "Inserts otimes dots" "\\otimes \\cdots \\otimes " nil nil nil t)
     ("cdotd" "Inserts cdot dots" "\\cdot \\cdots \\cdot " nil nil nil t)
     ("sed" "Inserts subset dots" "\\subseteq \\cdots \\subseteq " nil nil nil t)
     ("ssed" "Inserts superset dots" "\\supseteq \\cdots \\supseteq " nil nil nil t)
     ("sned" "Inserts subsetneq dots" "\\subsetneq \\cdots \\subsetneq " nil nil nil t)
     ("ssned" "Inserts supersetneq dots" "\\supsetneq \\cdots \\supsetneq " nil nil nil t)
     ("leqd" "Inserts leq dots" "\\leq \\cdots \\leq " nil nil nil t)
     ("geqd" "Inserts geq dots" "\\geq \\cdots \\geq " nil nil nil t)


     ("osuml"       "Insert \\bigoplus\\limits_{}^{}"
      "\\bigoplus\\limits_{?}^{}"  cdlatex-position-cursor nil nil t)
     ("oprodl"       "Insert \\bigotimes\\limits_{}^{}"
      "\\bigotimes\\limits_{?}^{}"  cdlatex-position-cursor nil nil t)
     ("cupl"       "Insert \\bigcup\\limits_{}^{}"
      "\\bigcup\\limits_{?}^{}"  cdlatex-position-cursor nil nil t)
     ("cupl"       "Insert \\bigcup\\limits_{}^{}"
      "\\bigcup\\limits_{?}^{}"  cdlatex-position-cursor nil nil t)
     ("prodl"       "Insert \\prod\\limits_{}^{}"
      "\\prod\\limits_{?}^{}"  cdlatex-position-cursor nil nil t)
     ("capl"       "Insert \\bigcap\\limits_{}^{}"
      "\\bigcap\\limits_{?}^{}"  cdlatex-position-cursor nil nil t)
     ("wedgel"       "Insert \\bigwedge\\limits_{}^{}"
      "\\bigwedge\\limits_{?}^{}"  cdlatex-position-cursor nil nil t)
     ("veel"       "Insert \\bigvee\\limits_{}^{}"
      "\\bigvee\\limits_{?}^{}"  cdlatex-position-cursor nil nil t)

     ("bin"       "Insert \\binom{}{}"
      "\\binom{?}{}"  cdlatex-position-cursor nil nil t)
     ("oper"       "Insert \\operatorname{}"
      "\\operatorname{?}"  cdlatex-position-cursor nil nil t)
     ))

  (cdlatex-math-modify-alist
   '((?A    "\\abs"          nil        t   nil nil )
     (?a    "\\norm"          nil        t   nil nil )
     (?h    "\\ch"          nil        t   nil nil )
     (?t "\\text" nil t nil nil)
     ( ?l    "\\operatorname"                   "\\textsl" t   nil nil )
     (?o "\\mathring" nil t nil nil)
     ( ?C    "\\Class"           nil        t   nil nil )
     ( ?B    "\\mathbb"            nil t   nil nil )
     ))

  (cdlatex-math-symbol-alist
   '((?o ("\\omega" "\\circ"))
     (?O ("\\Omega" "\\degree"))
     (?+  ("\\cup" "\\oplus"))
     (?*  ("\\times" "\\otimes" "\\bullet"))
     (?T ("\\top" "\\bot" "\\Type"))
     (?t ("\\tau" "\\tan" "\\arctan"))
     (?P ("\\Pi" "\\Prop"))
     (?V ("\\vdash" "\\dashv"))
     (?4 ("\\quad" "\\qquad"))
     (?j ("\\iota" "\\jmath"))
     (?I ("\\mid" "\\Im"))
     (?_ ("\\downarrow" "\\Downarrow"))
     ))

  :hook
  (org-mode . org-cdlatex-mode)
  (LaTeX-mode . turn-on-cdlatex)
  )

;;; -> Programming -> LaTeX -> Math delimiters

(use-package math-delimiters
  :load-path "~/.emacs.d/lisp/math-delimiters"
  :bind
  (:map org-mode-map
	("$" . math-delimiters-insert))
  (:map TeX-mode-map
	("$" . math-delimiters-insert))
  :custom
  (math-delimiters-compressed-display-math nil))

;;; -> Programming -> Lisp

(use-package lisp-mode
  :ensure nil  ; built-in package
  :hook ((emacs-lisp-mode . setup-check-parens)
         (lisp-mode . setup-check-parens)
         (scheme-mode . setup-check-parens)
         (clojure-mode . setup-check-parens)
	 (racket-mode . setup-check-parens))
  :config
  (defun setup-check-parens ()
    (add-hook 'before-save-hook #'check-parens nil t)))



;;; -> Programming -> Lisp -> Common lisp

(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

;;; -> Programming -> Lisp -> Racket

(use-package racket-mode
  :defer t)

;;; -> Programming -> Lisp -> Clojure

(use-package clojure-mode
  :custom
  (clojure-indent-style 'always-indent)
  (clojure-indent-keyword-style 'always-indent)
  (clojure-enable-indent-specs nil)
  (clojure-align-forms-automatically t)
  :hook
  (clojure-mode . aggressive-indent-mode))

(use-package cider
  :custom (cider-edit-jack-in-command t))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;;; -> Programming -> Haskell

(use-package haskell-mode
  :defer t)

;;; -> Programming -> Nix

(use-package nix-mode
  :mode "\\.nix\\'")

;;; -> Programming -> OCaml
;;; Adapted from
;;; https://batsov.com/articles/2022/08/23/setting-up-emacs-for-ocaml-development/

;; Major mode for OCaml programming
(use-package tuareg
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package utop
  :ensure t
  :commands utop
  :config
  ;; optional: use utop as the default REPL for tuareg
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

;; (use-package ob-ocaml
;;   :ensure nil   ;; if not available from MELPA, see fallback below
;;   :after org
;;   :config
;;   (setq org-babel-default-header-args:ocaml
;; 	'((:session . "utop")    ; use utop session by default
;;           (:results . "output")  ; capture printed output
;;           (:exports . "both")    ; export code and results
;;           (:cache . "no"))))

;; Major mode for editing Dune project files
(use-package dune)

;; Merlin provides advanced IDE features
(use-package merlin
  :hook tuareg-mode)

(use-package merlin-eldoc
  :hook ((tuareg-mode) . merlin-eldoc-setup))

;; Disabled since using flymake for now, might be worth switching?
;; This uses Merlin internally
;; (use-package flycheck-ocaml
;;   :ensure t
;;   :config
;;   (flycheck-ocaml-setup))

;;; -> Programming -> Agda

(when (eq system-type 'darwin)
  (let ((coding-system-for-read 'utf-8))
    (shell-command-to-string "agda --emacs-mode locate")))


;;; -> Programming -> Lean
;;; Currently lean simply works better in vscode. Figures.

;; (use-package lsp-ui)

;; (use-package lean4-mode
;;   :vc (:url "https://github.com/leanprover-community/lean4-mode")
;;   ;; to defer loading the package until required
;;   :commands (lean4-mode))

;;; -> Programming -> Octave

(use-package octave
  :mode (("\\.m\\'" . octave-mode))
  :config
  ;; (setq octave-block-offset 4)
  ;; (add-hook 'octave-mode-hook
  ;;           (lambda ()
  ;;             (setq comment-start "% ")
  ;;             (setq comment-add 0)))
  )

;;; --> Programming -> Restclient

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
	      ("C-c C-c" . restclient-http-send-current)
	      ("C-c C-v" . restclient-http-send-current-stay-in-window)
	      ("C-c C-r" . restclient-http-send-current-raw))
  :config
  (setq restclient-log-request t
        restclient-same-buffer-response t
        restclient-same-buffer-response-name "*HTTP Response*"))

;;; --> Programming -> Docker

(use-package dockerfile-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'"))

(use-package docker
  :ensure t
  :bind ("M-s d" . docker))

;;; --> Video Trimmer
(use-package video-trimmer
  :load-path "~/.emacs.d/lisp/"
  :commands video-trimmer-trim
  :bind (:map dired-mode-map
	      ("V" . video-trimmer-trim))
  :custom
  (video-trimmer-move-by-increment 1.0)
  (video-trimmer-auto-show-transient-menu t))

;;; --> Misc functions

(defun copy-current-line ()
  "Copy the current line into the kill ring without affecting the cursor position."
  (interactive)
  (let ((line-text (buffer-substring (line-beginning-position) (line-end-position))))
    (kill-new line-text))
  (message "Copied current line."))

;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
	      (move-file-to-trash filename)
	      (message "Deleted file %s." filename)
	      (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(defun run-emacs-with-current-directory (&optional arg)
  "Run Emacs with the current file's directory as the configuration directory.
Calling with single prefix ARG (C-u) enables debugging.
Calling with double prefix ARG (C-u C-u) runs Emacs with -Q."
  (interactive "P")
  (let* ((emacs-path "/opt/homebrew/Cellar/emacs-plus@30/30.2/Emacs.app/Contents/MacOS/Emacs")
         (current-dir (if buffer-file-name
                          (file-name-directory buffer-file-name)
                        default-directory))
         (args (cond ((equal arg '(16)) '("-Q"))  ; C-u C-u
                     (t (list "--init-directory" (expand-file-name current-dir))))))
    (when (equal arg '(4))  ; single C-u
      (setq args (cons "--debug-init" args)))
    (apply #'start-process "emacs" nil emacs-path args)))

(defun quit-window--and-kill ()
  "Quit and kill the buffer, also closing its window."
  (interactive)
  (quit-window t))

(defun random-line ()
  "Jump to random line in the buffer."
  (interactive)
  (goto-line (1+ (random (count-lines (point-min) (point-max))))))

(defvar-local hoagie-narrow-toggle-markers nil
  "A cons cell (beginning . end) that is updated when using `hoagie-narrow-toggle'.")

(defun hoagie-narrow-toggle ()
  "Toggle widening/narrowing of the current buffer.
If the buffer is narrowed, store the boundaries in
`hoagie-narrow-toggle-markers' and widen.
If the buffer is widened, then narrow to region if
`hoagie-narrow-toggle-markers' is non nil (and then discard those
markers, resetting the state)."
  (interactive)
  (if (buffer-narrowed-p)
      (progn
        (setf hoagie-narrow-toggle-markers (cons (point-min)
                                                 (point-max)))
        (widen))
    ;; check for toggle markers
    (if (not hoagie-narrow-toggle-markers)
        (message "No narrow toggle markers.")
      ;; do the thing
      (narrow-to-region (car hoagie-narrow-toggle-markers)
                        (cdr hoagie-narrow-toggle-markers))
      (setf hoagie-narrow-toggle-markers nil))))

(global-set-key (kbd "C-x n t") #'hoagie-narrow-toggle)

(defun time-until-date ()
  "Calculate and display the time remaining until a target date and time.
Uses org-mode's calendar picker for date selection and prompts for time.
Shows the remaining time in HH:MM format suitable for a physical timer.

Accepts time in HH:MM format or military time (HHMM, e.g., 1800 for 18:00).
If more than 100 hours remain, shows days + hours instead."
  (interactive)

  (let* ((date (org-read-date nil nil nil "Select target date: "))
         (time-input (read-string "Enter time (HH:MM or HHMM military, default 23:59): " nil nil "23:59"))
         ;; Convert military time format if needed
         (time (cond
                ;; If it contains a colon, use as-is (HH:MM format)
                ((string-match-p ":" time-input) time-input)
                ;; If it's exactly 4 digits, treat as military time (HHMM)
                ((string-match-p "^[0-9]\\{4\\}$" time-input)
                 (concat (substring time-input 0 2) ":" (substring time-input 2 4)))
                ;; If it's 3 digits, assume HMM format (e.g., 630 = 06:30)
                ((string-match-p "^[0-9]\\{3\\}$" time-input)
                 (concat "0" (substring time-input 0 1) ":" (substring time-input 1 3)))
                ;; Otherwise, use as-is
                (t time-input)))
         (target-datetime (concat date " " time ":00"))
         (current-time (current-time))
         (target-time (date-to-time target-datetime))
         (time-diff (time-subtract target-time current-time))
         (diff-seconds (float-time time-diff)))

    (if (< diff-seconds 0)
        (message "Target time is in the past!")
      (let* ((total-minutes (floor (/ diff-seconds 60)))
             (hours (floor (/ total-minutes 60)))
             (minutes (mod total-minutes 60)))

        (cond
         ;; If less than or equal to 100 hours, show HH:MM format for timer
         ((<= hours 100)
          (let ((result (format "%02d:%02d" hours minutes)))
            (message "Time until %s %s: %s (set timer to %s)"
                     date time result result)
            result))

         ;; If more than 100 hours, show days and remaining hours
         ((> hours 100)
          (let* ((days (floor (/ hours 24)))
                 (remaining-hours (mod hours 24)))
            (message "Time until %s %s: %d days, %d hours, %d minutes (beyond 100h timer limit)"
                     date time days remaining-hours minutes)
            (format "%dd %02dh %02dm" days remaining-hours minutes))))))))

;;;
;;; End of configuration file.
;;; init.el ends here
