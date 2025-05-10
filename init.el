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
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq use-package-always-ensure t)
(setq package-native-compile t)
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
  (delete-selection-mode 1)
  (global-auto-revert-mode 1)
  (pixel-scroll-precision-mode 1)
  :custom
  (inhibit-startup-message t)
  (frame-resize-pixelwise t)
  (cursor-type 'bar)
  (echo-keystrokes .01)
  (confirm-kill-emacs #'y-or-n-p)
  (make-backup-files nil)
  (global-auto-revert-non-file-buffers t)
  (delete-by-moving-to-trash t)
  (sentence-end-double-space nil)
  (pixel-scroll-precision-use-momentum t)

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

  (server-start)

  )

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package emacs-everywhere)

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
    (mapc #'disable-theme custom-enabled-themes)
    (pcase `(,appearance ,my/redshift?)
      ('(light nil) (load-theme my/light-theme t))
      ('(dark nil)  (load-theme my/dark-theme t))
      ('(light t)   (load-theme my/light-theme-redshift t))
      ('(dark t)    (load-theme my/dark-theme-redshift t))))
  (my/apply-theme 'light))

(use-package nerd-icons
  :defer t)

(use-package doom-modeline
  :functions doom-modeline-mode
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 24)
  (doom-modeline-icon t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.01))

(use-package paren
  :init
  (electric-pair-mode 1)
  (show-paren-mode 1)
  :config
  (add-function :before-until electric-pair-inhibit-predicate
		(lambda (c) (eq c ?<)))
  :custom
  (show-paren-delay 0))

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
	 ("q" . quit-window--and-kill)))

;;; -> Look and feel -> Tabs, frames, windows

(use-package tab-bar
  :custom
  (tab-bar-new-tab-choice 'scratch-buffer)
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-last-tab-choice 'delete-frame)
  (tab-bar-auto-width nil)
  :bind (("s-t" . tab-bar-new-tab)
	 ("s-T" . tab-undo)
	 ("s-w" . tab-close)
	 ("C-s-f" . toggle-frame-fullscreen)
	 ("s-o" . other-window))
  :config
  (tab-bar-mode 1))

(use-package visual-fill-column
  :defines
  visual-fill-column-width
  visual-fill-column-center-text
  :hook
  (visual-fill-column-mode . efs/org-mode-visual-fill)
  (text-mode . visual-line-mode)
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

;;; Try to figure out shrface
;; (use-package shrface
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; Add shrface directory to load path - this happens at startup
;;   (add-to-list 'load-path (expand-file-name "lisp/shrface" user-emacs-directory))
  
;;   :custom
;;   ;; Basic settings
;;   (shr-cookie-policy nil)
;;   (shrface-bullets-bullet-list '("▼" "▽" "▿" "▾"))
;;   (shrface-href-versatile t)
;;   (shrface-toggle-bullets nil)
  
;;   :hook
;;   ;; Hook for outline view changes
;;   (outline-view-change . shrface-outline-visibility-changed)
  
;;   :config
;;   ;; Language detection for code blocks
;;   (require 'shr-tag-pre-highlight)
;;   (setq shr-tag-pre-highlight-lang-modes
;;         '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
;;           ("asymptote" . asy) ("dot" . fundamental) ("sqlite" . sql)
;;           ("calc" . fundamental) ("C" . c) ("cpp" . c++) ("C++" . c++)
;;           ("screen" . shell-script) ("shell" . sh) ("bash" . sh)
;;           ("rust" . rustic) ("awk" . bash) ("json" . js)
;;           ("emacslisp" . emacs-lisp) ("el" . emacs-lisp)))
  
;;   ;; Load integrations
;;   (require 'shrface-core)
;;   (require 'shrface-wiki-summary)
;;   (require 'shrface-wallabag)
;;   (require 'shrface-eww)
;;   (require 'shrface-elfeed))

;; Keeps windows still when opening minibuffers
(use-package sinister
  :vc (:url "https://github.com/positron-solutions/sinister")
  :config
  (sinister-stillness-mode 1)
  )

;;; -> OS specific configuration

(pcase system-type
  ;;; MacOS Config 
  ('darwin
   (setq ns-alternate-modifier 'meta)
   (setq ns-right-alternate-modifier 'meta)
   (setq ns-right-control-modifier 'control)
   (setq ns-control-modifier 'control)
   (define-key key-translation-map (kbd "ESC") (kbd "C-g")) ; works in gui only

   ;;(setq ns-right-command-modifier 'hyper) ;; <- Currently unused, enough modifiers atm
   (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
   (defvar org-roam-directory "~/Documents/org")
   (defvar org-directory "~/Documents/org")
   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
   (setq ring-bell-function 'ignore)
   (global-set-key (kbd "C-s-<tab>") #'ns-next-frame)
   (defvar yt-dlp-folder "~/Movies/Youtube"
     "Main directory for downloading videos using yt-dlp.")
   ;; Registers ;;
   (set-register ?r '(file . "~/.emacs.d/init.el"))
   (set-register ?t `(file . ,(concat org-directory "/tasks.org")))
   (set-register ?p `(file . ,(concat org-directory "/journals/Journelly.org")))
   )
  ;;; Android configuration
  ('android
   (defvar org-roam-directory "~/Documents/org")
   (defvar org-directory "~/Documents/org")
   (defvar yt-dlp-folder "~/Movies/Youtube"
     "Main directory for downloading videos using yt-dlp.")
   (defvar elfeed-db-directory "~/Documents/elfeed")
   ;; Registers ;;
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

   (define-key key-translation-map (kbd "ESC") (kbd "C-g")) ; works in gui only
   
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
  ;; Linux Configuration
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
    :custom
    (mixed-pitch-set-height 180))
   )
  )

;;; --> Searching and navigation

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
	 ("C-S-s" . swiper-all)
	 :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1)
  (counsel-mode)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq swiper-use-visual-line-p #'ignore)
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

(use-package expand-region
  :bind ("s-f" . er/expand-region))

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
  :bind ("C-c n h" . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (use-package-enable-imenu-support t)
  :hook (org-mode . (lambda () (imenu-add-to-menubar "Imenu")))
  )

(use-package yasnippet
  :functions yas-global-mode
  :init (yas-global-mode 1))

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
  org-roam-dailie
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

(use-package avy
  :bind (("s-j" . avy-goto-char-timer)
	 ("s-l" . avy-goto-line))
  )

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
  ;;; TODO: Figure out a better key
	 ;; ("C-c e" . crux-eval-and-replace)
	 ([remap move-beginning-of-line] . crux-move-beginning-of-line))
  )

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

(use-package casual-dired
  :defines dired-mode-map
  :ensure nil
  :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu))
  :custom
  (dired-listing-switches "-lagGFDh"))

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

;;; TODO: Wgrep for deadgrep

;;; -> Searching and navigation -> Lookuppers

(use-package wiki-summary
  :bind ("C-c n w" . wiki-summary)
  ;; :vc (:url "https://github.com/Tevqoon/wiki-summary.el")
  :load-path "~/.emacs.d/lisp/wiki-summary.el/"
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

;;; --> AI configuration
;;; -> AI configuration -> GPTel

(use-package gptel
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
   ("C-c g e" . elysium-query)
   ("C-c g b" . my/gptel-toggle-tool-results-local)
   ("C-c g T" . my/gptel-translate-to-english)
   ("C-c g f" . my/gptel-finish)
   (:map gptel-mode-map
	 ("C-c g t" . gptel-org-set-topic))
   )
  :hook
  (org-mode . my/gptel-enable-tool-results-in-org-mode)
  (org-mode . org/enable-gptel-for-chatlog-buffer)
  :custom
  (gptel-default-mode 'org-mode)
  :init
  (setq gptel-model 'gpt-4o-mini)
  :config
  (require 'gptel-org)
  (defvar org-roam-chatlogs-directory "chatlogs/"
    "The directory to save gptel chatlogs in.")
  
  (gptel-make-anthropic "Claude"
    :stream t
    :models
    '(claude-3-7-sonnet-latest
      claude-3-5-haiku-latest
      claude-3-5-sonnet-latest)
    :host "api.anthropic.com"
    :key 'gptel-api-key-from-auth-source
    )
  (gptel-make-gemini "Gemini"
    :stream t
    ;; :host "api.anthropic.com"
    :key 'gptel-api-key-from-auth-source
    )

  
  (defvar gptel-tools-files `(,(expand-file-name "lisp/ai-tools-list.el" user-emacs-directory)
			      ;;,(expand-file-name "lisp/gptel-subtask/gptel-subtask-tool.el" user-emacs-directory)
			      )
    "the list of all the files which contain gptel tools.")

  ;; This macro runs before our tool file.
  (defmacro my/gptel-tool-definer (&rest args)
    "Define a gptel tool and save it to a variable named my/gptel-tool-tools--NAME.
ARGS are passed directly to `gptel-make-tool`.
The variable will be set to the tool structure created by `gptel-make-tool`.

Example usage:
  (my/gptel-tool-definer
   :name \"get_weather\"
   :function (lambda (location) ...)
   :description \"Get the current weather\"
   ...)"
    (let* ((name-plist-pos (cl-position :name args))
           (tool-name (and name-plist-pos (nth (1+ name-plist-pos) args)))
           (var-name (and tool-name
			  (intern (format "my/gptel-tool-tools--%s" tool-name)))))
      (unless tool-name
	(error "Tool name is required"))
      `(progn
	 ;; Define the gptel tool
	 (gptel-make-tool ,@args)
	 
	 ;; Store the tool structure in the variable
	 (defvar ,var-name nil
           ,(format "The gptel tool named '%s'." tool-name))
	 
	 ;; Set the variable to the tool structure
	 (setq ,var-name
               (cdr (assoc ,tool-name
                           (cdr (assoc (or (plist-get ',args :category) "misc")
                                       gptel--known-tools)))))
	 
	 ;; Return the variable name for convenience
	 ',var-name)))

  (defun my/gptel--reload-tools ()
    "reload the tools in `gptel-tools-files`."
    (interactive)
    (message "Reloading GPTel tools.")
    (setq gptel--known-tools nil
          gptel-tools nil)
    (dolist (tool-file gptel-tools-files)
      (load-file tool-file)))

  ;; Load custom tools
  (my/gptel--reload-tools)
  
  ;; Tool results default to nil
  (setq-default gptel-include-tool-results nil)

  ;; Org mode shows tool results (since they're minified)
  (defun my/gptel-enable-tool-results-in-org-mode ()
    "Enable tool results locally in org-mode."
    (setq-local gptel-include-tool-results t))
  
  (defun my/gptel-toggle-tool-results-local ()
    "Toggle gptel tool results inclusion buffer-locally between 'auto and t.
If not set buffer-locally, starts with 'auto."
    (interactive)
    (let* ((current-local-value (and (local-variable-p 'gptel-include-tool-results)
                                     (buffer-local-value 'gptel-include-tool-results (current-buffer)))))
      (set (make-local-variable 'gptel-include-tool-results)
           (if (eq current-local-value t) 'auto t))
      (message "Gptel tool results inclusion set to %s locally" 
               (buffer-local-value 'gptel-include-tool-results (current-buffer)))))

  ;; Vulpea interface
  (defun org/enable-gptel-for-chatlog-buffer ()
    "Enable gptel-mode if the current buffer has the `chatlog' tag."
    (when (and (buffer-file-name)
               (member "chatlog" (vulpea-buffer-tags-get)))
      (gptel-mode 1)))
  
  (defun org/save-gptel-chat-as-node ()
    "Save the current gptel chat buffer as an org-roam node and link it in today's journal.
If the buffer already has an ID property, just save the buffer."
    (interactive)
    (cond
     ((not (derived-mode-p 'org-mode))
      (message "Current buffer is not in org-mode"))
     
     ((org-id-get 1) ; If ID exists, just save the buffer
      (save-buffer))
    
     (t              ; Otherwise, proceed with the full node creation
      (let* ((title (read-string "Title for chat node: " (format-time-string "Chat %Y-%m-%d %H:%M")))
             (file-name (concat (format-time-string "Chat-%Y-%m-%d_%H-%M") ".org"))
             (chatlog-directory (concat org-roam-directory "/" org-roam-chatlogs-directory))
             (full-path (expand-file-name file-name chatlog-directory)))

	;; Create the chatlogs directory if it doesn't exist
	(unless (file-directory-p chatlog-directory)
          (make-directory chatlog-directory t))

	;; Save the current buffer to the specified file
	(write-file full-path)     ; Directly save the current buffer content to the specified path
	(save-excursion            ; Add the title
          (goto-char (point-min))
	  (org-id-get-create)      ; Generate an ID for the new node
          (re-search-forward ":END:" nil t)
          (insert (concat "\n#+title: " title)))
	(save-buffer)              ; Save the buffer to get the id going and to activate vulpea etc

	;; Link the new node in today's journal
	(let ((node-id (org-id-get)))
          (org-roam-dailies-autocapture-today "c" (org-roam-link-make-string node-id title))
          (message "Chat saved as '%s' and linked in today's journal." title))))))

  ;;; -> GPTel -> rewrite utilities
  (defun my/gptel-translate-to-english ()
    "Rewrite the current buffer's text to English while preserving Org IDs and links."
    (interactive)
    (gptel--suffix-rewrite "Translate this text to English while keeping Org IDs and links intact."))

  (defun my/gptel-finish ()
    "Finalize the content at point or in the current buffer while preserving Org IDs and links."
    (interactive)
    (gptel--suffix-rewrite "Finish this content while keeping Org IDs and links intact."))
  
  )

;; (use-package gptel-subtask
;;   :ensure nil
;;   :load-path "~/.emacs.d/lisp/gptel-subtask/"
;;   :after gptel
;;   :requires gptel
;;   )

;;; End of GPTel package block

;;; -> AI configuration -> elysium

(use-package elysium
  :hook
  (prog-mode . smerge-mode)
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  ;; Can be customized to horizontal
  (elysium-window-style 'vertical)) 

;;; End of elysium package block

;;; -> AI configuration -> aidermacs

(use-package aidermacs
  :if (eq system-type 'darwin)
  :after gptel vterm
  :bind ("C-c g a" . aidermacs-transient-menu)
  :custom
  (aidermacs-backend 'vterm)
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "gpt-4o-mini")
  (aidermacs-extra-args '("--edit-format editor-diff" "--copy-paste"))
  ;; (aidermacs-architect-model "your-architect-model")  ;; Optional
  ;; (aidermacs-editor-model "your-editor-model")        ;; Optional
  :config
  ;; Set API keys from the auth source once for both OpenAI and Claude
  (setenv "OPENAI_API_KEY" (gptel-api-key-from-auth-source "api.openai.com"))
  (setenv "CLAUDE_API_KEY" (gptel-api-key-from-auth-source "api.anthropic.com")))

;;; End of aidermacs package block

;;; --> Org mode

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
  (defun js/org-rename-buffer-to-title ()
    "Rename buffer to value of #+TITLE:."
    (interactive)
    (let ((title (cadar (org-collect-keywords '("title")))))
      (when title (rename-buffer title))))
  
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
    "Get the title for a given URL based on its type."
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
    "Return the current `link' formatted as an org link with its title."
    (let ((title (js/get-link-title url)))
      (if title
          (org-link-make-string url title)
	(org-link-make-string url))))  ; Just create a plain link if no title

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
          (let ((text (buffer-substring-no-properties (region-beginning) (region-end)))
		(count 0))
            (with-temp-buffer
              (insert text)
              (goto-char (point-min))
              
              ;; 1. Try org-mode links [[url][description]]
              (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\[" nil t)
		(let ((url (match-string 1)))
                  (when (string-match-p "^\\(https?://\\|www\\.\\)" url)
                    (browse-url url)
                    (setq count (1+ count)))))
              
              ;; 2. Try Markdown links [description](url)
              (goto-char (point-min))
              (while (re-search-forward "\\[.*?\\](\\([^)]*\\))" nil t)
		(let ((url (match-string 1)))
                  (when (string-match-p "^\\(https?://\\|www\\.\\)" url)
                    (browse-url url)
                    (setq count (1+ count)))))
              
              ;; 3. Try HTML href attributes
              (goto-char (point-min))
              (while (re-search-forward "href=[\"']\\([^\"']+\\)[\"']" nil t)
		(let ((url (match-string 1)))
                  (when (string-match-p "^\\(https?://\\|www\\.\\)" url)
                    (browse-url url)
                    (setq count (1+ count)))))
              
              ;; 4. Try plain text URLs if nothing else found
              (when (= count 0)
		(goto-char (point-min))
		(while (re-search-forward "\\(https?://\\|www\\.\\)[^\s\n\"]+" nil t)
                  (let ((url (match-string 0)))
                    (when (string-match "^www\\." url)
                      (setq url (concat "https://" url)))
                    (when (string-match "\\([.,:;\"']+\\)$" url)
                      (setq url (substring url 0 (match-beginning 1))))
                    (browse-url url)
                    (setq count (1+ count))))))
            (message "Opened %d URLs in browser" count))
	
	;; Otherwise just use the built-in browse-url for single URL
	(let ((url (thing-at-point 'url)))
          (if url
              (browse-url url)
            (message "No URL at point"))))))

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

  )
;;; End of org-mode package block

(use-package org-mac-link
  :if (eq system-type 'darwin)
  :ensure org)

(use-package xenops
  :after org
  :defer nil
  :bind
  (:map org-mode-map
	("C-c n !" . xenops-mode))
  :config
  (setq xenops-reveal-on-entry t)
  (setq xenops-math-image-scale-factor 1.6) ;; Scaling factor for SVG math images
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
  (org-latex-create-formula-image-program 'imagemagick)
  (org-latex-packages-alist '(("" "/Users/jure/.emacs.d/defaults/js" t)))
  (org-export-in-background t)
  
  :config
  (add-to-list
   'org-preview-latex-process-alist
   '(imagemagick-lualatex
     :programs ("lualatex" "convert") :description "pdf > png"
     :message
     "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf" :image-output-type "png"
     :image-size-adjust (1.0 . 1.0) :latex-compiler
     ("lualatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %O")))

  (plist-put org-format-latex-options :scale 1.6)
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
  :after org
  :hook
  org-mode
  (org-after-todo-state-change . mm/org-insert-trigger)
  (org-after-todo-statistics . org-summary-todo)
  :config
  (defun org-user/add-trigger-next ()
    "A NEXT heading propagates itself upon completion
     to the next sibling with state TODO"
    (interactive)
    (org-set-property "TRIGGER" "next-sibling(todo-only) todo!(NEXT) chain!(\"TRIGGER\") self delete-property!(\"TRIGGER\")"))

  (defun mm/org-insert-trigger ()
    "Automatically insert chain-find-next trigger when entry becomes NEXT"
    (cond ((equal org-state "NEXT")
           (org-user/add-trigger-next))))

   ;; Automatically complete the parent with a statistics cookie when all children are complete
  (defun org-summary-todo (_n-done n-not-done)
    "Switch entry to DONE when all subentries are done"
    (let (org-log-done org-todo-log-states) ; turn off logging
      (when (= n-not-done 0)
	(org-todo "DONE"))))
  )

;;; -> Org mode -> Org roam

(use-package org-roam
  :functions
  org-roam-node-from-ref
  :bind (("C-c n b " . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
	 ("C-c n n i" . org-roam-node-insert-from-url)
         ("C-c n d" . org-roam-dailies-map)
         ("C-c n n r" . org-roam-refile)
         ("C-c n n g" . org-id-get-create)
         ("C-c n n t" . org-roam-extract-subtree)
         ("C-c n n a" . org-roam-alias-add)
         ("C-c n c" . org-capture-task)
         ("C-c n n u" . org-roam-ui-open)
	 ("C-c n o" . open-urls-at-point-or-region)
	 ("C-c n r" . js/roamify-url-at-point)
	 ("C-c n t" . org-roam-tag-add)

         :map org-mode-map
         ("C-M-i" . completion-at-point)

	 ;; Narrowing and movement
	 ("C-c n ." . my/org-narrow-to-heading-content) ;; current position
	 ("C-c n >" . my/org-next-heading-narrow)       ;; move forward
	 ("C-c n <" . my/org-previous-heading-narrow)   ;; move backward
         )
  :hook (org-roam-mode . visual-line-mode)
  
  :custom
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

  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "journals/")
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:40}" 'face 'org-tag)))

  (org-archive-file-header-format nil)

  :config
  (add-to-list 'org-roam-file-exclude-regexp ".stversions/" t)
  
;;; -> org-roam -> Aesthetics
  ;; Color roam links differently
  (defface org-roam-link
    '((t :foreground "orange" :underline t))
    "Face for Org-roam links."
    :group 'org-roam-faces)
  
  (org-link-set-parameters "id" :face 'org-roam-link)
  ;; Folded backlink buffer
  (add-to-list 'magit-section-initial-visibility-alist (cons 'org-roam-node-section 'hide))
;;; -> org-roam -> Dailies
  (setq org-extend-today-until 4)

  ;; Fuck it, when nothing else works, bring in a global variable
  (defvar org-roam-capture-content nil
    "Variable to pass content to capture templates.")
  
  (setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+startup: content")
         :unnarrowed t)
        ))

  (defvar org-roam-autocapture-templates
    '(("r" "reference" plain "%?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
       :unnarrowed t))
    "A list of templates to use for automatic capture."
    )

  (setq org-roam-dailies-capture-templates
	`(("d" "default" entry "* %?"
	   :target (file+head "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n#+startup: content"))
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
			 :templates '(("t" "task" entry "** TODO %?"
				       :target (node "C6C9881B-7EF4-4DAF-A502-84D396372A68")
				       :unnarrowed nil))
			 ;:props (list :override-default-time (current-time))
			 )
      ))

  ;;; -> org-roam -> Autocapture -> Browser integration

  (defvar org-roam-capture--browser nil
    "Variable to pass current browser to capture templates.")
  
  ;;; TODO: Simplify the autocapture templates
  (defvar org-roam-dailies-autocapture-templates
    '(("w" "url capture" entry "* %(eval org-roam-capture-content)"
       :target (file+head+olp "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+startup: content" ("Web"))
       :immediate-finish t)
      ("r" "url reading capture" entry "* PROCESS %(eval org-roam-capture-content)"
       :target (file+head+olp "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+startup: content" ("Processing"))
       :immediate-finish t)
      ("e" "elfeed link capture" entry "* %(eval org-roam-capture-content)"
       :target (file+head+olp "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+startup: content" ("Elfeed"))
       :immediate-finish t)
      ("p" "process capture" entry "* PROCESS %(eval org-roam-capture-content)"
       :target (file+head+olp "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+startup: content" ("Processing"))
       :immediate-finish t)
      ("c" "chatlog capture" entry "* %(eval org-roam-capture-content)"
       :target (file+head+olp "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+startup: content" ("Chats"))
       :immediate-finish t))
    
    "A list of templates to use for automatic daily capture."
    )
  
  (defun org-roam-dailies-autocapture-today (keys &optional contents)
    "A function to automatically capture content into a daily template."
    (let ((org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory))
          (org-roam-dailies-directory "./")
	  (org-roam-capture-content (or contents org-roam-capture-content))
	  )
      (org-roam-capture- :keys keys
			 :node (org-roam-node-create)
			 :templates org-roam-dailies-autocapture-templates
			 :props (list :override-default-time (current-time)))
      ))

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
    "Association list of available browsers with their corresponding URL retrieval functions.")

  (defun js/retrieve-url (&optional browser)
    "Retrieve the URL of the given browser page as a string.
Using the org-mac-link, this comes pre-formatted with the url title."
    (let* ((browser (or browser (caar js/browsers))) ;; Default to first browser in list
           (url-function (cdr (assoc browser js/browsers))))
      (if url-function
          (funcall url-function)
	(user-error "Browser %s not supported" browser))))

  (defun js/extract-org-link (org-link-string)
    "Extract the URL and description from an Org formatted link string."
    (with-temp-buffer
      (insert org-link-string)
      (org-mode)
      (goto-char (point-min))
      (let ((link-context (org-element-link-parser)))
	(when link-context
          (let ((raw-link (org-element-property :raw-link link-context))
		(description (buffer-substring-no-properties
                              (org-element-property :contents-begin link-context)
                              (org-element-property :contents-end link-context))))
            (list raw-link description))))))

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
	(wallabag-add-entry url)
	;; Sync wallabag changes to server
	(run-with-timer 2 nil #'wallabag-request-and-synchronize-entries)
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

  (defun js/roamify-url-at-point ()
    "Convert a URL at point into an org-roam node and replace the link."
    (interactive)
    (let* ((context (org-element-context))
           (type (org-element-type context))
           (link-type (org-element-property :type context))
           (url (org-element-property :raw-link context))
           (end (org-element-property :end context))
           (beg (org-element-property :begin context))
	   (title-beg (org-element-property :contents-begin context))
	   (title-end (org-element-property :contents-end context))
	   (working-title (or (buffer-substring-no-properties title-beg title-end)
			      (js/get-link-title url))))
      (cond ((not (equal type 'link))
	     (message "No link found at point."))
	    ((equal link-type "id")
	     (message "Is already roam link."))
	    (t
	     ;; TODO: Add failsafe for roamify if title doesn't exist
	     (let* ((node-title working-title)
		    (node (org-roam-node-create :title node-title)))
	       (defun roamify-finalizer ()
		 "The functions to run after successfully capturing the new node."
		 ;;; TODO: Handle headings with existing id
		 (when-let* ((full-node (org-roam-node-from-ref url))
			     (new-id (org-roam-node-id full-node)))
		   (delete-region beg end)
		   (insert (org-roam-link-make-string new-id node-title))
		   (message "Created org-roam node: %s" node-title)))
	       
	       (org-roam-capture-
		:keys "d"
		:node node
		:info (list :ref url)
		:props (list :finalize #'roamify-finalizer))
	       )))))

  ;;; -> org-roam -> Archiving
  (defun archived-backlink-p (backlink)
    "Checks whether the backlink lives under the Archived today heading."
    (let* ((properties (org-roam-backlink-properties backlink))
	   (outline (plist-get properties :outline)))
      (equal "Archived today" (car outline))))

  ;; https://freerangebits.com/posts/2024/01/archiving-in-org-mode/
  (defun pjones:org-archive-subtree-to-daily (&optional _find-done)
    "Arhive the current subtree to the roam daily file."
    (interactive "P")
    (require 'org-roam)
    (when-let* ((today (save-window-excursion
			 (org-roam-dailies-goto-today "d")
			 (buffer-file-name)))
		(org-archive-location
		 (concat today "::* Archived today :ARCHIVE:"))
		(heading (org-get-heading t t t t))
		;; Take note of the originating org id
		(file-id (save-excursion
			   (goto-char (point-min))
			   (org-roam-id-at-point))))
      (when file-id ;; Add a source node property if we started out in a place with an id
	(org-set-property "ARCHIVE_NODE" (org-roam-link-make-string file-id)))
      (org-archive-subtree 0))
    (save-buffer))

  (custom-set-variables
   '(org-archive-default-command #'pjones:org-archive-subtree-to-daily))

  (setq org-archive-subtree-save-file-p t)

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

  ) ;;
;;; End of org-roam package block

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
    "Return non-nil if current buffer has a todo entry."
    (org-element-map
        (org-element-parse-buffer 'headline)
        'headline
      (lambda (h)
        (eq (org-element-property :todo-type h)
            'todo))
      nil 'first-match))

  (defun org/has-anki-flashcards-p ()
    "Return non-nil if current buffer has ANKI-related properties."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward ":ANKI_\\(NOTE_TYPE\\|DECK\\|NOTE_ID\\|TAGS\\):" nil t)))

  (defun org/has-gptel-chatlog-p ()
    "Return non-nil if current buffer has GPTEL-related properties."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward ":GPTEL_\\(TOPIC\\|MESSAGES\\|MODEL\\|CONTEXT\\):" nil t)))

  ;; Exclude the relevant tags from inheritance
  (dolist (tag (cons "summary" tags/updating-tags))
    (add-to-list 'org-tags-exclude-from-inheritance tag))
  
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

  (defun tags/maybe-update-tags ()
    "Update tags if enabled for the current buffer."
    (when (and tags/update-tags-enabled
               (not (member (buffer-name) prune/ignored-files))
               (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (tags/org-update-all-tags)
      ))

  (defun tags/enable-tag-updating ()
    "Enable tag updating for the current buffer."
    (setq-local tags/update-tags-enabled t)
    (add-hook 'before-save-hook #'tags/maybe-update-tags nil t)
    (tags/maybe-update-tags))

  :hook
  (org-mode . tags/enable-tag-updating)

  ) ;;
;;; End of vulpea package block

;;; -> Org mode -> Anki

(use-package anki-editor
  :bind
  (:map org-mode-map
	("C-c n p" . my/anki-flashcard-push-current-buffer)
	("C-c n n p" . my/anki-flashcard-push-all)
	("C-c n q" . anki-flashcard-queue-display))
  :after org
  :defer nil
  :vc (:url "https://github.com/anki-editor/anki-editor" :rev :newest)
  :custom
  (anki-editor-latex-style 'mathjax)
  (anki-editor-ignored-org-tags '("project" "flashcards" "ex-flashcards"))

  :config
  (defvar anki-tag-list '()
    "Keeps track of the most recently used flashcard tags.")

  ;; Ensure it's saved on exit
  (add-to-list 'closing-variables 'anki-tag-list)

  (defun anki/my/after-snippet-tag-handler ()
    "Select or create an Anki tag, prioritizing recent tags."
    (let* ((tag (completing-read "Enter tag: " 
				 (delete-dups (cons "" anki-tag-list))
				 nil 
				 nil 
				 (car anki-tag-list))))
      (when (not (string-empty-p tag))
	(setq anki-tag-list (delete nil (cons tag (remove tag anki-tag-list)))))
      tag))

  (tags/make-db-searcher "flashcards")

  ;;; -> Org mode -> Anki -> Overrides
  
  ;; Define environments that should always use Anki builtin LaTeX
  (defcustom anki-editor-builtin-latex-environments '("tikzcd")
    "LaTeX environments that will always be translated using Anki's built-in LaTeX.
This is useful for environments not supported by MathJax."
    :type '(repeat string))

  ;; Helper function to detect if code contains a builtin-only environment
  (defun anki-editor--contains-builtin-env (latex-code)
    "Check if LATEX-CODE contains any environment that should use builtin LaTeX."
    (cl-some (lambda (env) 
               (string-match-p (format "\\\\begin{%s}" env) latex-code)) 
             anki-editor-builtin-latex-environments))

  ;; Override the ox-latex function to handle special environments
  (defun anki-editor--ox-latex (latex _contents _info)
    "Transcode LATEX from Org to HTML.
CONTENTS is nil. INFO is a plist holding contextual information."
    (let* ((code (org-remove-indentation (org-element-property :value latex)))
           (original-style anki-editor-latex-style)
           (contains-special-env (anki-editor--contains-builtin-env code)))
      
      ;; Temporarily override style if needed
      (when (and (eq original-style 'mathjax) contains-special-env)
	(setq anki-editor-latex-style 'builtin))
      
      ;; Process the LaTeX code
      (setq code (cl-ecase (org-element-type latex)
                   (latex-fragment (anki-editor--translate-latex-fragment code))
                   (latex-environment (anki-editor--translate-latex-env code))))
      
      ;; Restore original style
      (setq anki-editor-latex-style original-style)
      
      ;; Return processed code
      (if anki-editor-break-consecutive-braces-in-latex
          (replace-regexp-in-string "}}" "} } " code)
	code)))

  
  (defun anki-editor-note-at-point ()
    "Make a note struct from current entry with modified field handling.
The front of the note will be the heading unless a '** Front' subheading exists.
The back field will be only the content before any subheadings unless a '** Back' subheading exists.
All other subheadings will be ignored."
    (let* ((deck (org-entry-get-with-inheritance anki-editor-prop-deck))
           (note-id (org-entry-get nil anki-editor-prop-note-id))
           (hash (org-entry-get nil anki-editor-prop-note-hash))
           (note-type (or (org-entry-get nil anki-editor-prop-note-type)
                          anki-editor-default-note-type))
           (tags (cl-set-difference (anki-editor--get-tags)
                                    anki-editor-ignored-org-tags
                                    :test #'string=))
           (heading (substring-no-properties (org-get-heading t t t t)))
           (content-before-subheading (anki-editor--note-contents-before-subheading))
           (front-field nil)
           (back-field nil)
           (fields '()))
      
      ;; Look for Front and Back subheadings
      (save-excursion
	(when (org-goto-first-child)
          (cl-loop
           for element = (org-element-at-point)
           for subheading = (substring-no-properties
                             (org-element-property :raw-value element))
           for begin = (save-excursion (anki-editor--skip-drawer element))
           for end = (org-element-property :contents-end element)
           for content = (and begin end 
                              (buffer-substring-no-properties
                               begin (min (point-max) end)))
           when (string= subheading "Front")
           do (setq front-field content)
           when (string= subheading "Back")
           do (setq back-field content)
           while (org-get-next-sibling))))
      
      ;; If Front/Back not explicitly defined, use defaults
      (unless front-field
	(setq front-field heading))
      (unless back-field
	(setq back-field content-before-subheading))
      
      ;; Build fields alist based on note type
      (cond
       ;; For Basic note type
       ((string= note-type "Basic")
	(setq fields (list (cons "Front" front-field)
                           (cons "Back" back-field))))
       
       ;; For other note types, you might need to customize further
       (t
	(setq fields (list (cons "Front" front-field)
                           (cons "Back" back-field)))))
      
      ;; Sort fields (as in the original function)
      (setq fields (sort fields (lambda (a b) (string< (car a) (car b)))))
      
      (unless deck (user-error "Missing deck"))
      (unless note-type (user-error "Missing note type"))
      
      (make-anki-editor-note :id note-id
                             :model note-type
                             :deck deck
                             :tags tags
                             :fields fields
                             :hash hash
                             :marker (point-marker))))

  ;;; -> Org mode -> Anki -> Flashcard Queue System
  (defvar anki-flashcard-queue-file
    (expand-file-name "anki-flashcard-queue.el" 
                      (expand-file-name "lisp" user-emacs-directory))
    "File to save the flashcard queue between Emacs sessions.")

  (defvar anki-flashcard-queue nil
    "List of files with flashcard changes that need to be pushed to Anki.")

  (defvar anki-flashcard-error-buffer "*Anki Flashcard Errors*"
    "Buffer name for displaying Anki flashcard push errors.")

  ;; Path conversion functions
  (defun anki-flashcard-make-relative-path (file)
    "Convert FILE to a path relative to `org-roam-directory`."
    (file-relative-name (expand-file-name file) org-roam-directory))

  (defun anki-flashcard-make-absolute-path (file)
    "Convert relative FILE to absolute path from `org-roam-directory`."
    (expand-file-name file org-roam-directory))

  ;; Queue management functions
  (defun anki-flashcard-queue-load ()
    "Load the flashcard queue from file."
    (when (file-exists-p anki-flashcard-queue-file)
      (load-file anki-flashcard-queue-file)))

  (defun anki-flashcard-queue-save ()
    "Save the flashcard queue to file."
    ;; Create directory if it doesn't exist
    (let ((dir (file-name-directory anki-flashcard-queue-file)))
      (unless (file-exists-p dir)
	(make-directory dir t)))
    
    (with-temp-file anki-flashcard-queue-file
      (insert ";; Anki flashcard queue - DO NOT EDIT MANUALLY\n")
      (insert ";; This file is auto-generated by Emacs\n\n")
      (insert "(setq anki-flashcard-queue\n")
      (insert "  '(\n")
      (dolist (file anki-flashcard-queue)
	(insert (format "    %S\n" file)))
      (insert "  ))\n")))

  (defun anki-flashcard-queue-add (file)
    "Add FILE to flashcard queue if not already present."
    (anki-flashcard-queue-load) ;; Ensure we have the latest queue
    (let ((rel-path (anki-flashcard-make-relative-path file)))
      (unless (member rel-path anki-flashcard-queue)
	(push rel-path anki-flashcard-queue)
	(anki-flashcard-queue-save))))

  (defun anki-flashcard-queue-remove (file)
    "Remove FILE from flashcard queue."
    (anki-flashcard-queue-load) ;; Ensure we have the latest queue
    (let ((rel-path (anki-flashcard-make-relative-path file)))
      (when (member rel-path anki-flashcard-queue)
	(setq anki-flashcard-queue (delete rel-path anki-flashcard-queue))
	(anki-flashcard-queue-save))))

  ;; Error handling functions
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

  ;; Save hook function for flashcard buffers
  (defun anki-flashcard-save-hook ()
    "Function to run on save for buffers with flashcards tag."
    (when (buffer-file-name)
      (anki-flashcard-queue-add (buffer-file-name))
      (message "Added to Anki flashcard queue: %s" (buffer-file-name))))

  (defun anki-flashcard-setup-buffer ()
    "Set up the current buffer for flashcards if it has the 'flashcards' tag."
    (when (and (buffer-file-name)
               (member "flashcards" (vulpea-buffer-tags-get)))
      ;; Add to local hook
      (add-hook 'after-save-hook #'anki-flashcard-save-hook nil t)))

  ;; Add this function to org-mode-hook
  (add-hook 'org-mode-hook #'anki-flashcard-setup-buffer)

  ;; Hook functions for tag changes
  (defun anki-flashcard-tag-added (tag buffer)
    "Add save hook when TAG 'flashcards' is added to BUFFER."
    (when (string= tag "flashcards")
      (with-current-buffer buffer
	;; Add to local hook
	(add-hook 'after-save-hook #'anki-flashcard-save-hook nil t)
	;; Add to queue if file already exists
	(when (buffer-file-name)
          (anki-flashcard-queue-add (buffer-file-name))))))

  (defun anki-flashcard-tag-removed (tag buffer)
    "Remove save hook when TAG 'flashcards' is removed from BUFFER."
    (when (string= tag "flashcards")
      (with-current-buffer buffer
	;; Remove the local hook
	(remove-hook 'after-save-hook #'anki-flashcard-save-hook t))))

  ;; Pushing functions
  (defun my/anki-flashcard-push-file (file)
    "Push FILE to Anki and handle errors. Returns t on success, nil on failure."
    (let ((abs-path (anki-flashcard-make-absolute-path file)))
      (if (file-exists-p abs-path)
          (condition-case err
              (progn
		;; Load the file
		(with-current-buffer (find-file-noselect abs-path)
                  (message "Pushing %s to Anki..." abs-path)
                  (save-excursion
                    (anki-editor-push-notes 'file))
                  t))
            (error
             (anki-flashcard-report-error file (error-message-string err))
             (message "Error pushing %s to Anki (see *Anki Flashcard Errors*)" abs-path)
             nil))
	(progn
          (anki-flashcard-report-error file "File does not exist")
          (message "Error pushing to Anki: File %s does not exist" abs-path)
          nil))))

  (defun my/anki-flashcard-push-current-buffer ()
    "Push current buffer to Anki if it's a flashcard file."
    (interactive)
    (if (buffer-file-name)
	(progn
          (anki-flashcard-queue-load)
          (anki-flashcard-clear-error-buffer)
          (save-buffer) ;; Ensure buffer is saved
          
          (if (my/anki-flashcard-push-file (anki-flashcard-make-relative-path (buffer-file-name)))
              (progn
		(anki-flashcard-queue-remove (buffer-file-name))
		(message "Successfully pushed %s to Anki" (buffer-file-name)))
            (message "Failed to push %s to Anki" (buffer-file-name))))
      (message "Buffer is not visiting a file")))

  (defun my/anki-flashcard-push-all ()
    "Push all files in the flashcard queue to Anki."
    (interactive)
    (anki-flashcard-queue-load)
    (anki-flashcard-clear-error-buffer)
    
    (if anki-flashcard-queue
	(let ((success-count 0)
              (error-count 0)
              (total (length anki-flashcard-queue))
              (queue-copy (copy-sequence anki-flashcard-queue)))
          
          ;; Process each file and track results
          (dolist (file queue-copy)
            (if (my/anki-flashcard-push-file file)
		(progn
                  (setq success-count (1+ success-count))
                  (setq anki-flashcard-queue (delete file anki-flashcard-queue)))
              (setq error-count (1+ error-count))))
          
          ;; Save updated queue
          (anki-flashcard-queue-save)
          
          ;; Provide feedback
          (if (> error-count 0)
              (progn
		(message "Pushed %d/%d files to Anki with %d errors. See *Anki Flashcard Errors*"
			 success-count total error-count)
		(display-buffer anki-flashcard-error-buffer))
            (message "Successfully pushed all %d files to Anki" total)))
      (message "No files in Anki flashcard queue")))

  ;; Define keymap first, before the mode that uses it
  (defvar anki-flashcard-queue-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "g") #'anki-flashcard-queue-refresh)
      (define-key map (kbd "p") #'my/anki-flashcard-push-all)
      (define-key map (kbd "c") #'anki-flashcard-queue-clear)
      (define-key map (kbd "q") #'quit-window)
      map)
    "Keymap for `anki-flashcard-queue-mode'.")

  ;; Now define the mode with explicit keymap assignment
  (define-derived-mode anki-flashcard-queue-mode special-mode "Anki Queue"
    "Major mode for displaying the Anki flashcard queue."
    :group 'anki-editor
    (use-local-map anki-flashcard-queue-mode-map) ;; Explicitly set the keymap
    (setq buffer-read-only t)
    (buffer-disable-undo))

  (defun anki-flashcard-queue-refresh ()
    "Refresh the Anki flashcard queue display."
    (interactive)
    (let ((buffer (get-buffer "*Anki Flashcard Queue*")))
      (when buffer
	(with-current-buffer buffer
          (let ((inhibit-read-only t)
		(pos (point)))
            (erase-buffer)
            (anki-flashcard-queue-load)
            (insert "Files with flashcard changes pending to be pushed to Anki:\n\n")
            (if anki-flashcard-queue
		(progn
                  (dolist (file anki-flashcard-queue)
                    (let ((abs-path (anki-flashcard-make-absolute-path file)))
                      (insert (format "- %s%s\n" file 
                                      (if (file-exists-p abs-path)
                                          ""
					" (FILE MISSING)")))))
                  (insert "\n\nCommands:\n")
                  (insert "  g - Refresh this display\n")
                  (insert "  p - Push all files to Anki\n")
                  (insert "  c - Clear the queue\n")
                  (insert "  q - Quit this window"))
              (insert "No files in queue.\n"))
            (goto-char (min pos (point-max))))))))

  (defun anki-flashcard-queue-display ()
    "Display the current flashcard queue with refresh capability."
    (interactive)
    (let ((buffer (get-buffer-create "*Anki Flashcard Queue*")))
      (with-current-buffer buffer
	(let ((inhibit-read-only t))
          (erase-buffer)
          (anki-flashcard-queue-mode) ;; This applies the keymap
          (anki-flashcard-queue-refresh)
          (display-buffer buffer)))))
  
  (defun anki-flashcard-queue-clear ()
    "Clear the flashcard queue."
    (interactive)
    (anki-flashcard-queue-load)
    (when (yes-or-no-p "Clear the entire Anki flashcard queue? ")
      (setq anki-flashcard-queue nil)
      (anki-flashcard-queue-save)
      (message "Anki flashcard queue cleared")))

  ;; Initialize existing flashcard buffers
  (defun anki-flashcard-setup-existing-buffers ()
    "Set up save hooks for existing buffers with flashcards tag."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (and (buffer-file-name)
                   (member "flashcards" (vulpea-buffer-tags-get)))
          (add-hook 'after-save-hook #'anki-flashcard-save-hook nil t)))))

  ;; Initialize
  (anki-flashcard-queue-load)
  (anki-flashcard-setup-existing-buffers)

  ;; Hook into vulpea tag system
  (add-hook 'tags/tag-added-hook #'anki-flashcard-tag-added)
  (add-hook 'tags/tag-removed-hook #'anki-flashcard-tag-removed)

  )

;;; -> Org mode -> Agenda

(use-package org-agenda
  :ensure org
  :defer t
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROCESS(p)" "PROJECT(P)" "ACTIVE(a)" "EXPLORE(e)" "HOLD(h)" "COURSE(C)" "EXAM(E)"
	       "|" "DONE(d)" "CANCELLED(c)" "FAILED(F)" "NAREDU(N)")))

  (org-agenda-start-with-log-mode t)
  (org-log-done nil)
  (org-log-into-drawer nil)
  (org-agenda-start-on-weekday nil)
  (org-reverse-note-order nil)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-todo-list-sublevels t)
  (org-agenda-dim-blocked-tasks t)
  (org-enforce-todo-dependencies t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-sticky t)
  
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

  ;; Function to check if any ancestor has a HOLD state
  (defun air-org-skip-subtree-if-ancestor-is-hold ()
    "Skip subtree if any ancestor has HOLD todo state."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (save-excursion
	(while (and (not (bobp))
                    (not (org-at-heading-p)))
          (outline-previous-heading))
	(if (cl-loop while (and (not (bobp)) (org-up-heading-safe))
                     thereis (equal "HOLD" (org-get-todo-state)))
            subtree-end
          nil))))

  (setq org-agenda-custom-commands
	(quote (("u" alltodo ""
		 ((org-agenda-skip-function
		   (lambda nil
                     (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                               (quote regexp) "\n]+>")))
		  (org-agenda-overriding-header "* Unscheduled TODO entries: ")))
		("d" "Daily agenda and all TODOs"
		 ((tags "PRIORITY=\"A\""
			((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			 (org-agenda-overriding-header "* High-priority unfinished tasks:")))
		  (todo "COURSE" ((org-agenda-overriding-header "* Active courses: ")))
		  (todo "EXAM" ((org-agenda-overriding-header "* Looming exams: ")
				(org-agenda-sorting-strategy '(deadline-up))))
		  (todo "ACTIVE" ((org-agenda-overriding-header "* Active projects: ")))
		  (todo "PROJECT" ((org-agenda-overriding-header "* Projects: ")))
		  (todo "NEXT" ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
							       (air-org-skip-if-blocked)
							       (air-org-skip-subtree-if-ancestor-is-hold)))
				(org-agenda-overriding-header "* Up next: ")))
		  (agenda "" ((org-agenda-span 'week)))
		  (todo "PROCESS" ((org-agenda-overriding-header "* To process:  ")))
		  (alltodo "" ((org-agenda-skip-function
				'(or (air-org-skip-subtree-if-habit)
				     (air-org-skip-subtree-if-priority ?A)
                                     (air-org-skip-if-blocked)
				     (org-agenda-skip-if nil '(scheduled deadline))
				     (org-agenda-skip-entry-if 'todo '("NEXT" "ACTIVE" "HOLD" "PROCESS" "EXPLORE" "PROJECT" "COURSE" "EXAM"))
				     (air-org-skip-subtree-if-ancestor-is-hold)))
			       (org-agenda-overriding-header "* All normal priority tasks:")))
		  (todo "HOLD" ((org-agenda-overriding-header "* Currently on hold: ")))
		  (todo "EXPLORE" ((org-agenda-overriding-header "* Things to explore: ")))
		  )))))
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
     (octave . t)
     ))


  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("hl" . "src haskell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python :results output"))
  (add-to-list 'org-structure-template-alist '("cs" . "src C"))
  (add-to-list 'org-structure-template-alist '("def" . "definicija"))
  (add-to-list 'org-structure-template-alist '("izr" . "izrek"))
  (add-to-list 'org-structure-template-alist '("thm" . "theorem"))
  (add-to-list 'org-structure-template-alist '("prf" . "proof"))
  (add-to-list 'org-structure-template-alist '("trd" . "trditev"))
  (add-to-list 'org-structure-template-alist '("lem" . "lema"))
  (add-to-list 'org-structure-template-alist '("abst" . "abstract"))
  (add-to-list 'org-structure-template-alist '("ex" . "example"))
  (add-to-list 'org-structure-template-alist '("item" . "itemize"))
  )

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
         ("i" . open-youtube-in-iina)
         ("I" . download-selected-youtube-videos)
         ("D" . elfeed-filter-downloaded)
	 ("s" . my/elfeed-show-default)
	 ("P" . js/log-elfeed-process)
	 ("B" . elfeed-browse-with-secondary-browser)
	 ("W" . my/elfeed-entries-to-wallabag)
	 ("<wheel-up>" . previous-line)
	 ("<wheel-down>" . next-line)
	 
         :map elfeed-show-mode-map
         ("SPC" . elfeed-scroll-up-command)
         ("S-SPC" . elfeed-scroll-down-command)
	 ("P" . js/log-elfeed-process)
         ;; If called with C-u then bring up the capture buffer
	 ("t" . elfeed-show-trash)
         ("i" . open-youtube-in-iina)
	 ("M-o" . ace-link-safari)
	 ("B" . elfeed-browse-with-secondary-browser)
	 ("W" . my/elfeed-entries-to-wallabag)
         )
  :hook
  (elfeed-show-mode . mixed-pitch-mode)
  (elfeed-show-mode . visual-line-mode)
  (elfeed-show-mode . efs/org-mode-visual-fill)
  (elfeed-search-mode . my/setup-elfeed-scroll)
  ;; (elfeed-search-mode . my/elfeed-setup-local-activation-hooks)
  :config
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
    (elfeed-filter-maker "+trash" "Showing trashed."))

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
  (defun my/elfeed-entries-to-wallabag (&optional entries)
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
      
      ;; Process each entry
      (dolist (entry entries)
	(let ((url (elfeed-entry-link entry))
              (title (elfeed-entry-title entry)))
          (if url
              (progn
		(message "Adding to wallabag: %s" title)
		(wallabag-add-entry url "Unread")
		(cl-incf added-count))
            (message "No URL found for entry: %s" title))))
      
      ;; Sync changes to server if we added anything
      (when (> added-count 0)
	(message "Syncing %d entries to wallabag server..." added-count)
	(run-with-timer 2 nil #'wallabag-request-and-synchronize-entries))
      
      (message "Added %d entries to wallabag" added-count)))
  
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
  :vc (:url "https://github.com/rtrppl/cuckoo-search" :rev :newest)
  :after (elfeed)
  :bind
  (:map elfeed-search-mode-map
	      ("C" . cuckoo-search)
	      ;; ("x" . cuckoo-search-saved-searches)
	      ))

(use-package elfeed-org
  :after elfeed
  :defer nil
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
  (advice-add 'my/elfeed-entries-to-wallabag :before #'js/log-elfeed-entries)

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
  :after elfeed
  :bind
  (:map elfeed-show-mode-map
	("F" . elfeed-tube-fetch)
	([remap save-buffer] . elfeed-tube-save)
	:map elfeed-search-mode-map
	("F" . elfeed-tube-fetch)
	([remap save-buffer] . elfeed-tube-save))
  :config
  (elfeed-tube-setup)
  (defvar yt-dlp-priority-tags '(asmr chess osrs gaming essays)
    "List of tags to prioritize when determining download subfolder.
     The first matching tag in this list determines the subfolder.")
  
  (defun open-youtube-in-iina ()
    "Create a playlist with selected elfeed entries and open it in IINA."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (iina-command "open -a IINA")
           (playlist-file (make-temp-file "emacs-iina-playlist" nil ".m3u8")))
      (with-temp-file playlist-file
	(dolist (entry entries)
          (if (member 'downloaded (elfeed-entry-tags entry))
              (let* ((relative-filename (elfeed-meta entry :filename))
                     (filename (expand-file-name relative-filename yt-dlp-folder)))
		(if (and filename (file-exists-p filename))
                    (insert filename)
                  (insert (elfeed-entry-link entry))
                  (elfeed-untag entry 'downloaded))
		(insert "\n")))))
      (start-process-shell-command "iina" nil (concat iina-command " \"" playlist-file "\""))
      (message "Opening YouTube playlist in IINA...")))

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
             (yt-dlp-command (format "yt-dlp --no-progress -S 'res:1080,ext,vcodec:h265,h264' --embed-subs --sub-lang 'en.*' --sponsorblock-mark all --sponsorblock-remove 'sponsor' -o '%s' '%s'" output-template (elfeed-entry-link entry)))
             (process (start-process-shell-command "yt-dlp" "*yt-dlp-output*" yt-dlp-command)))
        ;; Use the constructed sentinel for each entry
        (set-process-sentinel process (eval (create-single-entry-sentinel entry base-filename)))))
    (message "Downloading selected YouTube videos..."))

  (defun elfeed-filter-downloaded ()
    (interactive)
    (elfeed-filter-maker "+downloaded" "Showing previously downloaded items."))

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

;;; -> Elfeed -> End

;;; --> Wallabag

(use-package request
  :ensure t)
(use-package emacsql
  :ensure t)

(use-package wallabag
  :defer t
  ;; :load-path "~/.emacs.d/lisp/wallabag/"
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
	 ("r" . wallabag-update-entry-archive))
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
  )
;;; --> Programming

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-timemachine
  :defer t)

(use-package flymake
  :ensure nil  ; built-in package
  :hook
  (prog-mode . flymake-mode)
  (prog-mode . subword-mode)
  (lisp-interaction-mode . (lambda () (flymake-mode -1)))
  :config
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
	      ("M-l" . flymake-show-buffer-diagnostics))
  )

;; (use-package hl-todo
;;   :ensure t
;;   :hook prog-mode)

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

  :config
  (setq-default TeX-master nil)
  
  :hook
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . turn-on-reftex)
  )

;;; -> Programming -> LaTeX -> CDLaTeX
(use-package cdlatex
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
     (?o "\\mathring" nil t nil nil)
     ( ?C    "\\mathcal"           nil        t   nil nil )
     ))

  (cdlatex-math-symbol-alist
   '((?o ("\\omega" "\\circ"))
     (?O ("\\Omega" "\\degree"))
     (?+  ("\\cup" "\\oplus"))
     (?*  ("\\times" "\\otimes" "\\bullet"))))

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

;;; TODO -> Programming -> Agda

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
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(defun run-emacs-with-current-directory (&optional arg)
  "Run Emacs with the current file's directory as the configuration directory.
Calling with single prefix ARG (C-u) enables debugging.
Calling with double prefix ARG (C-u C-u) runs Emacs with -Q."
  (interactive "P")
  (let* ((emacs-path "/opt/homebrew/Cellar/emacs-plus@30/30.1/Emacs.app/Contents/MacOS/Emacs")
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

;;;
;;; End of configuration file.
;;; init.el ends here
