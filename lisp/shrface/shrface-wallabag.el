;;; wallabag.el --- shrface integration for wallabag -*- lexical-binding: t; -*-

(require 'shrface)
(require 'wallabag nil t)
(require 'shrface-core)

(defun shrface-wallabag-setup ()
  "Set up shrface features for wallabag buffers."
  (shrface-basic-setup)
  (if (string-equal system-type "android")
      (setq-local touch-screen-enable-hscroll nil)))

(defun my-wallabag-render-html (begin end)
  "Render HTML from BEGIN to END with shrface styling."
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        ;; Use a larger width to avoid unnecessary line wrapping
        (shr-width 7000)
        (shr-indentation 0)
        (shr-table-vertical-line "|")
        (shr-external-rendering-functions
         (append '((title . eww-tag-title)
                   (form . eww-tag-form)
                   (input . eww-tag-input)
                   (button . eww-form-submit)
                   (textarea . eww-tag-textarea)
                   (select . eww-tag-select)
                   (link . eww-tag-link)
                   (meta . eww-tag-meta)
                   (code . shrface-tag-code)
                   (pre . shrface-shr-tag-pre-highlight))
                 shrface-supported-faces-alist))
        (shrface-toggle-bullets nil)
        (shrface-href-versatile t)
        (shr-use-fonts nil))
    (shr-render-region begin end))
  
  ;; Support for annotations if paw-annotation-mode is enabled
  (when (and (boundp 'paw-annotation-mode)
             paw-annotation-mode)
    (when (fboundp 'paw-clear-annotation-overlay)
      (paw-clear-annotation-overlay))
    (when (fboundp 'paw-show-all-annotations)
      (paw-show-all-annotations))
    
    ;; Show current buffer's overlays
    (when (get-buffer-window "*wallabag-entry*")
      (select-window (get-buffer-window "*wallabag-entry*")))
    (when (and (boundp 'paw-annotation-show-wordlists-words-p)
               paw-annotation-show-wordlists-words-p
               (fboundp 'paw-focus-find-words))
      (paw-focus-find-words :wordlist t))
    (when (and (boundp 'paw-annotation-show-unknown-words-p)
               paw-annotation-show-unknown-words-p
               (fboundp 'paw-focus-find-words))
      (paw-focus-find-words))))

;; Add hooks for wallabag
(with-eval-after-load 'wallabag
  (add-hook 'wallabag-entry-mode-hook #'org-indent-mode)
  (add-hook 'wallabag-entry-mode-hook #'eldoc-mode)
  (when (fboundp 'eldoc-box-hover-mode)
    (add-hook 'wallabag-entry-mode-hook #'eldoc-box-hover-mode))
  (add-hook 'wallabag-entry-mode-hook #'shrface-wallabag-setup)
  
  ;; Clean up imenu-list when quitting wallabag
  (advice-add 'wallabag-entry-quit :after
              (lambda (&rest _args)
                (when (get-buffer "*Ilist*")
                  (kill-buffer "*Ilist*"))))
  
  ;; Set wallabag render function
  (setq wallabag-render-html-function #'my-wallabag-render-html))

(provide 'shrface-wallabag)
