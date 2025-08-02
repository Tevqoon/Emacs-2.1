;;; elfeed.el --- shrface integration for elfeed -*- lexical-binding: t; -*-

(require 'shrface)
(require 'elfeed nil t)
(require 'shrface-core)

(defun shrface-elfeed-setup ()
  "Set up shrface features for elfeed-show buffers."
  (shrface-basic-setup)
  (if (string-equal system-type "android")
      (setq-local touch-screen-enable-hscroll nil)))

(defun shrface-elfeed-advice (orig-fun &rest args)
  "Advice for `elfeed-insert-html' to use shrface styling."
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        ;; Using a large width to avoid unnecessary line wrapping
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
    (apply orig-fun args)
    
    ;; Support for annotations if available
    (with-current-buffer "*elfeed-entry*"
      (when (and (boundp 'paw-annotation-mode)
                 paw-annotation-mode)
        (when (fboundp 'paw-clear-annotation-overlay)
          (paw-clear-annotation-overlay))
        (when (fboundp 'paw-show-all-annotations)
          (paw-show-all-annotations))
        (when (and (boundp 'paw-annotation-show-wordlists-words-p)
                   paw-annotation-show-wordlists-words-p
                   (fboundp 'paw-focus-find-words))
          (paw-focus-find-words :wordlist t))
        (when (and (boundp 'paw-annotation-show-unknown-words-p)
                   paw-annotation-show-unknown-words-p
                   (fboundp 'paw-focus-find-words))
          (paw-focus-find-words))))))

;; Add hooks and advice for elfeed
(with-eval-after-load 'elfeed
  (add-hook 'elfeed-show-mode-hook #'org-indent-mode)
  (add-hook 'elfeed-show-mode-hook #'eldoc-mode)
  (when (fboundp 'eldoc-box-hover-mode)
    (add-hook 'elfeed-show-mode-hook #'eldoc-box-hover-mode))
  (add-hook 'elfeed-show-mode-hook #'shrface-elfeed-setup)
  
  (advice-add 'elfeed-insert-html :around #'shrface-elfeed-advice))

(provide 'shrface-elfeed)
