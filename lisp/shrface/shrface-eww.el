;;; eww.el --- shrface integration for eww -*- lexical-binding: t; -*-

(require 'shrface)
(require 'eww nil t)
(require 'shrface-core)

(defun shrface-eww-setup ()
  "Set up shrface features for eww buffers."
  (shrface-basic-setup)
  
  ;; Support for annotations if available
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
      (paw-focus-find-words))))

(defun shrface-eww-advice (orig-fun &rest args)
  "Advice for `eww-display-html' to use shrface styling."
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        (shr-table-vertical-line "|")
        (shr-width 65)
        (shr-indentation 0)
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
    (apply orig-fun args)))

;; Add hooks and advice for eww
(with-eval-after-load 'eww
  (require 'shrface)
  (advice-add 'eww-display-html :around #'shrface-eww-advice)
  (add-hook 'eww-after-render-hook #'org-indent-mode)
  (add-hook 'eww-after-render-hook #'eldoc-mode)
  (when (fboundp 'eldoc-box-hover-mode)
    (add-hook 'eww-after-render-hook #'eldoc-box-hover-mode))
  (add-hook 'eww-after-render-hook #'shrface-eww-setup))

(provide 'shrface-eww)
