;;; core.el --- Core shared functions for shrface integrations -*- lexical-binding: t; -*-

(require 'shrface)

;; Custom pre-tag highlighting function that all modules will use
(defun shrface-shr-tag-pre-highlight (pre)
  "Highlighting code in PRE."
  (let* ((shr-folding-mode 'none)
         (shr-current-font 'default)
         (code (with-temp-buffer
                 (shr-generic pre)
                 (buffer-string)))
         (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                   (let ((sym (language-detection-string code)))
                     (and sym (symbol-name sym)))))
         (mode (and lang
                    (shr-tag-pre-highlight--get-lang-mode lang)))
         start end)
    (shr-ensure-newline)
    (shr-ensure-newline)
    (setq start (point))
    (insert
     (or (and (fboundp mode)
              (with-demoted-errors "Error while fontifying: %S"
                (shr-tag-pre-highlight-fontify code mode)))
         code))
    (shr-ensure-newline)
    (setq end (point))
    (pcase (frame-parameter nil 'background-mode)
      ('light
       (add-face-text-property start end '(:background "#D8DEE9" :extend t)))
      ('dark
       (add-face-text-property start end '(:background "#292b2e" :extend t))))
    (shr-ensure-newline)
    (insert "\n")))

;; Basic setup function that all modules can use
(defun shrface-basic-setup ()
  "Set up basic shrface features for buffers."
  (unless shrface-toggle-bullets
    (shrface-regexp))
  (setq-local imenu-create-index-function #'shrface-imenu-get-tree))

(provide 'shrface-core)
