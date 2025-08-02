;;; wiki-summary.el --- shrface integration for wiki-summary -*- lexical-binding: t; -*-

(require 'shrface)
(require 'wiki-summary nil t)
(require 'shrface-core)

(defun shrface-wiki-summary-setup ()
  "Set up shrface features for wiki-summary buffers."
  (shrface-basic-setup)
  ;; Enable outline navigation for sections
  (setq-local outline-regexp "^\\(==+ .+? ==+\\)$")
  (setq-local outline-level (lambda ()
                             (/ (- (match-end 1) (match-beginning 1)
                                  (length (match-string 2)) 2) 2)))
  (outline-minor-mode 1))

(defun shrface-wiki-summary-format-article (summary title)
  "Format SUMMARY with TITLE using shrface styling."
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        (shr-width 80)
        (shr-indentation 2)
        (shr-table-vertical-line "|")
        (shr-use-fonts nil)
        (shr-external-rendering-functions
         (append '((code . shrface-tag-code)
                   (pre . shrface-shr-tag-pre-highlight))
                 shrface-supported-faces-alist)))
    (let ((buf (generate-new-buffer (format "*wiki-summary: %s*" title)))
          (origin-buffer (current-buffer)))
      (with-current-buffer buf
        (wiki-summary-mode)
        (let ((inhibit-read-only t))
          (insert summary)
          (fill-paragraph)
          (goto-char (point-min)))
        ;; Store article information for later use
        (setq-local wiki-summary-article-title title)
        (setq-local wiki-summary-article-language wiki-summary-language-string)
        (setq-local wiki-summary-is-full-article nil)
        (setq-local wiki-summary-origin-buffer origin-buffer)
        ;; Apply shrface and set up imenu
        (shrface-wiki-summary-setup))
      (pop-to-buffer buf)
      buf)))

;; Add hook to wiki-summary-mode
(add-hook 'wiki-summary-mode-hook #'org-indent-mode)
(add-hook 'wiki-summary-mode-hook #'eldoc-mode)
(add-hook 'wiki-summary-mode-hook #'shrface-wiki-summary-setup)

;; Modify wiki-summary functions to use shrface
(advice-add 'wiki-summary-format-summary-in-buffer :override #'shrface-wiki-summary-format-article)

(provide 'shrface-wiki-summary)
