;;; elfeed-summary.el --- Summarize Elfeed entries using gptel -*- lexical-binding: t; -*-

(require 'gptel)
(require 'elfeed-show)

(defvar elfeed-summary-response-overlay nil
  "Overlay for displaying GPTel streaming responses dynamically.")

(defcustom elfeed-summary-prompts '(("Decide to read" . "Assess this article:
- **Category:** News/Analysis/Opinion/Tutorial/Technical/Other
- **Information value:** High (novel insights, deep expertise, strong evidence) | Medium | Low (common knowledge, surface-level, promotional)
- **Worth reading:** Yes/No and why
- **Key points:** 3-5 specific takeaways

Be direct about whether this deserves your attention.")
                                    ("Short summary" . "Summarize in under 150 words:
- Main argument or finding
- Supporting evidence or data
- Practical implications or actionability

Be specific, avoid vague phrasing. Then assess: substantive for your information diet, or noise?")
                                    ("Comprehensive summary" . "Provide detailed coverage:
- **Main argument:** Core claim and context
- **Evidence & findings:** Key data or research backing it
- **Practical implications:** What can be applied or acted upon
- **Limitations & gaps:** Unsupported claims or missing context
- **Source quality:** Author credibility and evidence rigor

Then assess: Does this merit deep engagement for your information diet?"))
  "Prompts for entry summaries."
  :type 'alist
  :group 'elfeed)

(defun elfeed-summary-remove-overlay ()
  "Remove the summary overlay."
  (when (overlayp elfeed-summary-response-overlay)
    (delete-overlay elfeed-summary-response-overlay)
    (setq elfeed-summary-response-overlay nil)))

(defun elfeed-summary-display (response)
  "Display RESPONSE after the Link line."
  (with-current-buffer (get-buffer "*elfeed-entry*")
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "Link: " nil t)
          (progn
            (end-of-line)
            (let ((pos (point)))
              (unless (overlayp elfeed-summary-response-overlay)
                (setq elfeed-summary-response-overlay (make-overlay pos pos)))

              (overlay-put elfeed-summary-response-overlay
                           'after-string
                           (concat "\n"
                                   (propertize response 'face (list :height 0.9))
                                   "\n"
                                   (propertize (make-string 60 ?_) 'face 'font-lock-comment-face)
                                   "\n"))))
        (message "Could not find Link: line")))))

(defun elfeed-summary (&optional arg)
  "Generate a summary of the current entry using gptel."
  (interactive "P")
  (unless (derived-mode-p 'elfeed-show-mode)
    (user-error "Not in elfeed-show-mode"))

  (elfeed-summary-remove-overlay)

  (let* ((prompt (assoc-default
                  (completing-read "Select a prompt: " elfeed-summary-prompts nil t)
                  elfeed-summary-prompts))
         (entry-text (buffer-string))
         (full-query (format "%s\n%s" prompt entry-text)))

    (message "Summarizing...")
    (gptel-request full-query
      :callback
      (lambda (response _info)
        (elfeed-summary-display response)
        (message "Summary complete")))))

;; Wrap the existing refresh function to remove summary
(let ((original-refresh elfeed-show-refresh-function))
  (setq elfeed-show-refresh-function
        (lambda ()
          (elfeed-summary-remove-overlay)
          (funcall original-refresh))))

(provide 'elfeed-summary)
;;; elfeed-summary.el ends here
