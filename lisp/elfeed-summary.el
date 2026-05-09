;;; elfeed-summary.el --- Summarize Elfeed entries using gptel -*- lexical-binding: t; -*-

(require 'gptel)
(require 'elfeed-show)

(defvar elfeed-summary-response-overlay nil
  "Overlay for displaying GPTel streaming responses dynamically.")

(defcustom elfeed-summary-prompts '(("One sentence summary" . "Summarize this article in one clear and factual sentence, capturing the core message without repeating the title.")
                                     ("General summary" . "Summarize the key points of this article in a clear and concise manner, highlighting the main arguments, findings, and conclusions.")
                                     ("Bullet list summary" . "Provide a bullet-point summary of the main takeaways from this article. Answer only in bullets!")
                                     ("Objective vs. Opinion analysis" . "Summarize this article by separating objective facts from the author's opinions, clearly distinguishing the two.")
                                     ("Simplified summary" . "Summarize this article in simple, easy-to-understand language as if explaining to a non-expert."))
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
