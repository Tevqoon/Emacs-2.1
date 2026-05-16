;;; elfeed-summary.el --- Summarize Elfeed entries using gptel -*- lexical-binding: t; -*-

(require 'gptel)
(require 'elfeed-show)

(defvar elfeed-summary-response-overlay nil
  "Overlay for displaying GPTel streaming responses dynamically.")

(defcustom elfeed-summary-prompts
  '(("Decide to read" . "Triage this RSS item. Be direct, no hedging.

**Step 1 — Flag the format:**
- Is this primarily informational or reactive/entertainment?
- Does the long-form medium (video, long article) add value, or is it stretching a small core?
- Is the title engineering urgency (ALL CAPS, crisis framing, vague hooks)?

**Step 2 — Output:**

- **Category:** News commentary / Analysis / Entertainment / Tutorial / Technical / Other
- **Verdict:** Watch/Read · Summary only · Skip
  - *Watch/Read:* format adds genuine value over a summary
  - *Summary only:* real content exists, but the medium adds more drag than signal
  - *Skip:* nothing worth extracting (filler, reaction, pure entertainment)
- **Reasoning:** One sentence on why this verdict.
- **If \"Summary only\":** 2–3 sentences. Just what happened or what's argued, stripped of affect and framing.

Do not pad. Do not soften.")

    ("Short summary" . "Summarize this item in under 150 words.

**Cover:**
- What's argued or what happened (the core, not the framing)
- Key evidence or specifics (numbers, names, sources)
- What's actually actionable, if anything

**Rules:**
- No affect, no dramatization, no rhetorical flourishes
- Skip throat-clearing intros and conclusions
- If the item is mostly filler, say so in one sentence and stop

**Then assess:** substantive for an information diet, or noise? One sentence.")

    ("Comprehensive summary" . "Provide detailed coverage of this item.

**Structure:**
- **Argument:** Core claim and the context it's responding to
- **Evidence:** Specific data, sources, or examples backing the claim
- **Implications:** What's actionable or what changes downstream
- **Gaps:** Unsupported claims, missing context, weak links in the reasoning
- **Source quality:** Author credibility, evidence rigor, primary vs derivative

**Rules:**
- Be specific. \"The author argues X\" not \"the author discusses important themes\"
- Name names, cite numbers
- Flag speculation as speculation

**Then assess:** Does this merit deep engagement, or is the comprehensive summary the whole value?"))
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
