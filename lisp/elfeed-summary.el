;;; elfeed-summary.el --- Summarize Elfeed entries using gptel -*- lexical-binding: t; -*-

(require 'gptel)
(require 'elfeed-show)

(defvar elfeed-summary-response-overlay nil
  "Overlay for displaying GPTel streaming responses dynamically.")

(defcustom elfeed-summary-prompts
  '(("Brief" . "Summarize this item in 3–4 sentences of dense prose. No labels, no structure, no evaluation.

First sentence: what happened or what is argued — written as a sharp subheadline would be, not as a topic sentence. Remaining sentences: key specifics (names, numbers, sources, methods, findings). If the item is mostly filler, say so in one sentence and stop.

No affect. No framing. No meta-commentary.")

    ("Short summary" . "Summarize this item in under 150 words.

First, identify the content kind: hard news / analysis / opinion / tutorial / research / entertainment.
Then summarize accordingly:

- *Hard news:* What happened, who, when, what changes downstream.
- *Analysis/opinion:* Core claim in one sentence. Key evidence or reasoning. What it implies if correct.
- *Tutorial/technical:* What problem it solves. The approach. What you'd need to act on it.
- *Research:* Question, method, finding, caveat.
- *Entertainment:* State the one extractable claim or observation if any exists, then stop.

**Rules:**
- No affect, no dramatization
- Skip intros and conclusions
- Be specific: names, numbers, sources over vague characterizations
- If the item is mostly filler, say so in one sentence and stop")

    ("Comprehensive summary" . "Provide detailed coverage of this item.

First, identify the content kind: hard news / analysis / opinion / tutorial / research / entertainment.
Structure your summary for that kind:

**For analysis or opinion:**
- *Claim:* The core argument and what it's responding to
- *Reasoning:* How the author gets from premises to conclusion
- *Evidence:* Specific data, examples, or sources cited
- *Gaps:* Unsupported assertions, missing context, weak links
- *Source:* Author's relevant expertise or track record

**For hard news:**
- *What happened:* Event, actors, timeline
- *Why it matters:* Immediate and downstream consequences
- *What's uncertain:* What's confirmed vs. reported vs. speculated
- *Source quality:* Primary, secondary, or derivative?

**For tutorial/technical:**
- *Problem:* What it solves
- *Approach:* Core method or insight
- *Prerequisites:* What you need to use it
- *Limitations:* Where it breaks down
- *Actionability:* Can you apply this directly?

**For research:**
- *Question:* What was investigated
- *Method:* How (briefly)
- *Finding:* The actual result, with effect sizes or specifics if available
- *Caveats:* Sample, scope, replication status
- *Implication:* What changes if this holds

**Rules:**
- Be specific. \"The author argues X\" not \"the author discusses important themes\"
- Name names, cite numbers, flag speculation as speculation
- Entertainment gets one extractable claim if any, then stop"))
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
