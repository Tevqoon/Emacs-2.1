;;; elfeed-summary.el --- Summarize Elfeed entries using gptel -*- lexical-binding: t; -*-

(require 'gptel)
(require 'elfeed-show)

(defvar elfeed-summary-response-overlay nil
  "Overlay for displaying GPTel streaming responses dynamically.")

(defcustom elfeed-summary-prompts
  '(("Decide to read" . "Triage this RSS item. Be direct, no hedging.

**Step 1 — Classify the content kind:**
Identify which of these best fits:
- *Hard news* — factual event or development, primary source
- *Analysis* — interprets or contextualizes events, requires prior knowledge to evaluate
- *Opinion/commentary* — argues a position, author's credibility matters
- *Tutorial/technical* — teaches a skill or explains a system
- *Entertainment/reaction* — primarily performance, personality, or affect
- *Research* — original data, methodology, findings

**Step 2 — Assess the medium:**
Does the long-form format (video runtime, article length) add signal the content kind actually needs?
- Hard news: rarely. A summary is almost always sufficient.
- Analysis/research: maybe. Nuance can require length.
- Opinion: only if the argument is dense or evolves. Most opinion pieces have one point repeated.
- Tutorial/technical: often yes. Steps, code, demos need the medium.
- Entertainment: no. The medium IS the content; if you're not watching for the experience, skip entirely.

**Step 3 — Output:**

- **Kind:** [one of the above]
- **Verdict:** Watch/Read · Summary only · Skip
  - *Watch/Read:* format adds genuine value over a summary
  - *Summary only:* real content exists, but the medium adds more drag than signal
  - *Skip:* nothing worth extracting beyond a one-liner
- **Core claim (always, even for Skip):** One sentence — what is actually being said or what happened, stripped of framing and affect.
- **Reasoning:** One sentence on why this verdict given the content kind and medium.
- **If \"Summary only\":** 2–3 sentences. What's argued or what happened, key specifics (names, numbers, sources), what's actionable if anything. No rhetorical flourishes.

Do not pad. Do not soften.")

    ("Short summary" . "Summarize this item in under 150 words.

First, identify the content kind: hard news / analysis / opinion / tutorial / research / entertainment.
Then summarize accordingly:

- *Hard news:* What happened, who, when, what changes downstream.
- *Analysis/opinion:* Core claim in one sentence. Key evidence or reasoning. What it implies if correct.
- *Tutorial/technical:* What problem it solves. The approach. What you'd need to act on it.
- *Research:* Question, method, finding, caveat.
- *Entertainment:* Don't summarize — state the one extractable claim or observation if any exists, then say it's entertainment.

**Rules:**
- No affect, no dramatization
- Skip intros and conclusions
- Be specific: names, numbers, sources over vague characterizations
- If the item is mostly filler, say so in one sentence and stop

**Then assess in one sentence:** substantive for an information diet, or noise?")

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
- Entertainment gets one extractable claim if any, then stop

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
