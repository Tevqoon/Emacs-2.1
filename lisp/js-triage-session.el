;;; js-triage-session.el --- Minimal todo triage with exponential snooze backoff
;;
;; A session-based navigator across org-agenda files:
;;   - Stable queue collected once at session start
;;   - Steps through entries one at a time (elfeed-style)
;;   - Exponential snooze backoff via SCHEDULED + SNOOZE_COUNT property
;;   - Entries with no future SCHEDULED timestamp are "due" and included
;;   - Completing actions (done/cancel/snooze) auto-advance to next entry
;;   - Inspect actions (state-change, refile, open) stay at current entry
;;
;; The triage keymap binds existing org/roam functions directly — this
;; file only owns the session machinery and the snooze logic.
;;
;; Entry points:
;;   js/session-process   — unscheduled PROCESS items only
;;   js/session-keyword   — prompted or given keyword (any single state)
;;   js/session-review    — multi-keyword unfocused review
;;   js/session-all       — all actionable unblocked/unscheduled items

(require 'org)
(require 'org-roam nil t)

;;; ─── Session state ────────────────────────────────────────────────────────

(defvar js/triage-session-queue nil
  "List of markers for the current triage session, consumed front-to-back.")

(defvar js/triage-session-label ""
  "Human-readable label for the active session.")

(defvar js/triage-session-total 0
  "Total items collected when the session started.")

(defvar js/triage-current-marker nil
  "Marker pointing to the entry currently being triaged.")

(defvar js/triage-session-history nil
  "Stack of markers that have been processed (popped from queue).")

;;; ─── Collection ──────────────────────────────────────────────────────────

(defun js/triage--due-p ()
  "Return non-nil if entry at point has no future SCHEDULED timestamp."
  (let ((sched (org-get-scheduled-time (point))))
    (or (not sched)
        (not (time-less-p (current-time) sched)))))

(defun js/triage--collect (todo-keywords &optional extra-pred)
  "Collect markers for entries matching TODO-KEYWORDS across agenda files.
TODO-KEYWORDS is a string or list of strings.
EXTRA-PRED, if non-nil, is called at point; entry included only if t.
Returns a list of live markers."
  (let* ((kws (if (listp todo-keywords) todo-keywords (list todo-keywords)))
         (match (concat "TODO={" (mapconcat #'regexp-quote kws "\\|") "}"))
         results)
    (org-map-entries
     (lambda ()
       (when (or (null extra-pred) (funcall extra-pred))
         (push (point-marker) results)))
     match
     'agenda)
    (nreverse results)))

;;; ─── Core navigation ──────────────────────────────────────────────────────

(defun js/triage-session-start (todo-keywords &optional label extra-pred)
  "Collect TODO-KEYWORDS entries and begin stepping through them."
  (setq js/triage-session-queue  (js/triage--collect todo-keywords extra-pred)
        js/triage-session-label  (or label
                                     (if (listp todo-keywords)
                                         (string-join todo-keywords "|")
                                       todo-keywords))
        js/triage-session-total  (length js/triage-session-queue)
	js/triage-session-history nil)
  (js/triage-goto-next))

(defun js/triage-goto-next ()
  "Advance to the next entry in the session, skipping dead markers."
  (let ((marker (pop js/triage-session-queue)))
    (cond
     ((null marker)
      (message "No more items in triage queue."))  ;; Queue exhausted
     ((and (markerp marker)
           (marker-buffer marker)
           (buffer-live-p (marker-buffer marker)))
      (setq js/triage-current-marker marker)
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker))
      (org-fold-show-context 'agenda)
      (org-show-entry)
      (recenter))
     (t
      (when js/triage-session-queue
	(js/triage-goto-next))))))

(defun js/triage-goto-current ()
  "Return to the currently active marker if it exists."
  (interactive)
  (if (null js/triage-current-marker)
      (message "No currently active marker.")
    (push js/triage-current-marker js/triage-session-queue)
    (js/triage-goto-next))
  )

(defun js/triage-goto-prev ()
  "Jump back to the previous triaged item."
  (interactive)
  (if (null js/triage-session-history)
      (message "No history to go back to.")
    (when js/triage-current-marker
      (push js/triage-current-marker js/triage-session-queue))
    (let ((marker (pop js/triage-session-history)))
      (push marker js/triage-session-queue)  ;; Re-queue it
      (js/triage-goto-next)
      )))

(defun js/triage--show-status ()
  "Display triage progress in the echo area."
  (message "[triage/%s] %d/%d remaining"
           js/triage-session-label
           (length js/triage-session-queue)
           js/triage-session-total))

(defun js/triage-session-active-p ()
  "Return non-nil if a triage session is currently active."
  (not (null js/triage-current-marker)))

;;; ─── Snooze machinery ────────────────────────────────────────────────────

(defun js/triage--snooze-for (days)
  "Snooze entry at point for DAYS days, incrementing SNOOZE_COUNT."
  (let ((count (string-to-number
                (or (org-entry-get nil "SNOOZE_COUNT") "0"))))
    (org-entry-put nil "SNOOZE_COUNT" (number-to-string (1+ count)))
    (org-schedule nil (format "+%dd" days))))

;;; ─── Completing actions (advance to next) ────────────────────────────────

(defun js/triage-next ()
  "Skip current entry without any action and advance."
  (interactive)
  (when js/triage-current-marker
    (push js/triage-current-marker js/triage-session-history))
  (js/triage-goto-next))

(defun js/triage-done ()
  "Mark entry DONE, bypassing state blocking, then advance."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((org-blocker-hook nil))
      (org-todo 'done)))
  (save-buffer)
  (js/triage-goto-next))

(defun js/triage-cancel ()
  "Mark entry CANCELLED, bypassing state blocking, then advance."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((org-blocker-hook nil))
      (org-todo "CANCELLED")))
  (save-buffer)
  (js/triage-goto-next))

(defun js/triage-snooze ()
  "Snooze with exponential backoff (2^SNOOZE_COUNT days), then advance."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((count (string-to-number
                   (or (org-entry-get nil "SNOOZE_COUNT") "0")))
           (days  (expt 2 count)))
      (js/triage--snooze-for days)
      (message "[triage/%s] Snoozed for %d day(s)." js/triage-session-label days)))
  (save-buffer)
  (js/triage-goto-next))

(defun js/triage-snooze-manual ()
  "Snooze for a manually specified number of days, then advance."
  (interactive)
  (let ((days (read-number "Snooze for how many days? " 7)))
    (save-excursion
      (org-back-to-heading t)
      (org-schedule nil (format "+%dd" days))))
  (save-buffer)
  (js/triage-goto-next))

;;; ─── Inspect actions (stay at current entry) ────────────────────────────

(defun js/triage-quit ()
  "Abandon the current triage session."
  (interactive)
  (setq js/triage-session-queue  nil
        js/triage-current-marker nil)
  (message "[triage/%s] Session abandoned." js/triage-session-label))

;;; ─── Entry points ─────────────────────────────────────────────────────────

(defun js/session-process ()
  "Triage unscheduled (due) PROCESS items across all agenda files."
  (interactive)
  (js/triage-session-start "PROCESS" "process" #'js/triage--due-p))

(defun js/session-keyword (keyword)
  "Triage all items with a specific TODO KEYWORD.
When called interactively, prompts for the keyword."
  (interactive
   (list (completing-read "Todo keyword: "
                          (flatten-list org-todo-keywords)
                          (lambda (k) (not (member k '("|"))))
                          t)))
  (js/triage-session-start keyword keyword #'js/triage--due-p))

(defun js/session-review ()
  "Unfocused review session across TODO, NEXT, FINISH, EXPLORE, IDEA."
  (interactive)
  (js/triage-session-start
   '("TODO" "NEXT" "FINISH" "EXPLORE" "IDEA") "review" #'js/triage--due-p))

(defun js/session-all ()
  "All actionable items: NEXT/TODO/FINISH that are due and unblocked."
  (interactive)
  (js/triage-session-start
   '("NEXT" "TODO" "FINISH") "all"
   (lambda ()
     (and (js/triage--due-p)
          (not (org-entry-blocked-p))))))

(provide 'js-triage-session)
;;; js-triage-session.el ends here
