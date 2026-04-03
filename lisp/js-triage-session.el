;;; js-triage-session.el --- Minimal todo triage with bidirectional interval snooze
;;
;; Navigation state:
;;   js/triage-session-queue   — list of remaining markers (current at head)
;;   js/triage-session-label   — human-readable session name
;;   js/triage-session-total   — count at session start
;;
;; "next" pops the head and appends it to the tail (circular).
;; "prev" brings the tail to the front.
;; A marker appears exactly once; collection runs once and the list
;; is never extended.
;;
;; Snooze uses a TRIAGE_INTERVAL property (days).
;;   js/triage-snooze-later  — grow interval by 1.5x  (later)
;;   js/triage-snooze-soon   — shrink interval by 0.5x (soon)
;;   js/triage-snooze-manual — set interval explicitly (escape hatch)
;; Migration from old SNOOZE_COUNT: js/triage--migrate-snooze-properties

(require 'org)
(require 'org-roam nil t)

;;; ─── Session state ────────────────────────────────────────────────────────

(defvar js/triage-session-queue nil
  "Circular list of markers.  Current entry is always at the head.")

(defvar js/triage-session-label ""
  "Human-readable label for the active session.")

(defvar js/triage-session-total 0
  "Total items collected when the session started.")

(defvar js/triage-default-session #'js/todo-buffer
  "Function called when `js/triage--visit-current' finds an empty queue.")

;;; ─── Collection ──────────────────────────────────────────────────────────

(defun js/triage--due-p ()
  "Return non-nil if entry at point has no future SCHEDULED timestamp."
  (let ((sched (org-get-scheduled-time (point))))
    (or (not sched)
        (not (time-less-p (current-time) sched)))))

(defun js/triage--collect (todo-keywords &optional extra-pred)
  "Collect markers for entries matching TODO-KEYWORDS across agenda files."
  (let* ((kws   (if (listp todo-keywords) todo-keywords (list todo-keywords)))
         (match (concat "TODO={" (mapconcat #'regexp-quote kws "\\|") "}"))
         results)
    (org-map-entries
     (lambda ()
       (when (or (null extra-pred) (funcall extra-pred))
         (push (point-marker) results)))
     match 'agenda)
    (nreverse results)))

;;; ─── Core navigation ──────────────────────────────────────────────────────

(defun js/triage-session-start (todo-keywords &optional label extra-pred)
  "Collect TODO-KEYWORDS entries and begin stepping through them."
  (let ((items (js/triage--collect todo-keywords extra-pred)))
    (setq js/triage-session-queue  items
          js/triage-session-label  (or label
                                       (if (listp todo-keywords)
                                           (string-join todo-keywords "|")
                                         todo-keywords))
          js/triage-session-total  (length items)))
  (js/triage--visit-current))

(defun js/triage--current ()
  "Return the marker at the head of the queue, or nil."
  (car js/triage-session-queue))

(defun js/triage--visit-current ()
  "Display the entry at the head of the queue."
  (let ((marker (js/triage--current)))
    (if (null marker)
	(if js/triage-default-session
            (funcall js/triage-default-session)
          (message "[triage/%s] No items." js/triage-session-label))
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker))
      (org-fold-show-context 'agenda)
      (org-show-entry)
      (recenter)
      (js/triage--show-status))))

(defun js/triage-goto-next ()
  "Cycle current entry to the back of the queue and show the next."
  (interactive)
  (when js/triage-session-queue
    (setq js/triage-session-queue
          (append (cdr js/triage-session-queue)
                  (list (car js/triage-session-queue)))))
  (js/triage--visit-current))

(defun js/triage-goto-prev ()
  "Bring the last entry to the front and show it."
  (interactive)
  (when js/triage-session-queue
    (setq js/triage-session-queue
          (cons (car (last js/triage-session-queue))
                (butlast js/triage-session-queue))))
  (js/triage--visit-current))

(defun js/triage--show-status ()
  "Display triage progress in the echo area."
  (message "[triage/%s] %d remaining"
           js/triage-session-label
           (length js/triage-session-queue)))

(defun js/triage-session-active-p ()
  "Return non-nil if a triage session is currently active."
  (not (null js/triage-session-queue)))

;;; ─── Removing entries (done/cancel/snooze) ───────────────────────────────

(defun js/triage--pop-current ()
  "Remove the head entry from the queue and return it."
  (let ((m (car js/triage-session-queue)))
    (setq js/triage-session-queue (cdr js/triage-session-queue))
    m))

;;; ─── Snooze machinery ────────────────────────────────────────────────────

(defun js/triage--get-interval ()
  "Return the current TRIAGE_INTERVAL for the entry at point (default 1)."
  (max 1 (string-to-number
          (or (org-entry-get nil "TRIAGE_INTERVAL") "1"))))

(defun js/triage--set-interval (days)
  "Write TRIAGE_INTERVAL and schedule entry at point DAYS days from now."
  (org-entry-put nil "TRIAGE_INTERVAL" (number-to-string days))
  (org-schedule nil (format "+%dd" days)))

(defun js/triage--fuzz-interval (days)
  "Add ±5% jitter to DAYS when >= 8, to prevent queue clustering."
  (if (>= days 8)
      (let ((fuzz (max 1 (ceiling (* 0.05 days)))))
        (+ days (nth (random 3) (list (- fuzz) 0 fuzz))))
    days))

;;; ─── Migration ───────────────────────────────────────────────────────────

(defun js/triage--migrate-snooze-properties ()
  "One-shot migration: convert SNOOZE_COUNT -> TRIAGE_INTERVAL across agenda files.
Old interval is recovered as 2^SNOOZE_COUNT."
  (interactive)
  (org-map-entries
   (lambda ()
     (let ((count (org-entry-get nil "SNOOZE_COUNT")))
       (when count
         (let ((interval (expt 2 (string-to-number count))))
           (org-entry-put nil "TRIAGE_INTERVAL" (number-to-string interval))
           (org-entry-delete nil "SNOOZE_COUNT")))))
   nil 'agenda)
  (message "[triage] Migration complete: SNOOZE_COUNT -> TRIAGE_INTERVAL."))

;;; ─── Completing actions (remove from queue, then show next) ──────────────

(defun js/triage-next ()
  "Skip current entry without any action and advance."
  (interactive)
  (js/triage-goto-next))

(defun js/triage-done ()
  "Mark entry DONE, remove from queue, then show next."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((org-blocker-hook nil))
      (org-todo 'done)))
  (save-buffer)
  (js/triage--pop-current)
  (js/triage--visit-current))

(defun js/triage-cancel ()
  "Mark entry CANCELLED, remove from queue, then show next."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((org-blocker-hook nil))
      (org-todo "CANCELLED")))
  (save-buffer)
  (js/triage--pop-current)
  (js/triage--visit-current))

(defun js/triage--snooze (days label)
  "Snooze entry at point for DAYS days, remove from queue, show next.
LABEL is used in the echo area message."
  (save-excursion
    (org-back-to-heading t)
    (js/triage--set-interval days)
    (message "[triage/%s] Snoozed (%s) for %d day(s)."
             js/triage-session-label label days))
  (save-buffer)
  (js/triage--pop-current)
  (js/triage--visit-current))

(defun js/triage-snooze-later ()
  "Snooze later: grow TRIAGE_INTERVAL by 1.5x."
  (interactive)
  (let* ((old    (save-excursion (org-back-to-heading t) (js/triage--get-interval)))
         (new    (max 1 (round (* old 1.5))))
         (fuzzed (js/triage--fuzz-interval new)))
    (js/triage--snooze fuzzed "later")))

(defun js/triage-snooze-soon ()
  "Snooze soon: shrink TRIAGE_INTERVAL by 0.5x (minimum 1 day)."
  (interactive)
  (let* ((old (save-excursion (org-back-to-heading t) (js/triage--get-interval)))
         (new (max 1 (round (* old 0.5)))))
    (js/triage--snooze new "soon")))

(defun js/triage-snooze-manual ()
  "Snooze for an explicitly specified number of days (escape hatch)."
  (interactive)
  (let ((days (read-number "Snooze for how many days? " 7)))
    (js/triage--snooze days "manual")))

;;; ─── Quit ─────────────────────────────────────────────────────────────────

(defun js/triage-quit ()
  "Abandon the current triage session."
  (interactive)
  (setq js/triage-session-queue nil)
  (message "[triage/%s] Session abandoned." js/triage-session-label))

;;; ─── Entry points ─────────────────────────────────────────────────────────

(defun js/session-process ()
  (interactive)
  (js/triage-session-start "PROCESS" "process" #'js/triage--due-p))

(defun js/session-project ()
  (interactive)
  (js/triage-session-start '("PROJECT" "ACTIVE") "Projects" #'js/triage--due-p))

(defun js/session-keyword (keyword)
  (interactive
   (list (completing-read "Todo keyword: "
                          (flatten-list org-todo-keywords)
                          (lambda (k) (not (member k '("|"))))
                          t)))
  (js/triage-session-start keyword keyword #'js/triage--due-p))

(defun js/session-review ()
  (interactive)
  (js/triage-session-start
   '("TODO" "NEXT" "FINISH" "EXPLORE" "IDEA") "review" #'js/triage--due-p))

(defun js/session-all ()
  (interactive)
  (js/triage-session-start
   '("NEXT" "TODO" "FINISH") "all"
   (lambda ()
     (and (js/triage--due-p)
          (not (org-entry-blocked-p))))))

(defun js/session-buffer ()
  "Start a triage session on the current buffer's TODO entries."
  (interactive)
  (unless (buffer-file-name)
    (user-error "[triage] Buffer is not visiting a file"))
  (let* ((tree (org-element-parse-buffer 'headline))
         (markers
          (org-element-map tree 'headline
            (lambda (hl)
              (when (org-element-property :todo-keyword hl)
                (copy-marker (org-element-property :begin hl)))))))
    (setq js/triage-session-queue markers
          js/triage-session-label (file-name-base (buffer-file-name))
          js/triage-session-total (length markers))
    (js/triage--visit-current)))

(defun js/session-subtree-notodo ()
  "Start a triage session on all headings in the top-level tree at point."
  (interactive)
  (unless (buffer-file-name)
    (user-error "[triage] Buffer is not visiting a file"))
  (save-excursion
    (org-back-to-heading t)
    (while (org-up-heading-safe))  ; go to top-level ancestor
    (let* ((markers (org-map-entries
                     (lambda () (copy-marker (point)))
                     nil 'tree)))
      (setq js/triage-session-queue markers
            js/triage-session-label (file-name-base (buffer-file-name))
            js/triage-session-total (length markers))
      (js/triage--visit-current))))

(provide 'js-triage-session)
;;; js-triage-session.el ends here
