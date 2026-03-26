;;; js-triage-session.el --- Minimal todo triage with exponential snooze backoff
;;
;; Navigation state:
;;   js/triage-session-queue   — list of remaining markers (current at head)
;;   js/triage-session-label   — human-readable session name
;;   js/triage-session-total   — count at session start
;;
;; "next" pops the head and appends it to the tail (circular).
;; "prev" is not supported in this model — use next to cycle.
;; A marker appears exactly once; collection runs once and the list
;; is never extended.

(require 'org)
(require 'org-roam nil t)

;;; ─── Session state ────────────────────────────────────────────────────────

(defvar js/triage-session-queue nil
  "Circular list of markers.  Current entry is always at the head.")

(defvar js/triage-session-label ""
  "Human-readable label for the active session.")

(defvar js/triage-session-total 0
  "Total items collected when the session started.")

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
        (message "[triage/%s] No items." js/triage-session-label)
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

(defun js/triage--snooze-for (days)
  "Snooze entry at point for DAYS days, incrementing SNOOZE_COUNT."
  (let ((count (string-to-number
                (or (org-entry-get nil "SNOOZE_COUNT") "0"))))
    (org-entry-put nil "SNOOZE_COUNT" (number-to-string (1+ count)))
    (org-schedule nil (format "+%dd" days))))

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

(defun js/triage-snooze ()
  "Snooze with exponential backoff (2^SNOOZE_COUNT days), remove, show next."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((count (string-to-number
                   (or (org-entry-get nil "SNOOZE_COUNT") "0")))
           (days  (expt 2 count)))
      (js/triage--snooze-for days)
      (message "[triage/%s] Snoozed for %d day(s)." js/triage-session-label days)))
  (save-buffer)
  (js/triage--pop-current)
  (js/triage--visit-current))

(defun js/triage-snooze-manual ()
  "Snooze for a manually specified number of days, remove, show next."
  (interactive)
  (let ((days (read-number "Snooze for how many days? " 7)))
    (save-excursion
      (org-back-to-heading t)
      (org-schedule nil (format "+%dd" days))))
  (save-buffer)
  (js/triage--pop-current)
  (js/triage--visit-current))

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

(provide 'js-triage-session)
;;; js-triage-session.el ends here
