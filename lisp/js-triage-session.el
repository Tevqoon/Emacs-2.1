;;; js-triage-session.el --- Generic todo triage with exponential snooze backoff
;;
;; Provides a session-based navigator that works across all org-agenda files:
;;   - Collects matching entries once at session start (stable queue)
;;   - Steps through them one at a time, elfeed-style
;;   - Snoozes via SCHEDULED + SNOOZE_COUNT property (2^n day backoff)
;;   - Items with no SCHEDULED timestamp are "due now" and included
;;   - Items with a future SCHEDULED timestamp are excluded (snoozed)
;;   - Actions: done / refile / change-state / snooze / skip / quit
;;
;; Entry points:
;;   js/session-process   — unscheduled PROCESS items only
;;   js/session-keyword   — prompted or given keyword (any single state)
;;   js/session-review    — multi-keyword unfocused review
;;   js/session-all       — all actionable unblocked/unscheduled items
;;
;; During a session, use js/triage-map (or bind globally):
;;   n / SPC  next / skip
;;   d        done
;;   r        refile (org-roam)
;;   t        change todo state
;;   s        snooze (exponential backoff)
;;   S        snooze with manual day count
;;   q        quit session
;;   ?        show remaining count

;;; ─── Session state ────────────────────────────────────────────────────────

(defvar js/triage-session-queue nil
  "List of markers for the current triage session, consumed front-to-back.")

(defvar js/triage-session-label ""
  "Human-readable label for the active session, shown in messages.")

(defvar js/triage-session-total 0
  "Total number of items collected when the session started.")

;;; ─── Collection ──────────────────────────────────────────────────────────

(defun js/triage--due-p ()
  "Return non-nil if the entry at point has no future SCHEDULED timestamp.
Entries with no SCHEDULED, or scheduled today/in the past, are considered due."
  (let ((sched (org-get-scheduled-time (point))))
    (or (not sched)
        (not (time-less-p (current-time) sched)))))

(defun js/triage--collect (todo-keywords &optional extra-pred)
  "Collect markers for org entries matching TODO-KEYWORDS across agenda files.
TODO-KEYWORDS is a string or list of strings.
EXTRA-PRED, if given, is called at point; entry included only if it returns non-nil.
Returns a list of live markers in file+position order."
  (let* ((kws (if (listp todo-keywords) todo-keywords (list todo-keywords)))
         ;; org-map-entries MATCH syntax for todo matching
         (match (concat "TODO={" (mapconcat #'regexp-quote kws "\\|") "}"))
         results)
    (org-map-entries
     (lambda ()
       (when (or (null extra-pred) (funcall extra-pred))
         (push (point-marker) results)))
     match
     'agenda)
    (nreverse results)))

;;; ─── Navigation core ─────────────────────────────────────────────────────

(defun js/triage-session-start (todo-keywords &optional label extra-pred)
  "Collect TODO-KEYWORDS entries and begin stepping through them.
TODO-KEYWORDS: string or list of strings.
LABEL: shown in messages (defaults to joined keyword list).
EXTRA-PRED: optional additional filter, called at point."
  (setq js/triage-session-queue (js/triage--collect todo-keywords extra-pred)
        js/triage-session-label  (or label
                                     (if (listp todo-keywords)
                                         (string-join todo-keywords "|")
                                       todo-keywords))
        js/triage-session-total  (length js/triage-session-queue))
  (if (null js/triage-session-queue)
      (message "[triage/%s] Nothing to process." js/triage-session-label)
    (message "[triage/%s] Starting: %d items"
             js/triage-session-label js/triage-session-total)
    (js/triage-next)))

(defun js/triage-next ()
  "Advance to the next entry in the active triage session."
  (interactive)
  (if (null js/triage-session-queue)
      (message "[triage/%s] Done. (%d processed)"
               js/triage-session-label js/triage-session-total)
    (let ((marker (pop js/triage-session-queue)))
      (if (and (markerp marker)
               (marker-buffer marker)
               (buffer-live-p (marker-buffer marker)))
          (progn
            (switch-to-buffer (marker-buffer marker))
            (goto-char (marker-position marker))
            (org-fold-show-context 'agenda)
            (org-show-entry)
            (recenter)
            (message "[triage/%s] %d / %d remaining"
                     js/triage-session-label
                     (length js/triage-session-queue)
                     js/triage-session-total))
        ;; Stale marker (entry deleted/moved); silently skip
        (js/triage-next)))))

(defalias 'js/triage-skip #'js/triage-next
  "Skip current entry without taking any action.")

(defun js/triage-quit ()
  "Abandon the current triage session."
  (interactive)
  (setq js/triage-session-queue nil)
  (message "[triage/%s] Session abandoned." js/triage-session-label))

(defun js/triage-status ()
  "Report how many items remain in the current session."
  (interactive)
  (message "[triage/%s] %d / %d remaining"
           js/triage-session-label
           (length js/triage-session-queue)
           js/triage-session-total))

;;; ─── Actions (each calls js/triage-next after acting) ───────────────────

(defun js/triage-done ()
  "Mark entry at point DONE and advance to the next."
  (interactive)
  (org-todo 'done)
  (js/triage-next))

(defun js/triage-refile ()
  "Refile entry at point to an org-roam node, then advance."
  (interactive)
  (let ((dest (org-roam-node-read nil nil nil 'require-match)))
    (org-roam-refile dest))
  (js/triage-next))

(defun js/triage-change-state ()
  "Interactively change the TODO state of the entry at point, then advance."
  (interactive)
  (call-interactively #'org-todo)
  (js/triage-next))

(defun js/triage--snooze-for (days)
  "Snooze the entry at point for DAYS days using SCHEDULED.
Increments the SNOOZE_COUNT property so future snoozes back off further."
  (let* ((count (string-to-number
                 (or (org-entry-get nil "SNOOZE_COUNT") "0"))))
    (org-entry-put nil "SNOOZE_COUNT" (number-to-string (1+ count)))
    (org-schedule nil (format "+%dd" days))
    (message "[triage/%s] Snoozed %d day(s) (was snooze #%d)"
             js/triage-session-label days count)))

(defun js/triage-snooze ()
  "Snooze entry at point with exponential backoff (2^SNOOZE_COUNT days).
First snooze = 1 day, second = 2, third = 4, fourth = 8, etc.
Items with no prior SNOOZE_COUNT start at 2^0 = 1 day."
  (interactive)
  (let* ((count (string-to-number
                 (or (org-entry-get nil "SNOOZE_COUNT") "0")))
         (days  (expt 2 count)))
    (js/triage--snooze-for days))
  (js/triage-next))

(defun js/triage-snooze-manual ()
  "Snooze entry at point for a manually specified number of days.
Does NOT increment SNOOZE_COUNT, so the backoff curve is preserved."
  (interactive)
  (let ((days (read-number "Snooze for how many days? " 7)))
    (org-schedule nil (format "+%dd" days))
    (message "[triage/%s] Snoozed %d day(s) (manual, count unchanged)"
             js/triage-session-label days))
  (js/triage-next))

;;; ─── Entry points ─────────────────────────────────────────────────────────

(defun js/session-process ()
  "Triage unscheduled (due) PROCESS items across all agenda files."
  (interactive)
  (js/triage-session-start
   "PROCESS" "process"
   #'js/triage--due-p))

(defun js/session-keyword (keyword)
  "Triage all items with a specific TODO KEYWORD.
When called interactively, prompts for the keyword.
Includes both scheduled-and-due and unscheduled items."
  (interactive
   (list (completing-read "Todo keyword: "
                          (flatten-list org-todo-keywords)
                          (lambda (k) (not (member k '("|"))))
                          t)))
  (js/triage-session-start keyword keyword #'js/triage--due-p))

(defun js/session-review ()
  "Unfocused review session across TODO, NEXT, FINISH, EXPLORE, IDEA.
All due (unscheduled or past-scheduled) items are included."
  (interactive)
  (js/triage-session-start
   '("TODO" "NEXT" "FINISH" "EXPLORE" "IDEA")
   "review"
   #'js/triage--due-p))

(defun js/session-all ()
  "All actionable items: NEXT/TODO/FINISH that are due and unblocked."
  (interactive)
  (js/triage-session-start
   '("NEXT" "TODO" "FINISH")
   "all"
   (lambda ()
     (and (js/triage--due-p)
          (not (org-entry-blocked-p))))))

;;; ─── Keymap ───────────────────────────────────────────────────────────────

(defvar js/triage-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "n")   #'js/triage-next)
    (define-key m (kbd "SPC") #'js/triage-skip)
    (define-key m (kbd "d")   #'js/triage-done)
    (define-key m (kbd "r")   #'js/triage-refile)
    (define-key m (kbd "t")   #'js/triage-change-state)
    (define-key m (kbd "s")   #'js/triage-snooze)
    (define-key m (kbd "S")   #'js/triage-snooze-manual)
    (define-key m (kbd "q")   #'js/triage-quit)
    (define-key m (kbd "?")   #'js/triage-status)
    m)
  "Keymap for triage session actions. Bind under a prefix of your choice.")

;; Suggested bindings — adapt to your prefix conventions.
;; These assume C-c j t as the triage prefix.
;;
;; Launchers:
;;   (global-set-key (kbd "C-c j t p") #'js/session-process)
;;   (global-set-key (kbd "C-c j t k") #'js/session-keyword)
;;   (global-set-key (kbd "C-c j t r") #'js/session-review)
;;   (global-set-key (kbd "C-c j t a") #'js/session-all)
;;
;; In-session actions (usable directly from the visited buffer):
;;   (global-set-key (kbd "C-c j t n") #'js/triage-next)
;;   (global-set-key (kbd "C-c j t SPC") #'js/triage-skip)
;;   (global-set-key (kbd "C-c j t d") #'js/triage-done)
;;   (global-set-key (kbd "C-c j t t") #'js/triage-change-state)
;;   (global-set-key (kbd "C-c j t s") #'js/triage-snooze)
;;   (global-set-key (kbd "C-c j t S") #'js/triage-snooze-manual)
;;   (global-set-key (kbd "C-c j t R") #'js/triage-refile)
;;   (global-set-key (kbd "C-c j t q") #'js/triage-quit)
;;   (global-set-key (kbd "C-c j t ?") #'js/triage-status)

;;; ─── Agenda view integration ─────────────────────────────────────────────
;;
;; Add this to your org-agenda-custom-commands to show only unscheduled (due)
;; PROCESS items. Paste into the relevant section of your org-agenda config:
;;
;; ("p" "Process Items"
;;  ((todo "PROCESS"
;;         ((org-agenda-overriding-header "* To process:  ")
;;          (org-agenda-sorting-strategy '(category-up alpha-up))
;;          (org-agenda-skip-function
;;           '(org-agenda-skip-entry-if 'scheduled))))))
;;
;; Note: org-agenda-skip-entry-if 'scheduled skips entries that have *any*
;; SCHEDULED cookie, including past ones. If you want past-scheduled items
;; to re-appear, replace with a custom predicate checking (not (js/triage--due-p)).

(provide 'js-triage-session)
;;; js-triage-session.el ends here
