;;; js-uptime.el --- Log Emacs session uptime on shutdown -*- lexical-binding: t; -*-

;;; Commentary:
;; Appends session uptime to an org file in `user-emacs-directory' on shutdown.
;; Entries are grouped by hostname for easy git merging across machines.

;;; Code:

(defvar js/uptime-log-file (expand-file-name "uptime-log.org" user-emacs-directory)
  "File where Emacs session uptimes are logged on shutdown.")

(defun js/uptime-log--seconds ()
  "Return uptime in seconds as an integer."
  (time-convert (time-since before-init-time) 'integer))

(defun js/uptime-log--human (seconds)
  "Format SECONDS as a human-readable uptime string."
  (format-seconds "%H, %M, %z%S" seconds))

(defun js/uptime-log-session ()
  "Append this session's uptime to `js/uptime-log-file', grouped by hostname."
  (let* ((host (system-name))
         (uptime-s (js/uptime-log--seconds))
         (human (js/uptime-log--human uptime-s))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (entry (format "- %s uptime=%ds (%s)\n" timestamp uptime-s human))
         (heading (format "* %s" host)))
    (with-temp-buffer
      (when (file-exists-p js/uptime-log-file)
        (insert-file-contents js/uptime-log-file))
      (goto-char (point-min))
      ;; Find or create host headline
      (if (re-search-forward (concat "^" (regexp-quote heading) "$") nil t)
          ;; Move to end of this headline's content (before next headline or EOF)
          (progn
            (forward-line 1)
            (while (and (not (eobp))
                        (not (looking-at "^\\* ")))
              (forward-line 1)))
        ;; No headline found — create it at end
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert heading "\n"))
      (insert entry)
      (write-region (point-min) (point-max) js/uptime-log-file nil 'silent))))

(provide 'js-uptime)
;;; js-uptime.el ends here
