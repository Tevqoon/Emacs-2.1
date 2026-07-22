;;; jrnl-video-watch.el --- mark actual watch time on a logged video heading -*- lexical-binding: t; -*-

;; Each logged YouTube video in the journal is its own heading, e.g.:
;;   ** [[elfeed:www.youtube.com#yt:video:xyOZFxj2gag][Some Title - Channel]]
;; By default the pipeline (extract.py) assumes every logged video was
;; watched in full. These commands attach a :WATCH_PCT: property to the
;; heading at point (0-100; 0 = dropped/never really watched) so the
;; watch-monitor's counted hours reflect what you actually watched instead
;; of the video's full length. Setting 100 removes the property again
;; (untouched headings default to "watched in full").
;;
;; The video id is read straight from the heading's own link text, using
;; the same patterns as extract.py's YTID_PATTERNS, so this only works
;; when point is on (or inside) a heading that itself contains the link.
;;
;; Usage: place point anywhere in the video's heading, then
;;   M-x jrnl-video-set-watch-pct   (prompts for a percentage)
;;   M-x jrnl-video-mark-unwatched  (shortcut for 0 — the "I dropped this" case)
;;
;; Suggested keybindings, e.g. in your init:
;;   (with-eval-after-load 'org
;;     (define-key org-mode-map (kbd "C-c C-x w") #'jrnl-video-set-watch-pct)
;;     (define-key org-mode-map (kbd "C-c C-x u") #'jrnl-video-mark-unwatched))

(require 'org)

(defconst jrnl-video-id-regexps
  '("yt:video:\\([A-Za-z0-9_-]\\{11\\}\\)"
    "youtu\\.be/\\([A-Za-z0-9_-]\\{11\\}\\)"
    "youtube\\.com/watch\\?[^]]*v=\\([A-Za-z0-9_-]\\{11\\}\\)"
    "youtube\\.com/embed/\\([A-Za-z0-9_-]\\{11\\}\\)"
    "youtube\\.com/shorts/\\([A-Za-z0-9_-]\\{11\\}\\)")
  "YouTube video-id patterns, mirroring extract.py's YTID_PATTERNS.")

(defun jrnl-video--id-at-heading ()
  "Return the YouTube video id on the heading at point, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (catch 'found
        (dolist (re jrnl-video-id-regexps)
          (when (string-match re line)
            (throw 'found (match-string 1 line))))
        nil))))

;;;###autoload
(defun jrnl-video-set-watch-pct (pct)
  "Mark the YouTube video heading at point as PCT% watched.

PCT is 0-100; 0 means you dropped/skipped it. Stored as the :WATCH_PCT:
property on that heading — extract.py picks it up on the next sync and
scales the video's counted watch time by PCT/100 (its full duration is
still shown for reference, only the counted hours are scaled). Setting
100 removes the property, since that's the default assumption for any
video without one."
  (interactive
   (list (read-number "Watch percent (0 = unwatched): " 100)))
  (when (or (< pct 0) (> pct 100))
    (user-error "Percent must be between 0 and 100"))
  (save-excursion
    (org-back-to-heading t)
    (let ((vid (jrnl-video--id-at-heading)))
      (unless vid
        (user-error "No YouTube link found on this heading"))
      (if (= pct 100)
          (org-entry-delete (point) "WATCH_PCT")
        (org-entry-put (point) "WATCH_PCT" (number-to-string (round pct))))
      (message "%s: watch%% set to %d" vid (round pct)))))

;;;###autoload
(defun jrnl-video-mark-unwatched ()
  "Mark the YouTube video heading at point as unwatched (dropped)."
  (interactive)
  (jrnl-video-set-watch-pct 0))

(provide 'jrnl-video-watch)
;;; jrnl-video-watch.el ends here
