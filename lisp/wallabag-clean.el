;;; wallabag-clean.el --- Edit & LLM-clean Wallabag entries  -*- lexical-binding: t; -*-

;; Author: Jure
;; Keywords: wallabag, html, convenience
;; Package-Requires: ((emacs "28.1") (request "0.3") (gptel "0.9"))

;;; Commentary:

;; Pull a Wallabag entry's HTML content into an editable buffer, optionally
;; run it through an LLM (Cerebras / gpt-oss-120b) to produce
;; php-readability-safe markup, and push the result back via PATCH.
;;
;; Entry points:
;;   `js/wallabag-edit-entry'  -- pull entry into a `wb-edit-mode' buffer
;;   `js/wallabag-edit-clean'  -- rewrite buffer via LLM in place
;;   `wb-edit-push' (C-c C-c)  -- PATCH buffer contents back

;;; Code:

(require 'request)
(require 'json)
(require 'cl-lib)
(require 'gptel)

;;;; External symbols defined elsewhere in the Wallabag setup.
(defvar wallabag-host)
(declare-function wb--auth-header "ext:wallabag")
(declare-function wb--ensure-token "ext:wallabag")

;;;; ----------------------------------------------------------------
;;;; Content editing -- pull an entry into a buffer, push back via PATCH
;;;; ----------------------------------------------------------------

(defvar-local wb-edit--entry-id nil
  "Wallabag entry id being edited in the current buffer.")

(defun wb--fetch-entry-sync (id)
  "Fetch full entry ID (with content) synchronously; return parsed alist."
  (let ((response
         (request (concat wallabag-host
                          (format "/api/entries/%d.json" id))
           :headers (list (wb--auth-header))
           :parser  #'json-read
           :sync    t
           :error   (cl-function
                     (lambda (&key error-thrown response &allow-other-keys)
                       (error "Wallabag: entry %d fetch failed (%s): %S"
                              id (request-response-status-code response)
                              error-thrown))))))
    (request-response-data response)))

(defun wb--patch-entry-content (id content)
  "PATCH CONTENT into entry ID synchronously."
  (request (concat wallabag-host (format "/api/entries/%d.json" id))
    :type    "PATCH"
    :headers (list (wb--auth-header))
    :data    (list (cons "content" content))
    :parser  #'json-read
    :sync    t
    :error   (cl-function
              (lambda (&key error-thrown response &allow-other-keys)
                (error "Wallabag: entry %d patch failed (%s): %S"
                       id (request-response-status-code response)
                       error-thrown)))))

(define-derived-mode wb-edit-mode html-mode "Wallabag-Edit"
  "Major mode for editing a Wallabag entry's HTML content."
  (setq-local header-line-format
              (substitute-command-keys
               "Wallabag entry edit -- \\[wb-edit-push] to push back")))

(define-key wb-edit-mode-map (kbd "C-c C-c") #'wb-edit-push)

(defun js/wallabag-edit-entry (id)
  "Pull Wallabag entry ID's HTML content into an editable buffer."
  (interactive "nWallabag entry id: ")
  (wb--ensure-token)
  (let* ((entry   (wb--fetch-entry-sync id))
         (title   (alist-get 'title   entry))
         (content (or (alist-get 'content entry) ""))
         (buf     (get-buffer-create
                   (format "*wallabag:%d %s*" id (or title "")))))
    (with-current-buffer buf
      (wb-edit-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content))
      (setq wb-edit--entry-id id)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun wb-edit-push ()
  "Push the current buffer's content back to Wallabag via PATCH."
  (interactive)
  (unless wb-edit--entry-id
    (user-error "Not a Wallabag edit buffer"))
  (when (zerop (buffer-size))
    (user-error "Buffer is empty — refusing to wipe entry %d" wb-edit--entry-id))
  (unless (buffer-modified-p)
    (user-error "Buffer unmodified, nothing to push"))
  (wb--patch-entry-content wb-edit--entry-id (buffer-string))
  (set-buffer-modified-p nil)
  (message "Wallabag: entry %d updated" wb-edit--entry-id))

;;;; ----------------------------------------------------------------
;;;; LLM cleanup of a wb-edit buffer (Cerebras / gpt-oss-120b)
;;;; ----------------------------------------------------------------

(defvar js/wallabag-rewrite-system-prompt
  "You are an HTML cleaner that prepares articles for Wallabag, which runs \
incoming content through php-readability (Firefox Reader View-style) \
extraction. Rewrite the given HTML so it survives that extraction intact, \
preserving ALL textual content verbatim.

RULES:
- Wrap everything in a single <article> tag. Nothing outside it.
- Title as <h1> inside <article> (not in <header>).
- Author/date/source line as <p><em>...</em></p> directly after the <h1>.
- Use <h2>/<h3> for section headings. NO <section>, <div>, <header>, \
<footer>, <nav>, or <aside> tags.
- Drop all class=, style=, id= attributes.
- Drop <figure>/<img> blocks that are tracking pixels or empty images.
- Extended quotations become <blockquote><p>...</p></blockquote>, one <p> \
per paragraph.
- Footnotes collected into <ol> under <h2>Footnotes</h2> at the end; \
inline markers in bracketed form like [1].
- Keep flat structure: no nested divs/sections.

CRITICAL — content preservation:
- Reproduce every sentence of body text verbatim. Do not summarize, \
abridge, or paraphrase.
- Repair obvious encoding damage in punctuation only (missing apostrophes, \
em-dashes, smart quotes).
- Any mathematical formula, equation, or symbolic expression must be copied \
CHARACTER-FOR-CHARACTER. Do not 'correct', simplify, or reinterpret math. \
If a formula looks malformed, leave it exactly as-is.

Output ONLY the cleaned inner HTML, starting with <article> and ending with \
</article>. No markdown fences, no commentary, no preamble."
  "System prompt for `js/wallabag-edit-clean'.")

(defun js/wallabag-edit-clean ()
  "Rewrite the current `wb-edit' buffer into Wallabag-safe HTML via Cerebras.

Replaces the buffer contents in place with the cleaned <article> markup,
leaving the buffer modified so you can review and push with
`wb-edit-push' (\\[wb-edit-push])."
  (interactive)
  (unless wb-edit--entry-id
    (user-error "Not a Wallabag edit buffer"))
  (when (zerop (buffer-size))
    (user-error "Buffer is empty — nothing to clean"))
  (let ((src (buffer-substring-no-properties (point-min) (point-max)))
        (target (current-buffer))
        (start (current-time))
        (gptel-backend (gptel-get-backend "Cerebras"))
        (gptel-model 'gpt-oss-120b)
        (gptel-use-tools nil)
        (gptel-use-context nil))
    (message "js/wallabag: cleaning entry %d (%d chars) via Cerebras…"
             wb-edit--entry-id (length src))
    (gptel-request src
      :system js/wallabag-rewrite-system-prompt
      :context target
      :callback
      (lambda (response info)
        (let ((buf (plist-get info :context)))
          (if (stringp response)
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (let ((inhibit-read-only t)
                        (clean (string-trim
                                (replace-regexp-in-string
                                 "\\`````\\(?:html\\)?\n?\\|\n?```\\'" ""
                                 response))))
                    (erase-buffer)
                    (insert clean)
                    (goto-char (point-min)))
                  (set-buffer-modified-p t)
                  (message "js/wallabag: cleaned in %.1fs (%d → %d chars) — review, then %s"
                           (float-time (time-subtract (current-time) start))
                           (length src) (buffer-size)
                           (substitute-command-keys "\\[wb-edit-push]"))))
            (message "js/wallabag: request failed — %s"
                     (plist-get info :status))))))))

(provide 'wallabag-clean)
;;; wallabag-clean.el ends here
