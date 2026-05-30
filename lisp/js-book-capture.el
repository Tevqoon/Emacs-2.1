;;; js-book-capture.el --- Create org-roam book notes via Open Library  -*- lexical-binding: t; -*-

;; Usage:
;;   (require 'js-book-capture)
;;   M-x js-book-capture
;;
;; Bind as desired, e.g.:
;;   (global-set-key (kbd "C-c n b") #'js-book-capture)

;;; Commentary:
;;
;; Searches Open Library by title/author/ISBN, lets you pick a result,
;; then creates a Vulpea note tagged :book: with structured metadata.
;; Author and genre fields are linked to existing org-roam nodes when
;; found, or you're offered to create them on the fly.

;;; Code:

(require 'json)
(require 'url)
(require 'vulpea)
(require 'org-roam)

;;;; ──────────────────────────────────────────────────────────────
;;;; HTTP helpers
;;;; ──────────────────────────────────────────────────────────────

(defun js-book--url-fetch-json (url)
  "Fetch URL synchronously and parse response body as JSON.
Returns the parsed Lisp object or nil on error."
  (condition-case err
      (with-current-buffer (url-retrieve-synchronously url t t 10)
        (set-buffer-multibyte t)
        (goto-char (point-min))
        ;; Skip HTTP headers
        (re-search-forward "^\r?\n" nil t)
        (let ((json-object-type 'alist)
              (json-array-type  'list)
              (json-key-type    'symbol)
              (coding-system-for-read 'utf-8))
          (decode-coding-region (point) (point-max) 'utf-8)
          (json-read)))
    (error
     (message "js-book-capture: HTTP error for %s — %s" url (error-message-string err))
     nil)))

;;;; ──────────────────────────────────────────────────────────────
;;;; Open Library API
;;;; ──────────────────────────────────────────────────────────────

(defconst js-book--ol-search-url
  "https://openlibrary.org/search.json?limit=12&fields=key,title,author_name,first_publish_year,subject,isbn&q=%s"
  "Open Library full-text search endpoint template.")

(defconst js-book--ol-isbn-url
  "https://openlibrary.org/api/books?format=json&jscmd=data&bibkeys=ISBN:%s"
  "Open Library ISBN lookup endpoint template.")

(defun js-book--ol-search (query)
  "Search Open Library for QUERY (title, author, or ISBN).
Returns a list of candidate alists with keys:
  title, authors, year, subjects, isbn."
  (let* ((encoded (url-hexify-string query))
         (url     (format js-book--ol-search-url encoded))
         (data    (js-book--url-fetch-json url))
         (docs    (alist-get 'docs data)))
    (mapcar #'js-book--ol-doc->candidate docs)))

(defun js-book--ol-doc->candidate (doc)
  "Convert a raw Open Library search DOC alist to a candidate alist."
  (let* ((title   (or (alist-get 'title doc) "Unknown Title"))
         (authors (or (alist-get 'author_name doc) '()))
         (year    (alist-get 'first_publish_year doc))
         (subjects (seq-take (or (alist-get 'subject doc) '()) 5))
         (isbns   (alist-get 'isbn doc))
         (isbn    (car isbns)))
    `((title    . ,title)
      (authors  . ,authors)
      (year     . ,year)
      (subjects . ,subjects)
      (isbn     . ,isbn))))

(defun js-book--ol-isbn-lookup (isbn)
  "Look up a single ISBN on Open Library; return a candidate alist or nil."
  (let* ((url  (format js-book--ol-isbn-url isbn))
         (data (js-book--url-fetch-json url))
         (key  (intern (format "ISBN:%s" isbn)))
         (book (alist-get key data)))
    (when book
      (let* ((title   (or (alist-get 'title book) "Unknown Title"))
             (authors (mapcar (lambda (a) (alist-get 'name a))
                              (alist-get 'authors book)))
             (year    (alist-get 'publish_date book))
             (subjects (seq-take
                        (mapcar (lambda (s) (alist-get 'name s))
                                (alist-get 'subjects book))
                        5)))
        `((title    . ,title)
          (authors  . ,authors)
          (year     . ,(when (stringp year)
                         (and (string-match "[0-9]\\{4\\}" year)
                              (string-to-number (match-string 0 year)))))
          (subjects . ,subjects)
          (isbn     . ,isbn))))))

;;;; ──────────────────────────────────────────────────────────────
;;;; Completing-read helpers
;;;; ──────────────────────────────────────────────────────────────

(defun js-book--candidate-label (candidate)
  "Build a human-readable string for CANDIDATE (for completing-read)."
  (let ((title   (alist-get 'title   candidate))
        (authors (alist-get 'authors candidate))
        (year    (alist-get 'year    candidate))
        (isbn    (alist-get 'isbn    candidate)))
    (format "%-50s  %-30s  %s  %s"
            (truncate-string-to-width title 48 nil nil "…")
            (truncate-string-to-width
             (string-join (or authors '("?")) ", ") 28 nil nil "…")
            (or (and year (number-to-string year)) "????")
            (or isbn ""))))

(defun js-book--select-candidate (candidates)
  "Present CANDIDATES in completing-read and return the chosen one."
  (let* ((table  (mapcar (lambda (c)
                           (cons (js-book--candidate-label c) c))
                         candidates))
         (choice (completing-read "Select book: " table nil t)))
    (cdr (assoc choice table))))

;;;; ──────────────────────────────────────────────────────────────
;;;; Org-roam node resolution
;;;; ──────────────────────────────────────────────────────────────

(defun js-book--meta-safe (s)
  "Escape % signs in S so vulpea's template expander leaves them alone."
  (replace-regexp-in-string "%" "%%" (or s "")))

(defun js-book--id-link (id title)
  "Return an org id: link string, with % escaped for vulpea meta."
  (js-book--meta-safe (format "[[id:%s][%s]]" id title)))

(defun js-book--find-or-offer-create (title tags &optional skippable)
  "Prompt to select or create a node, starting with TITLE as initial input.
If an existing node is chosen, return an id: link to it.
If a new name is typed, create a node tagged TAGS and link it.
If SKIPPABLE is non-nil, blank RET or C-g return nil (omit field).
Otherwise, C-g inserts TITLE as plain unlinked text."
  (condition-case nil
      (let* ((prompt (if skippable
                         (format "Link %s (C-g to skip): " title)
                       (format "Link %s (C-g for plain text): " title)))
             (note (vulpea-select prompt
                                  :initial-prompt title
                                  :require-match nil)))
        (cond
         ;; Existing node selected
         ((and note (vulpea-note-id note))
          (js-book--id-link (vulpea-note-id note) (vulpea-note-title note)))
         ;; New title typed — create it
         ((and note (not (string-blank-p (vulpea-note-title note))))
          (let ((new-note (vulpea-create (vulpea-note-title note) nil :tags tags)))
            (js-book--id-link (vulpea-note-id new-note) (vulpea-note-title new-note))))
         ;; Blank
         (t (unless skippable (js-book--meta-safe title)))))
    ;; C-g
    (quit (unless skippable (js-book--meta-safe title)))))

;;;; ──────────────────────────────────────────────────────────────
;;;; Genre / subject pruning
;;;; ──────────────────────────────────────────────────────────────

(defun js-book--pick-genre-and-link (subjects tags)
  "Prompt for a genre using the same flow as author linking.
Pre-fills with the first API subject. Blank RET or C-g skips genre entirely."
  (js-book--find-or-offer-create (or (car subjects) "") tags 'skippable))

;;;; ──────────────────────────────────────────────────────────────
;;;; Note creation
;;;; ──────────────────────────────────────────────────────────────

(defun js-book--build-meta (candidate genre-link author-links)
  "Build the vulpea :meta plist from CANDIDATE, GENRE-LINK, AUTHOR-LINKS."
  (let* ((year  (alist-get 'year candidate))
         (isbn  (alist-get 'isbn candidate))
         (meta  '()))
    ;; Authors — one entry per author (already % -escaped by js-book--id-link)
    (dolist (link (reverse author-links))
      (push (cons "author" link) meta))
    (when genre-link
      (push (cons "genre" genre-link) meta))
    (when year
      (push (cons "year" (number-to-string year)) meta))
    (when isbn
      (push (cons "isbn" (js-book--meta-safe isbn)) meta))
    (push (cons "rating" " ") meta)        ; single space — vulpea skips empty strings
    (push (cons "status" "to-read") meta)
    (nreverse meta)))

;;;; ──────────────────────────────────────────────────────────────
;;;; Lift (update existing note into a book note)
;;;; ──────────────────────────────────────────────────────────────

(defun js-book--lift-note (note meta isbn)
  "Lift an existing NOTE into a book note.
Adds the book tag if missing, sets ROAM_REFS to ISBN if provided,
and merges META into the note's description list — only keys that
are not already set are written, so existing data is preserved."
  (let* ((existing-keys (mapcar #'car (vulpea-note-meta note)))
         (new-meta (seq-filter (lambda (kv)
                                 (not (member (car kv) existing-keys)))
                               meta)))
    (vulpea-utils-with-note-sync note
      ;; Add :book: tag if not already there
      (unless (member "book" (vulpea-buffer-tags-get))
        (vulpea-buffer-tags-add "book"))
      ;; Set ROAM_REFS if we have an ISBN and it's not set
      (when (and isbn
                 (not (org-entry-get nil "ROAM_REFS")))
        (org-set-property "ROAM_REFS" (format "[[isbn:%s]]" isbn)))
      ;; Merge only missing meta keys
      (when new-meta
        (vulpea-buffer-meta-set-batch new-meta)))))

;;;; ──────────────────────────────────────────────────────────────
;;;; Entry point
;;;; ──────────────────────────────────────────────────────────────

;;;###autoload
(defun js-book-capture (&optional as-heading)
  "Interactively create or update an org-roam book note.

First searches Open Library by title/author/ISBN, then prompts to
select an existing note to link or lift, or create a new one.

Without prefix arg: creates a standalone file-level note.
With prefix arg (C-u): inserts as a heading under the node at point.

If an existing note is selected at the title step, it is 'lifted'
into a book note: the :book: tag and any missing metadata are added
without disturbing existing content."
  (interactive "P")
  (let* (;; ── 1. Resolve parent (heading mode only) ────────────
         (parent (when as-heading
                   (let* ((roam-node (org-roam-node-at-point 'assert))
                          (vn        (vulpea-db-get-by-id (org-roam-node-id roam-node))))
                     (unless vn
                       (user-error "No vulpea note found for node at point"))
                     vn)))

         ;; ── 2. Get search query ──────────────────────────────
         (query (read-string "Search Open Library (title / author / ISBN): "))

         ;; ── 3. Fetch candidates ──────────────────────────────
         (candidates
          (or
           (and (string-match-p "\\`[0-9]\\{10,13\\}\\'" (string-trim query))
                (when-let ((c (js-book--ol-isbn-lookup (string-trim query))))
                  (list c)))
           (progn
             (message "Searching Open Library…")
             (js-book--ol-search query))))

         (_ (unless candidates
              (user-error "No results found for %S" query)))

         ;; ── 4. Pick an Open Library candidate ────────────────
         (candidate (if (= (length candidates) 1)
                        (car candidates)
                      (js-book--select-candidate candidates)))

         (api-title  (alist-get 'title   candidate))
         (authors    (alist-get 'authors candidate))
         (subjects   (alist-get 'subjects candidate))

         ;; ── 5. Select or name the target note ────────────────
         ;; vulpea-select over ALL notes; api-title pre-filled.
         ;; Existing note → lift. New title typed → create.
         (target-note (vulpea-select
                       "Book note (select existing or type new title): "
                       :initial-prompt api-title
                       :require-match nil))
         (existing-note (and target-note
                             (vulpea-note-id target-note)
                             target-note))
         (title (if existing-note
                    (vulpea-note-title existing-note)
                  (and target-note
                       (vulpea-note-title target-note))))

         (_ (unless (and title (not (string-blank-p title)))
              (user-error "No title given, aborting")))

         ;; ── 6. Resolve author nodes ──────────────────────────
         (author-links
          (delq nil
                (mapcar (lambda (a)
                          (js-book--find-or-offer-create a '("person" "author")))
                        (or authors (list (read-string "Author name: "))))))

         ;; ── 7. Pick genre ────────────────────────────────────
         (genre-link (js-book--pick-genre-and-link subjects '("genre")))

         ;; ── 8. Build metadata and properties ─────────────────
         (meta  (js-book--build-meta candidate genre-link author-links))
         (isbn  (or (alist-get 'isbn candidate)
                    (let ((input (read-string "ISBN (leave blank to skip): ")))
                      (unless (string-blank-p input) input)))))

    (if existing-note
        ;; ── Lift path ────────────────────────────────────────
        (progn
          (js-book--lift-note existing-note meta isbn)
          (find-file (vulpea-note-path existing-note)))

      ;; ── Create path ──────────────────────────────────────
      (let* ((properties (when isbn
                           (list (cons "ROAM_REFS" (format "[[isbn:%s]]" isbn)))))
             (meta-body  (when (and parent meta)
                           (mapconcat (lambda (kv)
                                        (format "- %s :: %s" (car kv) (cdr kv)))
                                      meta "\n")))
             (note (vulpea-create title nil
                                  :tags '("book")
                                  :meta (unless parent meta)
                                  :body (when parent meta-body)
                                  :properties properties
                                  :parent parent)))
        (message "Created book note: %s" (vulpea-note-path note))
        (find-file (vulpea-note-path note))
        (when parent
          (goto-char (vulpea-note-pos note)))))))

(provide 'js-book-capture)
;;; js-book-capture.el ends here
