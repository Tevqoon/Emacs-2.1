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
        (goto-char (point-min))
        ;; Skip HTTP headers
        (re-search-forward "^\r?\n" nil t)
        (let ((json-object-type 'alist)
              (json-array-type  'list)
              (json-key-type    'symbol))
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

(defun js-book--find-or-offer-create (title tags)
  "Return an id: link string for the node with TITLE.
If an exact node exists, return a link to it.
If not, ask whether to create one tagged TAGS.
If user declines, return TITLE as a plain string."
  (let ((node (org-roam-node-from-title-or-alias title)))
    (cond
     ;; Existing node -> link
     (node
      (js-book--id-link (org-roam-node-id node) title))
     ;; Offer to create
     ((yes-or-no-p (format "No node for %S. Create one? " title))
      (let ((new-note (vulpea-create title nil :tags tags)))
        (js-book--id-link (vulpea-note-id new-note) title)))
     ;; Plain text fallback
     (t (js-book--meta-safe title)))))

;;;; ──────────────────────────────────────────────────────────────
;;;; Genre / subject pruning
;;;; ──────────────────────────────────────────────────────────────

(defun js-book--pick-genre (subjects)
  "Let the user choose one genre from SUBJECTS (or enter a custom one).
Returns a string, or nil if left empty."
  (when subjects
    (let* ((choice (completing-read
                    "Genre (leave blank to skip): "
                    subjects nil nil)))
      (unless (string-blank-p choice) choice))))

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
;;;; Entry point
;;;; ──────────────────────────────────────────────────────────────

;;;###autoload
(defun js-book-capture (&optional as-heading)
  "Interactively create an org-roam book note.

Prompts for a search query (title, author, or ISBN).  Searches
Open Library, lets you pick a result, resolves author/genre nodes,
then creates a Vulpea note tagged :book:.

Without prefix arg: creates a standalone file-level note.
With prefix arg (C-u): inserts as a heading under the node at point."
  (interactive "P")
  (let* (;; ── 1. Resolve parent (heading mode only) ────────────
         (parent (when as-heading
                   (let* ((roam-node (org-roam-node-at-point 'assert))
                          (vn        (vulpea-db-get-by-id (org-roam-node-id roam-node))))
                     (unless vn
                       (user-error "No vulpea note found for node at point"))
                     vn)))

         ;; ── 2. Get search query ──────────────────────────────
         (query (read-string "Search (title / author / ISBN): "))

         ;; ── 3. Fetch candidates ──────────────────────────────
         (candidates
          (or
           ;; Pure ISBN? Try dedicated endpoint first for richer data.
           (and (string-match-p "\\`[0-9]\\{10,13\\}\\'" (string-trim query))
                (when-let ((c (js-book--ol-isbn-lookup (string-trim query))))
                  (list c)))
           (progn
             (message "Searching Open Library…")
             (js-book--ol-search query))))

         (_ (unless candidates
              (user-error "No results found for %S" query)))

         ;; ── 4. Pick a candidate ──────────────────────────────
         (candidate (if (= (length candidates) 1)
                        (car candidates)
                      (js-book--select-candidate candidates)))

         (title    (alist-get 'title   candidate))
         (authors  (alist-get 'authors candidate))
         (subjects (alist-get 'subjects candidate))

         ;; ── 5. Allow title edit ──────────────────────────────
         (title (read-string "Book title: " title))

         ;; ── 6. Resolve author nodes ──────────────────────────
         (author-links
          (mapcar (lambda (a)
                    (js-book--find-or-offer-create a '("person" "author")))
                  (or authors
                      (list (read-string "Author name: ")))))

         ;; ── 7. Pick genre ────────────────────────────────────
         (genre-raw  (js-book--pick-genre subjects))
         (genre-link (when genre-raw
                       (js-book--find-or-offer-create genre-raw '("genre"))))

         ;; ── 8. Build metadata and properties ─────────────────
         (meta       (js-book--build-meta candidate genre-link author-links))
         (isbn       (or (alist-get 'isbn candidate)
                         (let ((input (read-string "ISBN (leave blank to skip): ")))
                           (unless (string-blank-p input) input))))
         ;; org-roam requires bracket link syntax for ROAM_REFS
         (properties (when isbn
                       (list (cons "ROAM_REFS" (format "[[isbn:%s]]" isbn)))))
         ;; For heading mode, serialize meta as a body description list since
         ;; vulpea--create-heading does not support :meta
         (meta-body  (when (and parent meta)
                       (mapconcat (lambda (kv)
                                    (format "- %s :: %s" (car kv) (cdr kv)))
                                  meta "\n")))

         ;; ── 9. Create note ───────────────────────────────────
         (note (vulpea-create title nil
                              :tags '("book")
                              :meta (unless parent meta)
                              :body (when parent meta-body)
                              :properties properties
                              :parent parent)))

    (message "Created book note: %s" (vulpea-note-path note))
    (find-file (vulpea-note-path note))
    (when parent
      (goto-char (vulpea-note-pos note)))))

(provide 'js-book-capture)
;;; js-book-capture.el ends here
