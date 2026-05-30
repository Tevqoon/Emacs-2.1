;;; vulpea-dblocks.el --- Org dynamic blocks powered by vulpea -*- lexical-binding: t -*-

;; Author: Jure Smolar
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1") (vulpea "0.3") (org "9.6"))
;; Keywords: org, roam, vulpea, dynamic-blocks

;;; Commentary:

;; Provides two org dynamic block types backed by vulpea's DB:
;;
;;   roam-links  — inserts heading links to matching roam nodes
;;   roam-table  — inserts an org table of matching nodes with metadata columns
;;
;; v0.4.0 is the consolidated baseline.  It keeps the v0.3.0 architecture
;; (the :from/:where source DSL, the row struct with :flatten support,
;; aggregates, and group-level sort) and fixes the v0.3.0 regression in
;; which a legacy `:meta-filter' with the `=' operator compared a
;; numericised value against a string, silently filtering out every row.
;; String equality now compares raw strings; ordering operators
;; (< > <= >=) numericise both sides.  As a result the v0.3.0 file no
;; longer needs the v0.2.0 file loaded alongside it to render tables.
;;
;; SOURCE DSL (:from)
;; ------------------
;; :from takes a source expression evaluated by `vulpea-dblocks--resolve-source':
;;   (all)                  all notes
;;   (tag NAME)             notes tagged NAME (NAME a string, or a 1-element list)
;;   (tags TAG...)          notes with ANY of the tags (alias for tags-any)
;;   (tags-any TAG...)      notes with ANY of the tags
;;   (tags-all TAG...)      notes with ALL of the tags
;;   (folder PATH)          notes under directory PATH
;;   (file PATH)            notes from file PATH
;;   (level N)              notes at outline level N
;;   (links-to X)           notes linking to title-or-id X
;;   (links-from X)         notes linked from title-or-id X
;;   (or SOURCE...)         union
;;   (and CLAUSE...)        intersection; (not SOURCE) clauses subtract
;;
;; FILTER (:where)
;; ---------------
;; :where takes either a (lambda (note) ...) / (function ...) form evaluated
;; directly, or a small DSL sexp evaluated in an environment providing:
;;   (field K)    field value for the note
;;   (now)        current time as float
;;   (today)      midnight today as float
;;   (days N)     N days in seconds
;;   (parse-ts S) parse timestamp string S to float
;; DSL evaluation errors exclude the note silently.
;;
;; LEGACY QUERY PARAMS (still supported, rewritten to :from/:where)
;; ---------------------------------------------------------------
;;   :tags         list of tags, notes with ANY
;;   :tags-every   list of tags, notes with ALL
;;   :exclude-tags list of tags to exclude
;;   :title-match  regex on title
;;   :meta-filter  (KEY OP VALUE), OP in (= < > <= >=)
;;   :limit        max number of results (applied after sort)
;;
;; roam-links additional params:
;;   :level        explicit heading depth (auto-detected from context otherwise)
;;   :meta-types   alist of (KEY . TYPE) for typed metadata reads
;;
;; roam-table additional params:
;;   :columns      list of column specs (see below)
;;   :sort         column name (or sort spec) to sort by
;;   :sort-dir     asc (default) or desc
;;   :group-by     metadata key or symbol to group results by
;;   :flatten      metadata key to explode into one row per value
;;   :meta-types   alist of (KEY . TYPE) for typed metadata reads
;;
;; COLUMN SPECS for roam-table:
;;   title         note title (always a link)
;;   tags          note tags as comma-separated string
;;   todo          todo state
;;   scheduled     scheduled timestamp
;;   deadline      deadline timestamp
;;   backlinks     number of notes linking to this note
;;   "key"         a string treated as a vulpea metadata key
;;   ("Header" . key)          custom header name
;;   (AGG . "Header")          aggregate column, e.g. ((avg "rating") . "Avg")
;;   (group . "Header")        the group value column in a grouped table
;;
;; Auto-update:
;;   Add #+ROAM_DBLOCKS_AUTOUPDATE: t to a file to refresh all blocks on save.
;;   Blocks are skipped if point is inside them (to avoid clobbering edits).

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'vulpea)

;;; ============================================================
;;; Per-render dynamic state
;;; ============================================================

(defvar vulpea-dblocks--field-memo nil
  "Hash table caching field lookups within one render pass.
Bound dynamically by render entry points.  Keys are (NOTE-ID . FIELD-STRING).")

(defvar vulpea-dblocks--current-meta-types nil
  "Alist of (KEY . TYPE) for typed metadata reads during a render pass.
Set from the :meta-types block parameter.")

(defvar vulpea-dblocks--memo-nil (make-symbol "vulpea-dblocks--memo-nil")
  "Sentinel stored in `vulpea-dblocks--field-memo' to cache a nil result.
Distinguishes a cached nil from a cache miss (gethash default).")

(defvar vulpea-dblocks--flatten-field nil
  "Field name being exploded by :flatten during a render pass, or nil.
When non-nil, `vulpea-dblocks--field' returns a row's :value slot instead of
querying the DB for this field, so each exploded row shows its own value.")

;;; ============================================================
;;; Row struct
;;; ============================================================

(cl-defstruct (vulpea-dblocks-row (:constructor vulpea-dblocks-row-make))
  "A pipeline row: a note plus an optional exploded value from :flatten."
  note    ; vulpea-note struct
  value)  ; exploded field value (nil when :flatten not active)

;;; ============================================================
;;; Utilities
;;; ============================================================

(defun vulpea-dblocks--note-title (note)
  "Return display title for NOTE, stripping todo keyword prefix if present."
  (let ((title (or (vulpea-note-title note) ""))
        (todo  (vulpea-note-todo note)))
    (if todo
        (string-trim
         (replace-regexp-in-string
          (concat "^" (regexp-quote todo) "\\s-+") "" title))
      title)))

(defun vulpea-dblocks--field (note-or-row field)
  "Return the value of FIELD for NOTE-OR-ROW.
FIELD is a symbol or string.  Returns a typed value (string, number, list, etc.).
Results are memoised in `vulpea-dblocks--field-memo' for the duration of a render.
When `:flatten' is active and FIELD matches `vulpea-dblocks--flatten-field', the
per-row exploded value is returned without memoisation."
  (let* ((is-row (vulpea-dblocks-row-p note-or-row))
         (note   (if is-row (vulpea-dblocks-row-note note-or-row) note-or-row))
         (key    (if (symbolp field) (symbol-name field) field)))
    ;; Flatten shortcut: each row has its own exploded value for this field.
    (if (and is-row
             vulpea-dblocks--flatten-field
             (equal key vulpea-dblocks--flatten-field))
        (vulpea-dblocks-row-value note-or-row)
      (let* ((id   (vulpea-note-id note))
             (memo vulpea-dblocks--field-memo)
             (mkey (cons id key))
             (hit  (when memo (gethash mkey memo vulpea-dblocks--memo-nil))))
        (if (not (eq hit vulpea-dblocks--memo-nil))
            hit
          (let ((val
                 (pcase key
                   ("title"    (vulpea-dblocks--note-title note))
                   ((or "tags" "file.tags") (vulpea-note-tags note))
                   ("todo"     (vulpea-note-todo note))
                   ("scheduled" (vulpea-note-scheduled note))
                   ("deadline" (vulpea-note-deadline note))
                   ("priority" (vulpea-note-priority note))
                   ("level"    (vulpea-note-level note))
                   ("id"       id)
                   ("aliases"  (vulpea-note-aliases note))
                   ("file.path" (vulpea-note-path note))
                   ("file.name" (file-name-base (vulpea-note-path note)))
                   ("file.mtime"
                    (when-let ((a (file-attributes (vulpea-note-path note))))
                      (float-time (file-attribute-modification-time a))))
                   ("file.ctime"
                    (when-let ((a (file-attributes (vulpea-note-path note))))
                      (float-time (file-attribute-status-change-time a))))
                   ("file.size"
                    (when-let ((a (file-attributes (vulpea-note-path note))))
                      (file-attribute-size a)))
                   ("file.inlinks"
                    (vulpea-db-query-links-to id))
                   ((or "backlinks" "file.backlinks-count")
                    (length (vulpea-db-query-links-to id)))
                   ("file.outlinks"
                    (let* ((links    (seq-filter
                                      (lambda (l) (string= (plist-get l :type) "id"))
                                      (vulpea-note-links note)))
                           (dest-ids (mapcar (lambda (l) (plist-get l :dest)) links)))
                      (when dest-ids (vulpea-db-query-by-ids dest-ids))))
                   (_
                    (let ((type (cdr (assoc key vulpea-dblocks--current-meta-types))))
                      (if type
                          (vulpea-note-meta-get note key type)
                        (vulpea-note-meta-get note key)))))))
            (when memo (puthash mkey val memo))
            val))))))

(defun vulpea-dblocks--field-as-string (note-or-row field)
  "Return field value formatted as a display string."
  (vulpea-dblocks--format-value (vulpea-dblocks--field note-or-row field)))

(defun vulpea-dblocks--note-column-value (note col)
  "Deprecated shim.  Use `vulpea-dblocks--field' instead."
  (vulpea-dblocks--field-as-string note col))

;;; ============================================================
;;; Source DSL
;;; ============================================================

(defun vulpea-dblocks--meta-filter-where (meta-filter)
  "Build a :where lambda sexp from a legacy META-FILTER (KEY OP VALUE).

OP semantics:
  =            string equality on the raw value; if VALUE is a number,
               a numeric match is also accepted (so (\"rating\" = 8)
               works whether rating is stored as \"8\").
  < > <= >=    numeric comparison; both sides are coerced to numbers.
  other        treated as string equality.

This corrects the v0.3.0 regression where `=' compared
(string-to-number v) against a string, filtering out every row."
  (let* ((key (nth 0 meta-filter))
         (op  (nth 1 meta-filter))
         (val (nth 2 meta-filter)))
    (pcase op
      ((or '= 'equal)
       `(lambda (n)
          (when-let ((v (vulpea-note-meta-get n ,key)))
            (or (equal v ,(if (numberp val) (number-to-string val) val))
                ,(when (numberp val)
                   `(= (string-to-number v) ,val))))))
      ((or '< '> '<= '>=)
       (let ((op-fn (pcase op ('< '<) ('> '>) ('<= '<=) ('>= '>=))))
         `(lambda (n)
            (when-let ((v (vulpea-note-meta-get n ,key)))
              (,op-fn (string-to-number v)
                      ,(if (numberp val) val `(string-to-number ,val)))))))
      (_
       `(lambda (n)
          (equal (vulpea-note-meta-get n ,key)
                 ,(if (numberp val) (number-to-string val) val)))))))

(defun vulpea-dblocks--normalize-params (params)
  "Rewrite legacy query params in PARAMS to :from/:where form.
Returns a plist with :from and/or :where added when absent.  Legacy keys
are left in place; `vulpea-dblocks--query' only reads :from and :where.
If :from or :where are already present they are left unchanged."
  (let* ((has-from     (plist-member params :from))
         (has-where    (plist-member params :where))
         (tags         (plist-get params :tags))
         (tags-every   (plist-get params :tags-every))
         (exclude-tags (plist-get params :exclude-tags))
         (meta-filter  (plist-get params :meta-filter))
         (title-match  (plist-get params :title-match))
         ;; Work on a copy so we don't mutate the caller's plist.
         ;; Do NOT use cl-remf — it corrupts the list when a key is absent.
         (p            (copy-sequence params)))
    ;; Build :from from legacy tag params
    (unless has-from
      (when (or tags tags-every exclude-tags)
        (let* ((base
                (cond
                 ((and tags tags-every)
                  `(and (tags-any ,@(if (listp tags) tags (list tags)))
                        (tags-all ,@(if (listp tags-every) tags-every (list tags-every)))))
                 (tags-every
                  `(tags-all ,@(if (listp tags-every) tags-every (list tags-every))))
                 (tags
                  `(tags-any ,@(if (listp tags) tags (list tags))))
                 (t nil)))
               (from-sexp
                (if exclude-tags
                    (let ((ex (if (listp exclude-tags) exclude-tags (list exclude-tags))))
                      (if base
                          `(and ,base (not (tags-any ,@ex)))
                        `(and (all) (not (tags-any ,@ex)))))
                  base)))
          (when from-sexp
            (setq p (plist-put p :from from-sexp))))))
    ;; Build :where from meta-filter and title-match
    (unless has-where
      (let* ((mf-where (when meta-filter
                         (vulpea-dblocks--meta-filter-where meta-filter)))
             (tm-where
              (when title-match
                `(lambda (n)
                   (string-match-p ,title-match (or (vulpea-note-title n) "")))))
             (combined
              (cond
               ((and mf-where tm-where)
                `(lambda (n) (and (funcall ,mf-where n) (funcall ,tm-where n))))
               (mf-where mf-where)
               (tm-where tm-where)
               (t nil))))
        (when combined
          (setq p (plist-put p :where combined)))))
    p))

(defun vulpea-dblocks--resolve-title-to-id (title-or-id)
  "Return the note ID for TITLE-OR-ID.
UUID strings pass through unchanged.  Other strings are resolved via
`vulpea-db-search-by-title'; exactly one match is required."
  (if (string-match-p
       "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$"
       title-or-id)
      title-or-id
    (let ((notes (vulpea-db-search-by-title title-or-id)))
      (cond
       ((null notes)
        (user-error "vulpea-dblocks: no note found with title %S" title-or-id))
       ((cdr notes)
        (user-error "vulpea-dblocks: title %S is ambiguous (%d matches: %s)"
                    title-or-id (length notes)
                    (mapconcat #'vulpea-note-id notes ", ")))
       (t (vulpea-note-id (car notes)))))))

(defun vulpea-dblocks--union-notes (lists)
  "Return the union of note LISTS, deduplicating by note ID."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (notes lists)
      (dolist (n notes)
        (let ((id (vulpea-note-id n)))
          (unless (gethash id seen)
            (puthash id t seen)
            (push n result)))))
    (nreverse result)))

(defun vulpea-dblocks--intersect-notes (lists)
  "Return the intersection of note LISTS, keyed by note ID.
Uses the smallest list as the seed for efficiency."
  (when lists
    (let* ((sorted (sort (copy-sequence lists)
                         (lambda (a b) (< (length a) (length b)))))
           (result (car sorted)))
      (dolist (other (cdr sorted))
        (let ((ids (make-hash-table :test #'equal)))
          (dolist (n other) (puthash (vulpea-note-id n) t ids))
          (setq result (seq-filter (lambda (n) (gethash (vulpea-note-id n) ids))
                                   result))))
      result)))

(defun vulpea-dblocks--resolve-and (clauses)
  "Evaluate an (and ...) source expression from CLAUSES.
Positive clauses are intersected; (not X) clauses subtract their IDs.
When there are no positive clauses the full DB is used as the base."
  (let* ((not-clauses (seq-filter (lambda (c) (eq (car-safe c) 'not)) clauses))
         (pos-clauses (seq-filter (lambda (c) (not (eq (car-safe c) 'not))) clauses))
         (base
          (cond
           ((null pos-clauses)
            (vulpea-db-query))
           ((= 1 (length pos-clauses))
            (vulpea-dblocks--resolve-source (car pos-clauses)))
           (t
            (vulpea-dblocks--intersect-notes
             (mapcar #'vulpea-dblocks--resolve-source pos-clauses)))))
         (excluded
          (let ((ids (make-hash-table :test #'equal)))
            (dolist (clause not-clauses)
              (dolist (n (vulpea-dblocks--resolve-source (cadr clause)))
                (puthash (vulpea-note-id n) t ids)))
            ids)))
    (seq-filter (lambda (n) (not (gethash (vulpea-note-id n) excluded))) base)))

(defun vulpea-dblocks--resolve-source (sexp)
  "Evaluate source expression SEXP and return a list of vulpea-note structs."
  (pcase sexp
    (`(all)
     (vulpea-db-query))
    (`(tag ,name)
     ;; Single-tag clause.  Accepts a bare string (tag "book") or tolerates
     ;; a one-element list (tag ("book")); anything else errors loudly rather
     ;; than silently querying a malformed tag and returning an empty table.
     (vulpea-db-query-by-tags-some
      (cond
       ((stringp name) (list name))
       ((and (listp name) (= 1 (length name)) (stringp (car name))) name)
       (t (user-error
           "vulpea-dblocks: (tag ...) wants a single tag string, got %S; \
use (tags ...) or (tags-any ...) for multiple tags"
           name)))))
    (`(tags . ,tags)
     ;; Alias for tags-any: notes having ANY of the given tags.
     (vulpea-db-query-by-tags-some tags))
    (`(tags-any . ,tags)
     (vulpea-db-query-by-tags-some tags))
    (`(tags-all . ,tags)
     (vulpea-db-query-by-tags-every tags))
    (`(folder ,path)
     (vulpea-db-query-by-directory path))
    (`(file ,path)
     (vulpea-db-query-by-file-path path))
    (`(level ,n)
     (vulpea-db-query-by-level n))
    (`(links-to ,x)
     (vulpea-db-query-by-links-some
      (list (vulpea-dblocks--resolve-title-to-id x))))
    (`(links-from ,x)
     (let* ((id    (vulpea-dblocks--resolve-title-to-id x))
            (links (seq-filter (lambda (l) (string= (plist-get l :type) "id"))
                               (vulpea-db-query-links-from id)))
            (dests (mapcar (lambda (l) (plist-get l :dest)) links)))
       (when dests (vulpea-db-query-by-ids dests))))
    (`(or . ,sources)
     (vulpea-dblocks--union-notes
      (mapcar #'vulpea-dblocks--resolve-source sources)))
    (`(and . ,clauses)
     (vulpea-dblocks--resolve-and clauses))
    (`(not . ,_)
     (user-error
      "vulpea-dblocks: (not ...) is only valid inside (and ...); \
write (and (all) (not ...)) for a global exclusion"))
    (_
     (user-error "vulpea-dblocks: unknown source expression: %S" sexp))))

;;; ============================================================
;;; Shared query logic
;;; ============================================================

(defun vulpea-dblocks--query (params)
  "Run a vulpea DB query according to PARAMS and return a list of notes.
Normalises legacy params (:tags, :tags-every, :exclude-tags, :meta-filter,
:title-match) to :from/:where before evaluating the source expression."
  (let* ((params (vulpea-dblocks--normalize-params params))
         (from   (or (plist-get params :from) '(all)))
         (notes  (vulpea-dblocks--resolve-source from)))
    (vulpea-dblocks--apply-where notes (plist-get params :where))))

;;; ============================================================
;;; Where predicate
;;; ============================================================

(defun vulpea-dblocks--compile-where (sexp)
  "Compile WHERE-SEXP to a unary predicate function (lambda (note) ...).
If SEXP starts with `lambda' or `function' it is evaled directly.
Otherwise it is treated as a DSL expression evaluated in an environment
that provides: (field K), (now), (today), (days N), (parse-ts S).
For DSL sexps, errors during evaluation (e.g. comparing nil to a
number when a field is absent) cause the note to be excluded silently."
  (if (memq (car-safe sexp) '(lambda function))
      (eval sexp t)
    (eval
     `(lambda (n)
        (ignore-errors
          (cl-flet ((field    (k) (vulpea-dblocks--field n k))
                    (now      ()  (float-time))
                    (today    ()  (let ((d (decode-time)))
                                   (float-time
                                    (encode-time 0 0 0
                                                 (nth 3 d) (nth 4 d) (nth 5 d)))))
                    (days     (d) (* d 86400.0))
                    (parse-ts (s) (float-time (date-to-time s))))
            ,sexp)))
     t)))

(defun vulpea-dblocks--apply-where (notes where-sexp)
  "Filter NOTES by WHERE-SEXP.  Returns NOTES unchanged if WHERE-SEXP is nil."
  (if (null where-sexp)
      notes
    (seq-filter (vulpea-dblocks--compile-where where-sexp) notes)))

;;; ============================================================
;;; Sorting and limiting
;;; ============================================================

(defun vulpea-dblocks--compare-values (a b)
  "Compare non-nil values A and B for sort ordering.
Returns negative if A < B, 0 if equal, positive if A > B.
Numbers compare numerically; anything else compares as formatted strings."
  (cond
   ((and (numberp a) (numberp b)) (- a b))
   (t (let ((sa (format "%s" a))
            (sb (format "%s" b)))
        (cond ((string< sa sb) -1)
              ((string< sb sa)  1)
              (t 0))))))

(defun vulpea-dblocks--normalize-sort-spec (sort-spec)
  "Normalise SORT-SPEC to a list of (FIELD-STRING . DIR) pairs.
Accepts nil, a bare symbol/string (ascending), a single (COL . DIR) cons,
or a list of (COL . DIR) conses.  DIR defaults to asc when absent.
(expr SEXP . DIR) entries are kept as-is for group-level sorting."
  (cond
   ((null sort-spec) nil)
   ;; List of pairs: ((col . dir) ...)
   ((and (listp sort-spec)
         (consp (car sort-spec))
         (listp (car (car sort-spec))))
    ;; Outer list whose first element's car is itself a list — not our format.
    (user-error "vulpea-dblocks: invalid sort spec: %S" sort-spec))
   ((and (listp sort-spec)
         (consp (car sort-spec)))
    (mapcar (lambda (pair)
              (if (eq (car-safe pair) 'expr)
                  pair  ; (expr SEXP . DIR) kept as-is — applied at group level
                (let ((k (car pair))
                      (d (or (cdr pair) 'asc)))
                  (cons (if (symbolp k) (symbol-name k) k) d))))
            sort-spec))
   ;; Single (COL . DIR) cons
   ((and (consp sort-spec)
         (not (eq (car sort-spec) 'expr)))
    (let ((k (car sort-spec))
          (d (or (cdr sort-spec) 'asc)))
      (list (cons (if (symbolp k) (symbol-name k) k) d))))
   ;; Bare symbol or string → single field asc
   ((or (symbolp sort-spec) (stringp sort-spec))
    (list (cons (if (symbolp sort-spec) (symbol-name sort-spec) sort-spec)
                'asc)))
   (t (user-error "vulpea-dblocks: invalid sort spec: %S" sort-spec))))

(defun vulpea-dblocks--sort-rows (rows sort-spec)
  "Sort ROWS by SORT-SPEC (a list of (FIELD-STRING . DIR) pairs).
Returns ROWS unchanged when SORT-SPEC is nil.
Nil field values always sort last regardless of direction."
  (if (null sort-spec)
      rows
    (seq-sort
     (lambda (row-a row-b)
       (catch 'done
         (dolist (pair sort-spec)
           (unless (eq (car-safe pair) 'expr)  ; expr pairs are for group-level sort
             (let* ((field (car pair))
                    (dir   (cdr pair))
                    (va    (vulpea-dblocks--field row-a field))
                    (vb    (vulpea-dblocks--field row-b field)))
               ;; Nil always sorts last, independent of direction.
               (cond
                ((and (null va) (null vb)))       ; equal, try next key
                ((null va) (throw 'done nil))     ; a nil → goes last → not before b
                ((null vb) (throw 'done t))       ; b nil → goes last → a before b
                (t
                 (let ((cmp (vulpea-dblocks--compare-values va vb)))
                   (unless (zerop cmp)
                     (throw 'done (if (eq dir 'desc) (> cmp 0) (< cmp 0))))))))))
         nil))
     rows)))

(defun vulpea-dblocks--flatten-rows (notes field)
  "Wrap NOTES as row structs, exploding FIELD when non-nil.
Without FIELD every note becomes one row with :value nil.
With FIELD each note is expanded into one row per value returned by
`vulpea-note-meta-get-list'; notes with no values emit one row with :value nil."
  (if (null field)
      (mapcar (lambda (n) (vulpea-dblocks-row-make :note n :value nil)) notes)
    (let (rows)
      (dolist (note notes)
        (let ((vals (vulpea-note-meta-get-list note field)))
          (if vals
              (dolist (v vals)
                (push (vulpea-dblocks-row-make :note note :value v) rows))
            (push (vulpea-dblocks-row-make :note note :value nil) rows))))
      (nreverse rows))))

(defun vulpea-dblocks--sort-notes (notes sort-col sort-dir)
  "Sort NOTES by SORT-COL in SORT-DIR (asc or desc).
Deprecated: used only by `org-dblock-write:roam-links'."
  (let* ((col      (if (stringp sort-col) sort-col
                     (and sort-col (symbol-name sort-col))))
         (is-meta  (and col (not (member col '("title" "tags" "todo"
                                               "scheduled" "deadline")))))
         (key-fn   (lambda (n)
                     (vulpea-dblocks--note-column-value
                      n (if is-meta col (intern (or col "title"))))))
         (cmp-fn   (if is-meta
                       (lambda (a b)
                         (let ((na (string-to-number a))
                               (nb (string-to-number b)))
                           (if (and (not (zerop na)) (not (zerop nb)))
                               (< na nb)
                             (string< a b))))
                     #'string<))
         (sorted   (seq-sort-by key-fn cmp-fn notes)))
    (if (eq sort-dir 'desc) (nreverse sorted) sorted)))

(defun vulpea-dblocks--apply-limit (notes limit)
  "Return at most LIMIT notes from NOTES, or all if LIMIT is nil."
  (if (and limit (> limit 0))
      (seq-take notes limit)
    notes))

;;; ============================================================
;;; Aggregate evaluation and group-level sort
;;; ============================================================

(defun vulpea-dblocks--format-value (val)
  "Format VAL as a table cell display string.
Numbers use %%g (drops trailing zeros); lists join with \", \"."
  (cond
   ((null val)    "")
   ((listp val)   (string-join (mapcar (lambda (x) (format "%s" x)) val) ", "))
   ((numberp val) (format "%g" val))
   (t             (format "%s" val))))

(defun vulpea-dblocks--eval-aggregate (agg-spec rows)
  "Evaluate AGG-SPEC over ROWS (list of `vulpea-dblocks-row'), returning a value.
Supported forms: (length), (count), (sum FIELD), (avg FIELD),
(min FIELD), (max FIELD), (first FIELD), (last FIELD)."
  (cl-flet ((num-vals (field)
              (delq nil
                    (mapcar (lambda (r)
                              (let ((v (vulpea-dblocks--field r field)))
                                (cond ((numberp v) v)
                                      ((stringp v)
                                       (let ((n (string-to-number v)))
                                         ;; Accept 0 only when string starts with digit.
                                         (when (or (/= n 0)
                                                   (string-match-p "^[0-9]" v))
                                           n))))))
                            rows))))
    (pcase agg-spec
      (`(length) (length rows))
      (`(count)  (length rows))
      (`(sum ,field)
       (apply #'+ (or (num-vals field) '(0))))
      (`(avg ,field)
       (let ((vals (num-vals field)))
         (when vals (/ (float (apply #'+ vals)) (length vals)))))
      (`(min ,field)
       (let ((vals (num-vals field))) (when vals (apply #'min vals))))
      (`(max ,field)
       (let ((vals (num-vals field))) (when vals (apply #'max vals))))
      (`(first ,field)
       (when rows (vulpea-dblocks--field (car rows) field)))
      (`(last ,field)
       (when rows (vulpea-dblocks--field (car (last rows)) field)))
      (_ (user-error "vulpea-dblocks: unknown aggregate: %S" agg-spec)))))

(defun vulpea-dblocks--eval-group-key (sexp rows)
  "Evaluate aggregate sort-key SEXP over ROWS without using `eval'.
Supports: (rows K) → list of field values; (avg SEXP), (sum SEXP),
(min SEXP), (max SEXP) → numeric aggregates over an inner SEXP;
(cnt) / (count) / (length) → group size."
  (pcase sexp
    (`(rows ,k)
     (delq nil (mapcar (lambda (r) (vulpea-dblocks--field r k)) rows)))
    (`(avg ,inner)
     (let* ((vals (vulpea-dblocks--eval-group-key inner rows))
            (nums (when (listp vals)
                    (delq nil (mapcar (lambda (v) (when (numberp v) v)) vals)))))
       (when nums (/ (float (apply #'+ nums)) (length nums)))))
    (`(sum ,inner)
     (let* ((vals (vulpea-dblocks--eval-group-key inner rows))
            (nums (when (listp vals)
                    (delq nil (mapcar (lambda (v) (when (numberp v) v)) vals)))))
       (when nums (apply #'+ nums))))
    (`(min ,inner)
     (let* ((vals (vulpea-dblocks--eval-group-key inner rows))
            (nums (when (listp vals)
                    (delq nil (mapcar (lambda (v) (when (numberp v) v)) vals)))))
       (when nums (apply #'min nums))))
    (`(max ,inner)
     (let* ((vals (vulpea-dblocks--eval-group-key inner rows))
            (nums (when (listp vals)
                    (delq nil (mapcar (lambda (v) (when (numberp v) v)) vals)))))
       (when nums (apply #'max nums))))
    ((or `(cnt) `(count) `(length))
     (length rows))
    (_ (user-error "vulpea-dblocks: unknown group key expression: %S" sexp))))

(defun vulpea-dblocks--sort-groups (groups sort-spec)
  "Sort GROUPS alist by (expr SEXP . DIR) entries in SORT-SPEC.
Field-based sort pairs are ignored at the group level."
  (let ((expr-specs (seq-filter (lambda (p) (eq (car-safe p) 'expr)) sort-spec)))
    (if (null expr-specs)
        groups
      (seq-sort
       (lambda (ga gb)
         (catch 'done
           (dolist (spec expr-specs)
             (let* ((sexp (cadr spec))
                    (dir  (cddr spec))
                    (va   (vulpea-dblocks--eval-group-key sexp (cdr ga)))
                    (vb   (vulpea-dblocks--eval-group-key sexp (cdr gb))))
               (cond
                ((and (null va) (null vb)))
                ((null va) (throw 'done nil))
                ((null vb) (throw 'done t))
                (t (let ((cmp (vulpea-dblocks--compare-values va vb)))
                     (unless (zerop cmp)
                       (throw 'done (if (eq dir 'desc) (> cmp 0) (< cmp 0)))))))))
           nil))
       groups))))

;;; ============================================================
;;; org-prepare-dblock advice: inject :auto-level
;;; ============================================================

(defun vulpea-dblocks--prepare-inject-level (orig-fn)
  "Around advice for `org-prepare-dblock'.
Injects :auto-level based on the enclosing heading before point moves."
  (let ((level (save-excursion
                 (condition-case nil
                     (progn (outline-back-to-heading t)
                            (1+ (org-current-level)))
                   (error 1)))))
    (let ((result (funcall orig-fn)))
      (plist-put result :auto-level level))))

;;; ============================================================
;;; roam-links
;;; ============================================================

(defun vulpea-dblocks--parse-existing-links (content level)
  "Parse existing roam-links dblock CONTENT at heading LEVEL.
Returns a list of (ID TITLE BODY) triples."
  (let ((prefix (make-string level ?*))
        (result '()))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^%s\\(?: CANCELLED\\)? \\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
                      (regexp-quote prefix))
              nil t)
        (let* ((id    (match-string 1))
               (title (match-string 2))
               (start (progn (forward-line 1) (point)))
               (end   (if (re-search-forward
                           (format "^%s " (regexp-quote prefix)) nil t)
                          (progn (goto-char (match-beginning 0)) (point))
                        (point-max)))
               (body  (string-trim (buffer-substring-no-properties start end))))
          (push (list id title body) result)
          (goto-char (if (< (point) end) (point) end)))))
    (nreverse result)))

(defun vulpea-dblocks--insert-links (notes level existing)
  "Insert heading links for NOTES at heading depth LEVEL.
EXISTING is a list of (ID TITLE BODY) preserving annotations.
Nodes no longer matching the query but with content become CANCELLED."
  (let* ((prefix   (make-string level ?*))
         (live-ids (mapcar #'vulpea-note-id notes))
         (sorted   (seq-sort-by #'vulpea-dblocks--note-title #'string< notes)))
    (dolist (note sorted)
      (let* ((id    (vulpea-note-id note))
             (title (vulpea-dblocks--note-title note))
             (entry (seq-find (lambda (e) (string= (car e) id)) existing))
             (body  (nth 2 entry)))
        (insert (format "%s [[id:%s][%s]]\n" prefix id title))
        (when (and body (not (string-empty-p body)))
          (insert body "\n"))))
    (dolist (entry existing)
      (let ((id    (nth 0 entry))
            (title (nth 1 entry))
            (body  (nth 2 entry)))
        (when (and (not (member id live-ids))
                   (not (string-empty-p body)))
          (insert (format "%s CANCELLED [[id:%s][%s]]\n" prefix id title))
          (insert body "\n"))))))

(defun org-dblock-write:roam-links (params)
  "Dynamic block: insert org-roam node links matching a query.

Query params (shared with roam-table):
  :from         source expression (see Commentary)
  :where        filter expression (see Commentary)
  :tags         list of tags, notes with ANY, e.g. (\"trail\")
  :tags-every   list of tags, notes with ALL
  :title-match  regex matched against note title
  :exclude-tags list of tags to exclude
  :meta-filter  (KEY OP VALUE) e.g. (\"rating\" >= 8)
  :limit        max number of results

Display params:
  :level        explicit heading depth (auto-detected from context if omitted)
  :meta-types   alist of (KEY . TYPE) for typed metadata reads

Annotations on existing entries are preserved across updates.
Entries removed from the query but with annotations become CANCELLED."
  (let* ((vulpea-dblocks--field-memo (make-hash-table :test #'equal))
         (vulpea-dblocks--current-meta-types (plist-get params :meta-types))
         (level    (or (plist-get params :level)
                       (plist-get params :auto-level)
                       1))
         (content  (or (plist-get params :content) ""))
         (existing (vulpea-dblocks--parse-existing-links content level))
         (notes    (vulpea-dblocks--query params))
         (notes    (vulpea-dblocks--sort-notes notes 'title 'asc))
         (notes    (vulpea-dblocks--apply-limit notes (plist-get params :limit))))
    (vulpea-dblocks--insert-links notes level existing)))

;;; ============================================================
;;; roam-table
;;; ============================================================

(defun vulpea-dblocks--parse-column-spec (col)
  "Normalise a column spec COL to (HEADER . KEY).
Symbols → (\"Symbol\" . symbol).
Strings → (string . string).
(HEADER . KEY) cons where HEADER is a string → passed through.
(AGG-EXPR . HEADER) or (group . HEADER) → swapped to (HEADER . AGG-EXPR)."
  (cond
   ;; Aggregate or group marker: car is a list or the symbol `group'.
   ;; Convention (AGG-EXPR . HEADER) → normalise to (HEADER . AGG-EXPR).
   ((and (consp col)
         (or (listp (car col)) (eq (car col) 'group)))
    (cons (cdr col) (car col)))
   ((consp col)   col)
   ((symbolp col) (cons (capitalize (symbol-name col)) col))
   ((stringp col) (cons col col))
   (t (error "Invalid column spec: %S" col))))

(defun vulpea-dblocks--aggregate-spec-p (key)
  "Return non-nil when KEY is an aggregate column spec (a list such as (avg FIELD))."
  (listp key))

(defun vulpea-dblocks--group-rows (rows group-col)
  "Group ROWS by GROUP-COL, returning a sorted alist of (GROUP-VALUE . ROWS)."
  (let ((groups '()))
    (dolist (row rows)
      (let* ((val (vulpea-dblocks--field-as-string row group-col))
             (val (if (string-empty-p val) "—" val))
             (existing (assoc val groups)))
        (if existing
            (setcdr existing (append (cdr existing) (list row)))
          (push (cons val (list row)) groups))))
    (sort groups (lambda (a b) (string< (car a) (car b))))))

(defun vulpea-dblocks--insert-table-rows (rows specs row-fn)
  "Insert table rows for ROWS using SPECS and ROW-FN."
  (dolist (row rows)
    (insert "| " (string-join (funcall row-fn row specs) " | ") " |\n")))

(defun vulpea-dblocks--table-row-fn (row specs)
  "Return list of cell values for ROW given column SPECS.
ROW may be a `vulpea-dblocks-row' struct or a bare `vulpea-note'."
  (let ((note (if (vulpea-dblocks-row-p row)
                  (vulpea-dblocks-row-note row)
                row)))
    (mapcar
     (lambda (spec)
       (let ((key (cdr spec)))
         (if (eq key 'title)
             (format "[[id:%s][%s]]"
                     (vulpea-note-id note)
                     (vulpea-dblocks--note-title note))
           (vulpea-dblocks--field-as-string row key))))
     specs)))

(defun vulpea-dblocks--insert-flat-table (rows columns)
  "Insert a flat org table for ROWS with COLUMNS.
Rows must be pre-sorted and pre-limited before this call."
  (let* ((specs   (mapcar #'vulpea-dblocks--parse-column-spec columns))
         (headers (mapcar #'car specs)))
    (insert "| " (string-join headers " | ") " |\n")
    (insert "|-\n")
    (vulpea-dblocks--insert-table-rows rows specs #'vulpea-dblocks--table-row-fn)
    (org-table-align)))

(defun vulpea-dblocks--insert-grouped-table (groups columns limit)
  "Insert an org table for GROUPS (sorted alist of (GROUP-VALUE . ROWS)).
When any column spec is an aggregate, renders one summary row per group.
Otherwise renders a group header followed by data rows (v0.2 style,
with correct column count)."
  (let* ((specs   (mapcar #'vulpea-dblocks--parse-column-spec columns))
         (headers (mapcar #'car specs))
         (has-agg (seq-some (lambda (s) (vulpea-dblocks--aggregate-spec-p (cdr s)))
                            specs)))
    (if has-agg
        ;; Aggregate summary: one row per group, all columns filled.
        (progn
          (insert "| " (string-join headers " | ") " |\n")
          (insert "|-\n")
          (dolist (group groups)
            (let* ((gval  (car group))
                   (grows (cdr group))
                   (cells (mapcar
                           (lambda (spec)
                             (let ((key (cdr spec)))
                               (cond
                                ((eq key 'group)
                                 gval)
                                ((vulpea-dblocks--aggregate-spec-p key)
                                 (vulpea-dblocks--format-value
                                  (vulpea-dblocks--eval-aggregate key grows)))
                                (t ""))))
                           specs)))
              (insert "| " (string-join cells " | ") " |\n"))))
      ;; Non-aggregate: group header row + per-note data rows.
      (dolist (group groups)
        (let* ((gval   (car group))
               (grows  (vulpea-dblocks--apply-limit (cdr group) limit))
               (blanks (make-list (1- (length specs)) ""))
               (hcells (cons (format "*%s*" gval) blanks)))
          (insert "| " (string-join hcells " | ") " |\n")
          (insert "| " (string-join headers " | ") " |\n")
          (insert "|-\n")
          (vulpea-dblocks--insert-table-rows grows specs #'vulpea-dblocks--table-row-fn)
          (insert "|-\n"))))
    (org-table-align)))

(defun org-dblock-write:roam-table (params)
  "Dynamic block: insert an org table of roam nodes matching a query.

Query params (shared with roam-links):
  :from         source expression (see Commentary)
  :where        filter expression (see Commentary)
  :tags         list of tags, notes with ANY
  :tags-every   list of tags, notes with ALL
  :title-match  regex matched against note title
  :exclude-tags list of tags to exclude
  :meta-filter  (KEY OP VALUE) e.g. (\"rating\" >= 8)
  :limit        max results (per group if :group-by is set)

Display params:
  :columns      list of column specs, default (title)
                symbols: title tags todo scheduled deadline backlinks
                strings: vulpea metadata keys, e.g. \"rating\" \"author\"
                conses:  (\"Header\" . key) for custom header names
  :sort         column to sort by, e.g. \"rating\" or title
  :sort-dir     asc (default) or desc
  :group-by     metadata key or symbol to group results into sections
  :flatten      metadata key to explode into one row per value
  :meta-types   alist of (KEY . TYPE) for typed metadata reads
                e.g. ((\"rating\" . number) (\"author\" . note))

Examples:
  #+BEGIN: roam-table :tags (\"book\") :columns (title \"rating\" \"author\") :sort \"rating\" :sort-dir desc
  #+END:

  #+BEGIN: roam-table :from (tag (\"book\")) :meta-filter (\"status\" = \"to-read\") :columns (title \"author\" \"status\")
  #+END:

  #+BEGIN: roam-table :tags (\"book\") :columns (title \"rating\") :group-by \"genre\" :sort \"rating\" :sort-dir desc
  #+END:

  #+BEGIN: roam-table :tags (\"book\") :columns (title backlinks) :sort backlinks :sort-dir desc :limit 5
  #+END:"
  (let* ((vulpea-dblocks--field-memo (make-hash-table :test #'equal))
         (vulpea-dblocks--current-meta-types (plist-get params :meta-types))
         (columns     (or (plist-get params :columns) '(title)))
         (sort-param  (plist-get params :sort))
         (sort-dir    (or (plist-get params :sort-dir) 'asc))
         (flatten-col (plist-get params :flatten))
         (vulpea-dblocks--flatten-field flatten-col)
         (group-by    (plist-get params :group-by))
         (limit       (plist-get params :limit))
         ;; Normalise :sort + :sort-dir: legacy single-column form becomes a cons.
         (sort-spec   (when sort-param
                        (vulpea-dblocks--normalize-sort-spec
                         (if (or (symbolp sort-param) (stringp sort-param))
                             (cons sort-param sort-dir)
                           sort-param))))
         ;; Pipeline: query → flatten → sort rows → [group → sort groups] → render
         (notes (vulpea-dblocks--query params))
         (rows  (vulpea-dblocks--flatten-rows notes flatten-col))
         (rows  (vulpea-dblocks--sort-rows rows sort-spec)))
    (if group-by
        (vulpea-dblocks--insert-grouped-table
         (vulpea-dblocks--sort-groups
          (vulpea-dblocks--group-rows rows group-by)
          sort-spec)
         columns limit)
      (vulpea-dblocks--insert-flat-table
       (vulpea-dblocks--apply-limit rows limit)
       columns))))

;;; ============================================================
;;; Auto-update
;;; ============================================================

(defun vulpea-dblocks--point-in-dblock-p ()
  "Return non-nil if point is currently inside a dynamic block."
  (save-excursion
    (let ((pos (point)))
      (and (re-search-backward org-dblock-start-re nil t)
           (save-excursion
             (re-search-forward org-dblock-end-re nil t)
             (> (point) pos))))))

(defun vulpea-dblocks-maybe-autoupdate ()
  "Update all dblocks if file has #+ROAM_DBLOCKS_AUTOUPDATE: t.
Skips update if point is inside a dblock (to preserve edits in progress)."
  (when (and (string= "t" (cadr (car (org-collect-keywords
                                      '("ROAM_DBLOCKS_AUTOUPDATE")))))
             (not (vulpea-dblocks--point-in-dblock-p)))
    (save-excursion
      (org-update-all-dblocks))))

;;;###autoload
(define-minor-mode vulpea-dblocks-mode
  "Minor mode enabling vulpea-powered dynamic blocks.
Activates auto-update hooks and the org-prepare-dblock advice."
  :global t
  :lighter " vdb"
  (if vulpea-dblocks-mode
      (progn
        (advice-add 'org-prepare-dblock :around
                    #'vulpea-dblocks--prepare-inject-level)
        (add-hook 'org-mode-hook #'vulpea-dblocks--setup-buffer-hooks))
    (advice-remove 'org-prepare-dblock
                   #'vulpea-dblocks--prepare-inject-level)
    (remove-hook 'org-mode-hook #'vulpea-dblocks--setup-buffer-hooks)))

(defun vulpea-dblocks--setup-buffer-hooks ()
  "Set up buffer-local hooks for auto-update."
  (add-hook 'before-save-hook #'vulpea-dblocks-maybe-autoupdate nil t))

;;;###autoload
(defun vulpea-dblocks-setup ()
  "Enable vulpea-dblocks-mode (convenience alias)."
  (vulpea-dblocks-mode +1))

(provide 'vulpea-dblocks)
;;; vulpea-dblocks.el ends here
