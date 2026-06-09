;;; vulpea-dblocks.el --- Org dynamic blocks powered by vulpea -*- lexical-binding: t -*-

;; Author: Jure Smolar
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1") (vulpea "0.3") (org "9.6"))
;; Keywords: org, roam, vulpea, dynamic-blocks

;;; Commentary:

;; Provides two org dynamic block types backed by vulpea's DB:
;;
;;   roam-links  — inserts heading links to matching roam nodes
;;   roam-table  — inserts an org table of matching nodes with metadata columns
;;
;; v0.6.0 decouples table direction.  Previously an `:editable' table wrote
;; its cell edits back to the underlying notes on every `org-update-dblock'
;; (C-c C-c), which meant a routine refresh could silently clobber changes
;; made to the note files out of band — especially for fields that were
;; empty in the table.  Now `org-update-dblock' on a roam-table ALWAYS pulls
;; (regenerates from the DB and never writes), and a separate command,
;; `vulpea-dblocks-push', writes the current table's edits back to the notes
;; before regenerating.  `:editable' no longer means "write back on update";
;; it means "this table may be pushed, and is protected from save-time
;; regeneration."  See the "Editable tables" section below.
;;
;; v0.4.0 was the consolidated baseline.  It keeps the v0.3.0 architecture
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
;; The DSL body is ordinary elisp: `and', `or', `not', `member', `let', etc.
;; all nest arbitrarily.  DSL evaluation errors exclude the note silently;
;; use the explicit (lambda (note) ...) form to let errors surface instead.
;;
;; LEGACY QUERY PARAMS (still supported, rewritten to :from/:where)
;; ---------------------------------------------------------------
;;   :tags         list of tags, notes with ANY
;;   :tags-every   list of tags, notes with ALL
;;   :exclude-tags list of tags to exclude
;;   :title-match  regex on title
;;   :meta-filter  (KEY OP VALUE), OP in (= < > <= >= in)
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
;;   (tags :exclude (a b))     tags column hiding tags a, b
;;   ("Header" tags :exclude (a b)) same, with a custom header
;;
;; Editable tables (push-to-notes):
;;   `:editable t' marks a table as pushable and protects it from save-time
;;   regeneration.  It does NOT cause writes on a normal update.
;;
;;     C-c C-c (`org-update-dblock')   pulls only: regenerates from the DB,
;;                                     never touches the note files.
;;     M-x vulpea-dblocks-push          writes the current table's tags and
;;                                     metadata cell edits back into the notes
;;                                     (via vulpea-buffer-*), re-parses the
;;                                     touched files, then regenerates.
;;
;;   This split exists so a routine refresh can never overwrite changes made
;;   to the underlying notes out of band — push is always deliberate.  Push
;;   is still last-write-wins at cell granularity: editing a cell here and
;;   editing the same field in the file means push wins; it just cannot
;;   happen by accident on a refresh.
;;
;;   Push requires a `title' column (its id: link anchors each row to a
;;   note).  Only tags and metadata columns are writable; title/todo/
;;   scheduled/deadline/backlinks/computed/aggregate cells are read-only and
;;   ignored.  Push is disabled when :group-by or :flatten is set (rows are
;;   not 1:1 with notes there); on such a table, and on a non-:editable
;;   table, `vulpea-dblocks-push' is a harmless pull.
;;
;;   A tags column with :exclude hides the listed tags; they cannot be removed
;;   by editing (re-added on push if the note already had them) and are never
;;   injected onto a note that lacked them.  Multi-valued metadata cells are
;;   joined and split on `vulpea-dblocks-meta-separator' (default " ;; "),
;;   not a comma, so values containing commas (plain strings or link
;;   descriptions) round-trip safely.
;;
;; Auto-update:
;;   Add #+ROAM_DBLOCKS_AUTOUPDATE: t to a file to refresh all blocks on save.
;;   Blocks are skipped if point is inside them (to avoid clobbering edits).
;;   Editable roam-tables are re-inserted verbatim on save rather than
;;   regenerated, so a save never pulls canonical DB state over edits you
;;   have not yet pushed.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'vulpea)

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup vulpea-dblocks nil
  "Org dynamic blocks powered by vulpea."
  :group 'org
  :prefix "vulpea-dblocks-")

(defcustom vulpea-dblocks-meta-separator " ;; "
  "Separator between values of a multi-valued metadata cell in roam-table.

Used only for metadata columns: a property with several values
\(e.g. multiple `author' lines) renders its values joined by this string,
and an editable cell is split back on it.  A comma is deliberately NOT
used so that values which themselves contain commas — plain strings like
\"a, b\" or link descriptions like [[id:..][Word, Another word]] — survive
the round-trip intact.

The leading/trailing spaces are cosmetic; splitting trims surrounding
whitespace, so \" ;; \" and \";;\" parse identically.  Tags columns are
unaffected (tags cannot contain commas and stay comma-joined)."
  :type 'string
  :group 'vulpea-dblocks)

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

(defvar vulpea-dblocks--in-autoupdate nil
  "Non-nil while `vulpea-dblocks-maybe-autoupdate' is running.
Editable tables (`:editable t') are re-inserted verbatim in this context
\(neither pushed nor regenerated), so a buffer save never clobbers
in-progress edits.  Only an explicit `org-update-dblock'/`org-update-all-
dblocks' (with this flag nil) regenerates such a block.")

(defvar vulpea-dblocks--pushing nil
  "Non-nil while `vulpea-dblocks-push' is propagating table edits to notes.
A plain `org-update-dblock' leaves this nil and therefore only pulls
\(regenerates from the DB), so out-of-band changes to the underlying note
files are never overwritten.  Only `vulpea-dblocks-push' binds this to t,
running write-back before the table is regenerated.")

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
                    (let* ((type (cdr (assoc key vulpea-dblocks--current-meta-types)))
                           (vals (if type
                                     (vulpea-note-meta-get-list note key type)
                                   (vulpea-note-meta-get-list note key))))
                      ;; Preserve scalar semantics for 0/1 values (so typed
                      ;; reads, sorting, and :where comparisons still see a
                      ;; number/string, not a one-element list); return the
                      ;; full list only when a property genuinely has 2+
                      ;; values (e.g. multiple authors); the metadata render
                      ;; path joins these on `vulpea-dblocks-meta-separator'
                      ;; and write-back round-trips them correctly.
                      (pcase vals
                        ('()        nil)
                        (`(,single) single)
                        (_          vals)))))))
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
  in           membership: VALUE is a list of candidates; matches when ANY
               of the note's values for KEY (multi-valued aware) equals,
               as a string, one of the candidates.  Candidates may be
               symbols, strings, or numbers, e.g.
               (\"status\" in (to-read antilibrary)).
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
      ((or 'in 'member)
       (let* ((cands (if (listp val) val (list val)))
              (cand-strings
               (mapcar (lambda (c)
                         (cond ((stringp c) c)
                               ((symbolp c) (symbol-name c))
                               ((numberp c) (number-to-string c))
                               (t (format "%s" c))))
                       cands)))
         `(lambda (n)
            (let ((vs (vulpea-note-meta-get-list n ,key)))
              (seq-some (lambda (v) (member v ',cand-strings)) vs)))))
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

(defun vulpea-dblocks--format-value (val &optional sep)
  "Format VAL as a table cell display string.
Numbers use %%g (drops trailing zeros); lists join with SEP (default \", \")."
  (let ((sep (or sep ", ")))
    (cond
     ((null val)    "")
     ((listp val)   (string-join (mapcar (lambda (x) (format "%s" x)) val) sep))
     ((numberp val) (format "%g" val))
     (t             (format "%s" val)))))

(defun vulpea-dblocks--format-meta-value (val)
  "Format a metadata VAL for a table cell.
Multi-valued metadata joins on `vulpea-dblocks-meta-separator' rather than
a comma, so values containing commas round-trip safely.  Scalars are
formatted exactly as `vulpea-dblocks--format-value'."
  (vulpea-dblocks--format-value val vulpea-dblocks-meta-separator))

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
(AGG-EXPR . HEADER) or (group . HEADER) → swapped to (HEADER . AGG-EXPR).
Tags with options:
  (tags :exclude (a b))          → (\"Tags\" . (tags :exclude (a b)))
  (\"Header\" tags :exclude (a b)) → (\"Header\" . (tags :exclude (a b)))"
  (cond
   ;; Tags column carrying options, no custom header: (tags :exclude (...))
   ((and (consp col) (eq (car col) 'tags))
    (cons "Tags" col))
   ;; Tags column with a custom header: ("Header" tags :exclude (...))
   ((and (consp col) (stringp (car col)) (eq (cadr col) 'tags))
    (cons (car col) (cons 'tags (cddr col))))
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
  "Return non-nil when KEY is an aggregate column spec (a list such as (avg FIELD)).
A tags-with-options key (tags :exclude (...)) is NOT an aggregate."
  (and (listp key)
       (not (eq (car-safe key) 'tags))))

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
         (cond
          ((eq key 'title)
           (format "[[id:%s][%s]]"
                   (vulpea-note-id note)
                   (vulpea-dblocks--note-title note)))
          ;; Tags column (bare `tags' or (tags :exclude (...))): render the
          ;; visible tags, dropping any excluded ones.
          ((vulpea-dblocks--spec-tags-p spec)
           (let* ((excl (vulpea-dblocks--spec-tags-excludes spec))
                  (tags (seq-difference (vulpea-note-tags note) excl)))
             (string-join tags ", ")))
          (t
           ;; Metadata (and computed scalar) column: multi-valued metadata
           ;; joins on the meta separator so comma-bearing values round-trip;
           ;; scalars format identically to `--field-as-string'.
           (vulpea-dblocks--format-meta-value
            (vulpea-dblocks--field row key))))))
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
  :meta-filter  (KEY OP VALUE) e.g. (\"rating\" >= 8) or (\"status\" in (a b))
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
  :editable     when t, marks the table as pushable and excludes it from
                save-triggered auto-rebuild.  A plain `org-update-dblock'
                (C-c C-c) on the table still only PULLS (regenerates from the
                DB, never writes to notes); to write tags/metadata cell edits
                back to the notes, run `vulpea-dblocks-push'.  Requires a
                `title' column to push.  Push is disabled with :group-by or
                :flatten.
  :meta-types   alist of (KEY . TYPE) for typed metadata reads
                e.g. ((\"rating\" . number) (\"author\" . note))

Examples:
  #+BEGIN: roam-table :tags (\"book\") :columns (title \"rating\" \"author\") :sort \"rating\" :sort-dir desc
  #+END:

  #+BEGIN: roam-table :from (tag (\"book\")) :meta-filter (\"status\" in (to-read antilibrary)) :columns (title \"author\" \"status\")
  #+END:

  #+BEGIN: roam-table :editable t :from (tag \"book\") :columns (title \"status\" \"rating\" (\"Tags\" tags :exclude (book)))
  #+END:

  #+BEGIN: roam-table :tags (\"book\") :columns (title \"rating\") :group-by \"genre\" :sort \"rating\" :sort-dir desc
  #+END:

  #+BEGIN: roam-table :tags (\"book\") :columns (title backlinks) :sort backlinks :sort-dir desc :limit 5
  #+END:"
  ;; Direction is decoupled (v0.6.0):
  ;;   * A plain `org-update-dblock' (--pushing nil) PULLS only: it
  ;;     regenerates from the DB and never writes to the note files, so a
  ;;     routine refresh can never clobber out-of-band edits to the notes.
  ;;   * `vulpea-dblocks-push' (--pushing t) writes cell edits back first,
  ;;     then regenerates from the now-updated DB.
  ;;   * An autoupdate sweep (--in-autoupdate t) over an editable table
  ;;     re-inserts the captured :content verbatim (neither pushes nor
  ;;     regenerates), so a save never pulls canonical state over unpushed
  ;;     edits.  `org-prepare-dblock' has already emptied the block, so we
  ;;     must re-insert rather than do nothing (which would empty it).
  (if (and vulpea-dblocks--in-autoupdate (plist-get params :editable))
      (let ((content (plist-get params :content)))
        (when (and content (not (string-empty-p content)))
          (insert (string-trim-right content "\n"))))
    (let* ((vulpea-dblocks--field-memo (make-hash-table :test #'equal))
           (vulpea-dblocks--current-meta-types (plist-get params :meta-types))
           ;; Write-back only when explicitly pushing (vulpea-dblocks-push).
           ;; A plain `org-update-dblock' pulls from the DB and never writes
           ;; back, so out-of-band edits to the note files are never clobbered.
           ;; Runs before the query so the re-render sees the updated DB.
           (_writeback (when vulpea-dblocks--pushing
                         (vulpea-dblocks--writeback params)))
           (columns     (or (plist-get params :columns) '(title)))
           (sort-param  (plist-get params :sort))
           (sort-dir    (or (plist-get params :sort-dir) 'asc))
           (flatten-col (plist-get params :flatten))
           (vulpea-dblocks--flatten-field flatten-col)
           (group-by    (plist-get params :group-by))
           (limit       (plist-get params :limit))
           ;; Normalise :sort + :sort-dir: legacy single-column form → cons.
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
         columns)))))

;;; ============================================================
;;; Write-back (vulpea-dblocks-push pushes table edits to notes)
;;; ============================================================
;;
;; Write-back is invoked ONLY from `vulpea-dblocks-push' (which binds
;; `vulpea-dblocks--pushing' to t).  A plain `org-update-dblock' never calls
;; it, so a routine refresh only pulls and can never overwrite changes made
;; to the note files out of band.
;;
;; Push edits the rendered tags / metadata cells back into the underlying
;; notes BEFORE the table is regenerated from the (now-updated) DB.
;;
;; Constraints:
;;   * Requires a `title' column (its id: link is the row->note key).
;;   * Only `tags' and metadata (string/cons) columns are writable.
;;     title/todo/scheduled/deadline/backlinks/computed/aggregate are read-only.
;;   * Disabled for grouped / flattened / aggregated tables (rows are not 1:1
;;     with notes there); push on such a table is a harmless pull.
;;   * Idempotent: a cell is written only when it differs from a freshly
;;     rendered cell of the note's current DB value.
;;   * Sync is vulpea-native via `vulpea-db-update-file' (no org-roam).
;;   * Still last-write-wins at cell granularity — push wins over a concurrent
;;     file edit to the same field — but this can no longer happen by accident
;;     on a refresh, only on a deliberate push.

;;; --- column-spec classification -----------------------------

(defun vulpea-dblocks--spec-title-p (spec)
  "Return non-nil if column SPEC (HEADER . KEY) is the title column."
  (eq (cdr spec) 'title))

(defun vulpea-dblocks--spec-tags-p (spec)
  "Return non-nil if SPEC is a tags column.
A tags column key is the symbol `tags', or a list whose car is `tags'
\(the (tags :exclude (...)) form)."
  (let ((key (cdr spec)))
    (or (eq key 'tags)
        (and (consp key) (eq (car key) 'tags)))))

(defun vulpea-dblocks--spec-tags-excludes (spec)
  "Return the list of excluded tag strings for a tags SPEC, or nil.
Reads the :exclude plist entry from a (tags :exclude (a b)) key."
  (let ((key (cdr spec)))
    (when (and (consp key) (eq (car key) 'tags))
      (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x))
              (plist-get (cdr key) :exclude)))))

(defun vulpea-dblocks--spec-meta-key (spec)
  "Return the metadata key string for SPEC if it is a writable meta column.
Returns nil for title/tags/builtin/computed/aggregate columns."
  (let ((key (cdr spec)))
    (cond
     ;; Any list key (aggregate, tags-with-options) is not a meta column.
     ((consp key) nil)
     ;; Builtin read-only symbols.
     ((memq key '(title tags todo scheduled deadline priority level id
                  backlinks aliases))
      nil)
     ;; Bare symbol that is not a builtin → treat as meta key.
     ((symbolp key) (symbol-name key))
     ;; String key → meta key, unless it is a builtin or file. form.
     ((stringp key)
      (if (or (string-prefix-p "file." key)
              (member key '("title" "tags" "todo" "scheduled" "deadline"
                            "priority" "level" "id" "backlinks" "aliases")))
          nil
        key))
     (t nil))))

;;; --- parsing the rendered table -----------------------------

(defconst vulpea-dblocks--id-link-re
  "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]*\\)\\]\\]"
  "Match an org id: link, capturing the id and the description.")

(defun vulpea-dblocks--split-table-row (line)
  "Split an org table LINE into a list of trimmed cell strings.
LINE must start and end with `|'.  The leading/trailing empty cells
produced by the surrounding pipes are dropped."
  (let* ((inner (string-trim line "^\\s-*|" "|\\s-*$"))
         (cells (split-string inner "|")))
    (mapcar #'string-trim cells)))

(defun vulpea-dblocks--cell-id (cell)
  "Return the id embedded in a title CELL via [[id:...][...]], or nil."
  (when (string-match vulpea-dblocks--id-link-re cell)
    (match-string 1 cell)))

(defun vulpea-dblocks--parse-table-rows (content specs)
  "Parse rendered table CONTENT into rows aligned to SPECS.
Returns a list of (ID . CELLS) where CELLS is a list of cell strings in
column order.  Only data rows with a resolvable id in the title column are
returned; header, separator and group rows are skipped."
  (let* ((title-idx (cl-position-if #'vulpea-dblocks--spec-title-p specs))
         (ncols     (length specs))
         rows)
    (when title-idx
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (and (string-match-p "^\\s-*|" line)
                       (not (string-match-p "^\\s-*|[-+]" line))) ; skip |---
              (let ((cells (vulpea-dblocks--split-table-row line)))
                (when (= (length cells) ncols)
                  (let ((id (vulpea-dblocks--cell-id (nth title-idx cells))))
                    (when id
                      (push (cons id cells) rows)))))))
          (forward-line 1))))
    (nreverse rows)))

;;; --- value normalisation for idempotent diffing -------------

(defun vulpea-dblocks--parse-meta-cell (cell)
  "Parse a rendered meta CELL string into a list of value strings.
Splits on `vulpea-dblocks-meta-separator' (matched ignoring surrounding
whitespace), trims, and drops empty fragments.  An empty cell yields nil
\(meaning: remove the property).  A comma is not a separator, so values
containing commas are preserved."
  (let* ((sep   (regexp-quote (string-trim vulpea-dblocks-meta-separator)))
         (parts (mapcar #'string-trim (split-string (or cell "") sep t))))
    (seq-remove #'string-empty-p parts)))

(defun vulpea-dblocks--meta-cell-changed-p (note key cell)
  "Return non-nil if meta CELL differs from NOTE's current KEY value.
Compares the parsed cell list against the freshly rendered current value
so that identical content (after round-trip) is treated as unchanged.
Both sides use `vulpea-dblocks-meta-separator', keeping the comparison
symmetric with how the cell was rendered."
  (let* ((current   (vulpea-note-meta-get-list note key))
         (rendered  (vulpea-dblocks--format-meta-value current))
         (cell-norm (string-join (vulpea-dblocks--parse-meta-cell cell)
                                 vulpea-dblocks-meta-separator)))
    (not (string= rendered cell-norm))))

(defun vulpea-dblocks--parse-tags-cell (cell)
  "Parse a rendered tags CELL into a list of tag strings (comma-separated)."
  (mapcar #'string-trim (split-string (or cell "") "," t "\\s-*")))

;;; --- per-field write-back -----------------------------------

(defun vulpea-dblocks--writeback-meta (note key cell)
  "Write meta CELL back to NOTE under KEY if changed.  Return non-nil if written.
An empty cell removes the property; otherwise the comma-split list is set.
For heading-level notes the write is scoped to the heading subtree (BOUND
\\='heading); without this the meta API defaults to whole-buffer scope and
appends the value at the end of the file instead of inside the heading."
  (when (vulpea-dblocks--meta-cell-changed-p note key cell)
    (let ((values (vulpea-dblocks--parse-meta-cell cell))
          (bound  (when (> (vulpea-note-level note) 0) 'heading)))
      (vulpea-utils-with-note note
        (if values
            (vulpea-buffer-meta-set key values nil bound)
          (vulpea-buffer-meta-remove key bound))
        (save-buffer)))
    t))

(defun vulpea-dblocks--writeback-tags (note cell excludes)
  "Write tags CELL back to NOTE, preserving EXCLUDES.  Return non-nil if written.
The new tag set is the visible (edited) tags from CELL, unioned with any
EXCLUDES the note already had.  Excluded tags are never shown in CELL and
are never added to a note that lacked them."
  (let* ((current   (vulpea-note-tags note))
         (kept-excl (seq-intersection current excludes))
         ;; Defensively drop any excluded tag a user typed into the visible
         ;; cell, so it cannot be used to inject onto a note that lacked it.
         (visible   (seq-difference (vulpea-dblocks--parse-tags-cell cell)
                                    excludes))
         (new-tags  (seq-uniq (append visible kept-excl)))
         ;; Set comparison, order-independent.
         (changed   (not (and (null (seq-difference current new-tags))
                              (null (seq-difference new-tags current))))))
    (when changed
      (vulpea-utils-with-note note
        (apply #'vulpea-buffer-tags-set new-tags)
        (save-buffer))
      t)))

;;; --- orchestration ------------------------------------------

(defun vulpea-dblocks--table-editable-p (params)
  "Return non-nil if the roam-table PARAMS opt into push (write-back).
Push is only allowed on flat tables (no :group-by, no :flatten)."
  (and (plist-get params :editable)
       (not (plist-get params :group-by))
       (not (plist-get params :flatten))))

(defun vulpea-dblocks--writeback (params)
  "Propagate edits in the current roam-table dblock back to notes.
Reads :content from PARAMS, matches rows to notes by title-cell id, and
writes changed tags / meta cells.  Returns the number of notes touched.
No-op (returns 0) unless `vulpea-dblocks--table-editable-p'.

Called only from `vulpea-dblocks-push' (via `vulpea-dblocks--pushing'); a
plain `org-update-dblock' never reaches this function.

After writing, the touched files are re-parsed into the vulpea DB with
`vulpea-db-update-file' so the table re-render in the same command reflects
the new state."
  (if (not (vulpea-dblocks--table-editable-p params))
      0
    (let* ((columns (or (plist-get params :columns) '(title)))
           (specs   (mapcar #'vulpea-dblocks--parse-column-spec columns))
           (content (or (plist-get params :content) ""))
           (rows    (vulpea-dblocks--parse-table-rows content specs))
           (paths   (make-hash-table :test #'equal)))
      (unless (cl-position-if #'vulpea-dblocks--spec-title-p specs)
        (user-error
         "vulpea-dblocks: push needs a `title' column to anchor rows"))
      (dolist (row rows)
        (let* ((id    (car row))
               (cells (cdr row))
               (note  (vulpea-db-get-by-id id)))
          (when note
            (cl-loop
             for spec in specs
             for cell in cells
             do (cond
                 ((vulpea-dblocks--spec-tags-p spec)
                  (when (vulpea-dblocks--writeback-tags
                         note cell (vulpea-dblocks--spec-tags-excludes spec))
                    (puthash (vulpea-note-path note) t paths)))
                 ((vulpea-dblocks--spec-meta-key spec)
                  (when (vulpea-dblocks--writeback-meta
                         note (vulpea-dblocks--spec-meta-key spec) cell)
                    (puthash (vulpea-note-path note) t paths))))))))
      ;; Re-parse touched files so the immediate re-render sees the edits.
      (maphash (lambda (path _) (vulpea-db-update-file path)) paths)
      (let ((n (hash-table-count paths)))
        (when (> n 0)
          (message "vulpea-dblocks: pushed edits to %d note%s"
                   n (if (= n 1) "" "s")))
        n))))

;;;###autoload
(defun vulpea-dblocks-push ()
  "Push edits in the roam-table dynamic block at point back to the notes.

A plain `org-update-dblock' (C-c C-c) only pulls: it regenerates the table
from the vulpea DB and never writes to the note files.  Run this command
when you have edited tags / metadata cells in an `:editable' table and want
those edits committed to the underlying notes.  After writing, the table is
regenerated so it reflects the canonical (merged) DB state.

Works from anywhere inside the block — point need not be on the `#+BEGIN'
line.  (`org-update-dblock' delegates to `org-prepare-dblock', which only
acts when point is `looking-at' the block start and otherwise errors with
\"Not at a dynamic block\"; interactive C-c C-c hides this by seeking the
start first via `org-ctrl-c-ctrl-c', but a direct call does not, so we seek
to the start ourselves.)

Errors if point is not in a dynamic block.  A non-editable, grouped, or
flattened table is pulled only (no edits are written)."
  (interactive)
  (let ((vulpea-dblocks--pushing t))
    (save-excursion
      (org-beginning-of-dblock)   ; seek to #+BEGIN from anywhere inside; errors if not in a block
      (org-update-dblock))))

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
Skips update if point is inside a dblock (to preserve edits in progress).
Editable roam-tables are re-inserted verbatim here rather than regenerated
\(see `vulpea-dblocks--in-autoupdate'), so a save never pulls canonical DB
state over edits that have not yet been pushed with `vulpea-dblocks-push'."
  (when (and (string= "t" (cadr (car (org-collect-keywords
                                      '("ROAM_DBLOCKS_AUTOUPDATE")))))
             (not (vulpea-dblocks--point-in-dblock-p)))
    (let ((vulpea-dblocks--in-autoupdate t))
      (save-excursion
        (org-update-all-dblocks)))))

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
