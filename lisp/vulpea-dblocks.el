;;; vulpea-dblocks.el --- Org dynamic blocks powered by vulpea -*- lexical-binding: t -*-

;; Author: Jure Smolar
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (vulpea "0.3") (org "9.6"))
;; Keywords: org, roam, vulpea, dynamic-blocks
;; URL: https://github.com/yourname/vulpea-dblocks

;;; Commentary:

;; Provides two org dynamic block types backed by vulpea's DB:
;;
;;   roam-links  — inserts heading links to matching roam nodes
;;   roam-table  — inserts an org table of matching nodes with metadata columns
;;
;; Both support the same query params:
;;   :tags         list of tags, notes with ANY
;;   :tags-every   list of tags, notes with ALL
;;   :title-match  regex on title
;;   :exclude-tags list of tags to exclude
;;   :meta-filter  (KEY OP VALUE) e.g. ("rating" >= 8)
;;   :limit        max number of results (applied after sort)
;;
;; roam-links additional params:
;;   :level        explicit heading depth (auto-detected from context otherwise)
;;
;; roam-table additional params:
;;   :columns      list of column specs (see below)
;;   :sort         column name to sort by
;;   :sort-dir     asc (default) or desc
;;   :group-by     metadata key or symbol to group results by
;;
;; Column specs for roam-table:
;;   title         note title (always a link)
;;   tags          note tags as comma-separated string
;;   todo          todo state
;;   scheduled     scheduled timestamp
;;   deadline      deadline timestamp
;;   backlinks     number of notes linking to this note
;;   "key"         a string treated as a vulpea metadata key
;;
;; Auto-update:
;;   Add #+ROAM_DBLOCKS_AUTOUPDATE: t to a file to refresh all blocks on open/save.
;;   Blocks are skipped if point is inside them (to avoid clobbering edits).

;;; Code:

(require 'org)
(require 'vulpea)

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

(defun vulpea-dblocks--note-backlinks (note)
  "Return the number of notes linking to NOTE."
  (length (vulpea-db-query-links-to (vulpea-note-id note))))

(defun vulpea-dblocks--note-column-value (note col)
  "Return the value of column COL for NOTE as a string.
COL is a symbol (title, tags, todo, scheduled, deadline, backlinks)
or a string treated as a vulpea metadata key."
  (pcase col
    ('title     (vulpea-dblocks--note-title note))
    ('tags      (string-join (vulpea-note-tags note) ", "))
    ('todo      (or (vulpea-note-todo note) ""))
    ('scheduled (or (vulpea-note-scheduled note) ""))
    ('deadline  (or (vulpea-note-deadline note) ""))
    ('backlinks (number-to-string (vulpea-dblocks--note-backlinks note)))
    ((pred stringp)
     (or (vulpea-note-meta-get note col) ""))))

;;; ============================================================
;;; Shared query logic
;;; ============================================================

(defun vulpea-dblocks--query (params)
  "Run a vulpea DB query according to PARAMS and return a list of notes.
Applies :tags, :tags-every, :title-match, :exclude-tags, :meta-filter."
  (let* ((tags         (plist-get params :tags))
         (tags-every   (plist-get params :tags-every))
         (title-match  (plist-get params :title-match))
         (exclude-tags (plist-get params :exclude-tags))
         (meta-filter  (plist-get params :meta-filter))
         (notes
          (cond
           (tags-every (vulpea-db-query-by-tags-every
                        (if (listp tags-every) tags-every (list tags-every))))
           (tags       (vulpea-db-query-by-tags-some
                        (if (listp tags) tags (list tags))))
           (t          (vulpea-db-query))))
         (notes
          (if (and tags tags-every)
              (let ((tags-list (if (listp tags) tags (list tags))))
                (seq-filter
                 (lambda (n)
                   (seq-every-p (lambda (tag) (member tag (vulpea-note-tags n)))
                                tags-list))
                 notes))
            notes))
         (notes
          (if title-match
              (seq-filter (lambda (n)
                            (string-match-p title-match
                                            (or (vulpea-note-title n) "")))
                          notes)
            notes))
         (notes
          (if exclude-tags
              (let ((ex (if (listp exclude-tags) exclude-tags (list exclude-tags))))
                (seq-filter
                 (lambda (n)
                   (not (seq-some (lambda (tag) (member tag (vulpea-note-tags n))) ex)))
                 notes))
            notes))
         (notes
          (if meta-filter
              (let* ((key   (nth 0 meta-filter))
                     (op    (nth 1 meta-filter))
                     (val   (nth 2 meta-filter))
                     (op-fn (pcase op
                              ('=  #'equal)
                              ('<  (lambda (a b) (< (string-to-number a) b)))
                              ('>  (lambda (a b) (> (string-to-number a) b)))
                              ('<= (lambda (a b) (<= (string-to-number a) b)))
                              ('>= (lambda (a b) (>= (string-to-number a) b)))
                              (_   #'equal))))
                (seq-filter
                 (lambda (n)
                   (when-let ((v (vulpea-note-meta-get n key)))
                     (funcall op-fn v val)))
                 notes))
            notes)))
    notes))

;;; ============================================================
;;; Sorting and limiting
;;; ============================================================

(defun vulpea-dblocks--sort-notes (notes sort-col sort-dir)
  "Sort NOTES by SORT-COL in SORT-DIR (asc or desc)."
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
  :tags         list of tags, notes with ANY, e.g. (\"trail\")
  :tags-every   list of tags, notes with ALL
  :title-match  regex matched against note title
  :exclude-tags list of tags to exclude
  :meta-filter  (KEY OP VALUE) e.g. (\"rating\" >= 8)
  :limit        max number of results

Display params:
  :level        explicit heading depth (auto-detected from context if omitted)

Annotations on existing entries are preserved across updates.
Entries removed from the query but with annotations become CANCELLED."
  (let* ((level    (or (plist-get params :level)
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
Symbols → capitalised name as header.
Strings → used as both header and metadata key.
Conses  → passed through as-is."
  (cond
   ((consp col)   col)
   ((symbolp col) (cons (capitalize (symbol-name col)) col))
   ((stringp col) (cons col col))
   (t (error "Invalid column spec: %S" col))))

(defun vulpea-dblocks--group-notes (notes group-col)
  "Group NOTES by GROUP-COL, returning an alist of (GROUP-VALUE . NOTES)."
  (let ((groups '()))
    (dolist (note notes)
      (let* ((val (vulpea-dblocks--note-column-value note group-col))
             (val (if (string-empty-p val) "—" val))
             (existing (assoc val groups)))
        (if existing
            (setcdr existing (append (cdr existing) (list note)))
          (push (cons val (list note)) groups))))
    (sort groups (lambda (a b) (string< (car a) (car b))))))

(defun vulpea-dblocks--insert-table-rows (notes specs row-fn)
  "Insert table rows for NOTES using SPECS and ROW-FN."
  (dolist (note notes)
    (insert "| " (string-join (funcall row-fn note specs) " | ") " |\n")))

(defun vulpea-dblocks--table-row-fn (note specs)
  "Return list of cell values for NOTE given column SPECS."
  (mapcar
   (lambda (spec)
     (let ((key (cdr spec)))
       (if (eq key 'title)
           (format "[[id:%s][%s]]"
                   (vulpea-note-id note)
                   (vulpea-dblocks--note-title note))
         (vulpea-dblocks--note-column-value note key))))
   specs))

(defun vulpea-dblocks--insert-table (notes columns sort-col sort-dir group-col limit)
  "Insert an org table for NOTES with COLUMNS.
SORT-COL and SORT-DIR control ordering; GROUP-COL groups into sections;
LIMIT caps results per group (or total if no grouping)."
  (let* ((specs   (mapcar #'vulpea-dblocks--parse-column-spec columns))
         (headers (mapcar #'car specs))
         (sorted  (vulpea-dblocks--sort-notes notes sort-col sort-dir))
         (row-fn  #'vulpea-dblocks--table-row-fn))
    (if group-col
        ;; Grouped: one table section per group value
        (let ((groups (vulpea-dblocks--group-notes sorted group-col)))
          (dolist (group groups)
            (let* ((group-val   (car group))
                   (group-notes (vulpea-dblocks--apply-limit (cdr group) limit)))
              (insert (format "| *%s* |\n" group-val))
              (insert "| " (string-join headers " | ") " |\n")
              (insert "|-\n")
              (vulpea-dblocks--insert-table-rows group-notes specs row-fn)
              (insert "|-\n"))))
      ;; Flat table
      (let ((limited (vulpea-dblocks--apply-limit sorted limit)))
        (insert "| " (string-join headers " | ") " |\n")
        (insert "|-\n")
        (vulpea-dblocks--insert-table-rows limited specs row-fn)))
    (org-table-align)))

(defun org-dblock-write:roam-table (params)
  "Dynamic block: insert an org table of roam nodes matching a query.

Query params (shared with roam-links):
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

Examples:
  #+BEGIN: roam-table :tags (\"book\") :columns (title \"rating\" \"author\") :sort \"rating\" :sort-dir desc
  #+END:

  #+BEGIN: roam-table :tags (\"book\") :columns (title \"rating\") :group-by \"genre\" :sort \"rating\" :sort-dir desc
  #+END:

  #+BEGIN: roam-table :tags (\"book\") :columns (title backlinks) :sort backlinks :sort-dir desc :limit 5
  #+END:"
  (let* ((columns  (or (plist-get params :columns) '(title)))
         (sort-col (plist-get params :sort))
         (sort-dir (or (plist-get params :sort-dir) 'asc))
         (group-by (plist-get params :group-by))
         (limit    (plist-get params :limit))
         (notes    (vulpea-dblocks--query params)))
    (vulpea-dblocks--insert-table notes columns sort-col sort-dir group-by limit)))

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
