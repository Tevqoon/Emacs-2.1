;;; zotero-mass-import.el --- Bulk org-roam note creation and metadata pull from Zotero -*- lexical-binding: t; -*-

;; Author: Jure Smolar
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org-roam "2.0") (vulpea "0.3"))
;; Keywords: org, org-roam, zotero, bib

;;; Commentary:

;; Companion to zotero-backend.el (org-roam-annotation-import).  Where the
;; backend syncs *annotations*, this package handles the library-level
;; plumbing, strictly one-way (Zotero -> org):
;;
;;   `zotero-mass-import-items'     create a bare citar-style literature
;;                                  note for every top-level Zotero item
;;                                  whose ref is not yet in the org-roam DB.
;;
;;   `zotero-mass-import-metadata'  stamp vulpea metadata (collection paths,
;;                                  item type, author, year, venue, dateAdded,
;;                                  DOI, URL) onto every note matched by ref.
;;                                  Zotero is unconditionally authoritative
;;                                  for the managed keys: differing values
;;                                  are overwritten, Zotero-empty fields are
;;                                  removed.  Unmanaged keys (status, order,
;;                                  rating, ...) are never touched.
;;
;;   `zotero-mass-import'           both, in that order.
;;
;; There is deliberately NO write-back to Zotero.  Collection membership and
;; bibliographic facts are edited in Zotero; reading state lives in org.
;; Consequently, do not treat managed columns as writable in :editable
;; roam-tables -- a pushed edit to e.g. "collection" survives only until the
;; next `zotero-mass-import-metadata' run.
;;
;; Collections render as full slash-joined paths ("Parent/Child") via the
;; parentCollection chain, multi-valued when an item is filed in several
;; collections.  Useful dblock views:
;;
;;   #+BEGIN: roam-table :from (tag "zotero") :flatten "collection"
;;            :group-by "collection"
;;            :columns ((group . "Collection") title "type" "status")
;;   #+END:
;;
;;   #+BEGIN: roam-table :from (tag "zotero")
;;            :where (not (field "collection"))
;;            :columns (title "type" "added") :sort "added" :sort-dir desc
;;   #+END:

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org-roam)
(require 'vulpea)
(require 'zotero-backend)

(defgroup zotero-mass-import nil
  "Bulk import of Zotero items and metadata into org-roam."
  :group 'annotation
  :prefix "zotero-mass-import-")

(defcustom zotero-mass-import-metadata-keys
  '("collection" "type" "author" "year" "venue" "added" "doi" "url")
  "Vulpea metadata keys managed by `zotero-mass-import-metadata'.
Remove a key here to stop managing it; existing values of removed keys
are then left alone on subsequent syncs."
  :group 'zotero-mass-import
  :type '(repeat string))

;;;; ----------------------------------------------------------------
;;;; Collections: key -> full path
;;;; ----------------------------------------------------------------

(defun zotero-mass-import--collection-paths ()
  "Return a hash table mapping collection key to its full slash-joined path.
Nested collections render as \"Parent/Child\" via the parentCollection
chain.  Orphaned parents (key referenced but not returned) terminate the
path at the deepest resolvable ancestor."
  (let ((raw   (make-hash-table :test 'equal))
        (paths (make-hash-table :test 'equal)))
    (dolist (c (zotero--get-all "/collections"))
      (let* ((d      (alist-get 'data c))
             (key    (alist-get 'key d))
             (name   (alist-get 'name d))
             (parent (alist-get 'parentCollection d)))
        (puthash key (cons name (and (stringp parent) parent)) raw)))
    (cl-labels ((path (k)
                  (or (gethash k paths)
                      (pcase-let ((`(,name . ,parent) (gethash k raw)))
                        (puthash k
                                 (if (and parent (gethash parent raw))
                                     (concat (path parent) "/" name)
                                   (or name k))
                                 paths)))))
      (maphash (lambda (k _) (path k)) raw))
    paths))

;;;; ----------------------------------------------------------------
;;;; Per-item metadata assembly
;;;; ----------------------------------------------------------------

(defun zotero-mass-import--creator-names (creators)
  "Return a list of \"Last, First\" strings for author-type CREATORS.
Order is preserved (first author first)."
  (let (names)
    (cl-loop for c across (or creators [])
             when (member (alist-get 'creatorType c) '("author" "bookAuthor"))
             do (let ((last  (alist-get 'lastName c))
                      (first (alist-get 'firstName c))
                      (name  (alist-get 'name c)))
                  (cond
                   ((and last (not (string-empty-p last)))
                    (push (if (and first (not (string-empty-p first)))
                              (format "%s, %s" last first)
                            last)
                          names))
                   ((and name (not (string-empty-p name)))
                    (push name names)))))
    (nreverse names)))

(defun zotero-mass-import--nonempty (s)
  "Return S as a one-element list if it is a non-empty string, else nil."
  (when (and (stringp s) (not (string-empty-p s)))
    (list s)))

(defun zotero-mass-import--item-metadata (data cpaths)
  "Build an alist of (META-KEY . VALUE-LIST) for top item DATA.
CPATHS is the table from `zotero-mass-import--collection-paths'.  Only
keys in `zotero-mass-import-metadata-keys' are included; a key mapped to
nil means \"remove this property from the note\".

Collections are sorted so Zotero's arbitrary membership order cannot
cause spurious rewrites; authors keep their order."
  (let* ((all
          `(("collection"
             . ,(sort (delq nil (mapcar (lambda (k) (gethash k cpaths))
                                        (append (alist-get 'collections data)
                                                nil)))
                      #'string<))
            ("type"   . ,(zotero-mass-import--nonempty
                          (alist-get 'itemType data)))
            ("author" . ,(zotero-mass-import--creator-names
                          (alist-get 'creators data)))
            ("year"   . ,(zotero-mass-import--nonempty (zotero--year data)))
            ("venue"
             . ,(zotero-mass-import--nonempty
                 (or (alist-get 'publicationTitle data)
                     (alist-get 'proceedingsTitle data)
                     (alist-get 'bookTitle data)
                     (alist-get 'university data)
                     (alist-get 'repository data))))
            ("added"
             . ,(when-let ((d (alist-get 'dateAdded data)))
                  (list (substring d 0 10))))
            ("doi" . ,(zotero-mass-import--nonempty (alist-get 'DOI data)))
            ("url" . ,(zotero-mass-import--nonempty (alist-get 'url data))))))
    (seq-filter (lambda (kv) (member (car kv) zotero-mass-import-metadata-keys))
                all)))

;;;; ----------------------------------------------------------------
;;;; Diff + stamp
;;;; ----------------------------------------------------------------

(defun zotero-mass-import--note-meta-differs-p (note metadata)
  "Non-nil when NOTE's current vulpea meta differs from METADATA.
Runs against the vulpea DB only, so unchanged notes never get their
buffers opened."
  (seq-some (lambda (kv)
              (not (equal (vulpea-note-meta-get-list note (car kv))
                          (cdr kv))))
            metadata))

(defun zotero-mass-import--stamp-metadata (note metadata)
  "Write METADATA onto NOTE's vulpea meta and re-parse the file.
Nil-valued keys are removed -- Zotero is the source of truth for the
managed keys.  Unmanaged keys are untouched."
  (vulpea-utils-with-note note
    (pcase-dolist (`(,key . ,values) metadata)
      (if values
          (vulpea-buffer-meta-set key values)
        (vulpea-buffer-meta-remove key)))
    (save-buffer))
  (vulpea-db-update-file (vulpea-note-path note)))

;;;; ----------------------------------------------------------------
;;;; Ref bookkeeping
;;;; ----------------------------------------------------------------

(defun zotero-mass-import--known-refs ()
  "Hash set of DB refs normalised to the form item import compares against:
cite refs as @key, others as TYPE:REF (scheme rejoined)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (row (org-roam-db-query [:select [ref type] :from refs]))
      (let ((ref  (car row))
            (type (cadr row)))
        (when (stringp ref)
          (puthash
           (cond ((equal type "cite") (concat "@" ref))
                 (type                (concat type ":" ref))
                 (t                   ref))
           t table))))
    table))

(defun zotero-mass-import--top-items ()
  "Return all top-level Zotero items that are actual bibliographic entries."
  (seq-remove
   (lambda (i)
     (member (alist-get 'itemType (alist-get 'data i))
             '("attachment" "note" "annotation")))
   (zotero--get-all "/items/top")))

(defun zotero-mass-import--item-ref (data citekeys)
  "Return (REF . CITEKEY-OR-NIL) for top item DATA given CITEKEYS table.
REF is @citekey when Better BibTeX knows one, else the zotero://select URL."
  (let* ((key     (alist-get 'key data))
         (citekey (gethash key citekeys))
         (have    (and citekey (not (string-empty-p citekey)))))
    (cons (if have (concat "@" citekey) (zotero--select-link key))
          (and have citekey))))

;;;; ----------------------------------------------------------------
;;;; Public commands
;;;; ----------------------------------------------------------------

;;;###autoload
(defun zotero-mass-import-items ()
  "Create a bare citar literature note for every top-level Zotero item.
Items whose ref (@citekey, else zotero://select) already exists in the
org-roam DB are skipped without opening their files."
  (interactive)
  (message "Zotero: fetching top-level items...")
  (let* ((items    (zotero-mass-import--top-items))
         (top-keys (delq nil (mapcar (lambda (i)
                                       (alist-get 'key (alist-get 'data i)))
                                     items)))
         (citekeys (zotero--citekeys-for top-keys))
         (known    (zotero-mass-import--known-refs))
         (n 0))
    (dolist (item items)
      (let* ((data    (alist-get 'data item))
             (title   (alist-get 'title data))
             (author  (zotero--creators-string (alist-get 'creators data)))
             (year    (zotero--year data))
             (refinfo (zotero-mass-import--item-ref data citekeys))
             (ref     (car refinfo))
             (citekey (cdr refinfo)))
        ;; Skip if this ref is already in the DB -- no buffer opened.
        (unless (gethash ref known)
          (let* ((entry (list :title (zotero--note-title author year title)
                              :citekey citekey
                              :ref ref
                              :select-url (zotero--select-link
                                           (alist-get 'key data))))
                 (annotation-capture-templates
                  (zotero--literature-capture-templates entry))
                 (node (zotero--find-or-create-node entry)))
            (save-window-excursion
              (with-current-buffer
                  (annotation--org-roam-node-open-or-create node)
                (goto-char (point-min))
                (when ref
                  (let ((existing (org-entry-get (point) "ROAM_REFS" t)))
                    (unless (and existing
                                 (string-match-p (regexp-quote ref) existing))
                      (org-roam-ref-add ref))))
                (org-roam-tag-add '("zotero" "literature"))
                (when-let ((slug (annotation--slugify author)))
                  (org-roam-tag-add (list slug)))
                (save-buffer)
                (cl-incf n)))))))
    (message "Zotero item import done: %d new note(s)" n)
    n))

;;;###autoload
(defun zotero-mass-import-metadata ()
  "Pull item metadata from Zotero into vulpea meta on each literature note.
Strictly one-way: differing values of managed keys are overwritten and
fields empty in Zotero are removed, so org-side edits to managed keys do
not survive a sync.  Unmanaged keys (status, order, rating, ...) are
never touched.  Notes are matched by ref (@citekey, falling back to the
zotero://select URL); Zotero items with no org note are skipped -- run
`zotero-mass-import-items' first to create those."
  (interactive)
  (message "Zotero: fetching collections and items...")
  (let* ((cpaths   (zotero-mass-import--collection-paths))
         (items    (zotero-mass-import--top-items))
         (top-keys (delq nil (mapcar (lambda (i)
                                       (alist-get 'key (alist-get 'data i)))
                                     items)))
         (citekeys (zotero--citekeys-for top-keys))
         (matched 0)
         (touched 0))
    (dolist (item items)
      (let* ((data (alist-get 'data item))
             (ref  (car (zotero-mass-import--item-ref data citekeys)))
             (node (org-roam-node-from-ref ref))
             (note (and node (vulpea-db-get-by-id (org-roam-node-id node)))))
        (when note
          (cl-incf matched)
          (let ((md (zotero-mass-import--item-metadata data cpaths)))
            (when (zotero-mass-import--note-meta-differs-p note md)
              (zotero-mass-import--stamp-metadata note md)
              (cl-incf touched))))))
    (message "Zotero metadata sync: %d/%d item(s) matched a note, %d updated"
             matched (length items) touched)))

;;;###autoload
(defun zotero-mass-import ()
  "Create notes for new Zotero items, then pull metadata onto all notes."
  (interactive)
  (zotero-mass-import-items)
  (zotero-mass-import-metadata))

(provide 'zotero-mass-import)
;;; zotero-mass-import.el ends here;;; zotero-mass-import.el --- Bulk org-roam note creation and metadata pull from Zotero -*- lexical-binding: t; -*-

;; Author: Jure Smolar
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org-roam "2.0") (vulpea "0.3"))
;; Keywords: org, org-roam, zotero, bib

;;; Commentary:

;; Companion to zotero-backend.el (org-roam-annotation-import).  Where the
;; backend syncs *annotations*, this package handles the library-level
;; plumbing, strictly one-way (Zotero -> org):
;;
;;   `zotero-mass-import-items'     create a bare citar-style literature
;;                                  note for every top-level Zotero item
;;                                  whose ref is not yet in the org-roam DB.
;;
;;   `zotero-mass-import-metadata'  stamp vulpea metadata (collection paths,
;;                                  item type, author, year, venue, dateAdded,
;;                                  DOI, URL) onto every note matched by ref.
;;                                  Zotero is unconditionally authoritative
;;                                  for the managed keys: differing values
;;                                  are overwritten, Zotero-empty fields are
;;                                  removed.  Unmanaged keys (status, order,
;;                                  rating, ...) are never touched.
;;
;;   `zotero-mass-import'           both, in that order.
;;
;; There is deliberately NO write-back to Zotero.  Collection membership and
;; bibliographic facts are edited in Zotero; reading state lives in org.
;; Consequently, do not treat managed columns as writable in :editable
;; roam-tables -- a pushed edit to e.g. "collection" survives only until the
;; next `zotero-mass-import-metadata' run.
;;
;; Collections render as full slash-joined paths ("Parent/Child") via the
;; parentCollection chain, multi-valued when an item is filed in several
;; collections.  Useful dblock views:
;;
;;   #+BEGIN: roam-table :from (tag "zotero") :flatten "collection"
;;            :group-by "collection"
;;            :columns ((group . "Collection") title "type" "status")
;;   #+END:
;;
;;   #+BEGIN: roam-table :from (tag "zotero")
;;            :where (not (field "collection"))
;;            :columns (title "type" "added") :sort "added" :sort-dir desc
;;   #+END:

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org-roam)
(require 'vulpea)
(require 'zotero-backend)

(defgroup zotero-mass-import nil
  "Bulk import of Zotero items and metadata into org-roam."
  :group 'annotation
  :prefix "zotero-mass-import-")

(defcustom zotero-mass-import-metadata-keys
  '("collection" "type" "author" "year" "venue" "added" "doi" "url")
  "Vulpea metadata keys managed by `zotero-mass-import-metadata'.
Remove a key here to stop managing it; existing values of removed keys
are then left alone on subsequent syncs."
  :group 'zotero-mass-import
  :type '(repeat string))

;;;; ----------------------------------------------------------------
;;;; Collections: key -> full path
;;;; ----------------------------------------------------------------

(defun zotero-mass-import--collection-paths ()
  "Return a hash table mapping collection key to its full slash-joined path.
Nested collections render as \"Parent/Child\" via the parentCollection
chain.  Orphaned parents (key referenced but not returned) terminate the
path at the deepest resolvable ancestor."
  (let ((raw   (make-hash-table :test 'equal))
        (paths (make-hash-table :test 'equal)))
    (dolist (c (zotero--get-all "/collections"))
      (let* ((d      (alist-get 'data c))
             (key    (alist-get 'key d))
             (name   (alist-get 'name d))
             (parent (alist-get 'parentCollection d)))
        (puthash key (cons name (and (stringp parent) parent)) raw)))
    (cl-labels ((path (k)
                  (or (gethash k paths)
                      (pcase-let ((`(,name . ,parent) (gethash k raw)))
                        (puthash k
                                 (if (and parent (gethash parent raw))
                                     (concat (path parent) "/" name)
                                   (or name k))
                                 paths)))))
      (maphash (lambda (k _) (path k)) raw))
    paths))

;;;; ----------------------------------------------------------------
;;;; Per-item metadata assembly
;;;; ----------------------------------------------------------------

(defun zotero-mass-import--creator-names (creators)
  "Return a list of \"Last, First\" strings for author-type CREATORS.
Order is preserved (first author first)."
  (let (names)
    (cl-loop for c across (or creators [])
             when (member (alist-get 'creatorType c) '("author" "bookAuthor"))
             do (let ((last  (alist-get 'lastName c))
                      (first (alist-get 'firstName c))
                      (name  (alist-get 'name c)))
                  (cond
                   ((and last (not (string-empty-p last)))
                    (push (if (and first (not (string-empty-p first)))
                              (format "%s, %s" last first)
                            last)
                          names))
                   ((and name (not (string-empty-p name)))
                    (push name names)))))
    (nreverse names)))

(defun zotero-mass-import--nonempty (s)
  "Return S as a one-element list if it is a non-empty string, else nil."
  (when (and (stringp s) (not (string-empty-p s)))
    (list s)))

(defun zotero-mass-import--item-metadata (data cpaths)
  "Build an alist of (META-KEY . VALUE-LIST) for top item DATA.
CPATHS is the table from `zotero-mass-import--collection-paths'.  Only
keys in `zotero-mass-import-metadata-keys' are included; a key mapped to
nil means \"remove this property from the note\".

Collections are sorted so Zotero's arbitrary membership order cannot
cause spurious rewrites; authors keep their order."
  (let* ((all
          `(("collection"
             . ,(sort (delq nil (mapcar (lambda (k) (gethash k cpaths))
                                        (append (alist-get 'collections data)
                                                nil)))
                      #'string<))
            ("type"   . ,(zotero-mass-import--nonempty
                          (alist-get 'itemType data)))
            ("author" . ,(zotero-mass-import--creator-names
                          (alist-get 'creators data)))
            ("year"   . ,(zotero-mass-import--nonempty (zotero--year data)))
            ("venue"
             . ,(zotero-mass-import--nonempty
                 (or (alist-get 'publicationTitle data)
                     (alist-get 'proceedingsTitle data)
                     (alist-get 'bookTitle data)
                     (alist-get 'university data)
                     (alist-get 'repository data))))
            ("added"
             . ,(when-let ((d (alist-get 'dateAdded data)))
                  (list (substring d 0 10))))
            ("doi" . ,(zotero-mass-import--nonempty (alist-get 'DOI data)))
            ("url" . ,(zotero-mass-import--nonempty (alist-get 'url data))))))
    (seq-filter (lambda (kv) (member (car kv) zotero-mass-import-metadata-keys))
                all)))

;;;; ----------------------------------------------------------------
;;;; Diff + stamp
;;;; ----------------------------------------------------------------

(defun zotero-mass-import--note-meta-differs-p (note metadata)
  "Non-nil when NOTE's current vulpea meta differs from METADATA.
Runs against the vulpea DB only, so unchanged notes never get their
buffers opened."
  (seq-some (lambda (kv)
              (not (equal (vulpea-note-meta-get-list note (car kv))
                          (cdr kv))))
            metadata))

(defun zotero-mass-import--stamp-metadata (note metadata)
  "Write METADATA onto NOTE's vulpea meta and re-parse the file.
Nil-valued keys are removed -- Zotero is the source of truth for the
managed keys.  Unmanaged keys are untouched."
  (vulpea-utils-with-note note
    (pcase-dolist (`(,key . ,values) metadata)
      (if values
          (vulpea-buffer-meta-set key values)
        (vulpea-buffer-meta-remove key)))
    (save-buffer))
  (vulpea-db-update-file (vulpea-note-path note)))

;;;; ----------------------------------------------------------------
;;;; Ref bookkeeping
;;;; ----------------------------------------------------------------

(defun zotero-mass-import--known-refs ()
  "Hash set of DB refs normalised to the form item import compares against:
cite refs as @key, others as TYPE:REF (scheme rejoined)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (row (org-roam-db-query [:select [ref type] :from refs]))
      (let ((ref  (car row))
            (type (cadr row)))
        (when (stringp ref)
          (puthash
           (cond ((equal type "cite") (concat "@" ref))
                 (type                (concat type ":" ref))
                 (t                   ref))
           t table))))
    table))

(defun zotero-mass-import--top-items ()
  "Return all top-level Zotero items that are actual bibliographic entries."
  (seq-remove
   (lambda (i)
     (member (alist-get 'itemType (alist-get 'data i))
             '("attachment" "note" "annotation")))
   (zotero--get-all "/items/top")))

(defun zotero-mass-import--item-ref (data citekeys)
  "Return (REF . CITEKEY-OR-NIL) for top item DATA given CITEKEYS table.
REF is @citekey when Better BibTeX knows one, else the zotero://select URL."
  (let* ((key     (alist-get 'key data))
         (citekey (gethash key citekeys))
         (have    (and citekey (not (string-empty-p citekey)))))
    (cons (if have (concat "@" citekey) (zotero--select-link key))
          (and have citekey))))

;;;; ----------------------------------------------------------------
;;;; Public commands
;;;; ----------------------------------------------------------------

;;;###autoload
(defun zotero-mass-import-items ()
  "Create a bare citar literature note for every top-level Zotero item.
Items whose ref (@citekey, else zotero://select) already exists in the
org-roam DB are skipped without opening their files."
  (interactive)
  (message "Zotero: fetching top-level items...")
  (let* ((items    (zotero-mass-import--top-items))
         (top-keys (delq nil (mapcar (lambda (i)
                                       (alist-get 'key (alist-get 'data i)))
                                     items)))
         (citekeys (zotero--citekeys-for top-keys))
         (known    (zotero-mass-import--known-refs))
         (n 0))
    (dolist (item items)
      (let* ((data    (alist-get 'data item))
             (title   (alist-get 'title data))
             (author  (zotero--creators-string (alist-get 'creators data)))
             (year    (zotero--year data))
             (refinfo (zotero-mass-import--item-ref data citekeys))
             (ref     (car refinfo))
             (citekey (cdr refinfo)))
        ;; Skip if this ref is already in the DB -- no buffer opened.
        (unless (gethash ref known)
          (let* ((entry (list :title (zotero--note-title author year title)
                              :citekey citekey
                              :ref ref
                              :select-url (zotero--select-link
                                           (alist-get 'key data))))
                 (annotation-capture-templates
                  (zotero--literature-capture-templates entry))
                 (node (zotero--find-or-create-node entry)))
            (save-window-excursion
              (with-current-buffer
                  (annotation--org-roam-node-open-or-create node)
                (goto-char (point-min))
                (when ref
                  (let ((existing (org-entry-get (point) "ROAM_REFS" t)))
                    (unless (and existing
                                 (string-match-p (regexp-quote ref) existing))
                      (org-roam-ref-add ref))))
                (org-roam-tag-add '("zotero" "literature"))
                (when-let ((slug (annotation--slugify author)))
                  (org-roam-tag-add (list slug)))
                (save-buffer)
                (cl-incf n)))))))
    (message "Zotero item import done: %d new note(s)" n)
    n))

;;;###autoload
(defun zotero-mass-import-metadata ()
  "Pull item metadata from Zotero into vulpea meta on each literature note.
Strictly one-way: differing values of managed keys are overwritten and
fields empty in Zotero are removed, so org-side edits to managed keys do
not survive a sync.  Unmanaged keys (status, order, rating, ...) are
never touched.  Notes are matched by ref (@citekey, falling back to the
zotero://select URL); Zotero items with no org note are skipped -- run
`zotero-mass-import-items' first to create those."
  (interactive)
  (message "Zotero: fetching collections and items...")
  (let* ((cpaths   (zotero-mass-import--collection-paths))
         (items    (zotero-mass-import--top-items))
         (top-keys (delq nil (mapcar (lambda (i)
                                       (alist-get 'key (alist-get 'data i)))
                                     items)))
         (citekeys (zotero--citekeys-for top-keys))
         (matched 0)
         (touched 0))
    (dolist (item items)
      (let* ((data (alist-get 'data item))
             (ref  (car (zotero-mass-import--item-ref data citekeys)))
             (node (org-roam-node-from-ref ref))
             (note (and node (vulpea-db-get-by-id (org-roam-node-id node)))))
        (when note
          (cl-incf matched)
          (let ((md (zotero-mass-import--item-metadata data cpaths)))
            (when (zotero-mass-import--note-meta-differs-p note md)
              (zotero-mass-import--stamp-metadata note md)
              (cl-incf touched))))))
    (message "Zotero metadata sync: %d/%d item(s) matched a note, %d updated"
             matched (length items) touched)))

;;;###autoload
(defun zotero-mass-import ()
  "Create notes for new Zotero items, then pull metadata onto all notes."
  (interactive)
  (zotero-mass-import-items)
  (zotero-mass-import-metadata))

(provide 'zotero-mass-import)
;;; zotero-mass-import.el ends here
