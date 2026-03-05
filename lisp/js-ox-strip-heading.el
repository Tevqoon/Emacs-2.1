;;; js-ox-strip-heading.el --- AST-level conditional headline removal -*- lexical-binding: t; -*-
;;
;; Strips matched headlines from the org parse tree before export, promoting
;; their contents up to the parent level.  Works for both LaTeX and HTML since
;; it operates on the AST, not the transcoded strings.
;;
;; A headline's AST structure is:
;;
;;   (headline (:level N ...) SECTION CHILD-HEADLINE ...)
;;       where SECTION = (section nil PARAGRAPH SPECIAL-BLOCK ...)
;;
;; Stripping means: replace the headline node in the parent with
;;   1. The contents of its SECTION (body paragraphs, blocks, etc.)
;;   2. Any child headlines that were nested inside it
;;
;; The section wrapper itself is dropped; its children are promoted.
;; This way "* ABC" naturally becomes the structural parent for the
;; orphaned bodies in the final document.
;;
;; Usage in init.el (after ox is available):
;;
;;   (require 'js-ox-strip-heading
;;            (expand-file-name "lisp/js-ox-strip-heading.el" user-emacs-directory))
;;
;; Registration happens automatically on load via `org-export-filter-parse-tree-functions'.

;;; --- Predicate list ---

(defcustom js/ox-strip-heading-predicates
  (list #'js/ox-headline-anki-note-p
        #'js/ox-headline-noheading-tag-p)
  "List of predicate functions controlling which headlines are stripped.

Each function receives one argument: the org-element headline node.
If ANY predicate returns non-nil, the headline's heading line is removed
and its contents are spliced into the parent in its place.

Built-in predicates:
  `js/ox-headline-anki-note-p'      — heading has ANKI_NOTE_TYPE property
  `js/ox-headline-noheading-tag-p'  — heading carries the :noheading: tag"
  :type '(repeat function)
  :group 'js-ox)

;;; --- Built-in predicates ---

(defun js/ox-headline-anki-note-p (headline)
  "Return non-nil if HEADLINE is an anki-editor note entry.
Checks for ANKI_NOTE_TYPE on the node directly (not inherited), so
parent grouping headings are not auto-stripped."
  (org-element-property :ANKI_NOTE_TYPE headline))

(defun js/ox-headline-noheading-tag-p (headline)
  "Return non-nil if HEADLINE carries the :noheading: org tag."
  (member "noheading" (org-element-property :tags headline)))

;;; --- Predicate dispatcher ---

(defun js/ox-strip-headline-p (headline)
  "Return non-nil if HEADLINE should be stripped per `js/ox-strip-heading-predicates'."
  (cl-some (lambda (pred) (funcall pred headline))
           js/ox-strip-heading-predicates))

;;; --- AST splice ---

(defun js/ox-splice-headline (headline)
  "Replace HEADLINE in the AST with its contents.

The headline's section body and any child headlines are inserted before
HEADLINE in the parent, then HEADLINE itself is extracted.  The section
wrapper node is dissolved; its children are promoted directly."
  (let* ((contents (org-element-contents headline))
         ;; A headline's first child is the section (body), if any.
         ;; Remaining children are nested headlines.
         (section  (when (eq 'section (org-element-type (car contents)))
                     (car contents)))
         ;; Nodes to splice in: section's children + sibling headlines.
         (to-insert (append (when section (org-element-contents section))
                            (if section (cdr contents) contents))))
    (dolist (node to-insert)
      (org-element-insert-before node headline))
    (org-element-extract headline)))

;;; --- Parse-tree filter ---

(defun js/ox-filter-strip-headings (tree _backend _info)
  "Parse-tree filter: strip headlines matching `js/ox-strip-heading-predicates'.

Walks the entire tree and splices out matched headlines, promoting their
body content and child headlines to the parent level.

Processes deepest nodes first (post-order) so that nested matched
headlines are handled before their parents."
  ;; Collect all matching headlines first (org-element-map traverses depth-first).
  ;; Reverse so we process leaves before ancestors — extracting a child before
  ;; its parent prevents the parent splice from trying to re-insert already-gone nodes.
  (let ((targets (nreverse
                  (org-element-map tree 'headline
                    (lambda (hl)
                      (when (js/ox-strip-headline-p hl) hl))))))
    (dolist (hl targets)
      (js/ox-splice-headline hl)))
  tree)

;;; --- Registration ---

(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-parse-tree-functions
               #'js/ox-filter-strip-headings))

(provide 'js-ox-strip-heading)
;;; js-ox-strip-heading.el ends here
