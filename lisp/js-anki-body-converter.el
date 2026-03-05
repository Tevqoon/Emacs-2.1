;;; js-anki-body-converter.el --- Extract Anki fields from org special blocks -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Converts the raw org body text of an anki-editor heading into
;; (Front Hint Back) field values by inspecting special blocks and
;; their affiliated keywords.
;;
;; Field hierarchy:
;;
;;   Front:
;;     1. Explicit "** Front" subheading content  [passed in as argument]
;;     2. #+attr_latex: :options [TITLE] on the primary content block
;;     3. Org heading text                         [passed in as argument]
;;
;;   Hint:
;;     1. Explicit "** Hint" subheading content   [passed in as argument]
;;     2. Body of first definition/def block
;;     3. Body of first theorem/lemma/etc. block
;;     4. Empty string
;;
;;   Back:
;;     1. Explicit "** Back" subheading content   [passed in as argument]
;;     2. Body of all proof/solution blocks + residual text (concatenated)
;;     3. Residual text alone (no proof blocks)
;;     4. Full raw body (no special blocks at all)

;;; --- Block type classifications ---

(defconst js/anki-hint-block-types
  '("definition" "def")
  "Block types whose body becomes the Hint field (highest priority).")

(defconst js/anki-hint-fallback-block-types
  '("theorem" "thm" "lemma" "lem" "corollary" "cor"
    "proposition" "prop" "remark" "rem" "example" "ex" "exercise")
  "Block types used as Hint fallback when no definition block is present.")

(defconst js/anki-back-block-types
  '("proof" "pf" "solution" "sol")
  "Block types whose body contributes to the Back field.")

;;; --- Parsed block representation ---
;;
;; Each parsed block is a plist with:
;;   :type     STRING    — downcased block type, e.g. "theorem"
;;   :options  STRING|NIL — value of :options from #+attr_latex, e.g. "Fermat's Last Theorem"
;;   :content  STRING    — raw org text between begin/end lines, trimmed

;;; --- Core parser ---

(defun js/anki-parse-body (body)
  "Parse BODY (raw org string) into a list of special blocks and residual text.

Returns a plist:
  :blocks   — list of block plists in source order, each with
              :type :options :content
  :residual — trimmed concatenation of all text NOT inside any block

The #+attr_latex :options value is extracted from affiliated keywords
preceding each block, which is how LaTeX theorem titles are specified:

  #+attr_latex: :options [Fermat's Last Theorem]
  #+begin_theorem
  ...
  #+end_theorem"
  (let (blocks residual-parts)
    (with-temp-buffer
      ;; org-element requires org-mode for correct parsing of affiliated keywords
      (let ((org-inhibit-startup t))
        (org-mode))
      (insert body)
      (let* ((tree (org-element-parse-buffer 'element))
             ;; The tree is org-data > section > elements.
             ;; Handle both cases: body may or may not produce a section wrapper.
             (section (car (org-element-contents tree)))
             (all-elements
              (if (eq (org-element-type section) 'section)
                  (org-element-contents section)
                (org-element-contents tree))))

        (dolist (elem all-elements)
          (if (eq (org-element-type elem) 'special-block)
              ;; --- Special block: extract type, attr_latex options, content ---
              (let* ((type (downcase (org-element-property :type elem)))
                     ;; org-export-read-attribute reads the :attr_latex plist.
                     ;; Requires ox to be loaded (it is, via anki-editor).
                     (attr-latex (org-export-read-attribute :attr_latex elem))
                     (options-raw (plist-get attr-latex :options))
                     ;; Strip surrounding brackets: [Title] -> Title
                     (options
                      (when options-raw
                        (if (string-match "^\\[\\(.*\\)\\]$" options-raw)
                            (match-string 1 options-raw)
                          options-raw)))
                     (cbeg (org-element-property :contents-begin elem))
                     (cend (org-element-property :contents-end elem))
                     (content (if (and cbeg cend)
                                  (string-trim
                                   (buffer-substring-no-properties cbeg cend))
                                "")))
                (push (list :type    type
                            :options options
                            :content content)
                      blocks))

            ;; --- Non-block element: accumulate as residual ---
            (let* ((beg  (org-element-property :begin elem))
                   (end  (org-element-property :end elem))
                   (text (string-trim (buffer-substring-no-properties beg end))))
              (unless (string-empty-p text)
                (push text residual-parts)))))))

    (list :blocks   (nreverse blocks)
          :residual (string-trim
                     (mapconcat #'identity (nreverse residual-parts) "\n\n")))))

;;; --- Lookup helpers ---

(defun js/anki--find-block (blocks types)
  "Return the first block in BLOCKS whose :type is a member of TYPES, or nil."
  (cl-find-if (lambda (b) (member (plist-get b :type) types)) blocks))

(defun js/anki--blocks-of-types (blocks types)
  "Return all blocks in BLOCKS whose :type is a member of TYPES."
  (cl-remove-if-not (lambda (b) (member (plist-get b :type) types)) blocks))

(defun js/anki--join (&rest strings)
  "Join non-nil, non-empty STRINGS with double newline."
  (mapconcat #'identity
             (cl-remove-if (lambda (s) (or (null s) (string-empty-p (string-trim s))))
                           strings)
             "\n\n"))

;;; --- Main entry point ---

(defun js/anki-derive-fields (heading body explicit-front explicit-hint explicit-back)
  "Derive Anki (Front Hint Back) fields from an org entry.

Arguments:
  HEADING        raw heading string (no tags/todo/priority)
  BODY           raw org string between the heading and its first subheading
  EXPLICIT-FRONT content of a '** Front' subheading, or nil
  EXPLICIT-HINT  content of a '** Hint' subheading, or nil
  EXPLICIT-BACK  content of a '** Back' subheading, or nil

Returns a plist (:front STRING :hint STRING :back STRING).

Examples:

  ;; Block-based note, title from #+attr_latex
  ;;
  ;;   ** Nondeterministic TM accepts L
  ;;   #+attr_latex: :options [Acceptance Condition]
  ;;   #+begin_theorem
  ;;   A NTM accepts L if all branches ...
  ;;   #+end_theorem
  ;;   #+begin_proof
  ;;   By induction on ...
  ;;   #+end_proof
  ;;
  ;; -> Front: \"Acceptance Condition\"
  ;;    Hint:  \"A NTM accepts L if all branches ...\"
  ;;    Back:  \"By induction on ...\"

  ;; Definition note, no attr_latex, heading becomes Front
  ;;
  ;;   ** Definition: NTM
  ;;   #+begin_definition
  ;;   A NTM has transition function delta : Q x Gamma -> P(...)
  ;;   #+end_definition
  ;;
  ;; -> Front: \"Definition: NTM\"
  ;;    Hint:  \"A NTM has transition function ...\"
  ;;    Back:  \"\" (no proof, no residual)

  ;; Explicit subheadings always win regardless of blocks
  ;;
  ;;   ** Some heading
  ;;   #+begin_theorem ... #+end_theorem
  ;;   ** Hint
  ;;   Override hint text.
  ;;
  ;; -> Hint: \"Override hint text.\""
  (let* ((parsed   (js/anki-parse-body body))
         (blocks   (plist-get parsed :blocks))
         (residual (plist-get parsed :residual))
         (has-blocks (not (null blocks)))

         ;; Locate blocks by role
         (hint-block    (js/anki--find-block blocks js/anki-hint-block-types))
         (hint-fb-block (js/anki--find-block blocks js/anki-hint-fallback-block-types))
         (back-blocks   (js/anki--blocks-of-types blocks js/anki-back-block-types))

         ;; Primary block = first hint-type or hint-fallback block.
         ;; Its :options is the preferred Front title.
         (primary-block (or hint-block hint-fb-block))
         (block-title   (and primary-block (plist-get primary-block :options)))

         ;; --- Front ---
         (front (or explicit-front
                    block-title
                    heading))

         ;; --- Hint ---
         (hint (or explicit-hint
                   (and hint-block    (plist-get hint-block    :content))
                   (and hint-fb-block (plist-get hint-fb-block :content))
                   ""))

         ;; --- Back ---
         (back
          (or explicit-back
              (if has-blocks
                  (apply #'js/anki--join
                         (append (mapcar (lambda (b) (plist-get b :content))
                                         back-blocks)
                                 (list residual)))
                ;; No special blocks at all: use raw body
                body))))

    (list :front front
          :hint  hint
          :back  (if (string-empty-p (string-trim back)) "" back))))

(provide 'js-anki-body-converter)
