;;; org-checklist-export.el --- Export org checklists to print PDFs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: org, pdf, checklist, export
;; URL:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to export org-mode files (especially
;; org-roam files) to print-ready PDFs using an external CLI tool called
;; `checklist`.
;;
;; The main value-add is intelligent content filtering for org-roam files:
;; - Preserves headers with checklist items and all checklist content
;; - Removes org-roam metadata, code blocks, and non-checklist content
;; - Processes org links appropriately for print output
;; - Uses async processes to avoid blocking Emacs
;; - Provides robust error handling and debugging capabilities
;;
;; Usage:
;;   M-x js/checklist-export-buffer    ; Export current buffer
;;   M-x js/checklist-export-file      ; Export specific file
;;   M-x js/checklist-show-filtered-content ; Preview filtering
;;   M-x js/checklist-export-with-preview   ; Preview then export
;;
;; Configuration:
;;   (setq js-checklist-cli-command "checklist")
;;   (setq js-checklist-auto-open-pdf t)
;;   (setq js-checklist-default-output-dir "~/Documents/")

;;; Code:

(require 'org)
(require 'cl-lib)

;;; Customization

(defgroup js-checklist nil
  "Export org checklists to print PDFs."
  :group 'org
  :prefix "js-checklist-")

(defcustom js-checklist-cli-command "checklist"
  "Command to run the checklist CLI tool."
  :type 'string
  :group 'js-checklist)

(defcustom js-checklist-auto-open-pdf t
  "Whether to automatically open generated PDFs."
  :type 'boolean
  :group 'js-checklist)

(defcustom js-checklist-default-output-dir nil
  "Default directory for output PDFs.
If nil, use the same directory as the input file."
  :type '(choice (const nil) directory)
  :group 'js-checklist)

(defcustom js-checklist-process-timeout 30
  "Timeout in seconds for CLI process execution."
  :type 'integer
  :group 'js-checklist)

;;; Internal Variables

(defvar js/checklist--active-processes nil
  "List of active checklist export processes.")

(defvar js/checklist--temp-files nil
  "List of temporary files to clean up.")

;;; Utility Functions

(defun js/checklist--message (format-string &rest args)
  "Display a formatted message with checklist prefix."
  (message (concat "[Checklist Export] " format-string) args))

(defun js/checklist--error (format-string &rest args)
  "Display a formatted error message."
  (user-error (concat "[Checklist Export Error] " format-string) args))

(defun js/checklist--debug (format-string &rest args)
  "Display debug message when debugging is enabled."
  (when (boundp 'js-checklist-debug)
    (message (concat "[Checklist Debug] " format-string) args)))

(defun js/checklist--cli-available-p ()
  "Check if the checklist CLI tool is available."
  (executable-find js-checklist-cli-command))

(defun js/checklist--validate-cli ()
  "Validate that the CLI tool is available, or signal an error."
  (unless (js/checklist--cli-available-p)
    (js/checklist--error "CLI tool '%s' not found in PATH. Please install the checklist tool first."
                          js-checklist-cli-command)))

(defun js/checklist--create-temp-file (suffix)
  "Create a temporary file with the given SUFFIX."
  (let ((temp-file (make-temp-file "checklist-export-" nil suffix)))
    (push temp-file js/checklist--temp-files)
    temp-file))

(defun js/checklist--cleanup-temp-files ()
  "Clean up all temporary files."
  (dolist (file js/checklist--temp-files)
    (when (file-exists-p file)
      (condition-case err
          (delete-file file)
        (error
         (js/checklist--debug "Failed to delete temp file %s: %s" file err)))))
  (setq js/checklist--temp-files nil))

(defun js/checklist--cleanup-process (process)
  "Remove PROCESS from active processes list."
  (setq js/checklist--active-processes
        (delq process js/checklist--active-processes)))

;;; Content Filtering Functions

(defun js/checklist--has-checklist-items-p (text)
  "Check if TEXT contains checklist items."
  (string-match-p "^[ \t]*- \\[[ Xx]\\]" text))

(defun js/checklist--remove-properties-drawers (text)
  "Remove PROPERTIES drawers from TEXT."
  (replace-regexp-in-string
   ":PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?" "" text))

(defun js/checklist--remove-file-metadata (text)
  "Remove org file metadata lines from TEXT, but preserve template and import directives."
  (let ((lines (split-string text "\n"))
        (result '()))
    (dolist (line lines)
      (cond
       ;; Keep template directives
       ((string-match "^[ \t]*#\\+TEMPLATE:" line)
        (push line result))
       ;; Keep import directives  
       ((string-match "^[ \t]*#\\+IMPORT:" line)
        (push line result))
       ;; Remove other metadata lines
       ((string-match "^[ \t]*#\\+[^:\n]*:" line)
        ;; Skip this line
        )
       ;; Keep all other lines
       (t
        (push line result))))
    ;; Reconstruct text
    (string-join (reverse result) "\n")))

(defun js/checklist--remove-code-blocks (text)
  "Remove code blocks from TEXT."
  ;; Use a more robust approach that handles multiline blocks properly
  (let ((lines (split-string text "\n"))
        (result '())
        (in-code-block nil))
    (dolist (line lines)
      (cond
       ;; Start of code block
       ((string-match "^[ \\t]*#\\+begin_src" line)
        (setq in-code-block t))
       ;; End of code block
       ((string-match "^[ \\t]*#\\+end_src" line)
        (setq in-code-block nil))
       ;; Regular line - keep if not in code block
       ((not in-code-block)
        (push line result))))
    ;; Reconstruct text
    (string-join (reverse result) "\n")))

(defun js/checklist--remove-latex-blocks (text)
  "Remove LaTeX blocks from TEXT."
  (let ((result text))
    ;; Remove block LaTeX
    (setq result (replace-regexp-in-string
                  "\\\\\\[\\(?:.*\n\\)*?\\\\\\]" "" result t t))
    ;; Remove inline LaTeX
    (setq result (replace-regexp-in-string
                  "\\$[^$]+\\$" "" result t t))
    result))

(defun js/checklist--process-org-links (text)
  "Process org links in TEXT for print output."
  (let ((result text))
    ;; Convert [[id:...][description]] → description
    (setq result (replace-regexp-in-string
                  "\\[\\[id:[^]]+\\]\\[\\([^]]+\\)\\]\\]" "\\1" result))
    ;; Convert [[https://...][description]] → description
    (setq result (replace-regexp-in-string
                  "\\[\\[https?://[^]]+\\]\\[\\([^]]+\\)\\]\\]" "\\1" result))
    ;; Remove [[id:...]] (no description)
    (setq result (replace-regexp-in-string
                  "\\[\\[id:[^]]+\\]\\]" "" result))
    result))

(defun js/checklist--extract-file-metadata (content)
  "Extract and preserve file-level metadata that should be kept."
  (let ((preserved-metadata ""))
    ;; Extract title
    (when (string-match "^[ \t]*#\\+title:.*$" content)
      (setq preserved-metadata (concat preserved-metadata (match-string 0 content) "\n")))
    ;; Add other file-level metadata here in the future as needed
    preserved-metadata))

(defun js/checklist--filter-section (section)
  "Filter a single SECTION, removing unwanted content but preserving structure."
  (let ((filtered section))
    (setq filtered (js/checklist--remove-properties-drawers filtered))
    (setq filtered (js/checklist--remove-code-blocks filtered))
    (setq filtered (js/checklist--remove-latex-blocks filtered))
    ;; Note: Link processing now happens at file level, not section level
    ;; Remove standalone text paragraphs (lines that don't start with -, *, #, or whitespace)
    ;; Note: # is preserved here to allow file metadata in sections if needed
    ;; Put - at the end of character class to avoid escaping issues
    ;; Process line by line to avoid multiline regex issues
    (let ((lines (split-string filtered "\n"))
          (result ""))
      (dolist (line lines)
        (let ((processed-line (replace-regexp-in-string "^[^*#[:space:]-].*" "" line)))
          (setq result (concat result processed-line "\n"))))
      (setq filtered (string-trim result)))
    filtered))

(defun js/checklist--filter-content (content)
  "Filter CONTENT to extract only checklist-relevant sections."
  (js/checklist--debug "Starting content filtering...")
  
  ;; First, extract file-level metadata we want to preserve
  (let ((preserved-metadata (js/checklist--extract-file-metadata content))
        ;; Then remove ALL file metadata (we'll add back what we want)
        (filtered-content (js/checklist--remove-file-metadata content)))
    
    ;; Process org links on the entire content first (before section splitting)
    (setq filtered-content (js/checklist--process-org-links filtered-content))
    (js/checklist--debug "Link processing complete")
    
    ;; Split content by org headers
    (let ((sections (split-string filtered-content "^\\* " t))
          (result "")
          (first-section t))
      
      (dolist (section sections)
        (let ((section-text (if first-section 
                                section 
                              (concat "* " section)))
              (is-first first-section))
          (setq first-section nil)
          
          ;; Check if this section has checklist items OR is first section with templates/imports
          (if (or (js/checklist--has-checklist-items-p section-text)
                  (and is-first 
                       (string-match-p "#\\+\\(TEMPLATE\\|IMPORT\\):" section-text)))
              (progn
                (js/checklist--debug "Keeping section with checklist items or templates/imports")
                (setq result (concat result (js/checklist--filter-section section-text) "\n")))
            (js/checklist--debug "Discarding section without checklist items"))))
      
      ;; Combine preserved metadata with filtered sections
      (setq result (concat preserved-metadata result))
      
      ;; Final cleanup and validation
      (setq result (string-trim result))
      (js/checklist--debug "Content filtering complete. Result length: %s" (length result))
      
      (if (string-empty-p result)
          (js/checklist--error "No checklist content found after filtering")
        result))))

;;; File Operations

(defun js/checklist--generate-output-path (input-file)
  "Generate output PDF path for INPUT-FILE."
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory input-file)))
         (output-name (concat base-name "-checklist.pdf"))
         (output-dir (or js-checklist-default-output-dir
                         (file-name-directory input-file))))
    (expand-file-name output-name output-dir)))

(defun js/checklist--ensure-file-saved (buffer)
  "Ensure BUFFER is saved to a file."
  (with-current-buffer buffer
    (unless (buffer-file-name)
      (js/checklist--error "Buffer must be saved to a file before export"))
    (when (buffer-modified-p)
      (if (y-or-n-p "Buffer has unsaved changes. Save before export? ")
          (save-buffer)
        (js/checklist--error "Export cancelled - buffer must be saved first")))))

;;; Process Management

(defun js/checklist--create-process-buffer (name)
  "Create a process buffer with NAME."
  (let ((buffer (get-buffer-create (format "*Checklist Export: %s*" name))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Checklist Export Process: %s\n" name)
              (format "Started at: %s\n\n" (current-time-string))))
    buffer))

(defun js/checklist--process-sentinel (process event temp-file output-file process-buffer)
  "Process sentinel for checklist export PROCESS."
  (js/checklist--cleanup-process process)
  
  (let ((status (process-status process))
        (exit-status (process-exit-status process)))
    
    (cond
     ((eq status 'exit)
      (if (= exit-status 0)
          (progn
            (js/checklist--message "Export completed: %s" output-file)
            (when (file-exists-p output-file)
              (js/checklist--message "PDF size: %s bytes" 
                                     (file-attribute-size (file-attributes output-file)))
              (when js-checklist-auto-open-pdf
                (js/checklist--open-pdf output-file))))
        (progn
          (js/checklist--message "Export failed with exit code %d" exit-status)
          (display-buffer process-buffer))))
     
     ((eq status 'signal)
      (js/checklist--message "Export process was terminated")
      (display-buffer process-buffer))
     
     (t
      (js/checklist--message "Export process ended unexpectedly: %s" event)
      (display-buffer process-buffer)))
    
    ;; Clean up temp file
    (when (file-exists-p temp-file)
      (delete-file temp-file))))

(defun js/checklist--open-pdf (pdf-file)
  "Open PDF-FILE using system default application."
  (cond
   ((eq system-type 'darwin)
    (start-process "open-pdf" nil "open" pdf-file))
   ((eq system-type 'gnu/linux)
    (start-process "open-pdf" nil "xdg-open" pdf-file))
   ((eq system-type 'windows-nt)
    (start-process "open-pdf" nil "start" "" pdf-file))
   (t
    (js/checklist--message "Please manually open: %s" pdf-file))))

(defun js/checklist--start-export-process (temp-file output-file)
  "Start the export process for TEMP-FILE to OUTPUT-FILE."
  (js/checklist--validate-cli)
  
  (let* ((process-name (format "checklist-%s" (file-name-nondirectory output-file)))
         (process-buffer (js/checklist--create-process-buffer process-name))
         (process (start-process process-name process-buffer
                                js-checklist-cli-command temp-file output-file)))
    
    (unless process
      (js/checklist--error "Failed to start checklist process"))
    
    ;; Add to active processes
    (push process js/checklist--active-processes)
    
    ;; Set up process sentinel
    (set-process-sentinel process
                          (lambda (proc event)
                            (js/checklist--process-sentinel proc event temp-file output-file process-buffer)))
    
    ;; Set up timeout
    (run-at-time js-checklist-process-timeout nil
                 (lambda ()
                   (when (and (process-live-p process)
                              (member process js/checklist--active-processes))
                     (kill-process process)
                     (js/checklist--message "Export process timed out after %d seconds" 
                                            js-checklist-process-timeout))))
    
    (js/checklist--message "Export started: %s → %s" temp-file output-file)
    process))

;;; Main Export Functions

(defun js/checklist--export-content (content output-file)
  "Export filtered CONTENT to OUTPUT-FILE."
  (let ((temp-file (js/checklist--create-temp-file ".org")))
    (with-temp-file temp-file
      (insert content))
    
    (js/checklist--start-export-process temp-file output-file)))

;;;###autoload
(defun js/checklist-export-buffer ()
  "Export current org buffer to PDF after filtering content."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (js/checklist--error "Current buffer is not in org-mode"))
  
  (js/checklist--ensure-file-saved (current-buffer))
  
  (let* ((input-file (buffer-file-name))
         (output-file (js/checklist--generate-output-path input-file))
         (content (buffer-string))
         (filtered-content (js/checklist--filter-content content)))
    
    (js/checklist--export-content filtered-content output-file)))

;;;###autoload
(defun js/checklist-export-file (file)
  "Export specific org FILE to PDF after filtering content."
  (interactive "fOrg file to export: ")
  
  (unless (file-exists-p file)
    (js/checklist--error "File does not exist: %s" file))
  
  (unless (string-match-p "\\.org$" file)
    (js/checklist--error "File must have .org extension: %s" file))
  
  (let* ((output-file (js/checklist--generate-output-path file))
         (content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
         (filtered-content (js/checklist--filter-content content)))
    
    (js/checklist--export-content filtered-content output-file)))

;;;###autoload
(defun js/checklist-show-filtered-content ()
  "Show filtered content of current buffer in a preview buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (js/checklist--error "Current buffer is not in org-mode"))
  
  (let* ((content (buffer-string))
         (filtered-content (js/checklist--filter-content content))
         (preview-buffer (get-buffer-create "*Checklist Preview*")))
    
    (with-current-buffer preview-buffer
      (erase-buffer)
      (org-mode)
      (insert filtered-content)
      (goto-char (point-min)))
    
    (display-buffer preview-buffer)
    (js/checklist--message "Filtered content preview ready")))

;;;###autoload
(defun js/checklist-export-with-preview ()
  "Show filtered content preview, then optionally export to PDF."
  (interactive)
  (js/checklist-show-filtered-content)
  
  (when (y-or-n-p "Export filtered content to PDF? ")
    (js/checklist-export-buffer)))

;;; Debugging Functions

;;;###autoload
(defun js/checklist-test-cli ()
  "Test if the checklist CLI tool is available and responsive."
  (interactive)
  (if (js/checklist--cli-available-p)
      (let ((process (start-process "checklist-test" "*Checklist Test*"
                                   js-checklist-cli-command "--help")))
        (set-process-sentinel process
                              (lambda (proc event)
                                (if (= (process-exit-status proc) 0)
                                    (js/checklist--message "CLI tool is working correctly")
                                  (js/checklist--message "CLI tool failed: %s" event))))
        (js/checklist--message "Testing CLI tool..."))
    (js/checklist--message "CLI tool '%s' not found in PATH" js-checklist-cli-command)))

;;;###autoload
(defun js/checklist-cleanup ()
  "Clean up all temporary files and kill active processes."
  (interactive)
  (dolist (process js/checklist--active-processes)
    (when (process-live-p process)
      (kill-process process)))
  (setq js/checklist--active-processes nil)
  (js/checklist--cleanup-temp-files)
  (js/checklist--message "Cleanup completed"))

;;;###autoload
(defun js/checklist-debug-toggle ()
  "Toggle debug mode for checklist export."
  (interactive)
  (if (boundp 'js-checklist-debug)
      (progn
        (makunbound 'js-checklist-debug)
        (js/checklist--message "Debug mode disabled"))
    (setq js-checklist-debug t)
    (js/checklist--message "Debug mode enabled")))

;;; Template Functions

;;;###autoload
(defun js/checklist-insert-date-template ()
  "Insert a date template directive at point."
  (interactive)
  (let ((position (completing-read "Template position: " 
                                   '("top-left" "top-right" "bottom-left" "bottom-right")
                                   nil t "top-right"))
        (format (read-string "Date format (default yyyy-mm-dd): " nil nil "yyyy-mm-dd")))
    (insert (format "#+TEMPLATE: date:%s:%s\n" position format))))

;;;###autoload
(defun js/checklist-insert-time-template ()
  "Insert a time template directive at point."
  (interactive)
  (let ((position (completing-read "Template position: " 
                                   '("top-left" "top-right" "bottom-left" "bottom-right")
                                   nil t "top-right"))
        (format (read-string "Time format (default HH:MM): " nil nil "HH:MM")))
    (insert (format "#+TEMPLATE: time:%s:%s\n" position format))))

;;;###autoload
(defun js/checklist-insert-datetime-template ()
  "Insert a datetime template directive at point."
  (interactive)
  (let ((position (completing-read "Template position: " 
                                   '("top-left" "top-right" "bottom-left" "bottom-right")
                                   nil t "top-right"))
        (format (read-string "DateTime format (default yyyy-mm-dd HH:MM): " nil nil "yyyy-mm-dd HH:MM")))
    (insert (format "#+TEMPLATE: datetime:%s:%s\n" position format))))

;;;###autoload
(defun js/checklist-insert-custom-template ()
  "Insert a custom template directive at point."
  (interactive)
  (let ((position (completing-read "Template position: " 
                                   '("top-left" "top-right" "bottom-left" "bottom-right")
                                   nil t "top-right"))
        (content (read-string "Custom content: ")))
    (insert (format "#+TEMPLATE: custom:%s:%s\n" position content))))

;;;###autoload
(defun js/checklist-insert-file-import ()
  "Insert a file import directive at point."
  (interactive)
  (let* ((file (read-file-name "Import file: " nil nil t nil 
                               (lambda (name) (string-match "\\.org$" name))))
         (position (completing-read "Import position: " 
                                    '("left" "right" "top" "bottom")
                                    nil t "left"))
         (width (read-number "Width percentage (1-100, default 50): " 50)))
    (when (and (>= width 1) (<= width 100))
      (insert (format "#+IMPORT: %s:%s:%d\n" file position width)))))

;;;###autoload
(defun js/checklist-create-daily-template ()
  "Create a daily checklist template with date and file import setup."
  (interactive)
  (let* ((template-name (read-string "Template name: " "daily-checklist"))
         (work-file (read-file-name "Work plan file (optional): " nil nil nil nil
                                    (lambda (name) (string-match "\\.org$" name))))
         (filename (concat template-name ".org")))
    
    (with-current-buffer (get-buffer-create filename)
      (erase-buffer)
      (insert "#+TITLE: Daily Checklist\n")
      (insert "#+TEMPLATE: date:top-right:yyyy-mm-dd\n")
      (when (and work-file (not (string-empty-p work-file)))
        (insert (format "#+IMPORT: %s:right:50\n" work-file)))
      (insert "\n")
      (insert "* Morning Routine\n")
      (insert "- [ ] Review calendar\n")
      (insert "- [ ] Check priority tasks\n")
      (insert "- [ ] Plan day structure\n")
      (insert "\n")
      (insert "* Daily Tasks\n")
      (insert "- [ ] \n")
      (insert "\n")
      (insert "* Evening Review\n")
      (insert "- [ ] Complete task review\n")
      (insert "- [ ] Prepare tomorrow's priorities\n")
      
      (org-mode)
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (search-backward "- [ ] " nil t)
      (end-of-line))))

;;;###autoload
(defun js/checklist-export-template-with-imports ()
  "Export current buffer with template and import support (same as regular export)."
  (interactive)
  (js/checklist--message "Using regular export - templates and imports are automatically supported")
  (js/checklist-export-buffer))

;;;###autoload
(defun js/checklist-list-templates ()
  "List all template directives in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((templates '())
          (imports '()))
      (while (re-search-forward "^#\\+TEMPLATE:\\s-*\\(.+\\)$" nil t)
        (push (match-string 1) templates))
      (goto-char (point-min))
      (while (re-search-forward "^#\\+IMPORT:\\s-*\\(.+\\)$" nil t)
        (push (match-string 1) imports))
      
      (with-output-to-temp-buffer "*Checklist Templates*"
        (princ "Templates in current buffer:\n\n")
        (if templates
            (dolist (template (nreverse templates))
              (princ (format "TEMPLATE: %s\n" template)))
          (princ "No templates found.\n"))
        (princ "\nFile Imports:\n\n")
        (if imports
            (dolist (import (nreverse imports))
              (princ (format "IMPORT: %s\n" import)))
          (princ "No imports found.\n"))))))

;;;###autoload
(defun js/checklist-validate-templates ()
  "Validate template and import directives in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((issues '()))
      ;; Check template syntax
      (while (re-search-forward "^#\\+TEMPLATE:\\s-*\\(.+\\)$" nil t)
        (let ((directive (match-string 1)))
          (unless (string-match "^\\(date\\|time\\|datetime\\|custom\\):\\(top-left\\|top-right\\|bottom-left\\|bottom-right\\)\\(:.+\\)?$" directive)
            (push (format "Invalid template syntax: %s" directive) issues))))
      
      ;; Check import syntax
      (goto-char (point-min))
      (while (re-search-forward "^#\\+IMPORT:\\s-*\\(.+\\)$" nil t)
        (let ((directive (match-string 1)))
          (unless (string-match "^.+:\\(left\\|right\\|top\\|bottom\\)\\(:[0-9]+\\)?$" directive)
            (push (format "Invalid import syntax: %s" directive) issues))
          ;; Check if file exists
          (when (string-match "^\\(.+?\\):" directive)
            (let ((file (match-string 1 directive)))
              (unless (file-exists-p file)
                (push (format "Import file not found: %s" file) issues))))))
      
      (if issues
          (with-output-to-temp-buffer "*Template Validation*"
            (princ "Template validation issues:\n\n")
            (dolist (issue issues)
              (princ (format "• %s\n" issue))))
        (js/checklist--message "All templates and imports are valid!")))))

;;; Cleanup on Emacs exit

(add-hook 'kill-emacs-hook #'js/checklist-cleanup)

(provide 'org-checklist-export)

;;; org-checklist-export.el ends here
