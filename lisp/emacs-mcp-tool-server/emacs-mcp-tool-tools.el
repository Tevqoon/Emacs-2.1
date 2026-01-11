;;; emacs-mcp-tool-tools.el --- Tool definitions for Emacs MCP tool server -*- lexical-binding: t; -*-

;;; Copyright (C) 2025

;;; Author: Jure Smolar
;;; Version: 0.1.0
;;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.1.0"))
;;; Keywords: tools, mcp

;;; Commentary:

;;; Tool definitions for the Emacs MCP tool server.
;;; Synchronous tools for direct Emacs operations like buffer management and elisp evaluation.
;;;
;;; MCP Tool State Isolation Principles (For agents writing new tools)
;;;
;;; CRITICAL: All MCP tools must preserve Emacs state completely.
;;; Tools should never affect:
;;; - Current buffer or point position
;;; - Window configuration or selected window
;;; - Buffer restrictions (narrowing)
;;; - Mark, region, or selection state
;;; - Any global variables or settings
;;;
;;; Required patterns for all tools:
;;; 1. save-current-buffer - preserve current buffer
;;; 2. save-excursion - preserve point and mark
;;; 3. save-window-excursion - preserve window layout
;;; 4. save-restriction + widen - handle narrowed buffers
;;; 
;;; Example template:
;;; (defun my-mcp-tool (args)
;;;   (mcp-server-lib-with-error-handling
;;;     (save-current-buffer
;;;       (save-excursion
;;;         (save-window-excursion
;;;           (save-restriction
;;;             ;; Tool implementation here
;;;             ))))))
;;;
;;; This ensures tools are truly "read-only" from the user's perspective.

;;; Code:

(require 'mcp-server-lib)
(require 'cl-lib)

;;; Tool Registry

(defvar emacs-mcp-tools '()
  "Registry of available MCP tools.")

(defvar emacs-mcp-resources '()
  "Registry of available MCP resources.")

;;; Direct Documentation Tools

;;; Tool/Resource Registration Functions

(defun emacs-mcp-register-tool (tool-def)
  "Register a tool definition.
TOOL-DEF should be a plist with :id, :description, :handler, and optional properties."
  (let ((id (plist-get tool-def :id)))
    (setq emacs-mcp-tools (assoc-delete-all id emacs-mcp-tools))
    (push (cons id tool-def) emacs-mcp-tools)))

(defun emacs-mcp-register-resource (resource-def)
  "Register a resource definition.
RESOURCE-DEF should be a plist with :uri, :name, :description, :handler, and optional properties."
  (let ((uri (plist-get resource-def :uri)))
    (setq emacs-mcp-resources (assoc-delete-all uri emacs-mcp-resources))
    (push (cons uri resource-def) emacs-mcp-resources)))

(defun emacs-mcp-tool-get-tools ()
  "Get all registered tools."
  emacs-mcp-tools)

(defun emacs-mcp-tool-get-resources ()
  "Get all registered resources."
  emacs-mcp-resources)

;;; -> Tools -> Functions

(defun emacs-mcp-tool-find-symbols-by-name (keyword)
  "Search for Emacs Lisp symbols matching KEYWORD.

MCP Parameters:
  keyword - The keyword or pattern to search for in symbol names"
  (mcp-server-lib-with-error-handling
    (message "Searching for symbols matching: %s" keyword)
    (let* ((regexp (regexp-quote keyword))
           (functions (apropos-internal regexp 'functionp))
           (variables (apropos-internal regexp 'boundp))
           (result-text ""))
      
      ;; Format function results
      (when functions
        (setq result-text (concat result-text (format "== FUNCTIONS (%d) ==\n" (length functions))))
        (dolist (sym functions)
          (let ((doc (documentation sym t)))
            (setq result-text (concat result-text 
                                      (format "%s: %s\n" 
                                              sym 
                                              (if doc
                                                  (car (split-string doc "\n"))
                                                "No documentation")))))))
      
      ;; Format variable results
      (when variables
        (setq result-text (concat result-text 
                                  (format "\n== VARIABLES (%d) ==\n" (length variables))))
        (dolist (sym variables)
          (let ((doc (documentation-property sym 'variable-documentation)))
            (setq result-text (concat result-text 
                                      (format "%s: %s\n" 
                                              sym 
                                              (if doc
                                                  (car (split-string doc "\n"))
                                                "No documentation")))))))
      
      ;; Return result
      (if (string= result-text "")
          (format "No symbols found containing '%s'" keyword)
        (concat 
         (format "Found %d symbols matching '%s':\n\n" 
                 (+ (length functions) (length variables))
                 keyword)
         result-text)))))

(defun emacs-mcp-tool-helpful-function-inspect (function_name)
  "Retrieve comprehensive documentation for FUNCTION_NAME.

MCP Parameters:
  function_name - The exact name of the Emacs Lisp function to inspect"
  (mcp-server-lib-with-error-handling
    (message "Looking up function: %s" function_name)
    (condition-case err
        (save-window-excursion
          (with-current-buffer (helpful-callable (intern function_name))
            (message "Found documentation for function: %s" function_name)
            (buffer-substring-no-properties (point-min) (point-max))))
      (error 
       (message "Error looking up function %s: %s" function_name (error-message-string err))
       (format "Could not find documentation for function '%s'. Error: %s" 
               function_name (error-message-string err))))))

(defun emacs-mcp-tool-helpful-variable-inspect (variable_name)
  "Retrieve comprehensive documentation for VARIABLE_NAME.

MCP Parameters:
  variable_name - The exact name of the Emacs Lisp variable to inspect"
  (mcp-server-lib-with-error-handling
   (message "Looking up variable: %s" variable_name)
   (condition-case err
       (save-window-excursion
         (with-current-buffer (helpful-variable (intern variable_name))
           (message "Found documentation for variable: %s" variable_name)
           (buffer-substring-no-properties (point-min) (point-max))))
     (error 
      (message "Error looking up variable %s: %s" variable_name (error-message-string err))
      (format "Could not find documentation for variable '%s'. Error: %s" 
              variable_name (error-message-string err))))))

(defun emacs-mcp-tool-get-buffer-contents (buffer_name)
  "Get contents and metadata of a specific buffer by name.

MCP Parameters:
  buffer_name - The exact name of the buffer to retrieve"
  (mcp-server-lib-with-error-handling
   (if-let ((buf (get-buffer buffer_name)))
     (save-current-buffer
       (save-excursion
         (with-current-buffer buf
           (save-restriction
             (widen)
             (format "Buffer: %s\nMode: %s\nFile: %s\nSize: %d characters\nModified: %s\nRead-only: %s\n\n%s"
                     (buffer-name)
                     major-mode
                     (or buffer-file-name "No file")
                     (buffer-size)
                     (if (buffer-modified-p) "Yes" "No")
                     (if buffer-read-only "Yes" "No")
                     (buffer-substring-no-properties (point-min) (point-max)))))))
     (format "Buffer '%s' not found" buffer_name))))

(defun emacs-mcp-tool-get-agenda-buffer ()
  "Get the contents of the org-agenda buffer by generating a fresh view.
All operations are performed without affecting current window configuration or buffer state.

MCP Parameters: None"
  (mcp-server-lib-with-error-handling
    (save-current-buffer
      (save-excursion
        (save-window-excursion
          (save-restriction
            ;; Generate agenda view
            (open-org-agenda)
            
            ;; Refresh: update files list and rebuild
            (roam-agenda-files-update)
            (org-agenda-redo)
            
            ;; Get the current agenda buffer
            (let ((agenda-buffer (current-buffer)))
              (with-current-buffer agenda-buffer
                (save-restriction
                  (widen)
                  (format "Buffer: %s\nMode: %s\nSize: %d characters\nGenerated: %s\n\n%s"
                          (buffer-name)
                          major-mode
                          (buffer-size)
                          (format-time-string "%Y-%m-%d %H:%M:%S")
                          (buffer-substring-no-properties (point-min) (point-max))))))))))))

(defun emacs-mcp-tool-add-to-processing (item &optional body)
  "Add an item to today's Processing section in org-roam dailies.

MCP Parameters:
  item - The text/topic to add to processing (e.g. 'look into algebraic effects for error handling')
  body - Optional body/summary content for the entry"
  
  (mcp-server-lib-with-error-handling
   (org-roam-dailies-autocapture-today "p" item body)
   (format "Added to Processing: %s" item)))

(defun emacs-mcp-tool-search-org-roam-dailies (query)
  "Search org-roam daily journal files for QUERY using ripgrep.

MCP Parameters:
  query - The search term to find in daily journals"
  (mcp-server-lib-with-error-handling
    (let* ((default-directory (expand-file-name org-roam-dailies-directory org-roam-directory))
           (output (shell-command-to-string
                    (format "rg --line-number --with-filename --max-count=5 --type org -i %s"
			    (shell-quote-argument query)))))
      (if (string-empty-p output)
          (format "No results found for '%s' in dailies" query)
        (format "Search results for '%s' in dailies:\n\n%s" query output)))))


;;; -> Tools -> Registration

;; Primary documentation tools (use these first for understanding Emacs functionality)
(emacs-mcp-register-tool
 '(:id "find_symbols_by_name"
   :description "Discover Emacs functions and variables by keyword. Essential for exploring Emacs functionality and finding relevant symbols. Use with helpful_function_inspect and helpful_variable_inspect for comprehensive Emacs introspection."
   :handler emacs-mcp-tool-find-symbols-by-name))

(emacs-mcp-register-tool
 '(:id "helpful_function_inspect"
   :description "Get comprehensive function documentation including signature, examples, source code, and usage details. Essential for understanding how Emacs functions work programmatically."
   :handler emacs-mcp-tool-helpful-function-inspect))

(emacs-mcp-register-tool
 '(:id "helpful_variable_inspect"
   :description "Get comprehensive variable documentation including current values, customization options, and detailed descriptions. Essential for understanding Emacs configuration and state."
   :handler emacs-mcp-tool-helpful-variable-inspect))

;; Live buffer interaction (context-specific - only for current editing session)
;; These are better as resource (templates) but claude desktop does not yet support them.
(emacs-mcp-register-tool
 '(:id "get_agenda_buffer"
   :description "Get the contents of the org-agenda buffer, opening it if necessary. Uses your custom agenda view (daily agenda with TODOs) if available."
   :handler emacs-mcp-tool-get-agenda-buffer))

(emacs-mcp-register-tool
 '(:id "add_to_processing"
   :description "Add an item to today's Processing queue in the daily journal. Use this when discussing something the user might want to remember, act on, or explore later - ideas, topics to research, things to try, etc."
   :handler emacs-mcp-tool-add-to-processing))

(emacs-mcp-register-tool
 '(:id "search_org_roam_dailies"
   :description "Search org-roam daily journal entries for a term. Returns matching lines with dates and line numbers. Use for finding when something was discussed or logged."
   :handler emacs-mcp-tool-search-org-roam-dailies))


;;; Built-in Resources

(emacs-mcp-register-resource
 '(:uri "emacs://buffers"
   :name "Buffer List"
   :title "Buffer List"
   :description "A list of all the open emacs buffers"
   :mimeType "text/plain"
   :handler emacs-mcp-resource-buffer-list
   :annotations (:audience ("assistant") :priority 0.8)))

(emacs-mcp-register-resource
 '(:uri "emacs://buffer/{name}"
   :name "Buffer by Name"
   :title "Named Buffer Contents"
   :description "Contents and metadata of a specific Emacs buffer by name"
   :mimeType "text/plain"
   :handler emacs-mcp-resource-buffer-by-name
   :annotations (:audience ("assistant") :priority 0.8)))

;;; Development Helper
(defun emacs-mcp-tool-reload-and-restart ()
  "Reload tool definitions and restart MCP server for development."
  (interactive)
  (message "Reloading emacs-mcp-tool-tools.el...")
  (let ((tools-file (expand-file-name "emacs-mcp-tool-tools.el" 
                                      "~/.emacs.d/lisp/emacs-mcp-tool-server/")))
    (setq emacs-mcp-tools nil)
    (setq emacs-mcp-resources nil)
    (load-file tools-file))
  (emacs-mcp-tool-restart-server))

;;; Resource Handlers
(defun emacs-mcp-resource-buffer-list ()
  "Return list of open buffers with metadata as a resource handler."
  (let ((buffers (buffer-list)))
    (mapconcat
     (lambda (buf)
       (with-current-buffer buf
         (format "- %s (%s) %s %s"
                 (buffer-name)
                 major-mode
                 (if buffer-file-name
                     (file-name-nondirectory buffer-file-name)
                   "[no file]")
                 (if (buffer-modified-p) "[modified]" ""))))
     buffers "\n")))

(defun emacs-mcp-resource-buffer-by-name (params)
  "Return buffer contents by name.
PARAMS should contain 'name' key with buffer name."
  (let ((name (alist-get "name" params nil nil #'string=)))
    (if-let ((buf (get-buffer name)))
        (with-current-buffer buf
          (format "Buffer: %s\nMode: %s\nFile: %s\nSize: %d\nModified: %s\n\nContents:\n%s"
                  (buffer-name)
                  major-mode
                  (or buffer-file-name "No file")
                  (buffer-size)
                  (if (buffer-modified-p) "Yes" "No")
                  (buffer-string)))
      (format "Buffer '%s' not found" name))))

(provide 'emacs-mcp-tool-tools)
;;; emacs-mcp-tools.el ends here
