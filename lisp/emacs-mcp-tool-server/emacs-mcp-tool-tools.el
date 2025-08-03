;;; emacs-mcp-tool-tools.el --- Tool definitions for Emacs MCP tool server -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.1.0"))
;; Keywords: tools, mcp

;;; Commentary:

;; Tool definitions for the Emacs MCP tool server.
;; Synchronous tools for direct Emacs operations like buffer management and elisp evaluation.

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

;;; Documentation Tool Implementations

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


;;; Built-in Tools

;; Basic tools
(emacs-mcp-register-tool
 '(:id "hello_world"
   :description "Returns a hello world message from Emacs"
   :handler emacs-mcp-tool-hello-world))

(emacs-mcp-register-tool
 '(:id "get_emacs_version"
   :description "Returns detailed Emacs version and configuration info"
   :handler emacs-mcp-tool-get-version))

(emacs-mcp-register-tool
 '(:id "eval_elisp"
   :description "Safely evaluate Emacs Lisp code"
   :handler emacs-mcp-tool-eval-elisp))

;; System tools
(emacs-mcp-register-tool
 '(:id "get_buffer_list"
   :description "Get list of open buffers"
   :handler emacs-mcp-tool-get-buffer-list))

(emacs-mcp-register-tool
 '(:id "search_buffers"
   :description "Search content across open buffers"
   :handler emacs-mcp-tool-search-buffers))

;; Direct documentation tools
(emacs-mcp-register-tool
 '(:id "find_symbols_by_name"
   :description "Searches for Emacs Lisp symbols (functions and variables) by keyword."
   :handler emacs-mcp-tool-find-symbols-by-name))

(emacs-mcp-register-tool
 '(:id "helpful_function_inspect"
   :description "Retrieves comprehensive documentation for any Emacs Lisp function."
   :handler emacs-mcp-tool-helpful-function-inspect))

(emacs-mcp-register-tool
 '(:id "helpful_variable_inspect"
   :description "Retrieves comprehensive documentation for any Emacs Lisp variable."
   :handler emacs-mcp-tool-helpful-variable-inspect))

(emacs-mcp-register-tool
 '(:id "restart_server" 
   :description "Restart the MCP server to reload configurations and apply changes"
   :handler emacs-mcp-tool-restart-server-tool))


;;; Built-in Resources

(emacs-mcp-register-resource
 '(:uri "emacs://version"
   :name "Emacs Version"
   :title "Emacs Configuration and Version Information"
   :description "Detailed Emacs version, build info, features, and configuration"
   :mimeType "text/plain"
   :handler emacs-mcp-resource-version
   :annotations (:audience ("assistant" "user") :priority 0.7)))

(emacs-mcp-register-resource
 '(:uri "emacs://buffer/current"
   :name "Current Buffer"
   :title "Current Active Buffer Contents"
   :description "Contents and metadata of the currently active Emacs buffer"
   :mimeType "text/plain"
   :handler emacs-mcp-resource-current-buffer
   :annotations (:audience ("assistant") :priority 0.9)))

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
    (load-file tools-file))
  (message "Restarting MCP server...")
  (emacs-mcp-tool-restart-server)
  (message "MCP server reloaded and restarted"))

;;; Tool Handlers

(defun emacs-mcp-tool-hello-world ()
  "Return hello world message."
  "Hello World from Emacs MCP Agent Server!")

(defun emacs-mcp-tool-get-version ()
  "Return detailed Emacs version information."
  (format "Emacs version: %s\nSystem: %s\nBuild time: %s\nFeatures: %s" 
          emacs-version
          system-configuration
          emacs-build-time
          (mapconcat #'symbol-name features " ")))

(defun emacs-mcp-tool-eval-elisp (code)
  "Evaluate Emacs Lisp CODE safely.

MCP Parameters:
  code - The Emacs Lisp code to evaluate"
  (mcp-server-lib-with-error-handling
    (condition-case err
        (let ((result (eval (read code))))
          (format "Result: %S" result))
      (error (format "Error: %s" (error-message-string err))))))

(defun emacs-mcp-tool-get-buffer-list ()
  "Get list of open buffers with metadata."
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

(defun emacs-mcp-tool-search-buffers (pattern)
  "Search for PATTERN across open buffers.

MCP Parameters:
  pattern - The text pattern to search for"
  (let ((results '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (while (search-forward pattern nil t)
            (push (format "%s:%d: %s"
                          (buffer-name)
                          (line-number-at-pos)
                          (string-trim (thing-at-point 'line)))
                  results)))))
    (if results
        (mapconcat #'identity (reverse results) "\n")
      (format "No matches found for '%s'" pattern))))

(defun emacs-mcp-tool-restart-server-tool ()
  "Restart the MCP server to reload configurations and apply changes.

MCP Parameters: None"
  (mcp-server-lib-with-error-handling
    (emacs-mcp-tool-restart-server)
    "MCP server restarted successfully"))

;;; Resource Handlers

(defun emacs-mcp-resource-version ()
  "Return detailed Emacs version information."
  (format "Emacs Version: %s\nSystem: %s\nBuild time: %s\nFeatures: %s\nLoad path: %s"
          emacs-version
          system-configuration
          emacs-build-time
          (mapconcat #'symbol-name features " ")
          (mapconcat #'identity load-path "\n")))

(defun emacs-mcp-resource-current-buffer ()
  "Return current buffer contents and metadata."
  (format "Buffer: %s\nMode: %s\nFile: %s\nSize: %d\nModified: %s\n\nContents:\n%s"
          (buffer-name)
          major-mode
          (or buffer-file-name "No file")
          (buffer-size)
          (if (buffer-modified-p) "Yes" "No")
          (buffer-string)))

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
