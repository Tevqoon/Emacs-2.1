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

;;; Tool Registration API

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

;; Agent tools
(emacs-mcp-register-tool
 '(:id "documentation_agent"
   :description "Agentic documentation explorer that uses introspection tools to answer Emacs-related questions. Provides comprehensive answers by automatically searching functions, variables, and symbols as needed."
   :handler emacs-mcp-tool-documentation-agent
   :async t))

;;; Built-in Resources

(emacs-mcp-register-resource
 '(:uri "emacs://version"
   :name "Emacs Version"
   :description "Current Emacs version and build info"
   :handler emacs-mcp-resource-version))

(emacs-mcp-register-resource
 '(:uri "emacs://buffer/current"
   :name "Current Buffer"
   :description "Contents of the current buffer"
   :handler emacs-mcp-resource-current-buffer))

(emacs-mcp-register-resource
 '(:uri "emacs://buffer/{name}"
   :name "Buffer by name"
   :description "Contents of a specific buffer"
   :handler emacs-mcp-resource-buffer-by-name))

;;; Agent Helpers

(defun emacs-mcp-extract-response-block (response)
  "Extract content between <RESPONSE> tags from RESPONSE."
  (if (string-match "<RESPONSE>\\(\\(?:.\\|\n\\)*?\\)</RESPONSE>" response)
      (match-string 1 response)
    response))

(cl-defun emacs-mcp-gptel-agent (prompt
                                 &key
                                 callback
                                 model
                                 tools
                                 tool-category
                                 system-prompt
                                 extract-block)
  "Execute a GPTel agent task asynchronously.

PROMPT is the task input text.
CALLBACK is called with the result when the task completes.
MODEL is the optional model to use (defaults to gpt-4o-mini).
TOOLS is an optional list of specific tools to enable.
TOOL-CATEGORY is an optional category name to enable all tools from that category.
SYSTEM-PROMPT overrides the default system message.
EXTRACT-BLOCK when non-nil, extracts content from <RESPONSE> tags."
  (let* ((gptel-model (or model 'gpt-4o-mini))
         ;; Handle tool selection by category or specific tools
         (gptel-tools (cond
                       (tool-category (emacs-mcp-get-tools-by-category tool-category))
                       (tools (if (eq tools t) gptel-tools tools))
                       (t nil)))
         (gptel-use-tools (when (or tools tool-category) t))
         (gptel-use-context nil)
         ;; Modify system prompt to include response block format if needed
         (block-format-instruction 
          (when extract-block
            "After you complete your thinking, place your final response between <RESPONSE> and </RESPONSE> tags. Only content within these tags will be returned to the user."))
         (modified-system-prompt 
          (if (and extract-block system-prompt)
              (concat system-prompt "\n\n" block-format-instruction)
            (if extract-block
                (concat (or system-prompt gptel--system-message) "\n\n" block-format-instruction)
              system-prompt)))
         (gptel--system-message (or modified-system-prompt gptel--system-message))
         ;; The callback function
         (wrapped-callback
          (lambda (response info)
            (when callback
              (if (plist-get info :error)
                  (funcall callback (format "Error: %s" (plist-get info :error)))
                (cond ((and (consp response)
                           (equal (car response) 'tool-result))
                       nil) ; Drop tool results
                      ((stringp response)
                       (funcall callback 
                               (if extract-block
                                   (emacs-mcp-extract-response-block response)
                                 response)))
                      (_ (error "GPTel agent error: Unexpected response format!"))))))))
    ;; The actual request to gptel
    (gptel-request prompt
      :callback wrapped-callback
      :stream nil)))

(defun emacs-mcp-get-tools-by-category (category)
  "Get all tools from a specific CATEGORY in gptel--known-tools."
  (when (require 'gptel nil t)
    (let ((category-tools (assoc category gptel--known-tools)))
      (when category-tools
        (mapcar #'cdr (cdr category-tools))))))

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

(defun emacs-mcp-tool-documentation-agent (query)
  "Documentation agent that uses introspection tools to answer queries.

MCP Parameters:
  query - The documentation question or request"
  (mcp-server-lib-with-error-handling
    (let ((system-prompt "You are a documentation agent for Emacs. You have access to introspection tools to explore Emacs functions, variables, and symbols.

When answering questions:
1. Use find_symbols_by_name to discover relevant functions and variables
2. Use helpful_function_inspect and helpful_variable_inspect for detailed information. These provide the soucre, which you should include if relevant.
3. Provide clear, practical explanations
4. Include relevant examples when appropriate

Focus on being helpful and accurate. Use the tools to gather information, then synthesize a comprehensive answer.")
          (result nil)
          (completed nil))
      
      ;; Use a callback to capture the result
      (emacs-mcp-gptel-agent 
       query
       :callback (lambda (response)
                   (setq result response
                         completed t))
       :tool-category "introspection"
       :system-prompt system-prompt
       :extract-block t)
      
      ;; Wait for completion with timeout
      (let ((timeout-count 0))
        (while (and (not completed) (< timeout-count 600)) ; 60 second timeout
          (sit-for 0.1)
          (setq timeout-count (1+ timeout-count))))
      
      (if completed
          (or result "No response received from documentation agent")
        "Documentation agent timed out. Please try again."))))

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
