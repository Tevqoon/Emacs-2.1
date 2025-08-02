;;; emacs-mcp-agent-server.el --- MCP Agent Server for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (mcp-server "0.1") (gptel "0.1"))
;; Keywords: ai, agents, mcp

;;; Commentary:

;; This package implements an MCP (Model Context Protocol) agent server that
;; delegates complex tasks to Emacs-based agents using gptel.
;; The server uses asynchronous communication with headless transport.
;;
;; Usage:
;; Register with Claude Code:
;; {
;;   "mcpServers": {
;;     "emacs-agent-server": {
;;       "command": "emacs",
;;       "args": [
;;         "--script", 
;;         "~/.emacs.d/elpa/mcp-server/mcp-server-headless-stdio-transport.el",
;;         "~/.emacs.d/lisp/emacs-mcp-tool-server/emacs-mcp-agent-server.el",
;;         "emacs-mcp-agent-server"
;;       ]
;;     }
;;   }
;; }

;;; Code:

;; Set up load paths for headless mode
(let ((elpa-dir (expand-file-name "~/.emacs.d/elpa/"))
      (lisp-dir (expand-file-name "~/.emacs.d/lisp/")))
  (when (file-directory-p elpa-dir)
    ;; Add mcp-server package
    (let ((mcp-server-dir (file-name-concat elpa-dir "mcp-server")))
      (when (file-directory-p mcp-server-dir)
        (add-to-list 'load-path mcp-server-dir)
        (let ((mcp-servers-dir (file-name-concat mcp-server-dir "mcp-servers")))
          (when (file-directory-p mcp-servers-dir)
            (add-to-list 'load-path mcp-servers-dir)))))
    
    ;; Add gptel package - find the versioned directory
    (dolist (dir (directory-files elpa-dir t "^gptel-"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))
    
    ;; Add our server directory  
    (when (file-directory-p lisp-dir)
      (add-to-list 'load-path (file-name-concat lisp-dir "emacs-mcp-tool-server")))))

;; Load core Emacs libraries first
(require 'eieio)
(require 'cl-lib)
(require 'json)

;; Load required libraries
(require 'mcp-server)

;; Conditional gptel loading
(condition-case nil
    (require 'gptel)
  (error 
   (message "Warning: gptel not available, agent functionality will be limited")))

;;; GPTel Helper Functions

(defun emacs-mcp-agent-extract-response-block (response)
  "Extract content between <RESPONSE> tags from RESPONSE."
  (if (string-match "<RESPONSE>\\(\\(?:.\\|\n\\)*?\\)</RESPONSE>" response)
      (match-string 1 response)
    response))

(defun emacs-mcp-agent-get-tools-by-category (category)
  "Get all tools from a specific CATEGORY in gptel--known-tools."
  (when (require 'gptel nil t)
    (let ((category-tools (assoc category gptel--known-tools)))
      (when category-tools
        (mapcar #'cdr (cdr category-tools))))))

(cl-defun emacs-mcp-agent-gptel-agent (prompt
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
                       (tool-category (emacs-mcp-agent-get-tools-by-category tool-category))
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
                                   (emacs-mcp-agent-extract-response-block response)
                                 response)))
                      (_ (error "GPTel agent error: Unexpected response format!"))))))))
    ;; The actual request to gptel
    (gptel-request prompt
      :callback wrapped-callback
      :stream nil)))

;;; Server Class Definition

(defclass emacs-mcp-agent-server (mcp-server)
  ((name :initform "emacs-agent-server")
   (version :initform "1.0.0"))
  "MCP agent server for delegating tasks to Emacs-based agents.")

;;; Agent Tool Definitions

(cl-defmethod mcp-server-enumerate-tools ((this emacs-mcp-agent-server))
  "Return list of available agent tools."
  '((:name "documentation_agent"
     :description "Agentic documentation explorer that uses introspection tools to answer Emacs-related questions. Provides comprehensive answers by automatically searching functions, variables, and symbols as needed."
     :properties ((:name query :type "string" :required t :description "The documentation question or request"))
     :async-lambda (lambda (request arguments cb-response)
                     (let ((query (gethash "query" arguments))
                           (system-prompt "You are a documentation agent for Emacs. You have access to introspection tools to explore Emacs functions, variables, and symbols.

When answering questions:
1. Use find_symbols_by_name to discover relevant functions and variables
2. Use helpful_function_inspect and helpful_variable_inspect for detailed information
3. Use eval_elisp to test concepts or demonstrate functionality when helpful
4. Provide clear, practical explanations
5. Include relevant examples when appropriate

Focus on being helpful and accurate. Use the tools to gather information, then synthesize a comprehensive answer."))
                       (emacs-mcp-agent-gptel-agent 
                        query
                        :callback (lambda (result)
                                    (mcp-server-write-tool-call-text-result request result cb-response))
                        :tool-category "introspection"
                        :system-prompt system-prompt
                        :extract-block t))))))


(provide 'emacs-mcp-agent-server)
;;; emacs-mcp-agent-server.el ends here
