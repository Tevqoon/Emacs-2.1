;;; emacs-mcp-agent-tools.el --- Agent definitions for Emacs MCP agent server -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (mcp-server "0.1") (gptel "0.1"))
;; Keywords: ai, agents, mcp

;;; Commentary:

;; Agent definitions for the Emacs MCP agent server.
;; Asynchronous agents that delegate complex tasks to gptel with tool categories.

;;; Code:

(require 'mcp-server)
(require 'gptel)
(require 'cl-lib)

;;; Agent Registry

(defvar emacs-mcp-agent-agents '()
  "Registry of available MCP agents.")

;;; Agent Registration API

(defun emacs-mcp-agent-register-agent (agent-def)
  "Register an agent definition.
AGENT-DEF should be a plist with :name, :description, :async-lambda, and optional properties."
  (let ((name (plist-get agent-def :name)))
    (setq emacs-mcp-agent-agents (assoc-delete-all name emacs-mcp-agent-agents))
    (push (cons name agent-def) emacs-mcp-agent-agents)))

(defun emacs-mcp-agent-get-agents ()
  "Get all registered agents."
  emacs-mcp-agent-agents)

;;; GPTel Helper Functions

(defun emacs-mcp-agent-extract-response-block (response)
  "Extract content between <RESPONSE> tags from RESPONSE."
  (if (string-match "<RESPONSE>\\\\(\\\\(?:.\\\\|\\n\\\\)*?\\\\)</RESPONSE>" response)
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
              (concat system-prompt "\\n\\n" block-format-instruction)
            (if extract-block
                (concat (or system-prompt gptel--system-message) "\\n\\n" block-format-instruction)
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

;;; Built-in Agents

;; Documentation Agent
(emacs-mcp-agent-register-agent
 (list :name "documentation_agent"
       :description "Agentic documentation explorer that uses introspection tools to answer Emacs-related questions. Provides comprehensive answers by automatically searching functions, variables, and symbols as needed."
       :properties (list (list :name "query"
                              :type "string"
                              :description "The documentation question or request"
                              :required t))
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
                          :extract-block t)))))

(provide 'emacs-mcp-agent-tools)
;;; emacs-mcp-agent-tools.el ends here