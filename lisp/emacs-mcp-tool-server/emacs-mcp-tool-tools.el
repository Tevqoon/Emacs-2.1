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

;;; Universal Agent Job System

(defvar emacs-mcp-agent-jobs (make-hash-table :test 'equal)
  "Hash table of active agent jobs.
Keys are job IDs, values are job structures.")

(defcustom emacs-mcp-agent-job-ttl 3600
  "Time to live for completed jobs in seconds (default: 1 hour)."
  :type 'integer
  :group 'emacs-mcp-tool)

(cl-defstruct (emacs-mcp-agent-job (:constructor emacs-mcp-agent-job--make))
  id agent-type state started-at finished-at progress logs result error)

(defun emacs-mcp-agent--generate-job-id ()
  "Generate a unique job ID."
  (format "job-%s-%d" 
          (format-time-string "%s") 
          (random 10000)))

(defun emacs-mcp-agent--make-job (agent-type)
  "Create a new agent job of AGENT-TYPE."
  (let* ((id (emacs-mcp-agent--generate-job-id))
         (job (emacs-mcp-agent-job--make 
               :id id
               :agent-type agent-type
               :state "queued"
               :started-at (current-time)
               :progress 0
               :logs nil)))
    (puthash id job emacs-mcp-agent-jobs)
    job))

(defun emacs-mcp-agent--update-job (job-id &rest updates)
  "Update job with JOB-ID with UPDATES plist.
UPDATES can include :state, :progress, :result, :error."
  (when-let ((job (gethash job-id emacs-mcp-agent-jobs)))
    (while updates
      (let ((key (pop updates))
            (value (pop updates)))
        (pcase key
          (:state (setf (emacs-mcp-agent-job-state job) value))
          (:progress (setf (emacs-mcp-agent-job-progress job) value))
          (:result (setf (emacs-mcp-agent-job-result job) value))
          (:error (setf (emacs-mcp-agent-job-error job) value))
          (:finished-at (setf (emacs-mcp-agent-job-finished-at job) value)))))
    (when (member (emacs-mcp-agent-job-state job) '("done" "error"))
      (setf (emacs-mcp-agent-job-finished-at job) (current-time)))
    (puthash job-id job emacs-mcp-agent-jobs)
    job))

(defun emacs-mcp-agent--log-message (job-id message)
  "Add a log MESSAGE to job with JOB-ID."
  (when-let ((job (gethash job-id emacs-mcp-agent-jobs)))
    (push (format "[%s] %s" 
                  (format-time-string "%H:%M:%S") 
                  message)
          (emacs-mcp-agent-job-logs job))
    (puthash job-id job emacs-mcp-agent-jobs)))

(defun emacs-mcp-agent--cleanup-jobs ()
  "Clean up expired jobs based on TTL."
  (let ((now (current-time)))
    (maphash 
     (lambda (job-id job)
       (when (and (emacs-mcp-agent-job-finished-at job)
                  (> (float-time 
                      (time-subtract now (emacs-mcp-agent-job-finished-at job)))
                     emacs-mcp-agent-job-ttl))
         (remhash job-id emacs-mcp-agent-jobs)))
     emacs-mcp-agent-jobs)))

;; Clean up jobs every 10 minutes
(run-at-time "10 min" 600 #'emacs-mcp-agent--cleanup-jobs)

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

;;; Agent Job Resource Handlers

(defun emacs-mcp-agent--job-status-resource (params)
  "Resource handler for agent job status.
PARAMS should contain 'agent_type' and 'id' keys."
  (let* ((agent-type (alist-get "agent_type" params nil nil #'string=))
         (job-id (alist-get "id" params nil nil #'string=))
         (job (gethash job-id emacs-mcp-agent-jobs)))
    (if job
        (json-encode
         `(("id" . ,(emacs-mcp-agent-job-id job))
           ("agentType" . ,(emacs-mcp-agent-job-agent-type job))
           ("state" . ,(emacs-mcp-agent-job-state job))
           ("progress" . ,(emacs-mcp-agent-job-progress job))
           ("startedAt" . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" 
                                               (emacs-mcp-agent-job-started-at job)))
           ("finishedAt" . ,(when (emacs-mcp-agent-job-finished-at job)
                              (format-time-string "%Y-%m-%dT%H:%M:%SZ" 
                                                  (emacs-mcp-agent-job-finished-at job))))
           ("error" . ,(emacs-mcp-agent-job-error job))))
      (json-encode `(("error" . ,(format "Job %s not found" job-id)))))))

(defun emacs-mcp-agent--job-logs-resource (params)
  "Resource handler for agent job logs.
PARAMS should contain 'agent_type' and 'id' keys."
  (let* ((job-id (alist-get "id" params nil nil #'string=))
         (job (gethash job-id emacs-mcp-agent-jobs)))
    (if job
        (json-encode
         `(("logs" . ,(vconcat (reverse (emacs-mcp-agent-job-logs job))))
           ("count" . ,(length (emacs-mcp-agent-job-logs job)))))
      (json-encode `(("error" . ,(format "Job %s not found" job-id)))))))

(defun emacs-mcp-agent--job-result-resource (params)
  "Resource handler for agent job result.
PARAMS should contain 'agent_type' and 'id' keys."
  (let* ((job-id (alist-get "id" params nil nil #'string=))
         (job (gethash job-id emacs-mcp-agent-jobs)))
    (cond
     ((not job)
      (json-encode `(("error" . ,(format "Job %s not found" job-id)))))
     ((string= (emacs-mcp-agent-job-state job) "done")
      (or (emacs-mcp-agent-job-result job) 
          (json-encode `(("result" . "Job completed with no result")))))
     ((string= (emacs-mcp-agent-job-state job) "error")
      (json-encode `(("error" . ,(or (emacs-mcp-agent-job-error job) 
                                     "Job failed with unknown error")))))
     (t
      (json-encode `(("status" . ,(format "Job %s still %s" 
                                          job-id 
                                          (emacs-mcp-agent-job-state job)))))))))

;;; Register Agent Job Resources

(emacs-mcp-register-resource
 '(:uri "emacs://agent/{agent_type}/{id}/status"
   :name "Agent Job Status"
   :description "Status and progress of an agent job"
   :handler emacs-mcp-agent--job-status-resource))

(emacs-mcp-register-resource
 '(:uri "emacs://agent/{agent_type}/{id}/logs"
   :name "Agent Job Logs"
   :description "Logs from an agent job"
   :handler emacs-mcp-agent--job-logs-resource))

(emacs-mcp-register-resource
 '(:uri "emacs://agent/{agent_type}/{id}/result"
   :name "Agent Job Result"
   :description "Result of a completed agent job"
   :handler emacs-mcp-agent--job-result-resource))


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
 '(:id "documentation_agent_start"
   :description "Start documentation analysis using immediate + background pattern. Returns quick results immediately and provides resource URIs for full analysis status and results."
   :handler emacs-mcp-tool-documentation-agent-start
   :async t))

;; Legacy compatibility (deprecated)
(emacs-mcp-register-tool
 '(:id "documentation_agent"
   :description "[DEPRECATED] Use documentation_agent_start instead. Quick symbol search with redirect to new agent."
   :handler emacs-mcp-tool-documentation-agent))


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

;;; New Resource-Based Documentation Agent

(defun emacs-mcp-tool-documentation-agent-start (query)
  "Start documentation analysis using immediate + background pattern.

MCP Parameters:
  query - The documentation question or request"
  (mcp-server-lib-with-error-handling
    (let* ((job (emacs-mcp-agent--make-job "doc-agent"))
           (job-id (emacs-mcp-agent-job-id job)))
      
      ;; Log the start
      (emacs-mcp-agent--log-message job-id (format "Starting documentation analysis for: %s" query))
      
      ;; Start background analysis immediately - let the agent handle everything
      (emacs-mcp-documentation-start-background-analysis job-id query)
      
      ;; Return immediate status + resource URIs
      (format "Documentation Analysis Started for: %s

The agent will use its introspection tools to search functions, variables, and symbols.

Status: emacs://agent/doc_agent/%s/status
Logs: emacs://agent/doc_agent/%s/logs  
Results: emacs://agent/doc_agent/%s/result"
              query job-id job-id job-id))))

(defun emacs-mcp-documentation-start-background-analysis (job-id query)
  "Start background deep analysis for documentation query."
  (emacs-mcp-agent--update-job job-id :state "running" :progress 10)
  (emacs-mcp-agent--log-message job-id "Starting GPTel deep analysis...")
  
  ;; Start background work with run-at-time
  (run-at-time 0.1 nil
               (lambda ()
                 (emacs-mcp-documentation-background-worker job-id query))))

(defun emacs-mcp-documentation-background-worker (job-id query)
  "Background worker for deep documentation analysis."
  (let ((system-prompt "You are a documentation agent for Emacs. You have access to introspection tools to explore Emacs functions, variables, and symbols.

When answering questions:
1. Use find_symbols_by_name to discover relevant functions and variables
2. Use helpful_function_inspect and helpful_variable_inspect for detailed information. These provide the source, which you should include if relevant.
3. Provide clear, practical explanations
4. Include relevant examples when appropriate

Focus on being helpful and accurate. Use the tools to gather information, then synthesize a comprehensive answer."))
    
    (emacs-mcp-agent--update-job job-id :progress 20)
    (emacs-mcp-agent--log-message job-id "Invoking GPTel with introspection tools...")
    
    ;; Use GPTel with callback for async operation
    (emacs-mcp-gptel-agent 
     query
     :callback (lambda (response)
                 (emacs-mcp-documentation-handle-completion job-id response))
     :tool-category "introspection"
     :system-prompt system-prompt
     :extract-block t)))

(defun emacs-mcp-documentation-handle-completion (job-id response)
  "Handle completion of documentation analysis."
  (if response
      (progn
        (emacs-mcp-agent--log-message job-id "Analysis completed successfully")
        (emacs-mcp-agent--update-job job-id 
                                     :state "done" 
                                     :progress 100 
                                     :result response))
    (progn
      (emacs-mcp-agent--log-message job-id "Analysis failed - no response")
      (emacs-mcp-agent--update-job job-id 
                                   :state "error" 
                                   :error "No response received from GPTel agent"))))

;; Legacy compatibility tool (deprecated)
(defun emacs-mcp-tool-documentation-agent (query)
  "Legacy documentation agent - redirects to new resource-based agent.

MCP Parameters:
  query - The documentation question or request"
  (mcp-server-lib-with-error-handling
    (format "This tool is deprecated. Use 'documentation_agent_start' instead.

Query: %s

For full agentic analysis, call: documentation_agent_start" 
            query)))


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
