;;; emacs-mcp-tools.el --- Tool definitions for Emacs MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.1.0"))
;; Keywords: tools, mcp

;;; Commentary:

;; Tool definitions for the Emacs MCP server.
;; Tools are defined as plists with :id, :description, :handler, and optional properties.

;;; Code:

(require 'mcp-server-lib)

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

(defun emacs-mcp-get-tools ()
  "Get all registered tools."
  emacs-mcp-tools)

(defun emacs-mcp-get-resources ()
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

(provide 'emacs-mcp-tools)
;;; emacs-mcp-tools.el ends here