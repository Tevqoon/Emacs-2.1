;;; emacs-mcp-agent-server.el --- MCP Agent Server for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.1.0") (gptel "0.1"))
;; Keywords: tools, ai, mcp

;;; Commentary:

;; This package implements an MCP (Model Context Protocol) server that allows
;; external LLMs to delegate tasks to Emacs-based agents using gptel.
;; 
;; Usage:
;; 1. M-x emacs-mcp-install-stdio-script (one-time setup)
;; 2. M-x emacs-mcp-start-server (start the server)
;; 3. Register with Claude: claude mcp add -s user -t stdio emacs-agent-server -- ~/.emacs.d/emacs-mcp-stdio.sh --init-function=emacs-mcp-start-server --stop-function=emacs-mcp-stop-server

;;; Code:

(require 'mcp-server-lib)
(require 'emacs-mcp-tools)

;;; Customization

(defgroup emacs-mcp nil
  "Emacs MCP Agent Server."
  :group 'applications
  :prefix "emacs-mcp-")

(defcustom emacs-mcp-enabled-tools 'all
  "List of enabled tools, or 'all for all tools."
  :type '(choice (const all)
                 (repeat string))
  :group 'emacs-mcp)

(defcustom emacs-mcp-enabled-resources 'all
  "List of enabled resources, or 'all for all resources."
  :type '(choice (const all)
                 (repeat string))
  :group 'emacs-mcp)

;;; Server Management

;;;###autoload
(defun emacs-mcp-install-stdio-script ()
  "Install the stdio transport script for MCP clients."
  (interactive)
  (mcp-server-lib-install)
  (message "MCP stdio script installed. Use: claude mcp add -s user -t stdio emacs-agent-server -- ~/.emacs.d/emacs-mcp-stdio.sh --init-function=emacs-mcp-start-server --stop-function=emacs-mcp-stop-server"))

;;;###autoload
(defun emacs-mcp-start-server ()
  "Start the MCP server and register all enabled tools and resources."
  (interactive)
  (if mcp-server-lib--running
      (message "MCP server already running")
    (emacs-mcp-register-all-tools)
    (emacs-mcp-register-all-resources)
    (mcp-server-lib-start)
    (message "MCP server started with %d tools and %d resources"
             (length (emacs-mcp-get-enabled-tools))
             (length (emacs-mcp-get-enabled-resources)))))

;;;###autoload
(defun emacs-mcp-stop-server ()
  "Stop the MCP server."
  (interactive)
  (when mcp-server-lib--running
    (mcp-server-lib-stop)
    (message "MCP server stopped")))

;;;###autoload
(defun emacs-mcp-restart-server ()
  "Restart the MCP server."
  (interactive)
  (emacs-mcp-stop-server)
  (emacs-mcp-start-server))

;;;###autoload
(defun emacs-mcp-server-status ()
  "Show MCP server status."
  (interactive)
  (if mcp-server-lib--running
      (message "MCP server running with %d tools, %d resources"
               (length (emacs-mcp-get-enabled-tools))
               (length (emacs-mcp-get-enabled-resources)))
    (message "MCP server not running")))

;;;###autoload
(defun emacs-mcp-show-metrics ()
  "Show MCP server metrics."
  (interactive)
  (mcp-server-lib-show-metrics))

;;;###autoload
(defun emacs-mcp-list-tools ()
  "List all available and enabled tools."
  (interactive)
  (let ((all-tools (emacs-mcp-get-tools))
        (enabled-tools (emacs-mcp-get-enabled-tools)))
    (with-current-buffer (get-buffer-create "*MCP Tools*")
      (erase-buffer)
      (insert "Emacs MCP Tools\n")
      (insert "===============\n\n")
      (dolist (tool all-tools)
        (let* ((id (car tool))
               (def (cdr tool))
               (desc (plist-get def :description))
               (enabled (member id enabled-tools)))
          (insert (format "%s %s\n  %s\n\n"
                          (if enabled "✓" "✗")
                          id
                          desc))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun emacs-mcp-list-resources ()
  "List all available and enabled resources."
  (interactive)
  (let ((all-resources (emacs-mcp-get-resources))
        (enabled-resources (emacs-mcp-get-enabled-resources)))
    (with-current-buffer (get-buffer-create "*MCP Resources*")
      (erase-buffer)
      (insert "Emacs MCP Resources\n")
      (insert "==================\n\n")
      (dolist (resource all-resources)
        (let* ((uri (car resource))
               (def (cdr resource))
               (name (plist-get def :name))
               (desc (plist-get def :description))
               (enabled (member uri enabled-resources)))
          (insert (format "%s %s (%s)\n  %s\n\n"
                          (if enabled "✓" "✗")
                          uri
                          name
                          desc))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; Internal Functions

(defun emacs-mcp-get-enabled-tools ()
  "Get list of enabled tool IDs."
  (if (eq emacs-mcp-enabled-tools 'all)
      (mapcar #'car (emacs-mcp-get-tools))
    emacs-mcp-enabled-tools))

(defun emacs-mcp-get-enabled-resources ()
  "Get list of enabled resource URIs."
  (if (eq emacs-mcp-enabled-resources 'all)
      (mapcar #'car (emacs-mcp-get-resources))
    emacs-mcp-enabled-resources))

(defun emacs-mcp-register-all-tools ()
  "Register all enabled tools with mcp-server-lib."
  (let ((enabled-tools (emacs-mcp-get-enabled-tools)))
    (dolist (tool (emacs-mcp-get-tools))
      (let ((id (car tool))
            (def (cdr tool)))
        (when (member id enabled-tools)
          (mcp-server-lib-register-tool
           (plist-get def :handler)
           :id id
           :description (plist-get def :description)))))))

(defun emacs-mcp-register-all-resources ()
  "Register all enabled resources with mcp-server-lib."
  (let ((enabled-resources (emacs-mcp-get-enabled-resources)))
    (dolist (resource (emacs-mcp-get-resources))
      (let ((uri (car resource))
            (def (cdr resource)))
        (when (member uri enabled-resources)
          (mcp-server-lib-register-resource
           uri
           (plist-get def :handler)
           :name (plist-get def :name)
           :description (plist-get def :description)))))))

;;; Tool/Resource Loading

;;;###autoload
(defun emacs-mcp-load-tools-from-file (file)
  "Load tool definitions from FILE."
  (interactive "fLoad tools from file: ")
  (load file)
  (message "Loaded tools from %s" file))

;;;###autoload
(defun emacs-mcp-load-tools-from-directory (dir)
  "Load all .el files from DIR as tool definitions."
  (interactive "DLoad tools from directory: ")
  (dolist (file (directory-files dir t "\\.el$"))
    (load file))
  (message "Loaded tool files from %s" dir))

;;; Keybindings

(defvar emacs-mcp-map (make-sparse-keymap)
  "Keymap for MCP server commands.")

(define-key emacs-mcp-map (kbd "i") #'emacs-mcp-install-stdio-script)
(define-key emacs-mcp-map (kbd "s") #'emacs-mcp-start-server)
(define-key emacs-mcp-map (kbd "k") #'emacs-mcp-stop-server)
(define-key emacs-mcp-map (kbd "r") #'emacs-mcp-restart-server)
(define-key emacs-mcp-map (kbd "?") #'emacs-mcp-server-status)
(define-key emacs-mcp-map (kbd "m") #'emacs-mcp-show-metrics)
(define-key emacs-mcp-map (kbd "t") #'emacs-mcp-list-tools)
(define-key emacs-mcp-map (kbd "R") #'emacs-mcp-list-resources)
(define-key emacs-mcp-map (kbd "l") #'emacs-mcp-load-tools-from-file)
(define-key emacs-mcp-map (kbd "L") #'emacs-mcp-load-tools-from-directory)

(global-set-key (kbd "C-c g m") emacs-mcp-map)

(provide 'emacs-mcp-agent-server)
;;; emacs-mcp-agent-server.el ends here