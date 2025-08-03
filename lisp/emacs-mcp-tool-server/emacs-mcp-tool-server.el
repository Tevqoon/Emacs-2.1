;;; emacs-mcp-tool-server.el --- MCP Tool Server for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.1.0") (helpful "0.1"))
;; Keywords: tools, ai, mcp

;;; Commentary:

;; This package implements an MCP (Model Context Protocol) tool server that
;; exposes direct Emacs functionality as synchronous tools.
;; 
;; Usage:
;; 1. M-x emacs-mcp-tool-install-stdio-script (one-time setup)
;; 2. M-x emacs-mcp-start-server (start the server)
;; 3. Register with Claude: claude mcp add -s user -t stdio emacs-tool-server -- ~/.emacs.d/emacs-mcp-tool-stdio.sh --init-function=emacs-mcp-tool-start-server --stop-function=emacs-mcp-tool-stop-server

;;; Code:

(require 'mcp-server-lib)
(require 'emacs-mcp-tool-tools)

;;; Customization

(defgroup emacs-mcp-tool nil
  "Emacs MCP Tool Server."
  :group 'applications
  :prefix "emacs-mcp-tool-")

(defcustom emacs-mcp-tool-enabled-tools 'all
  "List of enabled tools, or 'all for all tools."
  :type '(choice (const all)
                 (repeat string))
  :group 'emacs-mcp-tool)

(defcustom emacs-mcp-tool-enabled-resources 'all
  "List of enabled resources, or 'all for all resources."
  :type '(choice (const all)
                 (repeat string))
  :group 'emacs-mcp-tool)

;;; Server Management

;;;###autoload
(defun emacs-mcp-tool-install-stdio-script ()
  "Install the stdio transport script for MCP clients."
  (interactive)
  (mcp-server-lib-install)
  (message "MCP stdio script installed. Use: claude mcp add -s user -t stdio emacs-tool-server -- ~/.emacs.d/emacs-mcp-tool-stdio.sh --init-function=emacs-mcp-tool-start-server --stop-function=emacs-mcp-tool-stop-server"))

;;;###autoload
(defun emacs-mcp-tool-start-server ()
  "Start the MCP server and register all enabled tools and resources."
  (interactive)
  (if mcp-server-lib--running
      (message "MCP server already running")
    (emacs-mcp-tool-register-all-tools)
    (emacs-mcp-tool-register-all-resources)
    (mcp-server-lib-start)
    (message "MCP server started with %d tools and %d resources"
             (length (emacs-mcp-tool-get-enabled-tools))
             (length (emacs-mcp-tool-get-enabled-resources)))))

;;;###autoload
(defun emacs-mcp-tool-stop-server ()
  "Stop the MCP server."
  (interactive)
  (when mcp-server-lib--running
    (mcp-server-lib-stop)
    (message "MCP server stopped")))

;;;###autoload
(defun emacs-mcp-tool-restart-server ()
  "Restart the MCP server."
  (interactive)
  (emacs-mcp-tool-stop-server)
  (emacs-mcp-tool-start-server))

;;;###autoload
(defun emacs-mcp-tool-server-status ()
  "Show MCP server status."
  (interactive)
  (if mcp-server-lib--running
      (message "MCP server running with %d tools, %d resources"
               (length (emacs-mcp-tool-get-enabled-tools))
               (length (emacs-mcp-tool-get-enabled-resources)))
    (message "MCP server not running")))

;;;###autoload
(defun emacs-mcp-tool-show-metrics ()
  "Show MCP server metrics."
  (interactive)
  (mcp-server-lib-show-metrics))

;;;###autoload
(defun emacs-mcp-tool-list-tools ()
  "List all available and enabled tools."
  (interactive)
  (let ((all-tools (emacs-mcp-tool-get-tools))
        (enabled-tools (emacs-mcp-tool-get-enabled-tools)))
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
(defun emacs-mcp-tool-list-resources ()
  "List all available and enabled resources."
  (interactive)
  (let ((all-resources (emacs-mcp-tool-get-resources))
        (enabled-resources (emacs-mcp-tool-get-enabled-resources)))
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

(defun emacs-mcp-tool-get-enabled-tools ()
  "Get list of enabled tool IDs."
  (if (eq emacs-mcp-tool-enabled-tools 'all)
      (mapcar #'car (emacs-mcp-tool-get-tools))
    emacs-mcp-tool-enabled-tools))

(defun emacs-mcp-tool-get-enabled-resources ()
  "Get list of enabled resource URIs."
  (if (eq emacs-mcp-tool-enabled-resources 'all)
      (mapcar #'car (emacs-mcp-tool-get-resources))
    emacs-mcp-tool-enabled-resources))

(defun emacs-mcp-tool-register-all-tools ()
  "Register all enabled tools with mcp-server-lib."
  (let ((enabled-tools (emacs-mcp-tool-get-enabled-tools)))
    (dolist (tool (emacs-mcp-tool-get-tools))
      (let ((id (car tool))
            (def (cdr tool)))
        (when (member id enabled-tools)
          (mcp-server-lib-register-tool
           (plist-get def :handler)
           :id id
           :description (plist-get def :description)))))))

(defun emacs-mcp-tool-register-all-resources ()
  "Register all enabled resources with mcp-server-lib."
  (let ((enabled-resources (emacs-mcp-tool-get-enabled-resources)))
    (dolist (resource (emacs-mcp-tool-get-resources))
      (let ((uri (car resource))
            (def (cdr resource)))
        (when (member uri enabled-resources)
          (mcp-server-lib-register-resource
           uri
           (plist-get def :handler)
           :name (plist-get def :name)
           :description (plist-get def :description)
           :mime-type (plist-get def :mimeType)))))))

;;; Tool/Resource Loading

;;;###autoload
(defun emacs-mcp-tool-load-tools-from-file (file)
  "Load tool definitions from FILE."
  (interactive "fLoad tools from file: ")
  (load file)
  (message "Loaded tools from %s" file))

;;;###autoload
(defun emacs-mcp-tool-load-tools-from-directory (dir)
  "Load all .el files from DIR as tool definitions."
  (interactive "DLoad tools from directory: ")
  (dolist (file (directory-files dir t "\\.el$"))
    (load file))
  (message "Loaded tool files from %s" dir))

;;; Keybindings

(defvar emacs-mcp-tool-map (make-sparse-keymap)
  "Keymap for MCP tool server commands.")

(define-key emacs-mcp-tool-map (kbd "i") #'emacs-mcp-tool-install-stdio-script)
(define-key emacs-mcp-tool-map (kbd "s") #'emacs-mcp-tool-start-server)
(define-key emacs-mcp-tool-map (kbd "k") #'emacs-mcp-tool-stop-server)
(define-key emacs-mcp-tool-map (kbd "r") #'emacs-mcp-tool-restart-server)
(define-key emacs-mcp-tool-map (kbd "?") #'emacs-mcp-tool-server-status)
(define-key emacs-mcp-tool-map (kbd "m") #'emacs-mcp-tool-show-metrics)
(define-key emacs-mcp-tool-map (kbd "t") #'emacs-mcp-tool-list-tools)
(define-key emacs-mcp-tool-map (kbd "R") #'emacs-mcp-tool-list-resources)
(define-key emacs-mcp-tool-map (kbd "l") #'emacs-mcp-tool-load-tools-from-file)
(define-key emacs-mcp-tool-map (kbd "L") #'emacs-mcp-tool-load-tools-from-directory)

(global-set-key (kbd "C-c g t") emacs-mcp-tool-map)

(provide 'emacs-mcp-tool-server)
;;; emacs-mcp-tool-server.el ends here
