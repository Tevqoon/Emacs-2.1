;;; emacs-mcp-agent-server.el --- MCP Agent Server for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (mcp-server "0.1") (gptel "0.1"))
;; Keywords: ai, agents, mcp

;;; Commentary:

;; This package implements an MCP (Model Context Protocol) agent server that
;; delegates complex tasks to Emacs-based agents using gptel.
;; The server uses asynchronous communication for agentic tasks.
;;
;; Usage:
;; 1. M-x emacs-mcp-agent-install-stdio-script (one-time setup)
;; 2. M-x emacs-mcp-agent-start-server (start the server)
;; 3. Register with Claude: claude mcp add -s user -t stdio emacs-agent-server -- ~/.emacs.d/emacs-mcp-agent-stdio.sh --init-function=emacs-mcp-agent-start-server --stop-function=emacs-mcp-agent-stop-server

;;; Code:

(require 'mcp-server)
(require 'emacs-mcp-agent-tools)

;;; Customization

(defgroup emacs-mcp-agent nil
  "Emacs MCP Agent Server."
  :group 'applications
  :prefix "emacs-mcp-agent-")

(defcustom emacs-mcp-agent-enabled-agents 'all
  "List of enabled agents, or 'all for all agents."
  :type '(choice (const all)
                 (repeat string))
  :group 'emacs-mcp-agent)

;;; Server Class Definition

(defclass emacs-mcp-agent-server (mcp-server)
  ((name :initform "emacs-agent-server")
   (version :initform "1.0.0")
   (agents :initform nil :documentation "List of registered agents"))
  "MCP agent server for delegating tasks to Emacs-based agents.")

;;; Server Instance Management

(defvar emacs-mcp-agent-server-instance nil
  "The current MCP agent server instance.")

(defvar emacs-mcp-agent-server-running nil
  "Whether the MCP agent server is currently running.")

;;; Agent Enumeration

(cl-defmethod mcp-server-enumerate-tools ((this emacs-mcp-agent-server))
  "Return list of available agent tools."
  (emacs-mcp-agent-get-enabled-agents))

;;; Server Management Functions

;;;###autoload
(defun emacs-mcp-agent-install-stdio-script ()
  "Install the stdio transport script for MCP agent clients."
  (interactive)
  (let ((script-path "~/.emacs.d/emacs-mcp-agent-stdio.sh"))
    (with-temp-file (expand-file-name script-path)
      (insert (emacs-mcp-agent-generate-stdio-script)))
    (shell-command (format "chmod +x %s" (expand-file-name script-path)))
    (message "MCP agent stdio script installed. Use: claude mcp add -s user -t stdio emacs-agent-server -- ~/.emacs.d/emacs-mcp-agent-stdio.sh --init-function=emacs-mcp-agent-start-server --stop-function=emacs-mcp-agent-stop-server")))

;;;###autoload
(defun emacs-mcp-agent-start-server ()
  "Start the MCP agent server."
  (interactive)
  (if emacs-mcp-agent-server-running
      (message "MCP agent server already running")
    (setq emacs-mcp-agent-server-instance (emacs-mcp-agent-server))
    (emacs-mcp-agent-register-all-agents)
    (setq emacs-mcp-agent-server-running t)
    (message "MCP agent server started with %d agents"
             (length (emacs-mcp-agent-get-enabled-agents)))))

;;;###autoload
(defun emacs-mcp-agent-stop-server ()
  "Stop the MCP agent server."
  (interactive)
  (when emacs-mcp-agent-server-running
    (setq emacs-mcp-agent-server-instance nil
          emacs-mcp-agent-server-running nil)
    (message "MCP agent server stopped")))

;;;###autoload
(defun emacs-mcp-agent-restart-server ()
  "Restart the MCP agent server."
  (interactive)
  (emacs-mcp-agent-stop-server)
  (emacs-mcp-agent-start-server))

;;;###autoload
(defun emacs-mcp-agent-server-status ()
  "Show MCP agent server status."
  (interactive)
  (if emacs-mcp-agent-server-running
      (message "MCP agent server running with %d agents"
               (length (emacs-mcp-agent-get-enabled-agents)))
    (message "MCP agent server not running")))

;;; Internal Functions

(defun emacs-mcp-agent-get-enabled-agents ()
  "Get list of enabled agent IDs."
  (if (eq emacs-mcp-agent-enabled-agents 'all)
      (mapcar #'car (emacs-mcp-agent-get-agents))
    emacs-mcp-agent-enabled-agents))

(defun emacs-mcp-agent-register-all-agents ()
  "Register all enabled agents with the server."
  (let ((enabled-agents (emacs-mcp-agent-get-enabled-agents)))
    (dolist (agent (emacs-mcp-agent-get-agents))
      (let ((id (car agent))
            (def (cdr agent)))
        (when (member id enabled-agents)
          (oset emacs-mcp-agent-server-instance agents
                (cons def (oref emacs-mcp-agent-server-instance agents))))))))

(defun emacs-mcp-agent-generate-stdio-script ()
  "Generate the stdio transport script content."
  "#!/bin/bash

# emacs-mcp-agent-stdio.sh - stdio transport script for Emacs MCP agent server
# This script provides stdio-based JSON-RPC communication for MCP clients
# to interact with Emacs agent server through emacsclient.

# Default functions for MCP server lifecycle
INIT_FUNCTION=\"emacs-mcp-agent-start-server\"
STOP_FUNCTION=\"emacs-mcp-agent-stop-server\"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --init-function=*)
            INIT_FUNCTION=\"${1#*=}\"
            shift
            ;;
        --stop-function=*)
            STOP_FUNCTION=\"${1#*=}\"
            shift
            ;;
        *)
            echo \"Unknown option: $1\" >&2
            exit 1
            ;;
    esac
done

# Function to check if Emacs daemon is running
check_emacs_daemon() {
    if ! emacsclient -e \"(+ 1 1)\" >/dev/null 2>&1; then
        echo \"Error: Emacs daemon not running. Start with: emacs --daemon\" >&2
        exit 1
    fi
}

# Function to start MCP server
start_mcp_server() {
    emacsclient -e \"(progn (require '$INIT_FUNCTION) ($INIT_FUNCTION))\" >/dev/null 2>&1
}

# Function to stop MCP server on exit
cleanup() {
    if [[ -n \"$STOP_FUNCTION\" ]]; then
        emacsclient -e \"($STOP_FUNCTION)\" >/dev/null 2>&1
    fi
}

# Set up cleanup trap
trap cleanup EXIT

# Check if Emacs daemon is running
check_emacs_daemon

# Start the MCP server
start_mcp_server

# Main stdio loop - forward stdin/stdout between MCP client and emacsclient
while IFS= read -r line; do
    # Send JSON-RPC request to Emacs and get response
    response=$(emacsclient -e \"(emacs-mcp-agent-handle-request '$line')\" 2>/dev/null)
    
    # Forward response back to client
    if [[ -n \"$response\" ]]; then
        echo \"$response\"
    fi
done")

;;; Request Handler

(defun emacs-mcp-agent-handle-request (request-json)
  "Handle MCP JSON-RPC request for agent server."
  (when emacs-mcp-agent-server-instance
    (mcp-server-process-request 
     emacs-mcp-agent-server-instance 
     request-json
     (lambda (response) response))))

;;; Keybindings

(defvar emacs-mcp-agent-map (make-sparse-keymap)
  "Keymap for MCP agent server commands.")

(define-key emacs-mcp-agent-map (kbd "i") #'emacs-mcp-agent-install-stdio-script)
(define-key emacs-mcp-agent-map (kbd "s") #'emacs-mcp-agent-start-server)
(define-key emacs-mcp-agent-map (kbd "k") #'emacs-mcp-agent-stop-server)
(define-key emacs-mcp-agent-map (kbd "r") #'emacs-mcp-agent-restart-server)
(define-key emacs-mcp-agent-map (kbd "?") #'emacs-mcp-agent-server-status)

(global-set-key (kbd "C-c g a") emacs-mcp-agent-map)

(provide 'emacs-mcp-agent-server)
;;; emacs-mcp-agent-server.el ends here