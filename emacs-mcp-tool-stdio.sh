#!/bin/bash

# emacs-mcp-tool-stdio.sh - stdio transport script for Emacs MCP tool server
# This script provides stdio-based JSON-RPC communication for MCP clients
# to interact with Emacs through emacsclient.

# Default functions for MCP server lifecycle
INIT_FUNCTION="emacs-mcp-tool-start-server"
STOP_FUNCTION="emacs-mcp-tool-stop-server"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --init-function=*)
            INIT_FUNCTION="${1#*=}"
            shift
            ;;
        --stop-function=*)
            STOP_FUNCTION="${1#*=}"
            shift
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
done

# Function to check if Emacs daemon is running
check_emacs_daemon() {
    if ! emacsclient -e "(+ 1 1)" >/dev/null 2>&1; then
        echo "Error: Emacs daemon not running. Start with: emacs --daemon" >&2
        exit 1
    fi
}

# Function to start MCP server
start_mcp_server() {
    emacsclient -e "(progn (require '$INIT_FUNCTION) ($INIT_FUNCTION))" >/dev/null 2>&1
}

# Function to stop MCP server on exit
cleanup() {
    if [[ -n "$STOP_FUNCTION" ]]; then
        emacsclient -e "($STOP_FUNCTION)" >/dev/null 2>&1
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
    response=$(emacsclient -e "(mcp-server-lib-handle-request '$line')" 2>/dev/null)
    
    # Forward response back to client
    if [[ -n "$response" ]]; then
        echo "$response"
    fi
done