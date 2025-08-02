# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

This project implements TWO different MCP (Model Context Protocol) servers using two different Emacs MCP libraries, each optimized for different use cases:

### 1. Tool Server (`emacs-mcp-tool-server.el`) - Synchronous Operations
- **Library**: `mcp-server-lib` 
- **Purpose**: Direct, synchronous Emacs operations (buffer management, elisp evaluation, etc.)
- **Tool Pattern**: `(defun my-tool (arguments) (let ((param (gethash "param" arguments))) ...))`
- **Registration**: `(mcp-server-lib-register-tool #'function :id "tool-id" :description "...")`
- **Transport**: `~/.emacs.d/emacs-mcp-stdio.sh --init-function=... --stop-function=...`
- **Error Handling**: `(mcp-server-lib-with-error-handling ...)`

### 2. Agent Server (`emacs-mcp-agent-server.el`) - Asynchronous Agents  
- **Library**: `mcp-server` (class-based)
- **Purpose**: Complex, async tasks using GPTel agents with tool access
- **Tool Pattern**: Class-based with `:async-lambda (lambda (request arguments cb-response) ...)`
- **Registration**: `(cl-defmethod mcp-server-enumerate-tools ((this my-server)) '(...))`
- **Transport**: `emacs --script mcp-server-headless-stdio-transport.el <server-file> <class-name>`
- **Response**: `(mcp-server-write-tool-call-text-result request result cb-response)`

### Why Two Different Libraries?

**`mcp-server-lib` (Tool Server):**
- Function-based, simple registration
- Synchronous execution with immediate responses
- Works through emacsclient to running daemon
- Perfect for direct Emacs operations

**`mcp-server` (Agent Server):**
- Class-based with method overrides
- Asynchronous execution with callback support
- Runs as headless Emacs process
- Essential for GPTel integration and complex agent workflows

### Transport Mechanisms

This project uses **stdio transport** which is the standard for MCP (Model Context Protocol) and preferred by Claude Code for reliability and simplicity.

**Tool Server Transport (`mcp-server-lib`):**
- Uses sophisticated `emacs-mcp-stdio.sh` script (from mcp-server-lib package)
- Communicates via emacsclient to running Emacs daemon
- Features: base64 encoding, debug logging, error handling, socket support
- Pattern: `External LLM → stdio script → emacsclient → Emacs daemon`

**Agent Server Transport (`mcp-server`):**
- Uses original `mcp-server-headless-stdio-transport.el` (from mcp-server package)  
- Spawns headless Emacs process per session using `read-from-minibuffer ""`
- Features: promising-future pattern, timeout handling, proper async callbacks
- Pattern: `External LLM → headless Emacs → process → stdout`

### Why stdio Transport (Not File Transport)?

The `mcp-server` package includes three transport options:
1. **stdio transport** (headless) - ✅ **Used for agent server**
2. **file transport** (`server.sh`) - Uses temp files, good for development/testing
3. **custom transport** - Not recommended

**stdio transport is preferred because:**
- Standard MCP protocol compliance  
- No file system dependencies or cleanup
- Better isolation (separate process per session)
- Self-contained (no daemon dependency for agents)
- Direct stdin/stdout communication
- Preferred by Claude Code ecosystem

### Core Components

- **MCP Tools**: Synchronous functions (hello_world, get_emacs_version, search_buffers)
- **MCP Agents**: Asynchronous GPTel-based agents (documentation_agent, etc.)
- **MCP Resources**: Data sources LLMs can read (emacs://version, emacs://buffer/current)
- **Dual Transport**: Two different stdio mechanisms for different use cases

## Setup and Usage

### One-Time Setup
1. Install packages: `M-x package-install RET mcp-server-lib RET` and `M-x package-install RET mcp-server RET`
2. Load packages: `(require 'emacs-mcp-tool-server)` and `(require 'emacs-mcp-agent-server)`
3. Install stdio script: `M-x emacs-mcp-tool-install-stdio-script` 
4. Ensure Emacs daemon is running: `emacs --daemon` or `M-x server-start`

### Register with Claude Desktop
```bash
# Tool Server (synchronous tools) - uses mcp-server-lib
claude mcp add -s user -t stdio emacs-tool-server -- ~/.emacs.d/emacs-mcp-stdio.sh --init-function=emacs-mcp-tool-start-server --stop-function=emacs-mcp-tool-stop-server

# Agent Server (asynchronous agents) - uses mcp-server headless transport
claude mcp add -s user -t stdio emacs-agent-server -- emacs --script ~/.emacs.d/elpa/mcp-server/mcp-server-headless-stdio-transport.el ~/.emacs.d/lisp/emacs-mcp-tool-server/emacs-mcp-agent-server.el emacs-mcp-agent-server
```

### Register with Claude Code
Add to Claude Code MCP config:
```json
{
  "mcpServers": {
    "emacs-tool-server": {
      "command": "~/.emacs.d/emacs-mcp-stdio.sh",
      "args": ["--init-function=emacs-mcp-tool-start-server", "--stop-function=emacs-mcp-tool-stop-server"]
    },
    "emacs-agent-server": {
      "command": "emacs",
      "args": [
        "--script", 
        "~/.emacs.d/elpa/mcp-server/mcp-server-headless-stdio-transport.el",
        "~/.emacs.d/lisp/emacs-mcp-tool-server/emacs-mcp-agent-server.el",
        "emacs-mcp-agent-server"
      ]
    }
  }
}
```

## Key Functions

### Tool Server Management (mcp-server-lib)
- `emacs-mcp-tool-install-stdio-script`: Install the transport script (one-time)
- `emacs-mcp-tool-start-server`: Start tool server and register tools/resources
- `emacs-mcp-tool-stop-server`: Stop the tool server
- `emacs-mcp-tool-restart-server`: Restart the tool server  
- `emacs-mcp-tool-server-status`: Check if tool server is running
- `emacs-mcp-tool-show-metrics`: View usage statistics

### Agent Server (mcp-server)
- Agent server uses headless transport - no explicit start/stop functions needed
- Server lifecycle managed by `mcp-server-headless-stdio-transport.el`
- Tools are enumerated via `cl-defmethod mcp-server-enumerate-tools`

### Available Tools (for LLMs)
- **hello_world**: Test tool that returns hello message
- **get_emacs_version**: Returns detailed Emacs version/build info  
- **search_buffers**: Search content across open buffers
- **get_buffer_list**: Get list of open buffers

### Available Agents (for LLMs)
- **documentation_agent**: Agentic documentation explorer using introspection tools

### Available Resources (for LLMs)
- **emacs://version**: Detailed Emacs version, features, and configuration
- **emacs://buffer/current**: Current buffer contents and metadata

## Keybindings

Tool server keybindings use the `C-c g t` prefix:
- `C-c g t i`: Install stdio script
- `C-c g t s`: Start tool server
- `C-c g t k`: Stop tool server  
- `C-c g t r`: Restart tool server
- `C-c g t ?`: Show server status
- `C-c g t m`: Show metrics
- `C-c g t t`: List tools
- `C-c g t R`: List resources

## Use Package Configuration

```elisp
;; Tool Server (synchronous operations)
(use-package emacs-mcp-tool-server
  :load-path "~/.emacs.d/lisp/emacs-mcp-tool-server"
  :after (mcp-server-lib)
  :config
  ;; Auto-start tool server when Emacs loads  
  (emacs-mcp-tool-start-server))

;; Agent Server (asynchronous GPTel agents)
(use-package emacs-mcp-agent-server
  :load-path "~/.emacs.d/lisp/emacs-mcp-tool-server" 
  :after (mcp-server gptel)
  :config
  ;; Agent server uses headless transport - no auto-start needed
  (message "Emacs MCP agent server ready for headless transport"))
```

## Extension Points for Future Development

### Adding New Tools
```elisp
(mcp-server-lib-register-tool
 #'my-function
 :id "tool_name"  
 :description "What this tool does")

(defun my-function (param1 param2)
  "Tool handler with parameters.
  
MCP Parameters:
  param1 - description
  param2 - description"
  (mcp-server-lib-with-error-handling
    ;; Implementation here
    ))
```

### Adding Resources
```elisp
;; Static resource
(mcp-server-lib-register-resource "emacs://my-resource"
  #'my-resource-function
  :name "Resource Name"
  :description "What this provides")

;; Template resource with variables  
(mcp-server-lib-register-resource "emacs://buffer/{name}"
  (lambda (params)
    (let ((name (alist-get "name" params nil nil #'string=)))
      ;; Return buffer contents for named buffer
      ))
  :name "Buffer by name")
```

### GPTel Agent Integration (Future)
```elisp
(mcp-server-lib-register-tool
 #'delegate-to-gptel-agent
 :id "delegate_to_agent"
 :description "Delegate complex tasks to specialized GPTel agents")

(defun delegate-to-gptel-agent (agent-type task)
  "Delegate TASK to AGENT-TYPE using gptel."
  ;; Integration with gptel for specialized agents
  )
```

## Development Commands

### Testing

**Tool Server Testing:**
- Test tools: Use `mcp-server-lib-ert` utilities for comprehensive testing
- Debug: `(setq mcp-server-lib-log-io t)` to log JSON-RPC messages
- Test script: `~/.emacs.d/emacs-mcp-stdio.sh --init-function=emacs-mcp-tool-start-server`

**Agent Server Testing:**
- Load file: `M-x load-file RET emacs-mcp-agent-server.el RET`
- Test headless: `emacs --script ~/.emacs.d/elpa/mcp-server/mcp-server-headless-stdio-transport.el ~/.emacs.d/lisp/emacs-mcp-tool-server/emacs-mcp-agent-server.el emacs-mcp-agent-server`
- Manual JSON-RPC test: Echo JSON request to the headless command

### Debugging

**Tool Server Debugging:**
- Enable logging: `(setq mcp-server-lib-log-io t)`
- View logs: Check `*mcp-server-lib-log*` buffer  
- Set environment: `EMACS_MCP_DEBUG_LOG=/path/to/logfile`

**Agent Server Debugging:**
- No built-in logging in headless mode
- Add `(message ...)` statements to `emacs-mcp-agent-server.el` for debugging
- Check stderr output from headless Emacs process
- Use file transport temporarily for development with extensive logging

## Files

- `emacs-mcp-agent-server.el` - Main implementation
- `CLAUDE.md` - This file (development guidance)
- `README.md` - User documentation

## Dependencies

- Emacs 27.1+
- `mcp-server-lib` package (from MELPA)
- `gptel` package (for future agent features)
- Running Emacs daemon for stdio transport

## Important Notes

- The server uses stdio transport, not network sockets
- Requires Emacs daemon to be running (`emacs --daemon` or `M-x server-start`)
- Tools are executed in the Emacs environment with full access to buffers, modes, etc.
- Use `mcp-server-lib-with-error-handling` for robust error handling in tools
- Resources can be static or templated with URI variables using `{variable}` syntax