# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

This is an Emacs Lisp package that implements an MCP (Model Context Protocol) server for delegating tasks to Emacs from external LLMs. The package uses the `mcp-server-lib` library to provide stdio-based communication through emacsclient, enabling Claude Desktop and Claude Code to interact with Emacs.

### How MCP Works with Emacs

The MCP server doesn't run on a network port. Instead, it uses stdio transport via an emacsclient wrapper script:

1. `mcp-server-lib` provides the MCP protocol implementation
2. `emacs-mcp-stdio.sh` script handles stdio communication with emacsclient  
3. External LLMs communicate through this script to the running Emacs daemon
4. Tools and resources are registered in Emacs and executed in the Emacs environment

### Core Components

- **MCP Tools**: Functions that LLMs can call (hello_world, get_emacs_version, eval_elisp)
- **MCP Resources**: Data sources LLMs can read (emacs://version, emacs://buffer/current)
- **Transport Layer**: stdio script that bridges JSON-RPC to emacsclient
- **Agent Framework**: Ready for extension with gptel-based specialized agents

## Setup and Usage

### One-Time Setup
1. Install package: `M-x package-install RET mcp-server-lib RET`
2. Load this package: `(require 'emacs-mcp-agent-server)`
3. Install stdio script: `M-x emacs-mcp-install-stdio-script` or `C-c g m i`
4. Ensure Emacs daemon is running: `emacs --daemon` or `M-x server-start`

### Register with Claude Desktop
```bash
claude mcp add -s user -t stdio emacs-agent-server -- ~/.emacs.d/emacs-mcp-stdio.sh --init-function=emacs-mcp-start-server --stop-function=emacs-mcp-stop-server
```

### Register with Claude Code
Add to Claude Code MCP config:
```json
{
  "mcpServers": {
    "emacs-agent-server": {
      "command": "~/.emacs.d/emacs-mcp-stdio.sh",
      "args": ["--init-function=emacs-mcp-start-server", "--stop-function=emacs-mcp-stop-server"]
    }
  }
}
```

## Key Functions

### Server Management
- `emacs-mcp-install-stdio-script`: Install the transport script (one-time)
- `emacs-mcp-start-server`: Start MCP server and register tools/resources
- `emacs-mcp-stop-server`: Stop the MCP server
- `emacs-mcp-restart-server`: Restart the server  
- `emacs-mcp-server-status`: Check if server is running
- `emacs-mcp-show-metrics`: View usage statistics

### Available Tools (for LLMs)
- **hello_world**: Test tool that returns hello message
- **get_emacs_version**: Returns detailed Emacs version/build info
- **eval_elisp**: Safely evaluate Emacs Lisp code with error handling

### Available Resources (for LLMs)
- **emacs://version**: Detailed Emacs version, features, and configuration
- **emacs://buffer/current**: Current buffer contents and metadata

## Keybindings

All keybindings use the `C-c g m` prefix:
- `C-c g m i`: Install stdio script
- `C-c g m s`: Start MCP server
- `C-c g m k`: Stop MCP server  
- `C-c g m r`: Restart MCP server
- `C-c g m ?`: Show server status
- `C-c g m m`: Show metrics

## Use Package Configuration

```elisp
(use-package emacs-mcp-agent-server
  :load-path "~/.emacs.d/lisp/emacs-mcp-agent-server"
  :after (mcp-server-lib)
  :config
  ;; Auto-start server when Emacs loads
  (emacs-mcp-start-server))
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
- Load file: `M-x load-file RET emacs-mcp-agent-server.el RET`
- Test tools: Use `mcp-server-lib-ert` utilities for comprehensive testing
- Debug: `(setq mcp-server-lib-log-io t)` to log JSON-RPC messages

### Debugging
- Enable logging: `(setq mcp-server-lib-log-io t)`
- View logs: Check `*mcp-server-lib-log*` buffer  
- Set environment: `EMACS_MCP_DEBUG_LOG=/path/to/logfile`
- Test script: `~/.emacs.d/emacs-mcp-stdio.sh --init-function=emacs-mcp-start-server`

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