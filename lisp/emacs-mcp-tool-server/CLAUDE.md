# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

This project implements a single MCP (Model Context Protocol) server for Emacs using the `mcp-server-lib` library:

**Tool Server (`emacs-mcp-tool-server.el`)**
- **Library**: `mcp-server-lib` 
- **Purpose**: Direct, synchronous Emacs operations (buffer management, elisp evaluation, etc.)
- **Tool Pattern**: `(defun my-tool (arguments) (let ((param (gethash "param" arguments))) ...))`
- **Registration**: `(mcp-server-lib-register-tool #'function :id "tool-id" :description "...")`
- **Transport**: `~/.emacs.d/emacs-mcp-stdio.sh --init-function=... --stop-function=...`
- **Error Handling**: `(mcp-server-lib-with-error-handling ...)`

### Why `mcp-server-lib`?

- Function-based, simple registration
- Synchronous execution with immediate responses
- Works through `emacsclient` to a running daemon
- Perfect for direct Emacs operations
- Future agent functionality will be implemented as tools within this server

## Direct Tool Access Architecture

This project provides **direct, immediate access** to Emacs functionality through MCP tools. Rather than using complex agentic workflows that require multiple round-trips and "babysitting", all tools return results immediately and synchronously.

### Core Philosophy

**Direct > Agentic**: When a model needs Emacs functionality, it calls the tool and gets the result immediately. No job management, no polling, no complex async patterns. This approach is:

- **More efficient**: Single tool call vs multiple agent management calls
- **More reliable**: Immediate results vs async failures and timeouts  
- **Simpler**: Direct function calls vs complex job orchestration
- **More predictable**: Deterministic results vs agent variability

### Documentation Tools

The server provides three core documentation tools that give immediate access to Emacs' introspection capabilities:

#### `find_symbols_by_name(keyword)`
- Searches for functions and variables matching a keyword
- Returns formatted list with brief descriptions
- Perfect for discovering relevant Emacs functionality

#### `helpful_function_inspect(function_name)`  
- Returns comprehensive function documentation using the `helpful` package
- Includes signature, documentation, source code, and references
- Provides the same detailed view a human developer would see

#### `helpful_variable_inspect(variable_name)`
- Returns comprehensive variable documentation using the `helpful` package
- Shows current value, documentation, customization options
- Includes all metadata about the variable

### Tool Integration Pattern

These tools are designed to work together in a simple, linear fashion:

1. **Discover** relevant symbols with `find_symbols_by_name`
2. **Inspect** specific functions/variables with the `helpful_*` tools
3. **Get immediate, comprehensive results** without any additional coordination

### Benefits Over Agentic Approaches

- **No timeout issues**: All operations complete immediately
- **No resource management**: No need to track jobs, poll status, or manage lifecycles
- **No error complexity**: Simple function calls with immediate error handling
- **Better user experience**: Users get answers in one step, not multiple waiting periods
- **Predictable performance**: Every call has consistent, immediate response time

This direct approach maintains all the power of sophisticated documentation analysis while eliminating the overhead and complexity of agent orchestration.

## Transport Mechanism (stdio)

The project uses stdio transport via the `mcp-server-lib` stdio script:

- **Script**: `~/.emacs.d/emacs-mcp-stdio.sh` (installed by `emacs-mcp-tool-install-stdio-script`)
- **Communication**: Via `emacsclient` to the running Emacs daemon
- **Features**: Base64 encoding, debug logging, error handling, socket support
- **Pattern**: `External LLM → stdio script → emacsclient → Emacs daemon`

### Why stdio transport?

- Standard MCP protocol compliance
- No file system temp-file dependencies or cleanup
- Reliable and simple
- Direct stdin/stdout communication
- Preferred in the Claude Code ecosystem

## Core Components

- **MCP Tools**: Synchronous functions (e.g., `hello_world`, `get_emacs_version`, `search_buffers`, `get_buffer_list`)
- **MCP Resources**: Data sources LLMs can read (e.g., `emacs://version`, `emacs://buffer/current`)
- **Transport**: `mcp-server-lib` stdio script to Emacs daemon

## Setup and Usage

### One-Time Setup

1. Install package: `M-x package-install RET mcp-server-lib RET`
2. Load package: `(require 'emacs-mcp-tool-server)`
3. Install the stdio script: `M-x emacs-mcp-tool-install-stdio-script`
4. Ensure Emacs daemon is running: `emacs --daemon` or `M-x server-start`

### Register with Claude Desktop

```bash
claude mcp add -s user -t stdio emacs-tool-server -- ~/.emacs.d/emacs-mcp-stdio.sh --init-function=emacs-mcp-tool-start-server --stop-function=emacs-mcp-tool-stop-server
```

### Register with Claude Code

Add to Claude Code MCP config:

```json
{
  "mcpServers": {
    "emacs-tool-server": {
      "command": "~/.emacs.d/emacs-mcp-stdio.sh",
      "args": [
        "--init-function=emacs-mcp-tool-start-server",
        "--stop-function=emacs-mcp-tool-stop-server"
      ]
    }
  }
}
```

## Key Functions

- `emacs-mcp-tool-install-stdio-script`: Install the transport script (one-time)
- `emacs-mcp-tool-start-server`: Start tool server and register tools/resources
- `emacs-mcp-tool-stop-server`: Stop the tool server
- `emacs-mcp-tool-restart-server`: Restart the tool server
- `emacs-mcp-tool-server-status`: Check if tool server is running
- `emacs-mcp-tool-show-metrics`: View usage statistics

## Available Tools (for LLMs)

- `hello_world`: Test tool that returns a hello message
- `get_emacs_version`: Returns detailed Emacs version/build info
- `search_buffers`: Search content across open buffers
- `get_buffer_list`: Get list of open buffers
- `eval_elisp`: Safely evaluate Emacs Lisp code
- `find_symbols_by_name`: Search for Emacs Lisp symbols (functions and variables) by keyword
- `helpful_function_inspect`: Get comprehensive documentation for any Emacs Lisp function
- `helpful_variable_inspect`: Get comprehensive documentation for any Emacs Lisp variable

## Available Resources (for LLMs)

- `emacs://version`: Detailed Emacs version, features, and configuration
- `emacs://buffer/current`: Current buffer contents and metadata

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

## Use-Package Configuration

```elisp
(use-package emacs-mcp-tool-server
  :load-path "~/.emacs.d/lisp/emacs-mcp-tool-server"
  :after (mcp-server-lib)
  :config
  ;; Auto-start tool server when Emacs loads
  (emacs-mcp-tool-start-server))
```

## Extension Points

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

### Agent Integration

Future agent functionality (using GPTel or other AI tools) will be implemented as tools within this server:

```elisp
(mcp-server-lib-register-tool
 #'my-agent-tool
 :id "agent_task"
 :description "Delegate complex tasks to specialized agents")

(defun my-agent-tool (task-type task-description)
  "Delegate TASK-DESCRIPTION to specialized agent of TASK-TYPE."
  (mcp-server-lib-with-error-handling
    ;; Integration with gptel or other agent frameworks
    ))
```

## Development Commands

### Testing

- Test tools: Use `mcp-server-lib-ert` utilities where available
- Debug: `(setq mcp-server-lib-log-io t)` to log JSON-RPC messages
- Test script: `~/.emacs.d/emacs-mcp-stdio.sh --init-function=emacs-mcp-tool-start-server`

### Debugging

- Enable logging: `(setq mcp-server-lib-log-io t)`
- View logs: Check `*mcp-server-lib-log*` buffer
- Environment variable: `EMACS_MCP_DEBUG_LOG=/path/to/logfile`

## Files

- `emacs-mcp-tool-server.el` — Main implementation
- `emacs-mcp-tool-tools.el` — Tool definitions
- `CLAUDE.md` — This file (development guidance)
- `README.md` — User documentation

## Dependencies

- Emacs 27.1+
- `mcp-server-lib` package (from MELPA)
- Running Emacs daemon for stdio transport

## Important Notes

- The server uses stdio transport, not network sockets
- Requires Emacs daemon to be running (`emacs --daemon` or `M-x server-start`)
- Tools are executed in the Emacs environment with full access to buffers, modes, etc.
- Use `mcp-server-lib-with-error-handling` for robust error handling in tools
- Resources can be static or templated with URI variables using `{variable}` syntax

## Implementation Status (2025-08-03)

### ✅ Completed: Direct Tool Architecture

**Core Achievement:** Successfully simplified from complex agentic infrastructure to direct, immediate tool access.

**Architectural Change:**
- **Removed**: Complex agent job system, async patterns, GPTel integration, resource-based job tracking
- **Added**: Direct documentation tools with immediate, synchronous results
- **Result**: Much simpler, more reliable, and more efficient tool interface

**Working Tools:**
- `find_symbols_by_name(keyword)` - Search symbols by keyword with immediate results
- `helpful_function_inspect(function_name)` - Get comprehensive function documentation
- `helpful_variable_inspect(variable_name)` - Get comprehensive variable documentation
- All existing tools: `hello_world`, `get_emacs_version`, `search_buffers`, `get_buffer_list`, `eval_elisp`

**User Experience Achieved:**
```
Direct: find_symbols_by_name("mapcar") → immediate symbol list
Detailed: helpful_function_inspect("mapcar") → comprehensive docs
Simple: Single tool call gets complete results
```

**Success Metrics:**
- ✅ Eliminated all timeout issues (immediate responses)
- ✅ Removed complex job management overhead  
- ✅ Simplified from multi-step agent workflows to single tool calls
- ✅ Maintained full documentation analysis capabilities
- ✅ Works seamlessly in both Claude Code and Claude Desktop

### ✅ Resolved: MCP Resource Protocol

**Problem:** MCP Inspector was showing undefined URI errors in `resources/list` calls.

**Root Cause:** Duplicate resource templates were registered - both old `{agent_type}` and new `{type}` URI formats caused conflicts in the resource registry.

**Solution:** 
- Cleared mcp-server-lib hash tables and re-registered clean resources
- Confirmed mcp-server-lib properly implements MCP resource listing protocol
- All resource operations now work correctly:
  - `resources/list` returns proper resource metadata
  - `resources/read` successfully reads both static and templated resources

**Status:** ✅ **FULLY WORKING** - Resources are now properly accessible via MCP protocol.

### 🔄 Ongoing: MCP Resource Protocol Compliance (Low Priority)

**Problem:** MCP Inspector shows validation errors because `mcp-server-lib` violates the 2025-06-18 MCP specification by mixing concrete resources and templates in `resources/list` responses.

**Root Cause Analysis:**
- **Line 699-705 in mcp-server-lib.el**: Concatenates both concrete resources and templates into single array
- **Missing handler**: No `resources/templates/list` method in dispatcher 
- **Validation failure**: Templates have `"uriTemplate"` field but `resources/list` requires `"uri"` field

**Attempted Solution:**
Implemented downstream fix in `emacs-mcp-tool-server.el`:
- Custom handlers `emacs-mcp-tool--handle-resources-list` (concrete only) and `emacs-mcp-tool--handle-resources-templates-list`
- Dispatcher override via `advice-add` to intercept resource calls
- Enhanced metadata support for `title` and `annotations`

**Current Status:**
- ✅ Code implemented and added to server startup/shutdown
- ❌ Advice override not applying correctly during server restart
- ❌ Still seeing mixed resources in `resources/list` responses

**Files Modified:**
- `emacs-mcp-tool-server.el` lines 44-84: Custom handlers and dispatcher
- `emacs-mcp-tool-server.el` lines 103, 115: Override application in start/stop

**Why Low Priority:**
- Claude Code works fine with current resource implementation
- Main functionality (direct tool access) is complete and working perfectly
- Issue is primarily cosmetic for MCP Inspector compliance
- Core goal achieved: Live Emacs documentation access for AI agents

**Future Resolution Approach:**
1. Debug why `advice-add` isn't taking effect during server restart
2. Alternative: Hook into `mcp-server-lib-process-jsonrpc` instead of dispatcher
3. Alternative: Patch mcp-server-lib directly if needed for strict compliance
4. Test with actual MCP Inspector to confirm fix effectiveness

**Next Steps When Resuming:**
```elisp
;; Debug the advice system
(advice--p (advice--symbol-function 'mcp-server-lib--dispatch-jsonrpc-method))

;; Alternative hook approach
(advice-add 'mcp-server-lib-process-jsonrpc :around #'custom-processor)
```