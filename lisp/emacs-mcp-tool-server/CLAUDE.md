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

## Agent Architecture & Vision

### Overview

This tool server serves as the foundation for a comprehensive **agent ecosystem** within Emacs. The documentation agent currently being developed is a **prototype** that establishes patterns for much more sophisticated agents with direct vision into Emacs' live environment.

### The Agent Dream

**Current State**: Basic MCP tools providing synchronous Emacs operations  
**Vision**: Sophisticated agents with direct access to live Emacs state, capable of complex analysis, automation, and collaboration

**Key Insight**: This documentation agent prototype will enable agents with direct vision into:
- **Live code execution** (compilation, testing, debugging)
- **Live project state** (file changes, git status, running processes)  
- **Live user workflow** (editing patterns, command usage, productivity metrics)
- **Live system integration** (databases, APIs, external tools)

### Core Agent Infrastructure

#### 1. Universal Job Management System
```elisp
;; Agent-agnostic job system using resource-based async patterns
;; Any agent can create jobs, track progress, return results
(defvar mcp-agent-jobs (make-hash-table :test 'equal))

;; Resource templates for status tracking
;; Pattern: emacs://agent/{type}/{id}/{status|logs|result}
```

**Benefits:**
- Scales to hundreds of parallel agent operations
- No polling overhead (resources read on-demand)
- Works within client timeout constraints
- Consistent progress tracking across all agent types

#### 2. Immediate + Background Pattern
**The Core Pattern**: Return quick results immediately (<100ms), continue deep analysis in background

```elisp
(defun agent-task-start (domain query)
  ;; Phase 1: Immediate response (quick search, basic analysis)
  (let ((quick-results (domain-quick-search query))
        (job-id (start-background-analysis domain query)))
    ;; Return partial results + resource URIs for full results
    (format "Quick analysis: %s\n\nDeep analysis: emacs://agent/%s/%s/result" 
            quick-results domain job-id)))
```

**Applications:**
- **Documentation Agent**: Quick symbol lookup + deep introspection analysis
- **Code Agent**: Syntax check + comprehensive static analysis
- **Project Agent**: Current status + trend analysis & planning

#### 3. Agent Tool Integration Layer
**Standardized tool access** for all agents:

```elisp
;; Agent declares tool dependencies
(defvar doc-agent-tools '(find_symbols_by_name helpful_function_inspect))
(defvar code-agent-tools '(compile_buffer run_tests static_analysis))

;; Universal tool invocation
(agent-invoke-tool agent-type tool-name params callback)
```

#### 4. Result Management & Streaming
**Chunked results** for large datasets:
- Return first chunk immediately (e.g., first 20 functions)
- Register remaining chunks as resources
- "Streaming" simulation via incremental tool calls
- Progressive enhancement of results

### Async Patterns & Research Findings

#### Resource-Based Async (MCP Standard)
**Research Finding**: Most efficient pattern for MCP operations without progress notification support.

**Pattern**: 
1. Tool returns immediately with resource URIs
2. Background work updates resource state  
3. Client reads resources on-demand (no polling overhead)
4. Much more efficient than job/poll patterns

```elisp
;; Immediate return with resource URIs
"Analysis started: emacs://doc-agent/job-123/status"

;; Resource templates provide real-time status
(mcp-server-lib-register-resource "emacs://doc-agent/{id}/status"
  #'doc-agent-status-handler)
```

#### Battle-Tested Patterns
Based on research of production MCP servers (Firecrawl, etc.):

- **Immediate + Background**: Quick partial results + deep analysis
- **Chunked results**: Handle large datasets without timeouts
- **Resource-based status**: Real-time progress without polling
- **Cross-client compatibility**: Works with strict timeout limits

### Future Agent Types & Capabilities

#### Code Analysis Agents
**Immediate**: Syntax check, basic metrics, quick symbol lookup  
**Background**: Deep static analysis, dependency graphs, security scanning, cross-references  
**Tools**: AST parsing, symbol resolution, compilation, testing frameworks

#### Project Management Agents  
**Immediate**: Current status, quick searches, immediate issues  
**Background**: Trend analysis, automated planning, resource optimization, workflow analysis  
**Tools**: Git history, file system monitoring, task tracking, CI/CD integration

#### Research & Writing Agents
**Immediate**: Existing notes, quick searches, citation lookup  
**Background**: Deep synthesis, cross-referencing, comprehensive analysis  
**Tools**: Org-mode integration, bibliography management, web research, document analysis

#### Development Workflow Agents
**Immediate**: Current state, quick fixes, immediate diagnostics  
**Background**: Comprehensive testing, deployment pipelines, monitoring, optimization  
**Tools**: Testing frameworks, CI/CD systems, monitoring tools, performance analysis

### Architectural Decision Process

#### When to Escalate Complex Decisions
For major architectural decisions or roadblocks, escalate to web research agent with focused prompts:

**Example Research Prompts:**
- "Best practices for multi-agent coordination in MCP environments"
- "Scalable async patterns for Emacs integration with external AI agents"  
- "Resource management strategies for long-running agent operations"

**Process:**
1. Identify architectural decision point or roadblock
2. Formulate specific, focused research prompt
3. Get external research and analysis  
4. Document decision rationale in CLAUDE.md
5. **Future**: Implement as tool call integration (not yet implemented)

#### Documentation-Driven Development
All architectural decisions and patterns must be documented in CLAUDE.md before implementation:
- Establishes clear patterns for future agents
- Enables consistent development across agent types
- Provides blueprint for sophisticated agent ecosystem

### Implementation Guidelines

#### Agent-Agnostic Design Principles
- **Universal job system**: Any agent can create and manage jobs
- **Consistent resource patterns**: `emacs://agent/{type}/{id}/{status|logs|result}`
- **Tool registry**: Agents declare tool dependencies explicitly
- **Composable results**: Agents can consume outputs from other agents

#### Resource URI Conventions
```
emacs://agent/{agent-type}/{job-id}/status   # Job status and progress
emacs://agent/{agent-type}/{job-id}/logs     # Incremental logs
emacs://agent/{agent-type}/{job-id}/result   # Final results
emacs://agent/{agent-type}/{job-id}/chunks/{n} # Chunked large results
```

#### Background Processing Standards
- Use `run-at-time` for non-blocking operations
- Update job state and resources incrementally
- Implement automatic cleanup with TTL
- Handle errors gracefully with detailed logging

#### Error Handling & Cleanup
- Jobs automatically cleaned up after TTL expiration
- Failed jobs provide detailed error information
- Partial results preserved even on failure
- Resource URIs remain valid during cleanup period

### The Documentation Agent Prototype

The current documentation agent implementation establishes these patterns:

1. **Immediate symbol lookup** + **background deep analysis**
2. **Resource-based status tracking** for long operations  
3. **Chunked results** for large searches (e.g., "all functions starting with js/")
4. **Tool integration** with GPTel and introspection tools
5. **Cross-client compatibility** with timeout resilience

**Success Metrics:**
- Timeouts eliminated for complex queries
- Immediate user feedback for all operations
- Scalable to hundreds of parallel documentation queries
- Foundation established for more sophisticated agents

This prototype validates the infrastructure and patterns needed for the full agent ecosystem vision.

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
- `documentation_agent`: Agentic documentation explorer using introspection tools

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