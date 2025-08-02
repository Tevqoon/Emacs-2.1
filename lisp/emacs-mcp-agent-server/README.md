# Emacs Hybrid MCP Agent Server

A Model Context Protocol (MCP) server that provides both direct Emacs operations and agentic delegation capabilities. External LLMs can use this server to perform immediate operations (symbol lookup, evaluation) or delegate complex analysis tasks to specialized AI agents running within Emacs via gptel.

## Features

### Hybrid Architecture
- **Direct Operations** (synchronous, <2s): Immediate responses for simple queries
- **Agentic Operations** (asynchronous, 30-120s): Complex analysis via specialized agents

### Direct Operations
- `describe_symbol` - Get comprehensive symbol documentation via helpful.el
- `search_symbols` - Find symbols matching patterns with type filtering
- `eval_elisp` - Safely evaluate Emacs Lisp with sanitization and confirmation
- `get_buffer_info` - Current buffer context (mode, point, region, project)
- `query_emacs_state` - Comprehensive Emacs session information

### Specialized Agents
- **documentation-explorer** - Analyzes functions, modes, and features
- **config-auditor** - Reviews configurations for conflicts and optimizations
- **code-analyzer** - Examines code patterns, structure, and relationships
- **context-helper** - Provides context-aware assistance

### Agentic Operations
- `delegate_analysis` - Route tasks to specialized agents
- `audit_configuration` - Deep configuration analysis
- `explore_codebase` - Pattern analysis across multiple files
- `synthesize_research` - Multi-source documentation synthesis

## Installation

### Prerequisites

1. **Emacs 27.1+**
2. **Required Packages:**
   ```elisp
   ;; Install via package.el or your preferred method
   (package-install 'gptel)      ; For LLM integration
   (package-install 'helpful)    ; Enhanced documentation
   ```

3. **MCP Server Library:**
   ```bash
   # Install utsahi/mcp-server.el
   git clone https://github.com/utsahi/mcp-server.el.git
   # Add to your load-path or install via package manager
   ```

4. **LLM API Setup:**
   Configure gptel with your preferred LLM provider:
   ```elisp
   ;; Example for Anthropic Claude
   (setq gptel-api-key "your-api-key-here")
   (setq gptel-model "claude-3-5-sonnet-20241022")
   
   ;; Or for OpenAI
   (setq gptel-api-key "your-openai-key")
   (setq gptel-backend (gptel-make-openai "ChatGPT" :key gptel-api-key))
   ```

### Installation Steps

1. **Download the Server:**
   ```bash
   cd ~/.emacs.d/lisp/
   git clone <this-repository> emacs-mcp-agent-server
   ```

2. **Add to Emacs Configuration:**
   ```elisp
   ;; Add to your init.el
   (add-to-list 'load-path "~/.emacs.d/lisp/emacs-mcp-agent-server")
   (require 'emacs-mcp-agent-server)
   
   ;; Optional: Auto-start server
   (add-hook 'after-init-hook #'js/mcp-auto-configure)
   ```

3. **Verify Installation:**
   ```elisp
   M-x js/mcp-auto-configure
   ```

## Quick Start

### 1. Start the Server
```elisp
;; In Emacs
M-x js/mcp-start-server

;; Or use keybinding
C-c g m s
```

### 2. Create Server Script (for external clients)
```elisp
M-x js/mcp-create-server-script
;; Creates mcp-server.sh in current directory
```

### 3. Test Direct Operations
```elisp
M-x js/mcp-test-direct-tool
;; Choose: describe_symbol
;; Symbol: org-mode
```

### 4. Test Agent Delegation
```elisp
M-x js/mcp-test-agent
;; Agent: documentation-explorer
;; Task: Analyze org-archive functions
```

## Usage

### Transport Modes

#### Development Mode (File Transport)
```bash
# Start server for development/testing
./mcp-server.sh

# Or manually
emacs --batch --load emacs-mcp-agent-server.el \
      --eval "(js/mcp-start-server-file-mode)"
```

#### Production Mode (STDIO Transport)
```bash
# Start server for external MCP clients
./mcp-server.sh --stdio

# Or manually
emacs --script emacs-mcp-agent-server.el js/emacs-hybrid-mcp-server
```

### MCP Client Integration

#### Example Tool Calls

**Direct Symbol Lookup:**
```json
{
  "method": "tools/call",
  "params": {
    "name": "describe_symbol",
    "arguments": {
      "symbol_name": "org-mode",
      "type": "function"
    }
  }
}
```

**Agent Delegation:**
```json
{
  "method": "tools/call", 
  "params": {
    "name": "delegate_analysis",
    "arguments": {
      "agent_type": "documentation-explorer",
      "task": "Analyze org-archive functions and their relationships",
      "include_context": true
    }
  }
}
```

**Configuration Audit:**
```json
{
  "method": "tools/call",
  "params": {
    "name": "audit_configuration", 
    "arguments": {
      "scope": "init-file",
      "focus": "conflicts"
    }
  }
}
```

#### Response Formats

**Direct Results:**
```json
{
  "type": "direct_result",
  "tool": "describe_symbol", 
  "data": {
    "symbol": "org-mode",
    "symbol_type": "function",
    "documentation": "...",
    "source_file": "/path/to/org.el",
    "bound": true,
    "function": true
  },
  "timestamp": "2025-01-XX"
}
```

**Agent Results:**
```json
{
  "type": "agent_result",
  "agent": "documentation-explorer",
  "job_id": "job-123-456", 
  "task": "Analyze org-archive functions",
  "response": "Detailed analysis...",
  "model": "claude-3-5-sonnet-20241022",
  "context_included": true,
  "timestamp": "2025-01-XX",
  "status": "success"
}
```

## Interactive Commands

### Server Management
- `C-c g m s` - Start server
- `C-c g m k` - Stop server  
- `C-c g m r` - Restart server
- `C-c g m S` - Show server status

### Development & Testing
- `C-c g m l` - List agents and active jobs
- `C-c g m t` - Test agent with custom task
- `C-c g m d` - Test direct tool
- `C-c g m g` - Show integration guide

### Transport Modes  
- `C-c g m f` - Start file transport mode
- `C-c g m o` - Start STDIO transport mode
- `C-c g m c` - Create server startup script

### Configuration
- `C-c g m a` - Auto-configure and check dependencies

## Configuration

### Customization Variables
```elisp
;; Server settings
(setq js/mcp-server-port 3000)              ; HTTP port (if used)
(setq js/mcp-agent-timeout 120)             ; Agent timeout (seconds)
(setq js/mcp-direct-timeout 30)             ; Direct operation timeout
(setq js/mcp-max-concurrent-agents 3)       ; Concurrent agent limit

;; Safety settings
(setq js/mcp-enable-logging t)              ; Enable detailed logging
(setq js/mcp-require-confirmation t)        ; Require eval_elisp confirmation
```

### Agent Configuration
```elisp
;; Customize agent prompts and models
(setq js/mcp-agent-configs
  '((documentation-explorer
     :model "claude-3-5-sonnet-20241022"
     :temperature 0.1
     :max-tokens 4000
     :system-prompt "Custom prompt..."
     :tools (helpful_function_inspect find_symbols_by_name))
    ;; ... other agents
    ))
```

## Troubleshooting

### Common Issues

1. **Server won't start**
   - Check that `mcp-server.el` is installed and in load-path
   - Verify all dependencies are available: `M-x js/mcp-auto-configure`

2. **Agent timeouts**
   - Increase timeout: `(setq js/mcp-agent-timeout 180)`
   - Check gptel configuration and API key

3. **Concurrent agent limit reached**
   - Increase limit: `(setq js/mcp-max-concurrent-agents 5)`
   - Check for stale jobs: `C-c g m l`

4. **Permission errors with eval_elisp**
   - Ensure confirmation is enabled: `(setq js/mcp-require-confirmation t)`
   - Dangerous operations are blocked by design

5. **Missing symbol documentation**
   - Install helpful.el: `(package-install 'helpful)`
   - Check that symbols are loaded: `M-x describe-function`

### Debug Information
```elisp
;; Check server status
M-x js/mcp-server-status

;; View active jobs and agent configs  
M-x js/mcp-list-agents

;; Test individual components
M-x js/mcp-test-direct-tool
M-x js/mcp-test-agent
```

### Logging
- Server logs to `*Messages*` buffer when `js/mcp-enable-logging` is `t`
- Job cleanup runs every 60 seconds
- Stale jobs are automatically removed after timeout

## Architecture

### Job Management
- Unique job IDs for tracking async operations
- Automatic cleanup of stale/failed jobs
- Concurrent operation limits with queuing

### Security Features
- Input sanitization for `eval_elisp`
- Blocked dangerous operations (file deletion, shell commands)
- User confirmation prompts for code execution
- Read-only operations by default

### Error Handling
- Comprehensive error catching and reporting
- Graceful degradation when dependencies missing
- Structured error responses for MCP clients

## Development

### Testing Framework
```elisp
;; Test direct tools
(js/mcp-test-direct-tool "describe_symbol" 
  '(("symbol_name" . "org-mode")))

;; Test agents
(js/mcp-test-agent "documentation-explorer" 
  "Analyze org-mode keybindings")
```

### Adding Custom Agents
```elisp
;; Add to js/mcp-agent-configs
(push '(my-custom-agent
        :model "claude-3-5-sonnet-20241022"
        :temperature 0.2
        :max-tokens 2000
        :system-prompt "You are a custom analysis specialist..."
        :tools (eval_elisp helpful_function_inspect))
      js/mcp-agent-configs)
```

## License

This project is licensed under the GPL v3 - see the LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Test your changes thoroughly
4. Submit a pull request

## Support

- Create issues on GitHub for bugs and feature requests
- Check the integration guide: `C-c g m g`
- Review the troubleshooting section above