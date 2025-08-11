# Emacs MCP Server

A Model Context Protocol (MCP) server implementation in Emacs Lisp that enables Claude Code CLI to interact with Emacs through a comprehensive set of tools for buffer management, Org-mode operations, workspace control, and Elisp debugging.

## Overview

This project provides two main components:

1. **MCP Server Framework** (`claude-code-mcp-server.el`, `claude-code-mcp-types.el`) - A complete MCP server implementation with type validation, security features, and macro system for defining tools
2. **Example Tool Collection** (`examples/mcp-tools.el`) - 25+ ready-to-use tools for buffer management, Org-mode operations, workspace control, and Elisp debugging

The framework enables you to create custom MCP tools for your specific Emacs workflow, while the example tools provide immediate functionality.

## Installation

**Prerequisites**: Emacs 27+, `nc` (netcat), Claude Code CLI

### Option 1: Using straight.el (Recommended)
(WIP - testing this)

1. **Install with straight.el**:
   
   Add to your `packages.el` (for Doom) or equivalent:
   ```elisp
   (package! emacs-mcp
     :recipe (:host github :repo "ElleNajt/emacs-mcp"
              :post-build (lambda ()
                           (let ((proxy-path (expand-file-name "mcp-proxy.sh" 
                                                             (straight--repos-dir "emacs-mcp"))))
                             (shell-command (format "claude mcp add -s user emacs %s" proxy-path))))))
   ```
   
   Add to your `config.el` or init file:
   ```elisp
   (use-package! emacs-mcp
     :config
     ;; Load the MCP server framework
     (require 'claude-code-mcp-server)
     (require 'claude-code-mcp-types)
     
     ;; Load example tools
     (require 'mcp-tools)
     
     ;; Start the MCP server
     (claude-code-mcp-start-server))
   ```

### Option 2: Manual Installation (For Testing)

1. **Clone this repository**:
   ```bash
   git clone https://github.com/yourusername/emacs-mcp.git
   cd emacs-mcp
   ```

2. **Try the example configuration**:
   The `examples/init-example.el` file provides a complete configuration for testing without installing. It expects to be run from the repository directory:
   ```elisp
   ;; Load from current directory (for testing only)
   (load-file "claude-code-mcp-server.el")
   (load-file "claude-code-mcp-types.el") 
   (load-file "example/mcp-tools.el")
   (claude-code-mcp-start-server)
   ```

3. **Install in Claude Code CLI**:
   ```bash
   /path/to/emacs-mcp/example/install-example.sh
   ```

4. **Test the connection**:
   ```bash
   claude
   # Ask Claude to interact with Emacs
   > Can you list the buffers in my Emacs?
   ```


## Framework Components

### Core Framework Files
- **`claude-code-mcp-server.el`**: TCP server implementing MCP protocol (port 8765)
- **`claude-code-mcp-types.el`**: Type system with JSON Schema generation and parameter validation
- **`mcp-proxy.sh`**: Proxy script that bridges stdio to TCP using netcat

### Example Tools
- **`examples/mcp-tools.el`**: Collection of 25+ MCP tools demonstrating the framework
- **`examples/init-example.el`**: Sample Emacs configuration
- **`examples/install-example.sh`**: Installation script for Claude Code CLI


## Example Tools (from `examples/mcp-tools.el`)

### Buffer Management
- **`mcp-get-buffer-list`**: List all live buffers with optional details
- **`mcp-view-buffer`**: Extract buffer contents with line numbers
- **`mcp-emacs-buffer-info`**: Comprehensive buffer analysis including variables and content
- **`mcp-open-file`**: Open files in Emacs (with security restrictions)

### Emacs Introspection
- **`mcp-emacs-search`**: Search for symbols, commands, variables, functions
- **`mcp-emacs-describe`**: Get detailed documentation for Emacs symbols
- **`mcp-get-variable-value`**: Query Emacs variable values
- **`mcp-emacs-keymap-analysis`**: Analyze buffer keymaps and bindings

### Org-Mode Integration
- **`mcp-get-agenda`**: Export org-agenda views
- **`mcp-org-agenda-todo-batch`**: Batch update TODO states in agenda
- **`mcp-org-schedule-todo`**: Schedule TODO items with dates
- **`mcp-org-archive-todo`**: Archive completed tasks
- **`mcp-org-capture`**: Create new items via capture templates
- **`mcp-org-get-all-todos`**: Comprehensive TODO listing across files
- **`mcp-org-agenda-goto`**: Navigate to agenda item sources with context

### Workspace Management (Doom Emacs)
- **`mcp-get-workspace-buffers`**: List buffers in workspaces
- **`mcp-rename-workspace`**: Rename workspaces with meaningful names
- **`mcp-create-workspace`**: Create new project workspaces
- **`mcp-delete-workspace`**: Safely delete workspaces (protects active sessions)
- **`mcp-move-protected-buffers-to-workspace`**: Move Claude/terminal sessions between workspaces
- **`mcp-setup-workspace-layout`**: Configure window layouts

### Elisp Debugging
- **`mcp-check-parens`**: Validate parentheses balance in Lisp files
- **`mcp-count-parens`**: Count parentheses between specific lines
- **`mcp-check-parens-range`**: Check balance in line ranges
- **`mcp-show-paren-balance`**: Display running balance counts by line

## Configuration

### Server Settings
```elisp
;; Enable/disable MCP server (default: t)
(setq claude-code-mcp-enabled t)

;; TCP port for MCP server (default: 8765)  
(setq claude-code-mcp-port 8765)
```

### Type System Settings
```elisp
;; Enable/disable parameter validation (default: t)
(setq claude-code-mcp-enable-validation t)

;; When disabled, all validation is bypassed for performance or debugging
(setq claude-code-mcp-enable-validation nil)
```

### Security Settings
```elisp
;; File access restrictions (default: t)
(setq claude-code-mcp-restrict-file-access t)

;; Patterns that block buffer access
(setq claude-code-mcp-blocked-buffer-patterns
      '("password" ".pem" "secret" ".key" "token" "credential" "auth" ".ssh"))
```

## Security

⚠️ This MCP server provides Claude with broad access to your Emacs environment, depending on the tools that you provide it with. The example tools have the following access:

- **Access**: Can read buffers, query variables, access Org data
- **Restrictions**: File access limited to current directory and `/tmp/ClaudeWorkingFolder/`
- **Protection**: Input validation, configurable buffer blocking patterns

## Troubleshooting

- **No tools available**: Load `examples/mcp-tools.el`
- **Port conflicts**: Change `claude-code-mcp-port`
- **Debugging**: Check `*Messages*` buffer for server logs

## Using the Framework

### Creating Custom Tools

The framework provides the `claude-code-defmcp` macro for defining your own MCP tools:

```elisp
(claude-code-defmcp my-custom-tool (name files)
  "Documentation for my custom tool."
  :mcp-description "Brief description for MCP clients"
  :mcp-schema '((name . (string "User name"))
                (files . ((list string) "File paths")))
  ;; Your tool implementation
  (format "Processing %s with %d files" name (length files)))
```

### Type System
- **Basic Types**: `string`, `integer`, `number`, `boolean`, `symbol`
- **Collections**: `(list type)` for arrays
- **Optional**: `(or type nil)` for optional parameters
- **Enums**: `(choice "option1" "option2")` for limited choices

### Example vs Custom Tools
- **Use examples directly**: Load `examples/mcp-tools.el` for immediate functionality
- **Create custom tools**: Use the framework to build tools specific to your workflow
- **Mix both**: Load examples and add your custom tools alongside them


## Related Projects

- [Model Context Protocol](https://github.com/modelcontextprotocol/specification)
- [Claude Code CLI](https://github.com/anthropics/claude-code)
- [claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el) - Emacs IDE integration for Claude Code, which also has an MCP server -- inspiration for the schema format
- [mcp-server-lib.el](https://github.com/laurynas-biveinis/mcp-server-lib.el) - Another Emacs Lisp MCP server implementation
- [Emacs](https://www.gnu.org/software/emacs/)
