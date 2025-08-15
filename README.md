# Emacs MCP Server

A Model Context Protocol (MCP) server implementation in Emacs Lisp that enables Claude Code CLI to interact with Emacs through a comprehensive set of tools for buffer management, Org-mode operations, workspace control, and Elisp debugging.

## Overview

This project provides three main components:

1. **Main Entry Point** (`emacs-mcp.el`) - Loads the core MCP framework
2. **MCP Server Framework** (`mcp-server.el`, `mcp-types.el`) - A complete MCP server implementation with type validation, security features, and macro system for defining tools
3. **Example Tool Collection** (`example/mcp-tools.el`) - 25+ optional ready-to-use tools for buffer management, Org-mode operations, workspace control, and Elisp debugging

The framework enables you to create custom MCP tools for your specific Emacs workflow, while the example tools provide immediate functionality and can be loaded optionally.

## Installation

**Prerequisites**: Emacs 27+, `nc` (netcat), Claude Code CLI

### Option 1: Using straight.el (Recommended)
(WIP - testing this)

1. **Install with straight.el**:
   
   Add to your `packages.el` (for Doom) or equivalent:
   ```elisp
(package! emacs-mcp
  :recipe (:host github :repo "ElleNajt/emacs-mcp"
           :files ("*.el" "example/*.el" "mcp-proxy.sh")))

;; Use the provided post-build hook that handles both MCP setup and agent installation
(load-file (expand-file-name "post-build-hook.el" 
                             (straight--repos-dir "emacs-mcp")))
   ```
   
   Add to your `config.el` or init file:
   ```elisp
   (use-package! emacs-mcp
     :config
     ;; Optionally load example tools
     (require 'mcp-tools)
     
     ;; Start the MCP server
     (emacs-mcp-start-server))
   ```
   
   Or for just the core framework without examples:
   ```elisp
   (use-package! emacs-mcp
     :config
     ;; Start the MCP server (core framework only)
     (emacs-mcp-start-server))
   ```

### Option 2: Manual Installation (For Testing)

1. **Clone this repository**:
   ```bash
   git clone https://github.com/yourusername/emacs-mcp.git
   cd emacs-mcp
   ```

2. **Try the example configuration**:
   The `example/init-example.el` file provides a complete configuration for testing without installing. It expects to be run from the repository directory:
   ```elisp
   ;; Load from current directory (for testing only)
   (load-file "emacs-mcp.el")  ; This loads the core framework
   (require 'mcp-tools)        ; Optionally load example tools
   (emacs-mcp-start-server)
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
- **`mcp-server.el`**: TCP server implementing MCP protocol (port 8765)
- **`mcp-types.el`**: Type system with JSON Schema generation and parameter validation
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
(setq emacs-mcp-enabled t)

;; TCP port for MCP server (default: 8765)  
(setq emacs-mcp-port 8765)
```

### Type System Settings
```elisp
;; Enable/disable parameter validation (default: t)
(setq emacs-mcp-enable-validation t)

;; When disabled, all validation is bypassed for performance or debugging
(setq emacs-mcp-enable-validation nil)
```

### Security Settings
```elisp
;; File access restrictions (default: t)
(setq emacs-mcp-restrict-file-access t)

;; Patterns that block buffer access
(setq emacs-mcp-blocked-buffer-patterns
      '("password" ".pem" "secret" ".key" "token" "credential" "auth" ".ssh"))
```

## Security

⚠️ This MCP server provides Claude with broad access to your Emacs environment, depending on the tools that you provide it with. The example tools have the following access:

- **Access**: Can read buffers, query variables, access Org data
- **Restrictions**: File access limited to current directory and `/tmp/ClaudeWorkingFolder/`
- **Protection**: Input validation, configurable buffer blocking patterns

## Claude Code Agent

This package includes the `emacs-manager` agent that gets automatically installed by the `post-build-hook.el` (shown in the straight installation example above):

**emacs-manager**: Specialized agent for Emacs management tasks including parentheses debugging, file reloading, configuration management, org-mode interaction (TODOs, scheduling, capture), Emacs debugging (Messages buffer, buffer analysis), and leveraging all emacs-mcp.el tools for comprehensive Emacs interaction.

The post-build hook automatically copies the agent from `agents/emacs-manager.md` to `~/.claude/agents/` during package installation.

## Troubleshooting

- **No tools available**: Load `examples/mcp-tools.el`
- **Port conflicts**: Change `emacs-mcp-port`
- **Debugging**: Check `*Messages*` buffer for server logs

## Using the Framework

### Creating Custom Tools

The framework provides the `emacs-mcp-defmcp` macro for defining your own MCP tools:

```elisp
(emacs-mcp-defmcp my-custom-tool (name files)
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
