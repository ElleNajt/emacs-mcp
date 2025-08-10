# Emacs MCP Server

A Model Context Protocol (MCP) server implementation in Emacs Lisp that enables Claude Code CLI to interact with Emacs through a comprehensive set of tools for buffer management, Org-mode operations, workspace control, and Elisp debugging.

## Overview

This project provides:
- **MCP Tools Protocol**: Complete implementation of MCP tools for Emacs interaction
- **MCP Server**: A TCP-based server (`claude-code-mcp-server.el`) that implements the MCP protocol
- **Tool Framework**: Macro system for defining MCP tools with type validation
- **Rich Tool Set**: 25+ tools for comprehensive Emacs interaction
- **Type System**: Emacs Lisp-style type specifications with JSON Schema generation
- **Security**: Input validation and access restrictions to protect sensitive data

## Setup Instructions

### Prerequisites

- Emacs 27+ (tested with Emacs 29)
- `nc` (netcat) command available in your PATH
- Claude Code CLI installed

### Installation

1. **Clone this repository**:
   ```bash
   git clone https://github.com/yourusername/emacsmcp.el.git
   cd emacsmcp.el
   ```

2. **Install in Claude Code CLI**:
   ```bash
   ./examples/install-example.sh
   ```

3. **Add to your Emacs init file**:
   Copy the contents of `examples/init-example.el` to your Emacs configuration, updating the paths to match where you cloned the repository.

4. **Restart Emacs** and the MCP server will start automatically.

5. **Test the connection**:
   ```bash
   claude
   # Ask Claude to interact with Emacs
   > Can you list the buffers in my Emacs?
   ```

## Quick Start

1. Install: `./examples/install-example.sh`
2. Add `examples/init-example.el` contents to your config (update paths)
3. Restart Emacs

## Architecture

### Core Components

- **`claude-code-mcp-server.el`**: Main MCP server implementation
  - TCP server on port 8765 (configurable)
  - JSON-RPC message handling
  - Tool discovery and execution framework
  - Security validation

- **`claude-code-mcp-types.el`**: Type system and validation
  - JSON Schema generation from Emacs Lisp type specs
  - Parameter validation with configurable enable/disable
  - Support for cl-deftype enums and complex types
  - Clear error messages for type mismatches

- **`mcp-proxy.sh`**: Ultra-thin proxy that bridges stdio to TCP
  - Uses `nc` to connect Claude Code CLI to Emacs MCP server
  - Enables standard MCP client integration

- **`examples/mcp-tools.el`**: Rich collection of MCP tools
  - 25+ tools for comprehensive Emacs interaction
  - Org-mode integration, workspace management, debugging utilities
  - Security patterns and blocked buffer protection

### Tool Definition Framework

Use the `claude-code-defmcp` macro to create new MCP tools with rich type specifications:

```elisp
(claude-code-defmcp my-tool (name files status)
  "Tool documentation."
  :mcp-description "Brief description for MCP clients"
  :mcp-schema '((name . (string "User name"))
                (files . ((list string) "List of file paths"))
                (status . ((choice "active" "inactive") "Current status")))
  ;; Implementation with validated parameters
  (format "Processing %s with %d files, status: %s" 
          name (length files) status))
```

#### Type System Features
- **Basic types**: `string`, `integer`, `number`, `boolean`, `symbol`
- **Arrays**: `(list string)` - array where each element is validated
- **Optional**: `(or string nil)` - allows nil/missing values
- **Enums**: `(choice "a" "b" "c")` - only specific values allowed
- **Complex**: `(list symbol)` - arrays of symbols for function names

## Available Tools

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

## Test Tools

The following test tools demonstrate the type validation system:

### Validation Examples
- **`mcp-test-validation`**: Test basic types (string, integer, array)
- **`mcp-test-symbols`**: Test symbol handling for function names  
- **`mcp-test-enum`**: Test enum validation with specific allowed values

These tools show how the type system validates parameters and provides clear error messages when types don't match.

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

## Security Considerations

⚠️ **Important Security Notice**: This MCP server provides Claude with broad access to your Emacs environment.

### Access Scope
- **Buffer Access**: Can read any open buffer (source code, notes, sensitive files)
- **File Access**: Restricted to current directory and `/tmp/ClaudeWorkingFolder/` by default
- **Emacs Variables**: Can query any Emacs variable value
- **Org Data**: Full access to agenda, TODOs, capture templates

### Protection Mechanisms
- **Input Validation**: Blocks Elisp injection, shell commands, directory traversal
- **Buffer Blocking**: Configurable patterns to block sensitive buffers
- **File Restrictions**: Limits file access to safe directories (configurable)
- **Parameter Sanitization**: Validates symbols, file paths, and content

### Best Practices
- Close sensitive buffers before enabling MCP
- Use dedicated Emacs sessions for Claude interactions
- Review blocked buffer patterns for your use case
- Consider disabling web access when using MCP tools
- Use Claude Code's directory restrictions in settings

## File Output

Many tools write results to `/tmp/ClaudeWorkingFolder/` for analysis:
- Agenda exports: `agenda.txt`
- Buffer contents: `<buffer-name>.txt`
- Analysis results: Various descriptive filenames
- Ensure this directory is accessible to Claude Code CLI

## Server Management

### Interactive Commands
- `M-x claude-code-start-mcp-server` - Start the server
- `M-x claude-code-stop-mcp-server` - Stop the server
- Auto-starts via `claude-code-start-hook` if enabled

### Status Checking
Check server status and view client connections through the `claude-code-mcp-server-process` variable.

## Troubleshooting

### Common Issues
1. **No tools available**: Load `examples/mcp-tools.el`
2. **Permission denied**: Add `/tmp/ClaudeWorkingFolder` to Claude Code settings
3. **Port conflicts**: Change `claude-code-mcp-port` to available port
4. **Connection issues**: Check proxy script permissions and nc availability
5. **Validation errors**: Toggle validation with `claude-code-mcp-enable-validation` or fix type schemas

### Debugging
- Check `*Messages*` buffer for server logs
- Verify server process with `(process-live-p claude-code-mcp-server-process)`
- Test tools directly: `(mcp-hello-world "test")`

## Development

### Adding New Components

#### Tools
1. Define using `claude-code-defmcp` macro
2. Include `:mcp-description` and `:mcp-schema`
3. Follow security patterns from existing tools
4. Test with various input types

#### Resources
1. Define using `claude-code-defmcp-resource` macro
2. Include `:mcp-description` and `:mcp-mime-type`
3. Return current data that updates when accessed
4. Use appropriate MIME types ("text/plain", "application/json", etc.)

#### Prompts
1. Define using `claude-code-defmcp-prompt` macro
2. Include `:mcp-description` and `:mcp-schema` for parameters
3. Return well-formatted prompts with clear instructions
4. Include examples and specific guidance

### Simplified Type System

The MCP server uses a simplified type system focused on practical use cases:

#### Supported Types
- **Basic**: `string`, `integer`, `number`, `boolean`, `symbol`
- **Arrays**: `(list type)` - arrays where each element is validated
- **Optional**: `(or type nil)` - allows nil/missing values  
- **Enums**: `(choice "val1" "val2" ...)` - specific allowed values only
- **Predefined Types**: `cl-deftype` enums like `mcp-search-type`, `mcp-priority`

#### Schema Format
```elisp
:mcp-schema '((name . (string "User name"))
              (files . ((list string) "Array of file paths"))  
              (status . ((choice "active" "inactive") "Status choice"))
              (priority . (mcp-priority "Task priority"))
              (count . ((or integer nil) "Optional number")))
```

#### Features
- **Simple & Clean**: Only the types actually used by existing tools
- **JSON Schema Generation**: Automatic conversion to JSON Schema for MCP clients
- **Parameter Validation**: Real-time validation with clear error messages (configurable)
- **Emacs-Aware**: Supports Emacs symbols for function names and variables
- **Extensible**: Define custom enum types with `cl-deftype` for reusable validation
- **Performance Toggle**: Disable validation entirely via `claude-code-mcp-enable-validation`

### Security Guidelines
- Validate all user inputs
- Use `claude-code-mcp-buffer-blocked-p` for buffer access
- Write outputs to `/tmp/ClaudeWorkingFolder/`
- Avoid shell commands or file system modifications

## License

This project follows the same license as Emacs itself.

## Contributing

Contributions welcome! Focus areas:
- Additional tool implementations
- Security enhancements  
- Documentation improvements
- Performance optimizations

## Related Projects

- [Model Context Protocol](https://github.com/modelcontextprotocol/specification)
- [Claude Code CLI](https://github.com/anthropics/claude-code)
- [claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el) - Emacs IDE integration for Claude Code, which also has an MCP server -- inspiration for the schema format
- [mcp-server-lib.el](https://github.com/laurynas-biveinis/mcp-server-lib.el) - Another Emacs Lisp MCP server implementation
- [Emacs](https://www.gnu.org/software/emacs/)
