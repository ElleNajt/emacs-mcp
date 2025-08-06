# Emacs MCP Server

A Model Context Protocol (MCP) server implementation in Emacs Lisp that enables Claude Code CLI to interact with Emacs through a rich set of tools for buffer management, Org-mode operations, workspace control, and Elisp debugging.

## Overview

This project provides:
- **Full MCP Protocol**: Complete implementation of MCP tools, resources, and prompts
- **MCP Server**: A TCP-based server (`claude-code-mcp-server.el`) that implements the MCP protocol
- **Tool Framework**: Macro systems for defining MCP tools, resources, and prompts
- **Rich Tool Set**: 25+ tools, 4 resources, and 4 prompts for comprehensive Emacs interaction
- **Security**: Input validation and access restrictions to protect sensitive data

## Quick Start

1. **Load the MCP server**:
   ```elisp
   (load-file "claude-code-mcp-server.el")
   ```

2. **Load example tools**:
   ```elisp
   (load-file "examples/mcp/mcp-tools.el")
   ```

3. **Start the server**:
   ```elisp
   (claude-code-start-mcp-server)
   ```

4. **Configure Claude Code CLI** to use the MCP server via the proxy script:
   ```bash
   # The mcp-proxy.sh script bridges stdio to the Emacs TCP server
   chmod +x mcp-proxy.sh
   ```

## Architecture

### Core Components

- **`claude-code-mcp-server.el`**: Main MCP server implementation
  - TCP server on port 8765 (configurable)
  - JSON-RPC message handling
  - Tool discovery and execution framework
  - Security validation

- **`mcp-proxy.sh`**: Ultra-thin proxy that bridges stdio to TCP
  - Uses `nc` to connect Claude Code CLI to Emacs MCP server
  - Enables standard MCP client integration

- **`examples/mcp/mcp-tools.el`**: Rich collection of MCP tools
  - 25+ tools for comprehensive Emacs interaction
  - Org-mode integration, workspace management, debugging utilities
  - Security patterns and blocked buffer protection

### Definition Frameworks

#### Tools
Use the `claude-code-defmcp` macro to create new MCP tools:

```elisp
(claude-code-defmcp my-tool (param1 param2)
  "Tool documentation."
  :mcp-description "Brief description for MCP clients"
  :mcp-schema '((param1 . ("string" "Parameter description"))
                (param2 . ("array" "Another parameter")))
  ;; Implementation
  (format "Result: %s %s" param1 param2))
```

#### Resources
Use the `claude-code-defmcp-resource` macro to create live data sources:

```elisp
(claude-code-defmcp-resource my-resource ()
  "Resource documentation."
  :mcp-description "Brief description of the resource"
  :mcp-mime-type "text/plain"
  ;; Return current data
  (format "Current status: %s" (current-time-string)))
```

#### Prompts
Use the `claude-code-defmcp-prompt` macro to create prompt templates:

```elisp
(claude-code-defmcp-prompt my-prompt (context focus)
  "Prompt documentation."
  :mcp-description "Brief description of the prompt"
  :mcp-schema '((context . ("string" "Context information"))
                (focus . ("string" "Focus area")))
  ;; Return formatted prompt
  (format "You are an expert in %s. Focus on %s..." context focus))
```

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

## Available Resources

Resources provide live, read-only access to current Emacs state. They're accessed via URI and automatically refresh when read.

### System Information
- **`mcp-emacs-config`** (`emacs://resource/mcp-emacs-config`): Current Emacs configuration and environment details including version, user info, system type, and loaded features
- **`mcp-emacs-performance-metrics`** (`emacs://resource/mcp-emacs-performance-metrics`): Real-time performance metrics including memory usage, GC statistics, uptime, and resource counts

### Buffer Status
- **`mcp-current-buffer-status`** (`emacs://resource/mcp-current-buffer-status`): Live status of all open buffers with file associations, modification status, and major modes

### Org-Mode Data
- **`mcp-org-agenda-summary`** (`emacs://resource/mcp-org-agenda-summary`): Current agenda overview with statistics, upcoming items, and full agenda content

## Available Prompts

Prompts generate contextual prompts for specific use cases. They take parameters and return formatted prompts ready for use.

### Development Assistance
- **`mcp-debug-emacs-issue`**: Generate debugging prompts for Emacs configuration, keybinding, package, or performance issues
- **`mcp-code-review-prompt`**: Create code review prompts focusing on specific languages (including Elisp) and review areas (security, performance, maintainability, style)
- **`mcp-elisp-learning-prompt`**: Educational prompts for learning Emacs Lisp at beginner, intermediate, or advanced levels

### Productivity
- **`mcp-org-mode-assistant`**: Generate prompts for org-mode workflows including planning, review, capture, agenda optimization, and workflow design

## Configuration

### Server Settings
```elisp
;; Enable/disable MCP server (default: t)
(setq claude-code-mcp-enabled t)

;; TCP port for MCP server (default: 8765)  
(setq claude-code-mcp-port 8765)
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
1. **No tools available**: Load `examples/mcp/mcp-tools.el`
2. **Permission denied**: Add `/tmp/ClaudeWorkingFolder` to Claude Code settings
3. **Port conflicts**: Change `claude-code-mcp-port` to available port
4. **Connection issues**: Check proxy script permissions and nc availability

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

### Schema Format and Type System

The MCP server now supports rich type specifications inspired by Emacs Lisp type syntax:

#### Basic Types
```elisp
:mcp-schema '((name . (string "User name"))
              (age . (integer "User age"))
              (active . (boolean "Is active")))
```

#### Complex Types
```elisp
:mcp-schema '((files . ((list string) "List of file paths"))
              (count . ((or integer nil) "Optional count"))
              (mode . (string "Mode selection" ((enum . ["read" "write" "append"]))))
              (tags . ((vector string) "Array of tags"))
              (options . (alist "Configuration options")))
```

#### Supported Type Expressions
- **Basic**: `string`, `integer`, `number`, `boolean`, `nil`
- **Collections**: `(list type)`, `(vector type)`, `list`, `vector`, `alist`, `plist`
- **Unions**: `(or type1 type2 ...)` - allows any of the specified types
- **Intersections**: `(and type1 type2 ...)` - must satisfy all types
- **Enums**: `(choice "val1" "val2" ...)` or `(member val1 val2 ...)`
- **Repeated**: `(repeat type)` - array of the specified type
- **Tuples**: `(cons type1 type2)` - fixed-length array with specific types

#### Extended JSON Schema Properties
You can add additional JSON Schema properties as a third element:
```elisp
:mcp-schema '((count . (integer "Number of items" ((minimum . 0) (maximum . 100))))
              (name . (string "User name" ((minLength . 1) (maxLength . 50))))
              (tags . ((list string) "Tags" ((minItems . 1) (uniqueItems . t)))))
```

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
- [Emacs](https://www.gnu.org/software/emacs/)