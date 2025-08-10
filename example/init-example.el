;;; init.el --- Emacs MCP Server configuration snippet -*- lexical-binding: t; -*-

;;; Commentary:
;; Add this to your Emacs init file (~/.emacs.d/init.el or ~/.emacs)
;; Update the path to match where you cloned emacsmcp.el

;;; Code:

;; Load the MCP server (update path as needed)
(add-to-list 'load-path "/path/to/emacsmcp.el")
(require 'claude-code-mcp-server)

;; Load the MCP tools
(load-file "/path/to/emacsmcp.el/examples/mcp-tools.el")

;; Auto-start the server when Emacs starts
(claude-code-start-mcp-server)

;;; init.el ends here