;;; init.el --- Emacs MCP Server configuration snippet -*- lexical-binding: t; -*-

;;; Commentary:
;; Add this to your Emacs init file (~/.emacs.d/init.el or ~/.emacs)
;; Update the path to match where you cloned emacsmcp.el

;;; Code:

;; Load the MCP server (calculate path dynamically)
(let ((emacsmcp-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." emacsmcp-dir))
  (require 'claude-code-mcp-server)
  ;; Load the MCP tools
  (load-file (expand-file-name "mcp-tools.el" emacsmcp-dir)))

;; Auto-start the server when Emacs starts
(claude-code-mcp-start-server)

;;; init.el ends here