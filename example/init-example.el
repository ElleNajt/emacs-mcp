;;; init.el --- Emacs MCP Server configuration snippet -*- lexical-binding: t; -*-

;;; Commentary:
;; Add this to your Emacs init file (~/.emacs.d/init.el or ~/.emacs)
;; Update the path to match where you cloned emacs-mcp.el

;;; Code:

;; Load the MCP server (calculate path dynamically)
(let ((emacs-mcp-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." emacs-mcp-dir))
  (require 'emacs-mcp-server)
  ;; Load the MCP tools
  (load-file (expand-file-name "mcp-tools.el" emacs-mcp-dir)))

;; Auto-start the server when Emacs starts
(emacs-mcp-start-server)

;;; init.el ends here