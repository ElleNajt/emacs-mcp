;;; emacs-mcp.el --- Model Context Protocol (MCP) support for Emacs -*- lexical-binding: t; -*-

;; Author: Claude AI
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai, mcp

;;; Commentary:
;; This is the main entry point for the emacs-mcp package.
;; It loads the MCP type system and server framework.
;; Example tools in the example/ directory are loaded separately as needed.

;;; Code:

;; Load core framework modules in dependency order
(require 'claude-code-mcp-types)
(require 'claude-code-mcp-server)

;; Add example directory to load path for optional loading
(let ((example-dir (expand-file-name "example" 
                                     (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-directory-p example-dir)
    (add-to-list 'load-path example-dir)))

(provide 'emacs-mcp)
;;; emacs-mcp.el ends here