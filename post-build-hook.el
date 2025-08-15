;;; post-build-hook.el --- Straight post-build hook for emacs-mcp -*- lexical-binding: t; -*-

;; This file contains the straight post-build hook configuration for emacs-mcp.el
;; Add this to your Emacs configuration to automatically:
;; 1. Set up the MCP proxy script permissions and install it in Claude Code CLI
;; 2. Copy agent definitions to ~/.claude/agents/

(add-hook 'straight-use-package-post-build-functions
          (lambda (package)
            (when (string= package "emacs-mcp")
              (let* ((build-dir (straight--build-dir package))
                     (proxy-path (expand-file-name "mcp-proxy.sh" build-dir))
                     (agents-dir (expand-file-name "agents" build-dir))
                     (claude-agents-dir (expand-file-name "~/.claude/agents")))
                
                ;; Set up MCP proxy script
                (when (file-exists-p proxy-path)
                  (set-file-modes proxy-path #o755)
                  (shell-command (format "claude mcp add -s user emacs %s" proxy-path))
                  (message "emacs-mcp: MCP server installed in Claude Code CLI"))
                
                ;; Copy agents to Claude agents directory
                (when (file-directory-p agents-dir)
                  (unless (file-directory-p claude-agents-dir)
                    (make-directory claude-agents-dir t))
                  
                  (dolist (agent-file (directory-files agents-dir t "\\.md$"))
                    (let ((target-file (expand-file-name 
                                       (file-name-nondirectory agent-file)
                                       claude-agents-dir)))
                      (copy-file agent-file target-file t)
                      (message "emacs-mcp: Copied agent %s to %s" 
                              (file-name-nondirectory agent-file)
                              claude-agents-dir)))
                  
                  (message "emacs-mcp: All agents installed successfully"))))))

;;; post-build-hook.el ends here