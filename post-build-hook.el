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
                     (claude-agents-dir (expand-file-name "~/.claude/agents")))
                
                (message "emacs-mcp: Post-build hook running for package: %s" package)
                (message "emacs-mcp: Build directory: %s" build-dir)
                (message "emacs-mcp: Directory contents: %s" (directory-files build-dir))
                
                ;; Set up MCP proxy script
                (if (file-exists-p proxy-path)
                    (progn
                      (set-file-modes proxy-path #o755)
                      (shell-command (format "claude mcp add -s user emacs %s" proxy-path))
                      (message "emacs-mcp: MCP server installed in Claude Code CLI"))
                  (message "emacs-mcp: WARNING - mcp-proxy.sh not found at %s" proxy-path))
                
                ;; Copy agent files to Claude agents directory
                ;; Note: straight flattens all files into build-dir, so agents/*.md become *.md
                (let ((all-md-files (directory-files build-dir t "\\.md$"))
                      (agent-files (directory-files build-dir t "emacs-manager\\.md$")))
                  (message "emacs-mcp: All .md files found: %s" all-md-files)
                  (message "emacs-mcp: Agent files found: %s" agent-files)
                  
                  (if agent-files
                      (progn
                        (unless (file-directory-p claude-agents-dir)
                          (make-directory claude-agents-dir t)
                          (message "emacs-mcp: Created Claude agents directory: %s" claude-agents-dir))
                        
                        (dolist (agent-file agent-files)
                          (let ((target-file (expand-file-name 
                                             (file-name-nondirectory agent-file)
                                             claude-agents-dir)))
                            (copy-file agent-file target-file t)
                            (message "emacs-mcp: Copied agent %s to %s" 
                                    (file-name-nondirectory agent-file)
                                    claude-agents-dir)))
                        
                        (message "emacs-mcp: All agents installed successfully"))
                    (message "emacs-mcp: WARNING - No agent files found in build directory")))))))

;;; post-build-hook.el ends here