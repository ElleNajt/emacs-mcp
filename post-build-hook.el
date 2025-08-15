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
                     (claude-agents-dir (expand-file-name "~/.claude/agents"))
                     (log-file "/tmp/emacs-mcp-post-build.log")
                     (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
                
                ;; Helper function to log both to file and messages
                (cl-flet ((log-msg (msg &rest args)
                           (let ((formatted-msg (apply 'format msg args)))
                             (message "emacs-mcp: %s" formatted-msg)
                             (with-temp-buffer
                               (insert (format "[%s] emacs-mcp: %s\n" timestamp formatted-msg))
                               (append-to-file (point-min) (point-max) log-file)))))
                  
                  (log-msg "=== POST-BUILD HOOK STARTED ===")
                  (log-msg "Post-build hook running for package: %s" package)
                  (log-msg "Build directory: %s" build-dir)
                  (log-msg "Directory exists: %s" (file-directory-p build-dir))
                  (log-msg "Directory contents: %s" (directory-files build-dir))
                  
                  ;; Set up MCP proxy script
                  (if (file-exists-p proxy-path)
                      (progn
                        (set-file-modes proxy-path #o755)
                        (let ((cmd-result (shell-command (format "claude mcp add -s user emacs %s" proxy-path))))
                          (log-msg "MCP server install command result: %s" cmd-result)
                          (log-msg "MCP server installed in Claude Code CLI")))
                    (log-msg "WARNING - mcp-proxy.sh not found at %s" proxy-path))
                  
                  ;; Copy agent files to Claude agents directory
                  ;; Note: straight flattens all files into build-dir, so agents/*.md become *.md
                  (let ((all-md-files (directory-files build-dir t "\\.md$"))
                        (agent-files (directory-files build-dir t "emacs-manager\\.md$")))
                    (log-msg "All .md files found: %s" all-md-files)
                    (log-msg "Agent files found: %s" agent-files)
                    (log-msg "Claude agents directory: %s" claude-agents-dir)
                    (log-msg "Claude agents directory exists: %s" (file-directory-p claude-agents-dir))
                    
                    (if agent-files
                        (progn
                          (unless (file-directory-p claude-agents-dir)
                            (make-directory claude-agents-dir t)
                            (log-msg "Created Claude agents directory: %s" claude-agents-dir))
                          
                          (dolist (agent-file agent-files)
                            (let ((target-file (expand-file-name 
                                               (file-name-nondirectory agent-file)
                                               claude-agents-dir)))
                              (copy-file agent-file target-file t)
                              (log-msg "Copied agent %s to %s" 
                                      (file-name-nondirectory agent-file)
                                      target-file)))
                          
                          (log-msg "All agents installed successfully"))
                      (log-msg "WARNING - No agent files found in build directory")))
                  
                  (log-msg "=== POST-BUILD HOOK COMPLETED ==="))))))

;;; post-build-hook.el ends here