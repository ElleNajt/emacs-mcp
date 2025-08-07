;;; test-mcp-tools.el --- Test functions for MCP tools -*- lexical-binding: t; -*-

;; Author: Claude Code Contributors  
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.2.0"))
;; Keywords: tools, ai, mcp, test

;;; Commentary:
;; Test functions for MCP tools. These are used for development and testing
;; but should not be exposed in production deployments.

;;; Code:

;; Load the main tools file first
(require 'mcp-tools)

;;;; Test Functions

(claude-code-defmcp mcp-test-validation (name age files)
                    "Test parameter validation with different types."
                    :parameters ((:name "name"
                                 :type "string"
                                 :required t
                                 :description "Your name")
                                (:name "age"
                                 :type "integer"
                                 :required t
                                 :description "Your age in years")
                                (:name "files"
                                 :type "(list string)"
                                 :required t
                                 :description "List of file names"))
                    (format "Hello %s (age %d)! You provided %d files: %s"
                            name age (length files) (mapconcat #'identity files ", ")))

(claude-code-defmcp mcp-test-symbols (function-names)
                    "Handle function names as symbols."
                    :parameters ((:name "function-names"
                                 :type "(list symbol)"
                                 :required t
                                 :description "List of function names as symbols"))
                    (let ((results '()))
                      (dolist (func-name function-names)
                        (when (and (symbolp func-name) (fboundp func-name))
                          (push (format "%s is a valid function" func-name) results)))
                      (mapconcat #'identity (nreverse results) "; ")))

(claude-code-defmcp mcp-test-enum (status priority)
                    "Test enum parameter validation with predefined types."
                    :parameters ((:name "status"
                                 :type "mcp-todo-status"
                                 :required t
                                 :description "Task status")
                                (:name "priority"
                                 :type "mcp-priority"
                                 :required t
                                 :description "Task priority"))
                    (format "Task is %s with %s priority" status priority))

(provide 'test-mcp-tools)

;;; test-mcp-tools.el ends here