;;; mcp-tools.el --- Example MCP tools for emacs-mcp.el -*- lexical-binding: t; -*-

;; Author: Claude Code Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0") (emacs-mcp "0.2.0"))
;; Keywords: tools, ai, mcp

;;; Commentary:
;; This file provides example MCP tools that can be exposed to Claude
;; through the emacs-mcp.el MCP integration. These tools demonstrate
;; how to create functions that Claude can discover and execute.
;;
;; To use these tools:
;; 1. Load emacs-mcp.el first to get the emacs-mcp-defmcp macro
;; 2. Load this file: (load-file "examples/mcp/mcp-tools.el")  
;; 3. Start emacs-mcp with MCP enabled
;; 4. Configure Claude Desktop to use the MCP server
;;
;; Functions defined with emacs-mcp-defmcp are automatically discovered
;; and made available to Claude.

;;; Code:

;;;; Configuration

(defcustom emacs-mcp-output-directory "/tmp/ClaudeWorkingFolder"
  "Directory for MCP tool output files."
  :type 'string
  :group 'emacs-mcp)

;;;; Helper functions

(defun emacs-mcp-ensure-output-directory ()
  "Ensure the MCP output directory exists."
  (unless (file-directory-p emacs-mcp-output-directory)
    (make-directory emacs-mcp-output-directory t)))

;;;; Tool-specific enum types

;; Define enum types used by the example tools
(cl-deftype mcp-agenda-type () '(choice "a" "t" "n" "m" "T" "N" "e" "E" "D"))
(cl-deftype mcp-search-type () '(choice "all" "commands" "variables" "functions"))
(cl-deftype mcp-target-type () '(choice "agenda_line" "agenda_text" "org_heading"))
(cl-deftype mcp-describe-type () '(choice "function" "variable" "symbol"))
(cl-deftype mcp-todo-status () '(choice "TODO" "DONE" "NEXT" "STARTED" "WAITING" "CANCELLED"))
(cl-deftype mcp-priority () '(choice "high" "medium" "low"))
(cl-deftype mcp-split-direction () '(choice "horizontal" "vertical"))
;; Define allowed function calls with argument type specifications
(defconst mcp-allowed-functions
  '(;; Profiler functions
    (profiler-start . ((choice "cpu" "mem" "cpu+mem")))  ; Symbol argument with specific values
    (profiler-stop . ())                                 ; No arguments
    (profiler-report . ())
    (profiler-reset . ())
    (profiler-running-p . ())
    
    ;; System functions
    (garbage-collect . ())
    (memory-report . ())
    (buffer-list . ())
    (process-list . ())
    
    ;; Variable operations  
    (symbol-value . (symbol))          ; Takes 1 symbol argument
    
    ;; Note: Documentation functions (describe-function, describe-variable, apropos-*) 
    ;; are handled by dedicated MCP tools (mcp-emacs-describe, mcp-emacs-search)
    
    ;; Utility functions (no arguments)
    (emacs-version . ())
    (current-time . ())
    (user-full-name . ())
    (system-name . ()))
  "Alist of allowed functions and their argument type specifications.")

(cl-deftype mcp-allowed-function () 
  '(choice "profiler-start" "profiler-stop" "profiler-report" "profiler-reset" "profiler-running-p" 
    "garbage-collect" "memory-report" "buffer-list" "process-list"
    "symbol-value" "emacs-version" "current-time" "user-full-name" "system-name"))

;;;; Type conversion helper function

(defun mcp-convert-arg-to-type (arg type-spec)
  "Convert string ARG to the appropriate type based on TYPE-SPEC."
  (cond
   ;; Handle choice types (e.g., (choice "cpu" "mem" "cpu+mem"))
   ((and (listp type-spec) (eq (car type-spec) 'choice))
    (let ((valid-values (cdr type-spec)))
      (if (member arg valid-values)
          (intern arg)  ; Convert to symbol
        (error "Invalid choice '%s', must be one of: %s" arg (mapconcat 'identity valid-values ", ")))))
   
   ;; Handle symbol type
   ((eq type-spec 'symbol)
    (intern arg))
   
   ;; Handle string type  
   ((eq type-spec 'string)
    arg)
   
   ;; Handle number/integer types
   ((or (eq type-spec 'number) (eq type-spec 'integer))
    (if (string-match-p "^-?[0-9]+\\.?[0-9]*$" arg)
        (string-to-number arg)
      (error "Invalid number: %s" arg)))
   
   ;; Handle boolean
   ((eq type-spec 'boolean)
    (cond 
     ((member arg '("t" "true" ":true")) t)
     ((member arg '("nil" "false" ":false")) nil)
     (t (error "Invalid boolean: %s (use 't'/'nil' or 'true'/'false')" arg))))
   
   ;; Unknown type, keep as string
   (t 
    (message "Warning: Unknown type %s, keeping as string" type-spec)
    arg)))

;;;; Configuration Variables

(defcustom emacs-mcp-blocked-buffer-patterns
  '("password" ".pem" "secret" ".key" "token" "credential" "auth" ".ssh")
  "List of patterns that will block buffer access in MCP tools.
Buffer names or file paths containing these patterns will be blocked
from access through MCP tools for security."
  :type '(repeat string)
  :group 'emacs-mcp)

;;;; Security Functions  

(defun emacs-mcp-buffer-blocked-p (buffer-name)
  "Check if a buffer should be blocked based on name or file path.
Returns t if BUFFER-NAME or its associated file path contains any
pattern from `emacs-mcp-blocked-buffer-patterns'."
  (when (and buffer-name emacs-mcp-blocked-buffer-patterns)
    (let ((buffer-obj (get-buffer buffer-name))
          (blocked nil))
      ;; Check buffer name against patterns
      (dolist (pattern emacs-mcp-blocked-buffer-patterns)
        (when (string-match-p (regexp-quote pattern) buffer-name)
          (setq blocked t)))
      ;; If buffer exists, also check its file path
      (when (and buffer-obj (not blocked))
        (let ((file-path (buffer-file-name buffer-obj)))
          (when file-path
            (dolist (pattern emacs-mcp-blocked-buffer-patterns)
              (when (string-match-p (regexp-quote pattern) file-path)
                (setq blocked t))))))
      blocked)))

;;;; Basic Utilities

(emacs-mcp-defmcp mcp-hello-world (name)
                  "Greet someone with a friendly hello message."
                  :parameters ((:name "name"
                                :type "string"
                                :required t
                                :description "Name of person to greet"))
                  (format "Hello, %s! 👋" name))

;;;; Emacs Variable Access

(emacs-mcp-defmcp mcp-set-allowed-variable (variable-name value)
                  "Set whitelisted Emacs variables safely."
                  :parameters ((:name "variable-name"
                                :type "(choice \"debug-on-error\" \"debug-on-quit\" \"emacs-mcp-enable-validation\")"
                                :required t
                                :description "Variable to set")
                               (:name "value"
                                :type "boolean"
                                :required t
                                :description "New value"))
                  (let ((var-symbol (intern variable-name)))
                    (unless (boundp var-symbol)
                      (error "Variable %s is not defined" variable-name))

                    (let ((current-value (symbol-value var-symbol))
                          (new-val (cond
                                    ((eq value :true) t)
                                    ((eq value :false) nil)
                                    ((eq value t) t)
                                    ((eq value nil) nil)
                                    (t value))))
                      (set var-symbol new-val)
                      (format "Variable '%s' changed from %s to %s"
                              variable-name current-value new-val))))

;;;; Emacs Variable Access



(emacs-mcp-defmcp mcp-call-allowed-function (function-name &rest args)
                  "Call allowed Emacs functions with type-validated arguments."
                  :description "Call approved Emacs functions with type validation"
                  :parameters ((:name "function-name"
                                :type "mcp-allowed-function"
                                :required t
                                :description "Function name (must be in allowed list)")
                               (:name "args"
                                :type "(or (list string) nil)"
                                :required nil
                                :description "Function arguments - use [] for no args, \"value\" for single arg, [\"val1\", \"val2\"] for multiple args"))
                  (let* ((func-symbol (intern function-name))
                         (func-spec (assoc func-symbol mcp-allowed-functions))
                         (expected-arg-types (cdr func-spec)))

                    (unless func-spec
                      (error "Function %s is not in allowed function list. Available: %s"
                             function-name
                             (mapconcat (lambda (f) (symbol-name (car f))) mcp-allowed-functions ", ")))
                    (unless (fboundp func-symbol)
                      (error "Function %s is not defined" function-name))

                    ;; Validate argument count (filter out nil args for &rest params)
                    (let* ((filtered-args (if (and (= (length args) 1) (null (car args)))
                                              '() ; Remove single nil arg for &rest
                                            args))
                           (expected-arg-count (length expected-arg-types))
                           (actual-arg-count (length filtered-args)))
                      (unless (= expected-arg-count actual-arg-count)
                        (error "Function %s expects %d argument(s), got %d"
                               function-name expected-arg-count actual-arg-count)))

                    (condition-case err
                        (let* ((filtered-args (if (and (= (length args) 1) (null (car args)))
                                                  '() ; Remove single nil arg for &rest
                                                args))
                               (converted-args
                                (cl-mapcar (lambda (arg type-spec)
                                             (mcp-convert-arg-to-type arg type-spec))
                                           filtered-args expected-arg-types))
                               (result (if filtered-args
                                           (apply func-symbol converted-args)
                                         (funcall func-symbol))))
                          (format "%s returned: %s" function-name result))
                      (error (format "Error calling %s: %s" function-name (error-message-string err))))))

(emacs-mcp-defmcp mcp-get-variable-value (variable-names)
                  "Get the current value of multiple Emacs variables."
                  :parameters ((:name "variable-names"
                                :type "(list string)"
                                :required t
                                :description "JSON array of variable names, e.g. [\"user-full-name\", \"system-name\"]"))
                  (let ((results '()))
                    (dolist (var-name variable-names)
                      (let ((var-symbol (intern var-name)))
                        (condition-case err
                            (push (format "%s: %s" var-name (symbol-value var-symbol)) results)
                          (error (push (format "%s: Error: %s" var-name (error-message-string err)) results)))))
                    (mapconcat 'identity (reverse results) "\n")))

;;;; Org-Mode Integration

(emacs-mcp-defmcp mcp-get-agenda (&optional agenda-type)
                  "Get the org-agenda view and write it to /tmp/ClaudeWorkingFolder/agenda_<type>.txt."
                  :description "Get org-agenda view and save to file for analysis"
                  :parameters ((:name "agenda-type"
                                :type "string"
                                :required nil
                                :description "Agenda type (default: 'a')"))
                  (let ((type (or agenda-type "a")))
                    (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                      (make-directory "/tmp/ClaudeWorkingFolder" t))
                    (let ((filename "/tmp/ClaudeWorkingFolder/agenda.txt"))
                      (save-window-excursion
                        (let ((org-agenda-window-setup 'current-window))
                          (org-agenda nil type)
                          (with-current-buffer "*Org Agenda*"
                            (write-region (point-min) (point-max) filename)))
                        (format "Agenda content written to %s" filename)))))

(emacs-mcp-defmcp mcp-org-agenda-todo-batch (batch-updates &optional agenda-type)
                  "Change the state of multiple agenda items in batch."
                  :description "Change the state of multiple agenda items in batch"
                  :parameters ((:name "batch-updates"
                                :type "string"
                                :required t
                                :description "List of [line-number, new-state] pairs")
                               (:name "agenda-type"
                                :type "string"
                                :required nil
                                :description "Agenda type to work with (default 'a')"))
                  (unless agenda-type (setq agenda-type "a"))
                  (save-window-excursion
                    (let ((org-agenda-window-setup 'current-window)
                          (processed-items '())
                          (failed-items '()))
                      (org-agenda nil agenda-type)
                      (with-current-buffer "*Org Agenda*"
                        ;; Process each update
                        (dolist (update batch-updates)
                          (let* ((update-list (if (vectorp update) (append update nil) update))
                                 (line-num (car update-list))
                                 (new-state (cadr update-list)))
                            (condition-case err
                                (progn
                                  (goto-char (point-min))
                                  (forward-line (1- line-num))
                                  (if (org-agenda-check-type nil 'agenda 'todo 'tags 'search)
                                      (progn
                                        (if new-state
                                            (org-agenda-todo new-state)
                                          (org-agenda-todo))
                                        (push (format "Line %d: %s" line-num (or new-state "cycled")) processed-items))
                                    (push (format "Line %d: No valid agenda item" line-num) failed-items)))
                              (error
                               (push (format "Line %d: %s" line-num (error-message-string err)) failed-items)))))

                        ;; Save all changes at once
                        (org-save-all-org-buffers)
                        ;; Refresh the agenda to keep line numbers in sync
                        (org-agenda-redo t)
                        ;; Regenerate the single agenda file
                        (write-region (point-min) (point-max) "/tmp/ClaudeWorkingFolder/agenda.txt")

                        ;; Return summary
                        (let ((success-count (length processed-items))
                              (failure-count (length failed-items)))
                          (format "Batch update complete: %d successful, %d failed\nSuccessful: %s\nFailed: %s"
                                  success-count failure-count
                                  (if processed-items (mapconcat 'identity (reverse processed-items) ", ") "none")
                                  (if failed-items (mapconcat 'identity (reverse failed-items) ", ") "none")))))))

(emacs-mcp-defmcp mcp-org-schedule-todo (org-file heading-text schedule-date &optional remove-schedule)
                  "Schedule a TODO item by adding SCHEDULED property."
                  :description "Schedule a TODO item by adding SCHEDULED property"
                  :parameters ((:name "org-file"
                                :type "string"
                                :required t
                                :description "Path to the org file containing the heading")
                               (:name "heading-text"
                                :type "string"
                                :required t
                                :description "Text of the heading to schedule")
                               (:name "schedule-date"
                                :type "string"
                                :required t
                                :description "Date/time to schedule (e.g., '2025-01-15', 'today', '+1d')")
                               (:name "remove-schedule"
                                :type "boolean"
                                :required nil
                                :description "Remove existing schedule instead of setting one"))
                  (save-window-excursion
                    (find-file org-file)
                    (goto-char (point-min))
                    (if (search-forward heading-text nil t)
                        (progn
                          (org-back-to-heading t)
                          (if (and remove-schedule (not (eq remove-schedule :false)))
                              (org-schedule '(4))
                            (org-schedule nil schedule-date))
                          (save-buffer)
                          (if (and remove-schedule (not (eq remove-schedule :false)))
                              (format "Successfully removed schedule from heading '%s' in %s" heading-text org-file)
                            (format "Successfully scheduled heading '%s' for %s in %s" heading-text schedule-date org-file)))
                      (error "Heading '%s' not found in %s" heading-text org-file))))

(emacs-mcp-defmcp mcp-org-archive-todo (org-file heading-text &optional archive-location)
                  "Archive a TODO item by moving it to the archive file."
                  :description "Archive a TODO item by moving it to the archive file"
                  :parameters ((:name "org-file"
                                :type "string"
                                :required t
                                :description "Path to the org file containing the heading")
                               (:name "heading-text"
                                :type "string"
                                :required t
                                :description "Text of the heading to archive")
                               (:name "archive-location"
                                :type "string"
                                :required nil
                                :description "Archive location (optional)"))
                  (save-window-excursion
                    (find-file org-file)
                    (goto-char (point-min))
                    (if (search-forward heading-text nil t)
                        (progn
                          (org-back-to-heading t)
                          (if (and archive-location (not (string-empty-p archive-location)))
                              (let ((org-archive-location archive-location))
                                (org-archive-subtree))
                            (org-archive-subtree))
                          (save-buffer)
                          (if (and archive-location (not (string-empty-p archive-location)))
                              (format "Successfully archived heading '%s' from %s to %s" heading-text org-file archive-location)
                            (format "Successfully archived heading '%s' from %s to default archive" heading-text org-file)))
                      (error "Heading '%s' not found in %s" heading-text org-file))))

(emacs-mcp-defmcp mcp-org-capture (&optional template-key title content immediate-finish)
                  "Add a new agenda item via org-capture mechanism."
                  :description "Add a new agenda item via org-capture mechanism"
                  :parameters ((:name "template-key"
                                :type "string"
                                :required nil
                                :description "Capture template key (optional)")
                               (:name "title"
                                :type "string"
                                :required nil
                                :description "Title/heading for the captured item (optional)")
                               (:name "content"
                                :type "string"
                                :required nil
                                :description "Content to capture (optional)")
                               (:name "immediate-finish"
                                :type "boolean"
                                :required nil
                                :description "Whether to immediately finish capture"))
                  (unless immediate-finish (setq immediate-finish t))
                  (cond
                   ((not template-key)
                    ;; Show available capture templates
                    (let ((templates org-capture-templates)
                          (result "=== AVAILABLE CAPTURE TEMPLATES ===\n"))
                      (if templates
                          (dolist (template templates)
                            (setq result (concat result
                                                 (format "%s: %s\n"
                                                         (car template)
                                                         (cadr template)))))
                        (setq result (concat result "No capture templates configured")))
                      result))
                   (content
                    ;; Capture with provided content
                    (condition-case err
                        (let ((org-capture-entry (assoc template-key org-capture-templates)))
                          (if org-capture-entry
                              (progn
                                (if title
                                    ;; Use org-capture interactively but fill in programmatically
                                    (let ((org-capture-templates-modified
                                           (mapcar (lambda (template)
                                                     (if (string= (car template) template-key)
                                                         (let ((template-copy (copy-tree template)))
                                                           ;; Find and replace the template string (4th element for 'entry' type)
                                                           (let ((template-str (nth 4 template-copy)))
                                                             (when (stringp template-str)
                                                               (setf (nth 4 template-copy)
                                                                     (replace-regexp-in-string
                                                                      "\\* TODO %\\?\\(\n%i\\)?\\(%U\\)?"
                                                                      (concat "* TODO " title
                                                                              "\n%U"
                                                                              (if content (concat "\n" content) ""))
                                                                      template-str))))
                                                           template-copy)
                                                       template))
                                                   org-capture-templates)))
                                      (let ((org-capture-templates org-capture-templates-modified))
                                        (org-capture-string "" template-key)))
                                  ;; Fallback to old behavior if no title
                                  (org-capture-string (or content "") template-key))
                                (when immediate-finish
                                  (org-capture-finalize))
                                (format "Successfully captured item using template '%s': %s%s"
                                        template-key
                                        (if title (concat "'" title "'") "")
                                        (if content (concat " - " content) "")))
                            (error "Capture template '%s' not found" template-key)))
                      (error (format "Capture failed: %s" (error-message-string err)))))
                   (t
                    ;; Interactive capture
                    (condition-case err
                        (let ((org-capture-entry (assoc template-key org-capture-templates)))
                          (if org-capture-entry
                              (progn
                                (org-capture nil template-key)
                                (format "Interactive capture started with template '%s'. Edit and press C-c C-c to finish." template-key))
                            (error "Capture template '%s' not found" template-key)))
                      (error (format "Capture failed: %s" (error-message-string err)))))))

(emacs-mcp-defmcp mcp-org-get-all-todos (&optional include-done org-files)
                  "Get all TODO items from org files, including unscheduled ones."
                  :description "Get all TODO items from org files, including unscheduled ones"
                  :parameters ((:name "include-done"
                                :type "boolean"
                                :required nil
                                :description "Include DONE items in results")
                               (:name "org-files"
                                :type "(list string)"
                                :required nil
                                :description "JSON array of org file paths, e.g. [\"todo.org\", \"work.org\"] (empty [] uses org-agenda-files)"))
                  (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                    (make-directory "/tmp/ClaudeWorkingFolder" t))

                  (let ((filename "/tmp/ClaudeWorkingFolder/all_todos.txt")
                        (result "=== ALL TODO ITEMS ===\n")
                        (files (or org-files (org-agenda-files)))
                        (todo-keywords (if include-done
                                           '("TODO" "NEXT" "STARTED" "WAITING" "DONE" "CANCELLED")
                                         '("TODO" "NEXT" "STARTED" "WAITING"))))
                    (dolist (file files)
                      (when (file-exists-p file)
                        (with-temp-buffer
                          (insert-file-contents file)
                          (org-mode)
                          (goto-char (point-min))
                          (setq result (concat result (format "\n=== FILE: %s ===\n" file)))
                          (let ((keyword-regex (concat "^\\*+ \\(" (mapconcat 'identity todo-keywords "\\|") "\\) ")))
                            (while (re-search-forward keyword-regex nil t)
                              (let* ((heading-start (line-beginning-position))
                                     (heading-end (line-end-position))
                                     (heading-text (buffer-substring heading-start heading-end))
                                     (scheduled (org-entry-get (point) "SCHEDULED"))
                                     (deadline (org-entry-get (point) "DEADLINE"))
                                     (line-num (line-number-at-pos)))
                                (setq result (concat result
                                                     (format "Line %d: %s\n" line-num heading-text)
                                                     (if scheduled (format "  SCHEDULED: %s\n" scheduled) "")
                                                     (if deadline (format "  DEADLINE: %s\n" deadline) "")
                                                     "\n"))))))))
                    (write-region result nil filename)
                    (format "All TODO items written to %s" filename)))

(emacs-mcp-defmcp mcp-org-agenda-goto (target-type target &optional agenda-type context-lines)
                  "Go to the source location of an agenda item and return file path and content."
                  :description "Go to the source location of an agenda item and return file path and content"
                  :parameters ((:name "target-type"
                                :type "string"
                                :required t
                                :description "Either 'agenda_line' or 'agenda_text'")
                               (:name "target"
                                :type "string"
                                :required t
                                :description "Either agenda line number (1-based) or agenda item text")
                               (:name "agenda-type"
                                :type "string"
                                :required nil
                                :description "Agenda type to work with (default 'a')")
                               (:name "context-lines"
                                :type "number"
                                :required nil
                                :description "Number of lines before/after to show for context"))
                  (unless agenda-type (setq agenda-type "a"))
                  (unless context-lines (setq context-lines 5))
                  (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                    (make-directory "/tmp/ClaudeWorkingFolder" t))

                  (let ((filename "/tmp/ClaudeWorkingFolder/agenda_goto_result.txt")
                        (result ""))
                    (save-window-excursion
                      (let ((org-agenda-window-setup 'current-window))
                        (org-agenda nil agenda-type)
                        (with-current-buffer "*Org Agenda*"
                          (goto-char (point-min))
                          (cond
                           ((string= target-type "agenda_line")
                            (forward-line (1- (string-to-number target)))
                            (condition-case err
                                (progn
                                  (org-agenda-goto)
                                  (let* ((file-name (buffer-file-name))
                                         (line-num (line-number-at-pos))
                                         (heading (org-get-heading t t t t))
                                         (start-line (max 1 (- line-num context-lines)))
                                         (end-line (+ line-num context-lines))
                                         (content ""))
                                    (setq result (concat result "=== AGENDA ITEM SOURCE ===\n"))
                                    (setq result (concat result (format "File: %s\n" file-name)))
                                    (setq result (concat result (format "Line: %d\n" line-num)))
                                    (setq result (concat result (format "Heading: %s\n\n" heading)))
                                    (setq result (concat result (format "=== CONTEXT (lines %d-%d) ===\n" start-line end-line)))
                                    (save-excursion
                                      (goto-char (point-min))
                                      (forward-line (1- start-line))
                                      (let ((current-line start-line))
                                        (while (and (<= current-line end-line) (not (eobp)))
                                          (setq content (concat content
                                                                (format "%4d%s %s\n"
                                                                        current-line
                                                                        (if (= current-line line-num) "→" " ")
                                                                        (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                                          (forward-line 1)
                                          (setq current-line (1+ current-line)))))
                                    (setq result (concat result content))))
                              (error (setq result (format "Error going to agenda item at line %s: %s\n" target (error-message-string err))))))
                           ((string= target-type "agenda_text")
                            (if (search-forward target nil t)
                                (condition-case err
                                    (progn
                                      (beginning-of-line)
                                      (org-agenda-goto)
                                      (let* ((file-name (buffer-file-name))
                                             (line-num (line-number-at-pos))
                                             (heading (org-get-heading t t t t))
                                             (start-line (max 1 (- line-num context-lines)))
                                             (end-line (+ line-num context-lines))
                                             (content ""))
                                        (setq result (concat result "=== AGENDA ITEM SOURCE ===\n"))
                                        (setq result (concat result (format "File: %s\n" file-name)))
                                        (setq result (concat result (format "Line: %d\n" line-num)))
                                        (setq result (concat result (format "Heading: %s\n\n" heading)))
                                        (setq result (concat result (format "=== CONTEXT (lines %d-%d) ===\n" start-line end-line)))
                                        (save-excursion
                                          (goto-char (point-min))
                                          (forward-line (1- start-line))
                                          (let ((current-line start-line))
                                            (while (and (<= current-line end-line) (not (eobp)))
                                              (setq content (concat content
                                                                    (format "%4d%s %s\n"
                                                                            current-line
                                                                            (if (= current-line line-num) "→" " ")
                                                                            (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                                              (forward-line 1)
                                              (setq current-line (1+ current-line)))))
                                        (setq result (concat result content))))
                                  (error (setq result (format "Error going to agenda item containing '%s': %s\n" target (error-message-string err)))))
                              (setq result (format "Agenda item containing '%s' not found\n" target))))
                           (t (error "target_type must be either 'agenda_line' or 'agenda_text'"))))))
                    (write-region result nil filename)
                    (format "Agenda goto result written to %s:\n%s" filename result)))

;;;; File Operations

(emacs-mcp-defmcp mcp-open-file (file-paths)
                  "Open files in Emacs and return buffer names."
                  :description "Open one or more files in Emacs in the background"
                  :parameters ((:name "file-paths"
                                :type "(list string)"
                                :required t
                                :description "JSON array of file paths to open, e.g. [\"file1.el\", \"file2.org\"]"))
                  (let ((results '())
                        (blocked-files '()))
                    (dolist (file-path file-paths)
                      (cond
                       ((and (boundp 'emacs-mcp-restrict-file-access)
                             emacs-mcp-restrict-file-access
                             (fboundp 'emacs-mcp-file-access-allowed-p)
                             (not (emacs-mcp-file-access-allowed-p file-path)))
                        (push (format "%s -> BLOCKED (file access restricted)" file-path) blocked-files))
                       (t
                        (condition-case err
                            (let ((buffer (find-file-noselect file-path)))
                              (push (format "%s -> %s" file-path (buffer-name buffer)) results))
                          (error (push (format "%s -> Error: %s" file-path (error-message-string err)) results))))))

                    (let ((result (format "Files opened:\n%s" (mapconcat 'identity (reverse results) "\n"))))
                      (if blocked-files
                          (concat result "\nBlocked files:\n" (mapconcat 'identity (reverse blocked-files) "\n"))
                        result))))

;;;; Emacs Search and Introspection

(emacs-mcp-defmcp mcp-emacs-search (pattern &optional type predicate)
                  "Search for Emacs symbols, commands, or variables matching a pattern."
                  :description "Search for Emacs symbols using apropos functions"
                  :parameters ((:name "pattern"
                                :type "string"
                                :required t
                                :description "Pattern to search for")
                               (:name "type"
                                :type "mcp-search-type"
                                :required nil
                                :description "Type: all, commands, variables, functions")
                               (:name "predicate"
                                :type "(or string nil)"
                                :required nil
                                :description "Optional predicate for filtering"))
                  (let ((search-type (or type "all"))
                        (result ""))
                    (cond
                     ((string= search-type "commands")
                      (setq result (format "=== COMMANDS MATCHING '%s' ===\n" pattern))
                      (dolist (sym (apropos-internal pattern 'commandp))
                        (when (commandp sym)
                          (setq result (concat result (symbol-name sym) "\n")))))
                     ((string= search-type "variables")
                      (setq result (format "=== VARIABLES MATCHING '%s' ===\n" pattern))
                      (dolist (sym (apropos-internal pattern))
                        (when (boundp sym)
                          (setq result (concat result (symbol-name sym) "\n")))))
                     ((string= search-type "functions")
                      (setq result (format "=== FUNCTIONS MATCHING '%s' ===\n" pattern))
                      (dolist (sym (apropos-internal pattern 'fboundp))
                        (when (fboundp sym)
                          (setq result (concat result (symbol-name sym) "\n")))))
                     (t
                      (setq result (format "=== ALL SYMBOLS MATCHING '%s' ===\n" pattern))
                      (let ((pred-func (when (and predicate (not (string-empty-p predicate)))
                                         (intern predicate))))
                        (dolist (sym (apropos-internal pattern pred-func))
                          (let ((types '()))
                            (when (fboundp sym) (push "function" types))
                            (when (boundp sym) (push "variable" types))
                            (when (commandp sym) (push "command" types))
                            (setq result (concat result (symbol-name sym)
                                                 (if types (concat " (" (mapconcat 'identity types ", ") ")") "")
                                                 "\n")))))))
                    result))

(emacs-mcp-defmcp mcp-emacs-describe (symbol-names &optional type)
                  "Get comprehensive documentation for Emacs symbols."
                  :description "Get detailed documentation for one or more Emacs symbols"
                  :parameters ((:name "symbol-names"
                                :type "(list string)"
                                :required t
                                :description "JSON array of symbol names, e.g. [\"describe-function\", \"symbol-value\"]")
                               (:name "type"
                                :type "string"
                                :required nil
                                :description "Type: function, variable, or symbol"))
                  (let ((desc-type (or type "symbol"))
                        (results '()))
                    (dolist (symbol-name symbol-names)
                      (let ((sym (intern symbol-name))
                            (desc ""))
                        (setq desc (format "=== SYMBOL: %s ===\n" symbol-name))
                        (cond
                         ((string= desc-type "function")
                          (if (fboundp sym)
                              (setq desc (concat desc (save-window-excursion
                                                        (with-temp-buffer
                                                          (describe-function sym)
                                                          (with-current-buffer "*Help*"
                                                            (buffer-string))))))
                            (setq desc (concat desc "Symbol is not a function"))))
                         ((string= desc-type "variable")
                          (if (boundp sym)
                              (setq desc (concat desc (save-window-excursion
                                                        (with-temp-buffer
                                                          (describe-variable sym)
                                                          (with-current-buffer "*Help*"
                                                            (buffer-string))))))
                            (setq desc (concat desc "Symbol is not a variable"))))
                         (t
                          (when (fboundp sym)
                            (setq desc (concat desc "=== FUNCTION ===\n"))
                            (setq desc (concat desc (save-window-excursion
                                                      (with-temp-buffer
                                                        (describe-function sym)
                                                        (with-current-buffer "*Help*"
                                                          (buffer-string)))) "\n\n")))
                          (when (boundp sym)
                            (setq desc (concat desc "=== VARIABLE ===\n"))
                            (setq desc (concat desc (save-window-excursion
                                                      (with-temp-buffer
                                                        (describe-variable sym)
                                                        (with-current-buffer "*Help*"
                                                          (buffer-string)))))))
                          (when (and (not (fboundp sym)) (not (boundp sym)))
                            (setq desc (concat desc "Symbol not found as function or variable")))))
                        (push desc results)))
                    (mapconcat 'identity (reverse results) "\n\n")))

(emacs-mcp-defmcp mcp-emacs-keymap-analysis (buffer-names &optional include-global)
                  "Dump keymaps for buffer contexts to files."
                  :description "Analyze keymaps for one or more buffer contexts"
                  :parameters ((:name "buffer-names"
                                :type "(list string)"
                                :required t
                                :description "List of buffer names to analyze")
                               (:name "include-global"
                                :type "boolean"
                                :required nil
                                :description "Include global keymap analysis"))
                  (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                    (make-directory "/tmp/ClaudeWorkingFolder" t))

                  (let ((successful-files '())
                        (blocked-buffers '()))
                    (dolist (buffer-name buffer-names)
                      (cond
                       ((emacs-mcp-buffer-blocked-p buffer-name)
                        (push (format "%s: BLOCKED (contains sensitive pattern)" buffer-name) blocked-buffers))
                       (t
                        (condition-case err
                            (let ((filename (format "/tmp/ClaudeWorkingFolder/keymap_analysis_%s.txt"
                                                    (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" buffer-name))))
                              (with-temp-buffer
                                (insert (format "=== KEYMAP ANALYSIS FOR BUFFER: %s ===\n\n" buffer-name))

                                ;; Get keymap data without creating popup windows
                                (let ((target-buffer (get-buffer buffer-name)))
                                  (if target-buffer
                                      (progn
                                        (insert (format "Major mode: %s\n\n"
                                                        (buffer-local-value 'major-mode target-buffer)))
                                        (insert "=== BUFFER KEY BINDINGS ===\n")
                                        ;; Get keymaps directly without describe-buffer-bindings
                                        (with-current-buffer target-buffer
                                          (let ((major-map (current-local-map))
                                                (minor-maps (current-minor-mode-maps)))
                                            (insert "=== MAJOR MODE KEYMAP ===\n")
                                            (when major-map
                                              (insert (format "Keymap: %s\n" major-map)))
                                            (insert "\n=== MINOR MODE KEYMAPS ===\n")
                                            (dolist (map minor-maps)
                                              (when map
                                                (insert (format "Keymap: %s\n" map))))
                                            (when include-global
                                              (insert "\n=== GLOBAL KEYMAP ===\n")
                                              (insert (format "Global keymap: %s\n" (current-global-map))))))
                                        (when include-global
                                          (insert "\n=== NOTE: Global bindings included above ===\n")))
                                    (insert (format "Error: Buffer '%s' not found\n" buffer-name))))

                                (write-region (point-min) (point-max) filename))
                              (push filename successful-files))
                          (error
                           (message "Error processing buffer '%s': %s" buffer-name (error-message-string err)))))))

                    (let ((result (format "Keymap analysis written to files: %s" (mapconcat 'identity (reverse successful-files) ", "))))
                      (if blocked-buffers
                          (concat result "\nBlocked buffers: " (mapconcat 'identity (reverse blocked-buffers) ", "))
                        result))))

;;;; Buffer Operations

(emacs-mcp-defmcp mcp-emacs-buffer-info (buffer-names &optional include-content include-variables)
                  "Get comprehensive buffer information and write to files."
                  :description "Get buffer info including content, mode details, and variables"
                  :parameters ((:name "buffer-names"
                                :type "(list string)"
                                :required t
                                :description "List of buffer names to analyze")
                               (:name "include-content"
                                :type "boolean"
                                :required nil
                                :description "Include buffer content")
                               (:name "include-variables"
                                :type "boolean"
                                :required nil
                                :description "Include key variables"))
                  (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                    (make-directory "/tmp/ClaudeWorkingFolder" t))

                  (let ((successful-files '())
                        (blocked-buffers '())
                        (with-content (if include-content include-content t))
                        (with-vars (if include-variables include-variables t)))
                    (dolist (buffer-name buffer-names)
                      (cond
                       ((emacs-mcp-buffer-blocked-p buffer-name)
                        (push (format "%s: BLOCKED (contains sensitive pattern)" buffer-name) blocked-buffers))
                       (t
                        (condition-case err
                            (with-current-buffer buffer-name
                              (let ((info "")
                                    (sanitized-name (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" buffer-name))
                                    (filename (format "/tmp/ClaudeWorkingFolder/buffer_info_%s.txt"
                                                      (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" buffer-name))))

                                (setq info (concat info (format "=== BUFFER INFO: %s ===\n" (buffer-name))))
                                (setq info (concat info (format "File: %s\n" (or (buffer-file-name) "no file"))))
                                (setq info (concat info (format "Major mode: %s\n" (symbol-name major-mode))))
                                (setq info (concat info (format "Buffer size: %d characters\n" (buffer-size))))
                                (setq info (concat info (format "Modified: %s\n\n" (if (buffer-modified-p) "yes" "no"))))

                                (when with-vars
                                  (setq info (concat info "=== KEY VARIABLES ===\n"))
                                  (setq info (concat info (format "default-directory: %s\n" default-directory)))
                                  (setq info (concat info (format "tab-width: %s\n" tab-width)))
                                  (setq info (concat info (format "fill-column: %s\n" fill-column)))
                                  (setq info (concat info (format "buffer-read-only: %s\n\n" (if buffer-read-only "yes" "no")))))

                                (when with-content
                                  (setq info (concat info "=== BUFFER CONTENT ===\n"))
                                  (let ((lines (split-string (buffer-string) "\n"))
                                        (line-num 0))
                                    (setq info (concat info (mapconcat
                                                             (lambda (line)
                                                               (setq line-num (1+ line-num))
                                                               (format "%4d→%s" line-num line))
                                                             lines "\n")))))

                                (write-region info nil filename)
                                (push filename successful-files)))
                          (error
                           (message "Error processing buffer '%s': %s" buffer-name (error-message-string err)))))))

                    (let ((result (format "Buffer info written to files: %s" (mapconcat 'identity (reverse successful-files) ", "))))
                      (if blocked-buffers
                          (concat result "\nBlocked buffers: " (mapconcat 'identity (reverse blocked-buffers) ", "))
                        result))))

(emacs-mcp-defmcp mcp-check-parens (file-paths)
                  "Run check-parens on Lisp files to validate parentheses balance."
                  :description "Validate parentheses balance in Lisp code files"
                  :parameters ((:name "file-paths"
                                :type "(list string)"
                                :required t
                                :description "List of file paths to check"))
                  (let ((results '()))
                    (dolist (file-path file-paths)
                      (let* ((existing-buffer (find-buffer-visiting file-path))
                             (temp-buffer (find-file-noselect file-path))
                             (buffer-was-created (not existing-buffer)))
                        (unwind-protect
                            (with-current-buffer temp-buffer
                              (condition-case err
                                  (progn
                                    (check-parens)
                                    (push (format "%s: Parentheses are balanced correctly" file-path) results))
                                (error
                                 (push (format "%s: Parentheses error at line %d, column %d: %s"
                                               file-path
                                               (line-number-at-pos (point))
                                               (current-column)
                                               (error-message-string err)) results))))
                          (when buffer-was-created
                            (kill-buffer temp-buffer)))))
                    (mapconcat 'identity (reverse results) "\n")))


;;;; Workspace Management

(emacs-mcp-defmcp mcp-get-workspace-buffers (&optional workspace-name)
                  "Get buffers in workspaces and write to file."
                  :description "Get list of buffers in each workspace"
                  :parameters ((:name "workspace-name"
                                :type "string"
                                :required nil
                                :description "Specific workspace name (optional)"))
                  (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                    (make-directory "/tmp/ClaudeWorkingFolder" t))

                  (let ((filename "/tmp/ClaudeWorkingFolder/workspace_buffers.txt")
                        (result ""))
                    (cond
                     ;; Check for workspace systems
                     ((and (fboundp '+workspace-buffer-list) (fboundp '+workspace-list-names))
                      (if workspace-name
                          (condition-case err
                              (let* ((workspace-names (+workspace-list-names))
                                     (current-workspace (+workspace-current-name)))
                                (if (member workspace-name workspace-names)
                                    (progn
                                      (+workspace-switch workspace-name t)
                                      (setq result (format "=== WORKSPACE: %s ===\n" workspace-name))
                                      (let ((buffers (mapcar 'buffer-name (+workspace-buffer-list))))
                                        (dolist (buf buffers)
                                          (setq result (concat result buf "\n"))))
                                      (unless (string= current-workspace workspace-name)
                                        (+workspace-switch current-workspace t)))
                                  (setq result (format "Workspace '%s' not found. Available: %s\n"
                                                       workspace-name
                                                       (mapconcat 'identity workspace-names ", ")))))
                            (error (setq result (format "Error accessing workspace: %s\n" (error-message-string err)))))
                        ;; Get all workspaces
                        (condition-case err
                            (let* ((workspace-names (+workspace-list-names))
                                   (current-workspace (+workspace-current-name)))
                              (setq result "=== ALL WORKSPACE BUFFERS ===\n")
                              (dolist (ws-name workspace-names)
                                (setq result (concat result (format "\n=== WORKSPACE: %s ===\n" ws-name)))
                                (+workspace-switch ws-name t)
                                (let ((buffers (mapcar 'buffer-name (+workspace-buffer-list))))
                                  (dolist (buf buffers)
                                    (setq result (concat result buf "\n")))))
                              (+workspace-switch current-workspace t))
                          (error (setq result (format "Error accessing workspaces: %s\n" (error-message-string err)))))))
                     (t
                      (setq result "No Doom workspace system found - showing current buffer list\n")
                      (let ((buffers (mapcar 'buffer-name (buffer-list))))
                        (dolist (buf buffers)
                          (setq result (concat result buf "\n"))))))

                    (write-region result nil filename)
                    (format "Workspace buffers written to %s" filename)))

(emacs-mcp-defmcp mcp-rename-workspace (workspace-identifier new-name)
                  "Rename a workspace by its slot number or current name."
                  :description "Rename a workspace by its slot number or current name"
                  :parameters ((:name "workspace-identifier"
                                :type "string"
                                :required t
                                :description "Current workspace name or slot number to rename")
                               (:name "new-name"
                                :type "string"
                                :required t
                                :description "New name for the workspace"))
                  (if (and (fboundp '+workspace/rename) (fboundp '+workspace-get))
                      (condition-case err
                          (let* ((current-workspace (+workspace-current))
                                 (workspace-names (+workspace-list-names))
                                 (target-workspace (or
                                                    ;; Try to find by name first
                                                    (cl-find workspace-identifier workspace-names :test 'string=)
                                                    ;; Try to find by number
                                                    (when (string-match-p "^[0-9]+$" workspace-identifier)
                                                      (let ((index (string-to-number workspace-identifier)))
                                                        (when (and (>= index 0) (< index (length workspace-names)))
                                                          (nth index workspace-names)))))))
                            (if target-workspace
                                (let ((current-workspace (+workspace-current-name))
                                      (old-name nil))
                                  (condition-case rename-err
                                      (progn
                                        ;; Switch to target workspace
                                        (+workspace-switch target-workspace t)
                                        ;; Rename it (this operates on current workspace)
                                        (setq old-name (+workspace-rename (+workspace-current-name) new-name))
                                        ;; Switch back to original workspace
                                        (unless (string= current-workspace target-workspace)
                                          (+workspace-switch current-workspace t))
                                        (if old-name
                                            (format "Successfully renamed Doom workspace '%s' to '%s'" old-name new-name)
                                          (format "Failed to rename Doom workspace '%s'" workspace-identifier)))
                                    (error
                                     ;; Try to switch back on error
                                     (ignore-errors (+workspace-switch current-workspace t))
                                     (format "Error during rename: %s" (error-message-string rename-err)))))
                              (format "Doom workspace '%s' not found. Available: %s" workspace-identifier (mapconcat 'identity workspace-names ", "))))
                        (error (format "Error renaming Doom workspace: %s" (error-message-string err))))
                    "Doom workspace system not available"))

(emacs-mcp-defmcp mcp-create-workspace (workspace-name)
                  "Create a new workspace with a given name."
                  :description "Create a new workspace with a given name"
                  :parameters ((:name "workspace-name"
                                :type "string"
                                :required t
                                :description "Name for the new workspace"))
                  (if (fboundp '+workspace-new)
                      (condition-case err
                          (progn
                            (+workspace-new workspace-name)
                            (format "Successfully created Doom workspace '%s'" workspace-name))
                        (error (format "Error creating Doom workspace: %s" (error-message-string err))))
                    "Doom workspace system not available"))

(emacs-mcp-defmcp mcp-delete-workspace (workspace-identifier)
                  "Delete a workspace by name or identifier."
                  :description "Delete a workspace by name or identifier"
                  :parameters ((:name "workspace-identifier"
                                :type "string"
                                :required t
                                :description "Workspace name or identifier to delete"))
                  (if (and (fboundp 'persp-kill) (fboundp '+workspace-list-names))
                      (condition-case err
                          (let* ((workspace-names (+workspace-list-names))
                                 (target-workspace (or
                                                    (cl-find workspace-identifier workspace-names :test 'string=)
                                                    (when (string-match-p "^[0-9]+$" workspace-identifier)
                                                      (let ((index (string-to-number workspace-identifier)))
                                                        (when (and (>= index 0) (< index (length workspace-names)))
                                                          (nth index workspace-names)))))))
                            (if target-workspace
                                ;; Check for active sessions and terminals before deleting
                                (let* ((persp-obj (persp-get-by-name target-workspace))
                                       (workspace-buffers (when persp-obj (mapcar 'buffer-name (persp-buffers persp-obj))))
                                       (claude-buffers (cl-remove-if-not
                                                        (lambda (buf-name)
                                                          (string-match-p "^\\*claude:.*:\\*$" buf-name))
                                                        workspace-buffers))
                                       (terminal-buffers (cl-remove-if-not
                                                          (lambda (buf-name)
                                                            (or (string-match-p "^\\*vterm\\*" buf-name)
                                                                (string-match-p "^\\*eat\\*" buf-name)
                                                                (string-match-p "^\\*eshell\\*" buf-name)
                                                                (string-match-p "^\\*shell\\*" buf-name)
                                                                (string-match-p "^\\*term\\*" buf-name)
                                                                (string-match-p "^\\*ansi-term\\*" buf-name)
                                                                (string-match-p "^\\*mistty\\*" buf-name)))
                                                          workspace-buffers))
                                       (protected-buffers (append claude-buffers terminal-buffers))
                                       (workspace-has-protected (> (length protected-buffers) 0)))
                                  (if workspace-has-protected
                                      (format "Cannot delete workspace '%s' - contains active sessions/terminals: %s. Please close these or move them to another workspace first."
                                              target-workspace
                                              (mapconcat 'identity protected-buffers ", "))
                                    (progn
                                      (persp-kill target-workspace)
                                      (format "Successfully deleted Doom workspace '%s'" target-workspace))))
                              (format "Doom workspace '%s' not found. Available: %s" workspace-identifier (mapconcat 'identity workspace-names ", "))))
                        (error (format "Error deleting Doom workspace: %s" (error-message-string err))))
                    "Doom workspace system not available"))

(emacs-mcp-defmcp mcp-move-protected-buffers-to-workspace (source-workspace target-workspace)
                  "Move all protected buffers from one workspace to another."
                  :description "Move all protected buffers (Claude Code sessions, terminals, etc.) from one workspace to another"
                  :parameters ((:name "source-workspace"
                                :type "string"
                                :required t
                                :description "Workspace containing protected buffers to move")
                               (:name "target-workspace"
                                :type "string"
                                :required t
                                :description "Workspace to move protected buffers to"))
                  (if (and (fboundp '+workspace-list-names) (fboundp '+workspace-buffer-list))
                      (condition-case err
                          (let* ((workspace-names (+workspace-list-names)))
                            (if (and (cl-find source-workspace workspace-names :test 'string=)
                                     (cl-find target-workspace workspace-names :test 'string=))
                                (let* ((source-buffers (mapcar 'buffer-name (+workspace-buffer-list source-workspace)))
                                       (claude-buffers (cl-remove-if-not
                                                        (lambda (buf-name)
                                                          (string-match-p "^\\*claude:.*:\\*$" buf-name))
                                                        source-buffers))
                                       (terminal-buffers (cl-remove-if-not
                                                          (lambda (buf-name)
                                                            (or (string-match-p "^\\*vterm\\*" buf-name)
                                                                (string-match-p "^\\*eat\\*" buf-name)
                                                                (string-match-p "^\\*eshell\\*" buf-name)
                                                                (string-match-p "^\\*shell\\*" buf-name)
                                                                (string-match-p "^\\*term\\*" buf-name)
                                                                (string-match-p "^\\*ansi-term\\*" buf-name)
                                                                (string-match-p "^\\*mistty\\*" buf-name)))
                                                          source-buffers))
                                       (protected-buffers (append claude-buffers terminal-buffers))
                                       (current-workspace (+workspace-current-name)))
                                  (if protected-buffers
                                      (progn
                                        ;; Switch to target workspace and add buffers
                                        (+workspace-switch target-workspace t)
                                        (dolist (buf-name protected-buffers)
                                          (when (get-buffer buf-name)
                                            (persp-add-buffer (get-buffer buf-name))))
                                        ;; Switch to source workspace and remove buffers
                                        (+workspace-switch source-workspace t)
                                        (dolist (buf-name protected-buffers)
                                          (when (get-buffer buf-name)
                                            (persp-remove-buffer (get-buffer buf-name))))
                                        ;; Switch back to original workspace
                                        (unless (or (string= current-workspace source-workspace)
                                                    (string= current-workspace target-workspace))
                                          (+workspace-switch current-workspace t))
                                        (format "Successfully moved %d protected buffers from '%s' to '%s': %s"
                                                (length protected-buffers)
                                                source-workspace
                                                target-workspace
                                                (mapconcat 'identity protected-buffers ", ")))
                                    (format "No protected buffers found in workspace '%s'" source-workspace)))
                              (format "Workspace not found. Source: %s, Target: %s. Available: %s"
                                      source-workspace
                                      target-workspace
                                      (mapconcat 'identity workspace-names ", "))))
                        (error (format "Error moving protected buffers: %s" (error-message-string err))))
                    "Doom workspace system not available"))

(emacs-mcp-defmcp mcp-setup-workspace-layout (workspace-name layout)
                  "Set up window layout for a workspace without switching away from current workspace."
                  :description "Set up window layout for a workspace without switching away from current workspace"
                  :parameters ((:name "workspace-name"
                                :type "string"
                                :required t
                                :description "Name of the workspace to configure")
                               (:name "layout"
                                :type "object"
                                :required t
                                :description "Layout configuration with primary_buffer, secondary_buffer, split_direction"))
                  (condition-case err
                      (let ((current-workspace (+workspace-current-name))
                            (target-workspace workspace-name)
                            (primary-buf (cdr (assoc 'primary_buffer layout)))
                            (secondary-buf (cdr (assoc 'secondary_buffer layout)))
                            (split-dir (or (cdr (assoc 'split_direction layout)) "horizontal")))
                        ;; Save current workspace state
                        (if (cl-find target-workspace (+workspace-list-names) :test 'string=)
                            (progn
                              ;; Switch to target workspace temporarily
                              (+workspace-switch target-workspace t)

                              ;; Set up primary buffer
                              (if (get-buffer primary-buf)
                                  (switch-to-buffer primary-buf)
                                (error "Primary buffer '%s' not found" primary-buf))

                              ;; Set up secondary buffer if specified
                              (when (and secondary-buf (get-buffer secondary-buf))
                                (if (string= split-dir "vertical")
                                    (split-window-below)
                                  (split-window-right))
                                (other-window 1)
                                (switch-to-buffer secondary-buf)
                                (other-window 1))

                              ;; Switch back to original workspace
                              (unless (string= current-workspace target-workspace)
                                (+workspace-switch current-workspace t))

                              (format "Successfully configured workspace '%s' with layout: %s%s"
                                      target-workspace
                                      primary-buf
                                      (if secondary-buf
                                          (format " + %s (%s split)" secondary-buf split-dir)
                                        "")))
                          (format "Workspace '%s' not found. Available: %s"
                                  target-workspace
                                  (mapconcat 'identity (+workspace-list-names) ", "))))
                    (error (format "Error setting up workspace layout: %s" (error-message-string err)))))

(emacs-mcp-defmcp mcp-view-buffer (buffer-names)
                  "Get buffer contents and write each to separate files."
                  :description "View buffer contents by writing to individual files"
                  :parameters ((:name "buffer-names"
                                :type "(list string)"
                                :required t
                                :description "List of buffer names to view"))
                  (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                    (make-directory "/tmp/ClaudeWorkingFolder" t))

                  (let ((successful-files '())
                        (blocked-buffers '()))
                    (dolist (buffer-name buffer-names)
                      (cond
                       ((emacs-mcp-buffer-blocked-p buffer-name)
                        (push (format "%s: BLOCKED (contains sensitive pattern)" buffer-name) blocked-buffers))
                       (t
                        (condition-case err
                            (with-current-buffer buffer-name
                              (let* ((sanitized-name (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" buffer-name))
                                     (filename (format "/tmp/ClaudeWorkingFolder/%s.txt" sanitized-name))
                                     (content (buffer-string))
                                     (lines (split-string content "\n"))
                                     (line-num 0))
                                (write-region (mapconcat (lambda (line)
                                                           (setq line-num (1+ line-num))
                                                           (format "%4d→%s" line-num line))
                                                         lines "\n")
                                              nil filename)
                                (push filename successful-files)))
                          (error
                           (message "Error processing buffer '%s': %s" buffer-name (error-message-string err)))))))

                    (let ((result (format "Buffer contents written to files: %s" (mapconcat 'identity (reverse successful-files) ", "))))
                      (if blocked-buffers
                          (concat result "\nBlocked buffers: " (mapconcat 'identity (reverse blocked-buffers) ", "))
                        result))))

;;;; Debugging Tools

(emacs-mcp-defmcp mcp-count-parens (file-path start-line end-line)
                  "Count opening and closing parentheses between two lines in a file."
                  :description "Count opening and closing parentheses between two lines in a file"
                  :parameters ((:name "file-path"
                                :type "string"
                                :required t
                                :description "Path to the file to analyze")
                               (:name "start-line"
                                :type "number"
                                :required t
                                :description "Starting line number (1-based)")
                               (:name "end-line"
                                :type "number"
                                :required t
                                :description "Ending line number (1-based)"))
                  (if (file-exists-p file-path)
                      (with-temp-buffer
                        (insert-file-contents file-path)
                        (goto-char (point-min))
                        (forward-line (1- start-line))
                        (let ((start-pos (point))
                              (open-count 0)
                              (close-count 0)
                              (in-string nil)
                              (in-comment nil))
                          (forward-line (- end-line start-line -1))
                          (let ((end-pos (point)))
                            (goto-char start-pos)
                            (while (< (point) end-pos)
                              (let ((char (char-after)))
                                (cond
                                 ;; Handle string state
                                 ((and (eq char ?\") (not in-comment))
                                  (unless (eq (char-before) ?\\)
                                    (setq in-string (not in-string))))
                                 ;; Handle comment state
                                 ((and (eq char ?\;) (not in-string))
                                  (setq in-comment t))
                                 ;; Count parentheses only if not in string or comment
                                 ((and (eq char ?\() (not in-string) (not in-comment))
                                  (setq open-count (1+ open-count)))
                                 ((and (eq char ?\)) (not in-string) (not in-comment))
                                  (setq close-count (1+ close-count)))
                                 ;; Reset comment state at newline
                                 ((eq char ?\n)
                                  (setq in-comment nil))))
                              (forward-char 1))
                            (format "Lines %d-%d: Opening parens: %d, Closing parens: %d, Net: %d"
                                    start-line end-line open-count close-count
                                    (- open-count close-count)))))
                    (format "File not found: %s" file-path)))

(emacs-mcp-defmcp mcp-check-parens-range (file-path start-line end-line)
                  "Check parentheses balance in a specific line range by copying to scratch buffer."
                  :description "Check parentheses balance in a specific line range by copying to scratch buffer"
                  :parameters ((:name "file-path"
                                :type "string"
                                :required t
                                :description "Path to the file to analyze")
                               (:name "start-line"
                                :type "number"
                                :required t
                                :description "Starting line number (1-based)")
                               (:name "end-line"
                                :type "number"
                                :required t
                                :description "Ending line number (1-based)"))
                  (if (file-exists-p file-path)
                      (with-temp-buffer
                        (insert-file-contents file-path)
                        (goto-char (point-min))
                        (forward-line (1- start-line))
                        (let ((region-start (point)))
                          (forward-line (- end-line start-line -1))
                          (let ((region-text (buffer-substring region-start (point))))
                            (with-temp-buffer
                              (insert region-text)
                              (emacs-lisp-mode)
                              (condition-case err
                                  (progn
                                    (check-parens)
                                    (format "%s (lines %d-%d): Parentheses are balanced correctly"
                                            file-path start-line end-line))
                                (error
                                 (format "%s (lines %d-%d): Parentheses error at line %d, column %d: %s"
                                         file-path
                                         (+ start-line (line-number-at-pos (point)) -2)
                                         end-line
                                         (+ start-line (line-number-at-pos (point)) -2)
                                         (current-column)
                                         (error-message-string err))))))))
                    (format "File not found: %s" file-path)))

(emacs-mcp-defmcp mcp-show-paren-balance (file-path start-line end-line)
                  "Show running parentheses balance count at the beginning of each line."
                  :description "Show running parentheses balance count at the beginning of each line"
                  :parameters ((:name "file-path"
                                :type "string"
                                :required t
                                :description "Path to the file to analyze")
                               (:name "start-line"
                                :type "number"
                                :required t
                                :description "Starting line number (1-based)")
                               (:name "end-line"
                                :type "number"
                                :required t
                                :description "Ending line number (1-based)"))
                  (if (file-exists-p file-path)
                      (with-temp-buffer
                        (insert-file-contents file-path)
                        (goto-char (point-min))
                        (forward-line (1- start-line))
                        (let ((result "")
                              (balance 0)
                              (current-line start-line)
                              (in-string nil)
                              (in-comment nil))
                          (while (and (<= current-line end-line) (not (eobp)))
                            (let ((line-start (point))
                                  (line-balance balance))
                              ;; Process each character in the line
                              (while (and (not (eolp)) (not (eobp)))
                                (let ((char (char-after)))
                                  (cond
                                   ;; Handle string state
                                   ((and (eq char ?\") (not in-comment))
                                    (unless (eq (char-before) ?\\)
                                      (setq in-string (not in-string))))
                                   ;; Handle comment state
                                   ((and (eq char ?\;) (not in-string))
                                    (setq in-comment t))
                                   ;; Count parentheses only if not in string or comment
                                   ((and (eq char ?\() (not in-string) (not in-comment))
                                    (setq balance (1+ balance)))
                                   ((and (eq char ?\)) (not in-string) (not in-comment))
                                    (setq balance (1- balance)))))
                                (forward-char 1))
                              ;; Add line with balance info
                              (let ((line-content (buffer-substring line-start (point))))
                                (setq result (concat result
                                                     (format "%4d [%+3d]: %s\n"
                                                             current-line
                                                             line-balance
                                                             (string-trim-right line-content)))))
                              ;; Reset comment state at newline
                              (setq in-comment nil)
                              (forward-line 1)
                              (setq current-line (1+ current-line))))
                          (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                            (make-directory "/tmp/ClaudeWorkingFolder" t))
                          (let ((output-file (format "/tmp/ClaudeWorkingFolder/paren_balance_%d_%d.txt" start-line end-line)))
                            (write-region result nil output-file)
                            (format "Parentheses balance written to %s (final balance: %+d)" output-file balance))))
                    (format "File not found: %s" file-path)))


;;;; Flymake/Flycheck Diagnostics Integration

(emacs-mcp-defmcp mcp-get-diagnostics (buffer-names)
  "Get Flymake and Flycheck diagnostics for specified buffers.
Collects diagnostics from both Flymake and Flycheck, covering LSP modes 
(eglot, lsp-mode) and other checkers that use these systems."
  :description "Get Flymake/Flycheck diagnostics for buffers"
  :parameters ((:name "buffer-names"
                :type "array"
                :required t
                :description "List of buffer names to get diagnostics for"))
  (let ((all-diagnostics '()))
    (dolist (buffer-name buffer-names)
      (when-let ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (let ((buffer-diagnostics '()))
            
            ;; Collect Flymake diagnostics
            (when (bound-and-true-p flymake-mode)
              (dolist (diag (flymake-diagnostics))
                (push (format "Line %d: [%s] %s (flymake)"
                             (line-number-at-pos (flymake-diagnostic-beg diag))
                             (flymake-diagnostic-type diag)
                             (flymake-diagnostic-text diag))
                      buffer-diagnostics)))
            
            ;; Collect Flycheck diagnostics  
            (when (and (bound-and-true-p flycheck-mode)
                       flycheck-current-errors)
              (dolist (err flycheck-current-errors)
                (push (format "Line %d: [%s] %s (%s)"
                             (or (flycheck-error-line err) 1)
                             (flycheck-error-level err)
                             (flycheck-error-message err)
                             (flycheck-error-checker err))
                      buffer-diagnostics)))
            
            (when buffer-diagnostics
              (push (format "=== %s ===\n%s"
                           buffer-name
                           (mapconcat 'identity (nreverse buffer-diagnostics) "\n"))
                    all-diagnostics))))))
    
    ;; Write results
    (emacs-mcp-ensure-output-directory)
    (let* ((output-file (format "%s/diagnostics_%s.txt"
                               emacs-mcp-output-directory
                               (format-time-string "%Y%m%d_%H%M%S")))
           (content (if all-diagnostics
                       (mapconcat 'identity (nreverse all-diagnostics) "\n\n")
                     "No diagnostics found.")))
      (write-region content nil output-file)
      (format "Diagnostics written to %s" output-file))))

(provide 'mcp-tools)


;;; mcp-tools.el ends here
