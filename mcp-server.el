;;; mcp-server.el --- MCP (Model Context Protocol) server for Emacs -*- lexical-binding: t; -*-

;; Author: Claude AI
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai, mcp

;;; Commentary:
;; This implements a Model Context Protocol (MCP) server in Emacs Lisp.
;; It provides tools that can be called by Claude Code CLI via the MCP protocol.
;; Communication happens via TCP on port 8765, with mcp-proxy.sh bridging stdio.

;;; Code:

(require 'json)
(require 'cl-lib)

;; Conditionally load type system module
(cl-eval-when (load eval)
  (let ((types-file (expand-file-name "mcp-types.el"
                                      (file-name-directory (or load-file-name buffer-file-name)))))
    (when (file-exists-p types-file)
      (load types-file nil t))))

;;;; Configuration

(defcustom emacs-mcp-port 8765
  "TCP port for the MCP server."
  :type 'integer
  :group 'emacsmcp)

(defcustom emacs-mcp-enabled t
  "Whether MCP server functionality is enabled."
  :type 'boolean
  :group 'emacsmcp)

;;;; State variables

(defvar emacs-mcp-server-process nil
  "The MCP TCP server process.")

(defvar emacs-mcp-client-connections nil
  "List of active MCP client connections.")



;;;; MCP Tool definition macro

(defun emacs-mcp-parse-new-schema (params)
  "Convert new parameter format to old schema format.
PARAMS is a list of parameter definitions in the new format:
  ((:name \"param1\" :type \"string\" :required t :description \"desc\"))
Returns an alist in the old format:
  ((param1 . (string \"desc\")))"
  (let ((result '()))
    (dolist (param params)
      (let ((name (plist-get param :name))
            (type (plist-get param :type))
            (desc (plist-get param :description))
            (required (plist-get param :required)))
        (when name
          ;; Convert string name to symbol for the alist key
          ;; Parse type expression if it's a string containing Lisp code
          (let ((parsed-type (if (and (stringp type)
                                      (string-prefix-p "(" type))
                                 (read type) ; Parse "(list string)" -> (list string)
                               (intern type)))) ; Keep simple types as symbols
            (push (cons (intern name)
                        (list parsed-type desc))
                  result)))))
    (nreverse result)))

(defmacro emacs-mcp-defmcp (name args docstring &rest body-and-properties)
  "Define an MCP tool function with embedded properties.
NAME is the function name, ARGS is the argument list, DOCSTRING is the documentation.
The DOCSTRING is automatically used as the MCP tool description.
The remaining BODY-AND-PROPERTIES can contain :parameters/:mcp-schema and function body.

Supports two schema formats:
Old format: :mcp-schema '((param . (type \"description\")))
New format: :parameters ((:name \"param\" :type \"string\" :required t :description \"desc\"))"
  (let ((schema nil)
        (body '()))
    
    ;; Parse body and properties
    (while body-and-properties
      (let ((item (car body-and-properties)))
        (cond
         ;; Old format schema
         ((eq item :mcp-schema)
          (setq schema (cadr body-and-properties))
          (setq body-and-properties (cddr body-and-properties)))
         ;; New format schema
         ((eq item :parameters)
          (let ((params (cadr body-and-properties)))
            ;; Convert new format to old format
            (setq schema (emacs-mcp-parse-new-schema params)))
          (setq body-and-properties (cddr body-and-properties)))
         (t
          (push item body)
          (setq body-and-properties (cdr body-and-properties))))))
    
    (setq body (nreverse body))
    
    `(progn
       (defun ,name ,args
         ,docstring
         ,@body)
       (put ',name :mcp-tool t)
       ;; Always use docstring as MCP description
       (put ',name :mcp-description ,docstring)
       ,(when schema
          `(put ',name :mcp-schema ',schema))
       ',name)))

;;;; JSON-RPC message handling

(defun emacs-mcp-send-response (process response)
  "Send JSON-RPC RESPONSE to PROCESS."
  (let ((json-str (json-encode response)))
    (process-send-string process (concat json-str "\n"))))

(defun emacs-mcp-process-message (process message-string)
  "Process a single MCP message from PROCESS."
  (condition-case err
      (when (and message-string (not (string-empty-p (string-trim message-string))))
        (let* ((request (json-parse-string message-string :object-type 'alist))
               (method (alist-get 'method request))
               (id (alist-get 'id request))
               (params (alist-get 'params request))
               (response (emacs-mcp-handle-method method id params)))
          (when response
            (emacs-mcp-send-response process response))))
    (error
     (let* ((request-id (or (ignore-errors 
                              (alist-get 'id (json-parse-string message-string :object-type 'alist)))
                            nil))
            (error-response `((jsonrpc . "2.0")
                              (id . ,request-id)
                              (error . ((code . -32603)
                                        (message . ,(format "Internal error: %s" (error-message-string err))))))))
       (emacs-mcp-send-response process error-response)))))

(defun emacs-mcp-handle-method (method id params)
  "Handle MCP METHOD with ID and PARAMS, return response."
  (cond
   ;; Initialize handshake
   ((string= method "initialize")
    `((jsonrpc . "2.0")
      (id . ,id)
      (result . ((protocolVersion . "2024-11-05")
                 (capabilities . ((tools . ((listChanged . t)))))
                 (serverInfo . ((name . "emacs-mcp-server")
                                (version . "1.0.0")))))))

   ;; List available tools
   ((string= method "tools/list")
    (let ((tools (emacs-mcp-discover-tools)))
      `((jsonrpc . "2.0")
        (id . ,id)
        (result . ((tools . ,(apply #'vector tools)))))))

   ;; Call a tool
   ((string= method "tools/call")
    (let* ((tool-name (alist-get 'name params))
           (tool-args (alist-get 'arguments params))
           (result (emacs-mcp-call-tool tool-name tool-args)))
      `((jsonrpc . "2.0")
        (id . ,id)
        (result . ((content . [((type . "text")
                                (text . ,result))]))))))



   ;; Method not found
   (t
    `((jsonrpc . "2.0")
      (id . ,id)
      (error . ((code . -32601)
                (message . ,(format "Method not found: %s" method))))))))

;;;; Tool discovery and execution

(defun emacs-mcp-discover-tools ()
  "Discover all available MCP tools."
  (let ((tools '()))
    (mapatoms
     (lambda (symbol)
       (when (and (fboundp symbol)
                  (get symbol :mcp-tool))
         (let* ((name (symbol-name symbol))
                (description (or (get symbol :mcp-description) 
                                 (format "MCP tool: %s" name)))
                (schema (or (get symbol :mcp-schema) '()))
                (input-schema (emacs-mcp-build-json-schema schema)))
           (push `((name . ,name)
                   (description . ,description)
                   (inputSchema . ,input-schema))
                 tools)))))
    (nreverse tools)))


(defun emacs-mcp-call-tool (tool-name tool-args)
  "Call MCP tool TOOL-NAME with TOOL-ARGS."
  (let ((symbol (intern tool-name)))
    (if (and (fboundp symbol) (get symbol :mcp-tool))
        (condition-case err
            (let* ((schema (get symbol :mcp-schema))
                   (validation-result (emacs-mcp-validate-parameters tool-args schema)))
              (if (car validation-result)
                  ;; Validation passed, execute function
                  (let* ((func-args (help-function-arglist symbol))
                         (ordered-params (emacs-mcp-map-args tool-args func-args)))
                    (format "%s" (apply symbol ordered-params)))
                ;; Validation failed, return error
                (format "Validation error in %s: %s" tool-name (cdr validation-result))))
          (error (format "Error executing %s: %s" tool-name (error-message-string err))))
      (format "Tool not found: %s" tool-name))))

(defun emacs-mcp-map-args (params-alist func-args)
  "Map PARAMS-ALIST to FUNC-ARGS order."
  (let ((mapped-args '())
        (in-rest nil))
    (dolist (arg-spec func-args)
      (cond
       ;; Handle &rest parameters
       ((eq arg-spec '&rest)
        (setq in-rest t))
       ;; Skip other special markers
       ((memq arg-spec '(&optional &key)))
       ;; Handle regular and rest arguments
       (t
        (let* ((arg-name (if (listp arg-spec) (car arg-spec) arg-spec))
               (arg-key (symbol-name arg-name))
               (param-value (alist-get (intern arg-key) params-alist)))
          ;; Convert JSON arrays (vectors) to Lisp lists
          (when (vectorp param-value)
            (setq param-value (append param-value nil)))
          ;; Convert JSON string arrays to Lisp lists
          (when (and (stringp param-value)
                     (string-prefix-p "[" param-value)
                     (string-suffix-p "]" param-value))
            (condition-case nil
                (setq param-value (append (json-parse-string param-value) nil))
              (error nil))) ; If parsing fails, keep as string
          ;; For &rest parameters, only add if value is not nil (wasn't provided)
          (if in-rest
              (when (and param-value (not (equal param-value '())))
                ;; For rest params, spread the list
                (if (listp param-value)
                    (setq mapped-args (append mapped-args param-value))
                  (push param-value mapped-args)))
            ;; For regular parameters, always add (can be nil)
            (push param-value mapped-args))))))
    (nreverse mapped-args)))

;;;; Network server

(defun emacs-mcp-filter (process string)
  "Filter function for MCP TCP connections.
PROCESS is the network process, STRING is the received data."
  ;; Buffer incomplete messages
  (let ((buffer (or (process-get process :mcp-buffer) "")))
    (setq buffer (concat buffer string))

    ;; Process complete JSON-RPC messages (ended by newline)
    (let ((lines (split-string buffer "\n" t)))
      (if (string-suffix-p "\n" buffer)
          ;; All lines are complete messages
          (progn
            (process-put process :mcp-buffer "")
            (dolist (line lines)
              (emacs-mcp-process-message process line)))
        ;; Last line is incomplete, save it for next call
        (let ((complete-lines (butlast lines))
              (incomplete-line (car (last lines))))
          (process-put process :mcp-buffer (or incomplete-line ""))
          (dolist (line complete-lines)
            (emacs-mcp-process-message process line)))))))

(defun emacs-mcp-sentinel (process event)
  "Sentinel function for MCP TCP connections.
PROCESS is the network process, EVENT is the event description."
  (when (string-match "^\\(closed\\|failed\\)" event)
    (setq emacs-mcp-client-connections
          (delq process emacs-mcp-client-connections))
    (message "MCP client disconnected")))

(defun emacs-mcp-server-filter (process string)
  "Handle incoming data from MCP clients.
PROCESS is the server process receiving data, STRING is the received data."
  (emacs-mcp-filter process string))

;;;; Server management

(defun emacs-mcp-start-server ()
  "Start the MCP TCP server."
  (interactive)
  (when (and emacs-mcp-enabled
             (not (and emacs-mcp-server-process
                       (process-live-p emacs-mcp-server-process))))
    (setq emacs-mcp-server-process
          (make-network-process
           :name "emacsmcp-mcp"
           :service emacs-mcp-port
           :server t
           :family 'ipv4
           :host 'local
           :filter #'emacs-mcp-server-filter))
    (message "Emacs MCP server started on port %d" emacs-mcp-port)))

(defun emacs-mcp-stop-server ()
  "Stop the MCP TCP server."
  (interactive)
  ;; Stop server process
  (when (and emacs-mcp-server-process
             (process-live-p emacs-mcp-server-process))
    (delete-process emacs-mcp-server-process)
    (setq emacs-mcp-server-process nil))

  ;; Close all client connections
  (dolist (conn emacs-mcp-client-connections)
    (when (process-live-p conn)
      (delete-process conn)))
  (setq emacs-mcp-client-connections nil)

  (message "Emacs MCP server stopped"))

;;;; Load example tools

(defun emacs-mcp-load-examples ()
  "Load example MCP tools from examples/mcp-tools.el."
  (interactive)
  (let ((examples-file (expand-file-name "examples/mcp-tools.el" 
                                         (file-name-directory (or load-file-name buffer-file-name)))))
    (when (file-exists-p examples-file)
      (load examples-file nil t)
      (message "Loaded MCP example tools from %s" examples-file))
    (unless (file-exists-p examples-file)
      (message "Example tools file not found: %s" examples-file))))

;;;; Auto-start server via hook

(add-hook 'emacs-mcp-start-hook #'emacs-mcp-start-server)

(provide 'mcp-server)

;;; mcp-server.el ends here
