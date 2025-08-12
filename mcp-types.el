;;; claude-code-mcp-types.el --- Type system for MCP server -*- lexical-binding: t; -*-

;; Author: Claude AI
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai, mcp, types

;;; Commentary:
;; This provides the type system for the MCP server, including:
;; - JSON Schema generation from Emacs Lisp type specifications
;; - Parameter validation (currently disabled)
;; - Type descriptions for error messages

;;; Code:

(require 'cl-lib)

;;;; Enhanced type system with cl-deftype support

;; Configuration
(defcustom claude-code-mcp-enable-validation t
  "Whether to enable parameter validation in MCP tools.
When nil, all validation is bypassed for performance or debugging."
  :type 'boolean
  :group 'claude-code)


;;;; Simplified JSON Schema generation

(defun claude-code-mcp-build-json-schema (schema)
  "Build JSON Schema from simplified type specifications.
Supports: string, integer, number, boolean, (list type), (choice vals...), (or type nil)."
  ;; Handle quoted schemas
  (when (and (consp schema) (eq (car schema) 'quote))
    (setq schema (cadr schema)))
  
  (cond
   ;; Empty or nil schema - no parameters
   ((or (null schema) (equal schema '()))
    `((type . "object") 
      (properties . ,(make-hash-table :test 'equal))
      (required . ,(vector))
      (additionalProperties . :json-false)))
   
   ;; Schema format: ((param-name . (type "description")) ...)
   ((listp schema)
    (let ((properties (make-hash-table :test 'equal))
          (required '()))
      (dolist (item schema)
        (when (and (consp item) (>= (length item) 2))
          (let* ((param-name (symbol-name (car item)))
                 (param-spec (cdr item))
                 (param-type (car param-spec))
                 (param-desc (cadr param-spec)))
            (push param-name required)
            (let ((json-type (claude-code-mcp-type-to-json-schema param-type)))
              (when param-desc
                ;; Combine manual description with automatic type-based format hint
                (let* ((type-hint (claude-code-mcp-simple-type-description param-type))
                       (combined-desc (if (and (consp param-type) (memq (car param-type) '(list or)))
                                          (format "%s - %s" param-desc type-hint)
                                        param-desc)))
                  (setq json-type (append json-type `((description . ,combined-desc))))))
              (puthash param-name json-type properties)))))
      
      `((type . "object")
        (properties . ,properties)
        (required . ,(vconcat (nreverse required)))
        (additionalProperties . :json-false))))
   
   ;; Fallback
   (t `((type . "object")
        (properties . ,(make-hash-table :test 'equal))
        (required . ,(vector))
        (additionalProperties . :json-false)))))

(defun claude-code-mcp-type-to-json-schema (type-spec)
  "Convert simplified type specification to JSON Schema.
Supports: string, integer, number, boolean, (list type), (choice vals...), (or type nil)."
  (cond
   ;; Simple types as symbols
   ((symbolp type-spec)
    (cond
     ((eq type-spec 'string) '((type . "string")))
     ((eq type-spec 'integer) '((type . "integer")))
     ((eq type-spec 'number) '((type . "number")))
     ((eq type-spec 'boolean) '((type . "boolean")))
     ((eq type-spec 'symbol) '((type . "string"))) ; Symbols as strings in JSON
     ;; Check for cl-deftype definitions
     ((get type-spec 'cl-deftype-handler)
      (let ((type-def (funcall (get type-spec 'cl-deftype-handler))))
        (claude-code-mcp-type-to-json-schema type-def)))
     (t '((type . "string"))))) ; Default fallback
   
   ;; Simple types as strings (for backward compatibility)
   ((stringp type-spec)
    (cond
     ((string= type-spec "string") '((type . "string")))
     ((string= type-spec "integer") '((type . "integer")))
     ((string= type-spec "number") '((type . "number")))
     ((string= type-spec "boolean") '((type . "boolean")))
     ((string= type-spec "array") '((type . "array")))
     ((string= type-spec "object") '((type . "object")))
     (t '((type . "string")))))
   
   ;; Complex type expressions
   ((consp type-spec)
    (let ((type-op (car type-spec)))
      (cond
       ;; (list element-type) -> array of element-type
       ((eq type-op 'list)
        (if (cdr type-spec)
            `((type . "array")
              (items . ,(claude-code-mcp-type-to-json-schema (cadr type-spec))))
          '((type . "array"))))
       
       ;; (choice "val1" "val2" ...) -> enum
       ((eq type-op 'choice)
        `((type . "string")
          (enum . ,(apply #'vector (cdr type-spec)))))
       
       ;; (or type nil) -> make type optional (nullable)
       ((eq type-op 'or)
        (let ((non-nil-types (cl-remove 'nil (cdr type-spec))))
          (if (= (length non-nil-types) 1)
              ;; Single type + nil = optional
              (let ((base-schema (claude-code-mcp-type-to-json-schema (car non-nil-types))))
                (append base-schema '((nullable . t))))
            ;; Multiple types - use anyOf
            `((anyOf . ,(apply #'vector 
                               (mapcar #'claude-code-mcp-type-to-json-schema
                                       (cdr type-spec))))))))
       
       ;; Unknown complex type - default to object
       (t '((type . "object"))))))
   
   ;; Default fallback
   (t '((type . "string")))))

;;;; Parameter validation (currently disabled)

(defun claude-code-mcp-validate-parameter (value type-spec param-name)
  "Validate VALUE against TYPE-SPEC for parameter PARAM-NAME.
Returns (success . error-message) pair."
  (condition-case err
      (if (claude-code-mcp-type-matches-p value type-spec)
          '(t . nil)
        (cons nil (format "Parameter '%s' expected %s, got %s" 
                          param-name
                          (claude-code-mcp-simple-type-description type-spec)
                          (claude-code-mcp-value-description value))))
    (error (cons nil (format "Validation error for parameter '%s': %s"
                             param-name (error-message-string err))))))

(defun claude-code-mcp-type-matches-p (value type-spec)
  "Check if VALUE matches simplified TYPE-SPEC."
  (cond
   ;; Basic types as symbols
   ((eq type-spec 'string) (stringp value))
   ((eq type-spec 'integer) (integerp value))
   ((eq type-spec 'number) (numberp value))
   ((eq type-spec 'boolean) (or (eq value t) (eq value nil) (eq value :json-false) (eq value :false) (eq value :true)))
   ((eq type-spec 'symbol) (symbolp value))
   
   ;; Check for cl-deftype definitions (handle both symbol and string forms)
   ((or (and (symbolp type-spec) (get type-spec 'cl-deftype-handler))
        (and (stringp type-spec) 
             (get (intern type-spec) 'cl-deftype-handler)))
    (let* ((type-sym (if (symbolp type-spec) type-spec (intern type-spec)))
           (type-def (funcall (get type-sym 'cl-deftype-handler))))
      ;; Handle choice types specially since they contain string values
      (if (and (consp type-def) (eq (car type-def) 'choice))
          (member value (cdr type-def))
        ;; For other cl-deftype definitions, try cl-typep
        (condition-case nil
            (cl-typep value type-sym)
          (error nil)))))
   
   ;; String fallbacks (backward compatibility)
   ((and (stringp type-spec) (string= type-spec "string")) (stringp value))
   ((and (stringp type-spec) (string= type-spec "integer")) (integerp value))
   ((and (stringp type-spec) (string= type-spec "number")) (numberp value))
   ((and (stringp type-spec) (string= type-spec "boolean")) (or (eq value t) (eq value nil) (eq value :json-false) (eq value :false) (eq value :true)))
   ((and (stringp type-spec) (string= type-spec "array")) (or (vectorp value) (listp value)))
   
   ;; Complex types
   ((consp type-spec)
    (let ((type-op (car type-spec)))
      (cond
       ;; (list element-type) - array where each element matches element-type
       ((eq type-op 'list)
        (or 
         ;; Accept actual lists or vectors
         (and (or (vectorp value) (listp value))
              (let ((element-type (cadr type-spec))
                    (elements (if (vectorp value) (append value nil) value)))
                (cl-every (lambda (elem) (claude-code-mcp-type-matches-p elem element-type)) elements)))
         ;; Accept JSON string arrays like "[\"item1\", \"item2\"]"
         (and (stringp value)
              (string-prefix-p "[" value)
              (string-suffix-p "]" value)
              (condition-case nil
                  (let* ((parsed-array (json-parse-string value))
                         (element-type (cadr type-spec)))
                    (cl-every (lambda (elem) (claude-code-mcp-type-matches-p elem element-type)) 
                              (append parsed-array nil)))
                (error nil)))))

       ;; (choice "val1" "val2" ...) - enum validation
       ((eq type-op 'choice)
        (member value (cdr type-spec)))

       ;; (or type nil) - optional type
       ((eq type-op 'or)
        (cl-some (lambda (type) (claude-code-mcp-type-matches-p value type)) (cdr type-spec)))

       ;; Default: accept anything
       (t t))))

   ;; Default: accept anything
   (t t)))

(defun claude-code-mcp-simple-type-description (type-spec)
  "Generate simple, clear type description for simplified TYPE-SPEC."
  (cond
   ;; Basic types
   ((eq type-spec 'string) "string")
   ((eq type-spec 'integer) "integer")
   ((eq type-spec 'number) "number")
   ((eq type-spec 'boolean) "boolean")
   ((eq type-spec 'symbol) "symbol")
   
   ;; cl-deftype definitions (handle both symbol and string forms)
   ((or (and (symbolp type-spec) (get type-spec 'cl-deftype-handler))
        (and (stringp type-spec) (get (intern type-spec) 'cl-deftype-handler)))
    (let* ((type-sym (if (symbolp type-spec) type-spec (intern type-spec)))
           (type-def (funcall (get type-sym 'cl-deftype-handler))))
      (format "%s (%s)" type-sym (claude-code-mcp-simple-type-description type-def))))
   
   ;; String fallbacks
   ((stringp type-spec) type-spec)
   
   ;; Complex types
   ((consp type-spec)
    (let ((type-op (car type-spec)))
      (cond
       ((eq type-op 'list)
        (format "array of %s - use JSON format: [\"%s1\", \"%s2\"]" 
                (claude-code-mcp-simple-type-description (cadr type-spec))
                (if (eq (cadr type-spec) 'string) "item" "val")
                (if (eq (cadr type-spec) 'string) "item" "val")))
       ((eq type-op 'choice)
        (format "one of: %s" (mapconcat (lambda (x) (format "\"%s\"" x)) (cdr type-spec) ", ")))
       ((eq type-op 'or)
        (let ((types (cdr type-spec)))
          (if (and (= (length types) 2) (memq 'nil types))
              ;; Handle (or type nil) - optional parameter
              (let ((non-nil-type (car (cl-remove 'nil types))))
                (format "%s (optional) - use [] if not needed" 
                        (claude-code-mcp-simple-type-description non-nil-type)))
            ;; Regular union type
            (format "(%s)" (mapconcat #'claude-code-mcp-simple-type-description types " or ")))))
       (t (format "%s" type-spec)))))
   
   (t (format "%s" type-spec))))

(defun claude-code-mcp-value-description (value)
  "Generate human-readable description of VALUE's type."
  (cond
   ((stringp value) (format "string \"%s\"" value))
   ((integerp value) (format "integer %d" value))
   ((floatp value) (format "float %g" value))
   ((eq value t) "boolean true")
   ((eq value nil) "boolean false")
   ((eq value :json-false) "boolean false")
   ((eq value :true) "boolean true")
   ((eq value :false) "boolean false")
   ((vectorp value) (format "array of length %d" (length value)))
   ((listp value) (format "array of length %d" (length value)))
   ((hash-table-p value) "object")
   (t (format "%s" (type-of value)))))

(defun claude-code-mcp-validate-parameters (params-alist schema)
  "Validate PARAMS-ALIST against simplified SCHEMA.
Returns (success . error-message) pair."
  (condition-case err
      (if (or (null schema) (not claude-code-mcp-enable-validation))
          '(t . nil) ; No schema or validation disabled
        ;; Handle quoted schemas
        (when (and (consp schema) (eq (car schema) 'quote))
          (setq schema (cadr schema)))
        ;; Perform actual validation when enabled
        (let ((errors '()))
          (dolist (param-spec schema)
            (when (and (consp param-spec) (>= (length param-spec) 2))
              (let* ((param-name (car param-spec))     ; e.g., buffer-names
                     (param-type (cadr param-spec))    ; e.g., (list string)
                     (param-desc (caddr param-spec))   ; e.g., "List of buffer names to analyze"
                     (param-key (symbol-name param-name))
                     (param-value (alist-get (intern param-key) params-alist))
                     (validation-result (claude-code-mcp-validate-parameter param-value param-type param-name)))
                (unless (car validation-result)
                  (push (cdr validation-result) errors)))))
          (if errors
              (cons nil (mapconcat 'identity (nreverse errors) "; "))
            '(t . nil))))
    (error (cons nil (format "Schema validation error: %s" (error-message-string err))))))

(provide 'claude-code-mcp-types)

;;; claude-code-mcp-types.el ends here
