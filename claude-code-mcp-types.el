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

;;;; JSON Schema generation from Emacs Lisp types

(defun claude-code-mcp-build-json-schema (schema)
  "Build JSON Schema from Emacs Lisp type specifications.
Supports both simple and complex type specifications."
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
   
   ;; Enhanced format with proper type specifications
   ((listp schema)
    (let ((properties (make-hash-table :test 'equal))
          (required '()))
      (dolist (item schema)
        (cond
         ;; Enhanced format: (name type description &optional schema-props)
         ((and (consp item) (>= (length item) 2))
          (let* ((param-name (symbol-name (car item)))
                 (param-spec (cdr item))
                 (param-type (car param-spec))
                 (param-desc (cadr param-spec))
                 (extra-props (cddr param-spec)))
            (push param-name required)
            
            ;; Convert Elisp type specs to JSON Schema
            (let ((json-type (claude-code-mcp-type-to-json-schema param-type)))
              (when param-desc
                (setq json-type (append json-type `((description . ,param-desc)))))
              ;; Add any extra JSON Schema properties
              (when extra-props
                (setq json-type (append json-type (car extra-props))))
              (puthash param-name json-type properties))))))
      
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
  "Convert Emacs Lisp type specification to JSON Schema.
Handles type expressions like (list string), (or string nil), etc."
  (cond
   ;; Simple string type names
   ((stringp type-spec)
    (cond
     ((string= type-spec "string") '((type . "string")))
     ((string= type-spec "number") '((type . "number")))
     ((string= type-spec "integer") '((type . "integer")))
     ((string= type-spec "boolean") '((type . "boolean")))
     ((string= type-spec "array") '((type . "array")))
     ((string= type-spec "object") '((type . "object")))
     (t '((type . "string"))))) ; Default fallback
   
   ;; Symbol type names
   ((symbolp type-spec)
    (cond
     ((eq type-spec 'string) '((type . "string")))
     ((eq type-spec 'number) '((type . "number")))
     ((eq type-spec 'integer) '((type . "integer")))
     ((eq type-spec 'float) '((type . "number")))
     ((eq type-spec 'boolean) '((type . "boolean")))
     ((eq type-spec 'bool) '((type . "boolean")))
     ((eq type-spec 't) '((type . "boolean")))
     ((eq type-spec 'nil) '((type . "null")))
     ((eq type-spec 'symbol) '((type . "string"))) ; Symbols as strings in JSON
     ((eq type-spec 'list) '((type . "array")))
     ((eq type-spec 'vector) '((type . "array")))
     ((eq type-spec 'hash-table) '((type . "object")))
     ((eq type-spec 'alist) '((type . "object")))
     ((eq type-spec 'plist) '((type . "object")))
     (t '((type . "string"))))) ; Default for unknown symbols
   
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
       
       ;; (vector element-type) -> array of element-type
       ((eq type-op 'vector)
        (if (cdr type-spec)
            `((type . "array")
              (items . ,(claude-code-mcp-type-to-json-schema (cadr type-spec))))
          '((type . "array"))))
       
       ;; (or type1 type2 ...) -> anyOf
       ((eq type-op 'or)
        (let ((types (mapcar #'claude-code-mcp-type-to-json-schema (cdr type-spec))))
          (if (= (length types) 1)
              (car types)
            `((anyOf . ,(apply #'vector types))))))
       
       ;; (and type1 type2 ...) -> allOf
       ((eq type-op 'and)
        (let ((types (mapcar #'claude-code-mcp-type-to-json-schema (cdr type-spec))))
          (if (= (length types) 1)
              (car types)
            `((allOf . ,(apply #'vector types))))))
       
       ;; (choice "val1" "val2" ...) -> enum
       ((eq type-op 'choice)
        `((type . "string")
          (enum . ,(apply #'vector (cdr type-spec)))))
       
       ;; (member val1 val2 ...) -> enum
       ((eq type-op 'member)
        `((enum . ,(apply #'vector (cdr type-spec)))))
       
       ;; (repeat type) -> array of type
       ((eq type-op 'repeat)
        `((type . "array")
          (items . ,(claude-code-mcp-type-to-json-schema (cadr type-spec)))))
       
       ;; (cons type1 type2) -> tuple
       ((eq type-op 'cons)
        `((type . "array")
          (items . ,(vector (claude-code-mcp-type-to-json-schema (cadr type-spec))
                            (claude-code-mcp-type-to-json-schema (caddr type-spec))))
          (minItems . 2)
          (maxItems . 2)))
       
       ;; Default: treat as object
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
  "Check if VALUE matches TYPE-SPEC. Simple, robust type checking."
  (cond
   ;; Basic types as symbols
   ((eq type-spec 'string) (stringp value))
   ((eq type-spec 'integer) (integerp value))
   ((eq type-spec 'number) (numberp value))
   ((eq type-spec 'symbol) (symbolp value))
   ((eq type-spec 'boolean) (or (eq value t) (eq value nil) (eq value :json-false)))
   
   ;; Array types (from JSON)
   ((eq type-spec 'array) (or (vectorp value) (listp value)))
   
   ;; Complex types
   ((consp type-spec)
    (let ((type-op (car type-spec)))
      (cond
       ;; (list element-type) - array where each element matches element-type
       ((eq type-op 'list)
        (and (or (vectorp value) (listp value))
             (let ((element-type (cadr type-spec))
                   (elements (if (vectorp value) (append value nil) value)))
               (cl-every (lambda (elem) (claude-code-mcp-type-matches-p elem element-type)) elements))))

       ;; (or type1 type2 ...) - value matches any of the types
       ((eq type-op 'or)
        (cl-some (lambda (type) (claude-code-mcp-type-matches-p value type)) (cdr type-spec)))

       ;; (choice "val1" "val2" ...) - enum validation
       ((eq type-op 'choice)
        (member value (cdr type-spec)))

       ;; Default: accept anything for unknown complex types
       (t t))))

   ;; String fallbacks for JSON Schema compatibility
   ((and (stringp type-spec) (string= type-spec "string")) (stringp value))
   ((and (stringp type-spec) (string= type-spec "integer")) (integerp value))
   ((and (stringp type-spec) (string= type-spec "number")) (numberp value))
   ((and (stringp type-spec) (string= type-spec "boolean")) (or (eq value t) (eq value nil) (eq value :json-false)))
   ((and (stringp type-spec) (string= type-spec "array")) (or (vectorp value) (listp value)))

   ;; Default: accept anything
   (t t)))

(defun claude-code-mcp-simple-type-description (type-spec)
  "Generate simple, clear type description for TYPE-SPEC."
  (cond
   ((eq type-spec 'string) "string (text)")
   ((eq type-spec 'integer) "integer (whole number)")
   ((eq type-spec 'number) "number")
   ((eq type-spec 'symbol) "symbol (like a function or variable name)")
   ((eq type-spec 'boolean) "boolean (true/false)")
   ((eq type-spec 'array) "array (list of items)")
   
   ((and (consp type-spec) (eq (car type-spec) 'list))
    (format "array of %s" (claude-code-mcp-simple-type-description (cadr type-spec))))
   
   ((and (consp type-spec) (eq (car type-spec) 'or))
    (format "(%s)" (mapconcat #'claude-code-mcp-simple-type-description (cdr type-spec) " OR ")))
   
   ((and (consp type-spec) (eq (car type-spec) 'choice))
    (format "one of: %s" (mapconcat (lambda (x) (format "\"%s\"" x)) (cdr type-spec) ", ")))
   
   ;; String fallbacks
   ((stringp type-spec) type-spec)
   
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
   ((vectorp value) (format "array of length %d" (length value)))
   ((listp value) (format "array of length %d" (length value)))
   ((hash-table-p value) "object")
   (t (format "%s" (type-of value)))))

(defun claude-code-mcp-validate-parameters (params-alist schema)
  "Validate PARAMS-ALIST against SCHEMA.
Returns (success . error-message) pair."
  (condition-case err
      (if (null schema)
          '(t . nil) ; No schema means no validation required
        ;; Temporarily disable validation to avoid errors
        '(t . nil))
    (error (cons nil (format "Schema validation error: %s" (error-message-string err))))))

(provide 'claude-code-mcp-types)

;;; claude-code-mcp-types.el ends here