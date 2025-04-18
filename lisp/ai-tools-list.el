;; -*- lexical-binding: t -*-
;;; Documentation lookup

;;; Function inspect
(my/gptel-tool-definer
 :name "helpful_function_inspect"
 :function (lambda (function-name)
             (message "Looking up function: %s" function-name)
             (condition-case err
                 (save-window-excursion
                   (with-current-buffer (helpful-callable (intern function-name))
                     (message "Found documentation for function: %s" function-name)
                     (buffer-substring-no-properties (point-min) (point-max))))
               (error 
                (message "Error looking up function %s: %s" function-name (error-message-string err))
                (format "Could not find documentation for function '%s'. Error: %s" 
                        function-name (error-message-string err)))))
 :include t
 :description "Retrieves comprehensive documentation for any Emacs Lisp function.

Purpose:
- Provides in-depth function documentation, source code, and usage examples
- Helps understand how functions work and how to use them correctly
- Shows function arguments, return values, and behavior details

When to use:
- When you need to understand a specific function's behavior
- When troubleshooting code that calls a particular function
- When learning how to implement a function correctly
- When exploring function implementation details

Note: Since this tool adds output directly to the current context, only use it
when necessary and avoid calling it multiple times for the same function."
 :args (list '(:name "function_name"
                     :type string
                     :description "The exact name of the Emacs Lisp function you want to inspect. Enter the function name without quotes or parentheses."))
 :category "introspection")

;;; Variable inspect
(my/gptel-tool-definer
 :name "helpful_variable_inspect"
 :function (lambda (variable-name)
             (message "Looking up variable: %s" variable-name)
             (condition-case err
                 (save-window-excursion
                   (with-current-buffer (helpful-variable (intern variable-name))
                     (message "Found documentation for variable: %s" variable-name)
                     (buffer-substring-no-properties (point-min) (point-max))))
               (error 
                (message "Error looking up variable %s: %s" variable-name (error-message-string err))
                (format "Could not find documentation for variable '%s'. Error: %s" 
                        variable-name (error-message-string err)))))
 :include t 
 :description "Retrieves comprehensive documentation for any Emacs Lisp variable.

Purpose:
- Provides detailed variable information including current value and documentation
- Shows customization options and default values
- Identifies where the variable is defined and referenced
- Displays variable type and constraints

When to use:
- When you need to understand what a variable controls
- When configuring Emacs behavior through variables
- When troubleshooting unexpected behavior related to configuration
- When learning about available customization options

Note: Since this tool adds output directly to the current context, only use it
when necessary and avoid calling it multiple times for the same variable."
 :args (list '(:name "variable_name"
                     :type string
                     :description "The exact name of the Emacs Lisp variable you want to inspect. Enter the variable name without quotes or special syntax."))
 :category "introspection")

;;; Symbol lookup
(defun gptel-tool-apropos (keyword)
  "Search for Emacs Lisp symbols matching a keyword.

This function searches for functions and variables whose names match the given
keyword. It returns and logs a formatted summary of matching symbols, including
their names and the first line of their documentation.

Arguments:
- KEYWORD: A string to match against symbol names.

Returns:
- A formatted string containing matching functions and variables.
- Logs results to the *gptel-tool-results* buffer.

Side effects:
- Creates or updates the *gptel-tool-results* buffer with search results.
- Displays a message indicating the search results have been logged."
  (message "Searching for symbols matching: %s" keyword)
  (let* ((regexp (regexp-quote keyword))
         (functions (apropos-internal regexp 'functionp))
         (variables (apropos-internal regexp 'boundp))
         (result-text "")
         (buffer-name "*gptel-tool-results*"))
    
    ;; Format function results
    (when functions
      (setq result-text (concat result-text (format "== FUNCTIONS (%d) ==\n" (length functions))))
      (dolist (sym functions)
        (let ((doc (documentation sym t)))
          (setq result-text (concat result-text 
                                    (format "%s: %s\n" 
                                            sym 
                                            (if doc
						(car (split-string doc "\n"))
					      "No documentation")))))))
    
    ;; Format variable results
    (when variables
      (setq result-text (concat result-text 
				(format "\n== VARIABLES (%d) ==\n" (length variables))))
      (dolist (sym variables)
        (let ((doc (documentation-property sym 'variable-documentation)))
          (setq result-text (concat result-text 
                                    (format "%s: %s\n" 
                                            sym 
                                            (if doc
						(car (split-string doc "\n"))
					      "No documentation")))))))
    
    ;; Format the final result string - this will be returned to the LLM
    (setq result-text (if (string= result-text "")
                          (format "No symbols found containing '%s'\n" keyword)
			(concat 
			 (format "Found %d symbols matching '%s':\n\n" 
				 (+ (length functions) (length variables))
				 keyword)
			 result-text)))
    
    ;; Log to debugging buffer without focusing it
    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        ;; Set buffer properties if it's new
        (when (= (buffer-size) 0)
          (special-mode)  
          (setq buffer-read-only nil))

	;; Make sure we can write to the buffer
	(setq buffer-read-only nil)
        
        ;; Add content with timestamp
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n\n" (make-string 70 ?-) "\n\n"))
        (insert (format "Search results for '%s' [%s]\n\n" 
                        keyword 
                        (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert result-text)
        
        ;; Make buffer read-only again but allow quit with 'q'
        (setq buffer-read-only t)
        (local-set-key (kbd "q") 'quit-window))
      
      ;; Just log a message without displaying the buffer
      (message "Results logged in %s buffer" buffer-name))
    
    ;; Always return the complete result text to the LLM
    result-text))

(my/gptel-tool-definer
 :name "find_symbols_by_name"
 :function 'gptel-tool-apropos
 :include t
 :description "Searches for Emacs Lisp symbols (functions and variables) by keyword.

Purpose:
- Discovers relevant functions and variables based on name patterns
- Provides quick overview of available functionality in a specific domain
- Helps identify symbols for further inspection
- Supports exploration of Emacs functionality

When to use:
- When exploring a specific Emacs feature or package
- When you need to find functions related to a concept
- As a first step before using more detailed inspection tools
- When you're unsure which exact function or variable to examine

Output format:
- Returns a formatted text listing of matching symbols
- Lists matching functions with brief descriptions
- Lists matching variables with brief descriptions
- Includes count of matches found
- Results are also logged to *gptel-tool-results* buffer for reference

Note: The search is case-sensitive and matches substrings within symbol names.
When the user explicitly asks to list symbols, you should list them exactly
as provided in the output. For your own discovery purposes without user explicitly
requesting the output, summarize the findings instead.
"
 :args (list '(:name "keyword"
		     :type string
		     :description "The keyword or pattern to search for in symbol names. This should typically be a feature name (like 'elfeed'), a concept (like 'buffer'), or any text pattern you expect to find in relevant symbol names. The search is case-sensitive and will match partial names."))
 :category "introspection")

;;; Clean context
(my/gptel-tool-definer
 :name "clean_whole_context"
 :function (lambda ()
             (message "Cleaning all conversation context...")
             (gptel-context-remove-all nil)
             (message "All conversation context has been removed")
             "All conversation context has been cleared. We're starting with a fresh slate.")
 :description "Removes all accumulated context from previous interactions.

Purpose:
- Clears conversation history to start fresh
- Prevents previous conversations from influencing new responses
- Helps when switching between different topics or projects
- Reduces potential confusion from mixing unrelated contexts

When to use:
- After completing a project or code implementation
- When starting a conversation on a new, unrelated topic
- When previous context is causing confusion or incorrect responses
- Before dealing with sensitive or confidential information
- When conversation has grown too long and resource-intensive

Example scenarios:
1. After finishing a large coding project, use this to reset the context
2. When switching between different programming tasks or languages
3. Before beginning a completely new line of inquiry or technical discussion
4. When the AI seems to be mixing up information from previous discussions"
 :include t
 :category "meta")

(defun list-all-gptel-tools ()
  "List all available gptel tools with their names and descriptions."
  (let (tool-list)
    (dolist (category gptel--known-tools)
      (let ((category-name (car category)))
        (dolist (tool-entry (cdr category))
          (let* ((tool-name (car tool-entry))
                 (tool-struct (cdr tool-entry))
                 ;; Using the structure accessor function
                 (description (gptel-tool-description tool-struct)))
            (push (cons tool-name description) tool-list)))))
    (nreverse tool-list)))

(defun list-gptel-tool-names ()
  "Return a list of all available gptel tool names."
  (let (tool-names)
    (dolist (category gptel--known-tools)
      (dolist (tool-entry (cdr category))
        (push (car tool-entry) tool-names)))
    (nreverse tool-names)))

(defun get-gptel-tool-description (tool-name)
  "Return the description for a specific tool by name.
If the tool is not found, return nil."
  (catch 'found
    (dolist (category gptel--known-tools)
      (dolist (tool-entry (cdr category))
        (when (string= (car tool-entry) tool-name)
          (throw 'found (gptel-tool-description (cdr tool-entry))))))
    nil))

;; GPTel list tools
(my/gptel-tool-definer :name "list_gptel_tool_names" :description "Returns a list of all available gptel tool names.

Purpose:
- Provides a quick overview of all available tools
- Helps users discover what tools are currently supported
- Useful for introspection and tool exploration

When to use:
- When you want to see what tools are available
- Before selecting a specific tool to use
- When exploring the current tool capabilities" :function 'list-gptel-tool-names :include t :category "introspection")


;; GPTel lookup tool
(my/gptel-tool-definer :name "get_gptel_tool_description" :description "Retrieves the full description for a specific tool by name.

Purpose:
- Provides detailed information about a specific tool
- Helps users understand the purpose and usage of individual tools
- Supports in-depth exploration of tool capabilities

When to use:
- When you want to learn more about a specific tool
- Before using a tool to understand its functionality
- When seeking detailed information about tool usage and purpose

Parameters:
- tool_name: The exact name of the tool to get description for" :function 'get-gptel-tool-description :include t :args [(:name "tool_name" :type "string" :description "Name of the tool to get description for")] :category "introspection")

;; Evaluate elisp
(my/gptel-tool-definer 
 :name "eval_elisp" 
 :description "Evaluates Emacs Lisp code and returns the result.
Purpose:
- Executes Emacs Lisp expressions directly in the current environment
- Tests code snippets to verify functionality
- Demonstrates how functions behave with actual output
- Helps troubleshoot issues by running diagnostic code
- Allows exploring Emacs state and configuration interactively
When to use:
- When testing proposed solutions before recommending them
- When verifying how a function works in practice
- When checking current values or states in Emacs
- When demonstrating the effects of configuration changes
- When creating custom examples tailored to user questions
Note: This tool executes code in the user's Emacs instance. Use with appropriate caution, especially with code that modifies state or performs file operations. Always explain the purpose of code you're evaluating when presenting results to users. If there is a different tool that will more directly accomplish what you want to do, strongly consider using it instead." 
 :function (lambda (expr)
             "Evaluate Emacs Lisp expression EXPR and return the result.
The expression is evaluated in the current buffer context."
             (condition-case err 
                 (let ((result (eval (read expr) t))) 
                   (format "%S" result)) 
               (error (format "Error: %S" err))))
 :args '((:name "expr" :type "string" :description "Emacs Lisp expression to evaluate"))
 :include t 
 :confirm t 
 :category "meta")

;;; Summarize contents of URL
;; (my/gptel-tool-definer
;;  :function (lambda (url callback)
;; 	     (let ((jina-proxy-url (concat "https://r.jina.ai/" url)))
;; 	       (url-retrieve jina-proxy-url
;; 			     (lambda (status)
;; 			       (if (plist-get status :error)
;; 				   (funcall callback nil)
;; 				 (goto-char (point-min))
;; 				 (forward-paragraph)
;; 				 (let ((dom (libxml-parse-html-region (point) (point-max))))
;; 				   (with-temp-buffer
;; 				     (shr-insert-document dom)
;; 				     (funcall callback (buffer-substring-no-properties (point-min) (point-max))))
;; 				   (kill-buffer))))
;; 			     nil t nil)))
;;  :name "read_url"
;;  :description "Extracts text content from web pages using Jina AI's proxy service and returns it directly.
;; When to use:
;; - When you want to get access to the contents of an URL.
;; - When you see URLs in the context that you think might be relevant to the conversation.
;; - When you need to analyze or reference web content in your current response
;; "
;;  :args (list '(:name "url"
;; 		     :type string
;; 		     :description "The URL to extract text from using Jina AI proxy"))
;;  :include t
;;  :category "web"
;;  :async t)

