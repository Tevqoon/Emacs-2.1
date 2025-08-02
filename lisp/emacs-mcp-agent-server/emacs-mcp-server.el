;;; mcp-emacsclient.el --- MCP Server via emacsclient -*- lexical-binding: t; -*-

(require 'json)

(defun mcp-hello-world ()
  "Return hello world message."
  "Hello World from Emacs MCP Server!")

(defun mcp-handle-request (request)
  "Handle MCP request."
  (let* ((method (gethash "method" request))
         (params (gethash "params" request))
         (id (gethash "id" request)))
    
    (cond
     ((string= method "initialize")
      `((jsonrpc . "2.0")
        (id . ,id)
        (result . ((protocolVersion . "2024-11-05")
                  (capabilities . ((tools . ((listChanged . :json-false)))))
                  (serverInfo . ((name . "emacs-mcp")
                               (version . "0.1.0")))))))
     
     ((string= method "tools/list")
      `((jsonrpc . "2.0")
        (id . ,id)
        (result . ((tools . [((name . "hello_world")
                             (description . "Returns hello world message")
                             (inputSchema . ((type . "object")
                                           (properties . ()))))])))))
     
     ((string= method "tools/call")
      (let ((tool-name (gethash "name" params)))
        (if (string= tool-name "hello_world")
            `((jsonrpc . "2.0")
              (id . ,id)
              (result . ((content . [((type . "text")
                                     (text . ,(mcp-hello-world)))]))))
          `((jsonrpc . "2.0")
            (id . ,id)
            (error . ((code . -32601)
                     (message . "Tool not found")))))))
     
     (t `((jsonrpc . "2.0")
          (id . ,id)
          (error . ((code . -32601)
                   (message . "Method not found"))))))))

(defun mcp-stdio-loop ()
  "STDIO loop for MCP server."
  (while t
    (let ((line (read-string "")))
      (when line
        (condition-case nil
            (let* ((request (json-parse-string line))
                   (response (mcp-handle-request request)))
              (princ (json-encode response))
              (princ "\n"))
          (error
           (princ (json-encode '((jsonrpc . "2.0") (id . null) 
                                (error . ((code . -32700) (message . "Parse error"))))))
           (princ "\n")))))))

;; Auto-start if loaded for MCP
(when (member "--mcp-server" command-line-args)
  (mcp-stdio-loop))

(provide 'mcp-emacsclient)
;;; mcp-emacsclient.el ends here