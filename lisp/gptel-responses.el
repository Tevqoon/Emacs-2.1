;;; https://github.com/karthink/gptel/issues/697

(cl-defstruct (gptel-openai-responses
               (:constructor gptel-openai--make-responses)
               (:copier nil)
               (:include gptel-openai)))

(cl-defun gptel-make-openai-responses
    (name &key curl-args models stream key request-params
          (header
           (lambda () (when-let* ((key (gptel--get-api-key)))
                   `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.openai.com")
          (protocol "https")
          (endpoint "/v1/responses"))
  "Create a openai responses backend."
  (declare (indent 1))
  (let ((backend (gptel-openai--make-responses
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
                  backend))))

(defun gptel-openai-response--process-output (output-item info)
  ;; both streams output_item.done.item and the output-item in response are the same.
  (pcase (plist-get output-item :type)
    ("function_call"
     (gptel--inject-prompt        ; First add the tool call to the prompts list
      (plist-get info :backend)
      (plist-get info :data)
      (copy-sequence output-item)) ;; copying to avoid following changes effect output-item
     (ignore-errors (plist-put output-item :args
                               (gptel--json-read-string
                                (plist-get output-item :arguments))))
     (plist-put output-item :arguments nil)
     (plist-put info :tool-use
                (append
                 (plist-get info :tool-use)
                 (list output-item))))
    ("reasoning"
     (gptel--inject-prompt        ; responses expects the reasoning blocks
      (plist-get info :backend) (plist-get info :data) output-item)
     (plist-put info :reasoning
                (append
                 (plist-get info :reasoning)
                 (list (map-nested-elt output-item '(:summary :text))))))
    (_ ;; TODO handle others
     )))

(defun gptel-openai-response--process-annotation (annotation info)
  "Returns string that can be added to content or nil."
  (pcase (plist-get annotation :type)
    ("file_citation"
     (format "[file_citation:%s]" (plist-get annotation :file_id)))
    (_ ;; TODO handle otheres
     )))

(cl-defmethod gptel--request-data ((backend gptel-openai-responses) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let* ((prompts (cl-call-next-method))
         (p prompts))
    ;; Adding built-in tools
    (when gptel-openai-responses--tools
      (plist-put prompts :tools (vconcat (plist-get prompts :tools)
                                         (mapcar (lambda (built-in-tool)
                                                   (if (functionp built-in-tool)
                                                       (funcall built-in-tool)
                                                     built-in-tool))
                                                 gptel-openai-responses--tools))))
    (while p
      (when (eq (car p) :messages)
        (setcar p :input))
      (setq p (cddr p)))
    prompts))

(cl-defmethod gptel--inject-prompt
    ((backend gptel-openai-responses) data new-prompt &optional _position)
  "JSON encode PROMPTS for sending to ChatGPT."
  (when (keywordp (car-safe new-prompt)) ;Is new-prompt one or many?
    (setq new-prompt (list new-prompt)))
  (let ((prompts (plist-get data :input)))
    (plist-put data :input (vconcat prompts new-prompt))))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai-responses) info)
  "Parse an OpenAI API data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it."
  (let* ((content-strs))
    (condition-case err
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (let ((json-response (save-excursion
                                   (gptel--json-read))))
              (pcase (plist-get json-response :type)
                ;; ("response.completed"
                ;;  ;; Once stream end processing
                ;;  )
                ("response.output_text.delta"
                 (push (plist-get json-response :delta) content-strs))
                ("response.output_text.annotation.added"
                 (let ((annotation (plist-get json-response :annotation)))
                   (push (gptel-openai-response--process-annotation annotation info) content-strs)))
                ("response.output_item.done"
                 (let ((output-item (plist-get json-response :item)))
                   (gptel-openai-response--process-output output-item info)))))))
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-openai-responses) response info)
  "Parse an OpenAI (non-streaming) RESPONSE and return response text.

Mutate state INFO with response metadata."
  (plist-put info :stop-reason
             (list (plist-get response :status)
                              (plist-get response :incomplete_details)))
  (plist-put info :output-tokens
             (map-nested-elt response '(:usage :total_tokens)))

  (cl-loop for output-item across (plist-get response :output)
           if (equal (plist-get output-item :type) "message")
             collect
             (string-join
              (list (map-nested-elt output-item '(:content 0 :text))
                    (string-join
                     (mapcar (lambda (annotation)
                               (gptel-openai-response--process-annotation annotation info))
                             (map-nested-elt output-item '(:content 0 :annotations)))
                     "\n"))
              "\n")
             into return-val
           else
             do (gptel-openai-response--process-output output-item info)
           finally return (funcall #'string-join return-val)))

(cl-defmethod gptel--parse-tool-results ((_backend gptel-openai-responses) tool-use)
  "Return a prompt containing tool call results in TOOL-USE."
  (mapcar
   (lambda (tool-call)
     (list
      :type "function_call_output"
      :call_id (plist-get tool-call :call_id)
      :output (plist-get tool-call :result)))
   tool-use))

(provide 'gptel-responses)
