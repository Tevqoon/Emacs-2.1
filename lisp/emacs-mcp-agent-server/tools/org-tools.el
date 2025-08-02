;;; org-tools.el --- Org-mode tools for Emacs MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude  
;; Keywords: org, mcp, tools

;;; Commentary:

;; Org-mode specific tools and resources for the MCP server.
;; Load this file to add org-mode capabilities.

;;; Code:

(require 'emacs-mcp-tools)

;; Org-mode tools
(emacs-mcp-register-tool
 '(:id "org_get_agenda"
   :description "Get current org-mode agenda"
   :handler org-mcp-tool-get-agenda))

(emacs-mcp-register-tool
 '(:id "org_search_headings"
   :description "Search org headings across files"
   :handler org-mcp-tool-search-headings))

(emacs-mcp-register-tool
 '(:id "org_capture"
   :description "Create new org capture entry"
   :handler org-mcp-tool-capture))

;; Org-mode resources
(emacs-mcp-register-resource
 '(:uri "org://agenda"
   :name "Org Agenda"
   :description "Current org-mode agenda view"
   :handler org-mcp-resource-agenda))

(emacs-mcp-register-resource
 '(:uri "org://file/{filename}"
   :name "Org File"
   :description "Contents of an org file"
   :handler org-mcp-resource-file))

;;; Tool implementations

(defun org-mcp-tool-get-agenda ()
  "Get current org agenda."
  (if (not (featurep 'org-agenda))
      (require 'org-agenda))
  (with-temp-buffer
    (org-agenda-list)
    (buffer-string)))

(defun org-mcp-tool-search-headings (query)
  "Search org headings for QUERY.

MCP Parameters:
  query - Search term for org headings"
  (if (not (featurep 'org))
      (require 'org))
  (let ((results '())
        (org-files (org-agenda-files)))
    (dolist (file org-files)
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (org-mode)
          (org-map-entries
           (lambda ()
             (when (string-match-p query (org-get-heading t t t t))
               (push (format "%s: %s"
                            (file-name-nondirectory file)
                            (org-get-heading t t t t))
                     results)))))))
    (if results
        (mapconcat #'identity (reverse results) "\n")
      (format "No headings found matching '%s'" query))))

(defun org-mcp-tool-capture (template-key title content)
  "Create org capture entry.

MCP Parameters:
  template-key - Org capture template key
  title - Title for the entry
  content - Content for the entry"
  (if (not (featurep 'org-capture))
      (require 'org-capture))
  (condition-case err
      (progn
        (org-capture nil template-key)
        (when title
          (insert title "\n"))
        (when content
          (insert content))
        (org-capture-finalize)
        "Capture entry created successfully")
    (error (format "Capture failed: %s" (error-message-string err)))))

;;; Resource implementations

(defun org-mcp-resource-agenda ()
  "Return org agenda as resource."
  (if (not (featurep 'org-agenda))
      (require 'org-agenda))
  (with-temp-buffer
    (org-agenda-list)
    (buffer-string)))

(defun org-mcp-resource-file (params)
  "Return org file contents.
PARAMS should contain 'filename' key."
  (let ((filename (alist-get "filename" params nil nil #'string=)))
    (if (and filename (file-exists-p filename))
        (with-temp-buffer
          (insert-file-contents filename)
          (buffer-string))
      (format "File not found: %s" filename))))

(provide 'org-tools)
;;; org-tools.el ends here