;;; org-ai-prompt.el --- Simple Claude integration for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, ai, claude, prompts
;; URL: https://github.com/yourusername/org-ai-prompt

;;; Commentary:

;; This package provides a simple way to send prompts to Claude from Org mode.
;;
;; Usage:
;; 1. Create a #+BEGIN_PROMPT block with your question
;; 2. Position cursor inside the block
;; 3. Press C-c C-c
;; 4. Claude's response appears in a #+BEGIN_RESPONSE block below
;;
;; Example:
;;   #+BEGIN_PROMPT
;;   What is the capital of France?
;;   #+END_PROMPT
;;
;;   [Press C-c C-c, response appears below]
;;
;; Setup:
;;   (require 'org-ai-prompt)
;;   (add-hook 'org-mode-hook #'org-ai-prompt-mode)

;;; Code:

(require 'org)
(require 'org-element)

;;; Customization

(defgroup org-ai-prompt nil
  "Simple Claude integration for Org mode."
  :group 'org
  :prefix "org-ai-prompt-")

(defcustom org-ai-prompt-command "claude"
  "Command to invoke Claude CLI."
  :type 'string
  :group 'org-ai-prompt)

(defcustom org-ai-prompt-executable-path nil
  "Path to Claude CLI executable.
If nil, the command will be searched in PATH."
  :type '(choice (const :tag "Use PATH" nil)
                 (file :tag "Executable path"))
  :group 'org-ai-prompt)

;;; Utility Functions

(defun org-ai-prompt--message (format-string &rest args)
  "Display a message for org-ai-prompt with FORMAT-STRING and ARGS."
  (message "[org-ai-prompt] %s" (apply #'format format-string args)))

(defun org-ai-prompt--error (format-string &rest args)
  "Display an error message for org-ai-prompt with FORMAT-STRING and ARGS."
  (error "[org-ai-prompt] %s" (apply #'format format-string args)))

;;; Block Detection and Content Extraction

(defun org-ai-prompt--element-is-prompt-block-p (element)
  "Return non-nil if ELEMENT is a PROMPT block."
  (and (eq (org-element-type element) 'special-block)
       (string-equal (org-element-property :type element) "PROMPT")))

(defun org-ai-prompt--extract-block-content (element)
  "Extract the content from a PROMPT block ELEMENT."
  (let* ((begin (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element)))
    (when (and begin end)
      (string-trim (buffer-substring-no-properties begin end)))))

;;; Response Insertion

(defun org-ai-prompt--insert-response (response marker)
  "Insert RESPONSE as a RESPONSE block at MARKER position."
  (when (and response (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        ;; Move to end of current block
        (when (org-at-block-p)
          (re-search-forward "^#\\+END_" nil t)
          (forward-line 1))
        ;; Insert response block
        (unless (looking-at "^$")
          (insert "\n"))
        (insert "#+BEGIN_RESPONSE\n")
        (insert (string-trim response))
        (insert "\n#+END_RESPONSE\n")
        (org-ai-prompt--message "Response inserted successfully")))))

;;; Process Execution

(defun org-ai-prompt--process-filter (process output)
  "Process filter for Claude CLI PROCESS OUTPUT."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output))))

(defun org-ai-prompt--process-sentinel (process event)
  "Process sentinel for Claude CLI PROCESS.
EVENT describes what happened to the process."
  (let ((status (process-status process))
        (marker (process-get process 'org-ai-prompt-marker)))
    (cond
     ((eq status 'exit)
      (let ((exit-code (process-exit-status process)))
        (if (= exit-code 0)
            ;; Success - insert the response
            (when (buffer-live-p (process-buffer process))
              (let ((response (with-current-buffer (process-buffer process)
                               (buffer-string))))
                (org-ai-prompt--insert-response response marker)
                (kill-buffer (process-buffer process))))
          ;; Error - show message
          (org-ai-prompt--error "Claude CLI exited with code %d: %s"
                               exit-code
                               (if (buffer-live-p (process-buffer process))
                                   (with-current-buffer (process-buffer process)
                                     (buffer-string))
                                 "")))))

     ((eq status 'signal)
      (org-ai-prompt--error "Claude CLI process killed: %s" event))

     (t
      (org-ai-prompt--message "Claude CLI process status: %s" event)))))

(defun org-ai-prompt--execute-claude (prompt-content marker)
  "Execute Claude with PROMPT-CONTENT asynchronously.
Insert response at MARKER when complete."
  (let* ((executable (or org-ai-prompt-executable-path org-ai-prompt-command))
         (process-buffer (generate-new-buffer " *org-ai-prompt-output*"))
         (process-name "org-ai-prompt"))

    ;; Start the async process: claude -p "prompt content"
    (org-ai-prompt--message "Sending prompt to Claude...")
    (condition-case err
        (let ((proc (start-process
                     process-name
                     process-buffer
                     executable
                     "-p"
                     prompt-content)))
          ;; Store the marker in process properties
          (process-put proc 'org-ai-prompt-marker marker)
          (set-process-filter proc #'org-ai-prompt--process-filter)
          (set-process-sentinel proc #'org-ai-prompt--process-sentinel)
          (org-ai-prompt--message "Waiting for Claude's response..."))
      (error
       (org-ai-prompt--error "Failed to start Claude CLI: %s" (error-message-string err))
       (kill-buffer process-buffer)))))

;;; Block Execution

(defun org-ai-prompt--execute-block ()
  "Execute the PROMPT block at point.
This function is called by org-mode's C-c C-c mechanism.
Returns t if a PROMPT block was executed, nil otherwise."
  (interactive)
  (let* ((element (org-element-at-point))
         ;; Check if we're IN a block by looking at parent elements
         ;; org-element-lineage walks up the tree to find containing special-blocks
         (block (org-element-lineage element '(special-block) t)))

    ;; Check if we found a PROMPT block in the lineage
    (when (and block
               (org-ai-prompt--element-is-prompt-block-p block))

      ;; Extract the prompt content
      (let ((prompt-content (org-ai-prompt--extract-block-content block)))
        (if (or (null prompt-content) (string-empty-p prompt-content))
            (org-ai-prompt--error "PROMPT block is empty")

          ;; Execute Claude with the prompt
          (org-ai-prompt--execute-claude prompt-content (point-marker))
          t)))))  ; Return t to indicate we handled the C-c C-c

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode org-ai-prompt-mode
  "Minor mode for simple Claude integration in Org mode.

This mode adds support for PROMPT blocks that send questions to Claude.

Key bindings:
  \\[org-ctrl-c-ctrl-c] - Execute prompt block at point (sends to Claude)

Usage:
  1. Create a #+BEGIN_PROMPT block with your question
  2. Position cursor inside the block
  3. Press C-c C-c
  4. Claude's response appears in a #+BEGIN_RESPONSE block below"
  :lighter " AI"
  :group 'org-ai-prompt
  (if org-ai-prompt-mode
      (org-ai-prompt--setup)
    (org-ai-prompt--teardown)))

(defun org-ai-prompt--setup ()
  "Set up org-ai-prompt-mode in the current buffer."
  ;; Add our ctrl-c-ctrl-c hook
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-ai-prompt--execute-block nil t))

(defun org-ai-prompt--teardown ()
  "Clean up org-ai-prompt-mode in the current buffer."
  ;; Remove our ctrl-c-ctrl-c hook
  (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-ai-prompt--execute-block t))

(provide 'org-ai-prompt)

;;; org-ai-prompt.el ends here
