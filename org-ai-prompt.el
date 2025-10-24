;;; org-ai-prompt.el --- AI prompt integration for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, ai, prompts, convenience
;; URL: https://github.com/yourusername/org-ai-prompt

;;; Commentary:

;; This package provides a minor mode for integrating AI prompts into Org mode
;; documents. It enables "Dialog Engineering" workflows where users can
;; intersperse notes, code, and AI prompts in a single org document.
;;
;; Features:
;; - New PROMPT and RESPONSE block types for org-mode
;; - Execute prompt blocks with C-c C-c to invoke AI CLI
;; - Automatic conversation context building from org buffer
;; - Support for multiple AI tools (Claude, Gemini, OpenAI, custom)
;; - Configurable AI CLI adapters
;; - Async process execution
;;
;; Usage:
;;   (require 'org-ai-prompt)
;;   (add-hook 'org-mode-hook #'org-ai-prompt-mode)
;;
;; See README for detailed configuration and usage instructions.

;;; Code:

(require 'org)
(require 'org-element)

;;; Customization

(defgroup org-ai-prompt nil
  "AI prompt integration for Org mode."
  :group 'org
  :prefix "org-ai-prompt-")

(defcustom org-ai-prompt-command "claude"
  "Command to invoke AI CLI.
Can be set to 'claude', 'gemini', 'openai', or a custom script path."
  :type 'string
  :group 'org-ai-prompt)

(defcustom org-ai-prompt-executable-path nil
  "Path to AI CLI executable.
If nil, the command will be searched in PATH."
  :type '(choice (const :tag "Use PATH" nil)
                 (file :tag "Executable path"))
  :group 'org-ai-prompt)

(defcustom org-ai-prompt-args-function nil
  "Function to format arguments for the specific AI CLI.
If nil, the default adapter will be selected based on
`org-ai-prompt-command'.

The function should accept the following arguments:
  MESSAGES - List of message plists with :role and :content
  SYSTEM-PROMPT - System prompt string or nil
  MODEL - Model name string or nil
  TEMPERATURE - Temperature value or nil
  MAX-TOKENS - Max tokens value or nil

It should return a list of command-line arguments."
  :type '(choice (const :tag "Auto-detect" nil)
                 (function :tag "Custom adapter function"))
  :group 'org-ai-prompt)

(defcustom org-ai-prompt-default-model nil
  "Default model to use if not specified in file properties."
  :type '(choice (const :tag "Use CLI default" nil)
                 (string :tag "Model name"))
  :group 'org-ai-prompt)

(defcustom org-ai-prompt-default-temperature nil
  "Default temperature to use if not specified in file properties."
  :type '(choice (const :tag "Use CLI default" nil)
                 (number :tag "Temperature value"))
  :group 'org-ai-prompt)

(defcustom org-ai-prompt-default-max-tokens nil
  "Default max tokens to use if not specified in file properties."
  :type '(choice (const :tag "Use CLI default" nil)
                 (integer :tag "Max tokens"))
  :group 'org-ai-prompt)

;;; Internal Variables

(defvar-local org-ai-prompt--active-processes nil
  "Alist of active processes and their markers for this buffer.")

;;; Utility Functions

(defun org-ai-prompt--message (format-string &rest args)
  "Display a message for org-ai-prompt with FORMAT-STRING and ARGS."
  (message "[org-ai-prompt] %s" (apply #'format format-string args)))

(defun org-ai-prompt--error (format-string &rest args)
  "Display an error message for org-ai-prompt with FORMAT-STRING and ARGS."
  (error "[org-ai-prompt] %s" (apply #'format format-string args)))

;;; File Properties Parsing

(defun org-ai-prompt--get-file-property (property)
  "Get the value of a file-level PROPERTY (e.g., 'AI_SYSTEM')."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:\\s-*\\(.*\\)$" property) nil t)
      (string-trim (match-string 1)))))

(defun org-ai-prompt--get-config ()
  "Get configuration from file properties and customization variables.
Returns a plist with :system, :model, :temperature, :max-tokens, and :command."
  (list :system (org-ai-prompt--get-file-property "AI_SYSTEM")
        :model (or (org-ai-prompt--get-file-property "AI_MODEL")
                   org-ai-prompt-default-model)
        :temperature (or (when-let ((temp (org-ai-prompt--get-file-property "AI_TEMPERATURE")))
                           (string-to-number temp))
                         org-ai-prompt-default-temperature)
        :max-tokens (or (when-let ((tokens (org-ai-prompt--get-file-property "AI_MAX_TOKENS")))
                          (string-to-number tokens))
                        org-ai-prompt-default-max-tokens)
        :command (or (org-ai-prompt--get-file-property "AI_COMMAND")
                     org-ai-prompt-command)))

;;; Buffer Parsing and Context Building

(defun org-ai-prompt--element-is-prompt-block-p (element)
  "Return non-nil if ELEMENT is a PROMPT block."
  (and (eq (org-element-type element) 'special-block)
       (string-equal (org-element-property :type element) "PROMPT")))

(defun org-ai-prompt--element-is-response-block-p (element)
  "Return non-nil if ELEMENT is a RESPONSE block."
  (and (eq (org-element-type element) 'special-block)
       (string-equal (org-element-property :type element) "RESPONSE")))

(defun org-ai-prompt--extract-block-content (element)
  "Extract the content from a special block ELEMENT."
  (let* ((begin (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element)))
    (when (and begin end)
      (string-trim (buffer-substring-no-properties begin end)))))

(defun org-ai-prompt--extract-src-block-as-markdown (element)
  "Extract a source block ELEMENT as markdown fenced code."
  (let* ((lang (org-element-property :language element))
         (value (org-element-property :value element)))
    (format "```%s\n%s```" (or lang "") (string-trim value))))

(defun org-ai-prompt--element-to-text (element)
  "Convert an org ELEMENT to text representation."
  (let ((type (org-element-type element)))
    (cond
     ;; Handle source code blocks
     ((eq type 'src-block)
      (org-ai-prompt--extract-src-block-as-markdown element))

     ;; Handle PROMPT blocks
     ((org-ai-prompt--element-is-prompt-block-p element)
      (org-ai-prompt--extract-block-content element))

     ;; Handle RESPONSE blocks - return nil, we'll skip these in user content
     ((org-ai-prompt--element-is-response-block-p element)
      nil)

     ;; Handle other elements by extracting their text
     (t
      (let* ((begin (org-element-property :begin element))
             (end (org-element-property :end element)))
        (when (and begin end)
          (string-trim
           (buffer-substring-no-properties begin end))))))))

(defun org-ai-prompt--build-conversation-context (end-pos)
  "Build conversation context from buffer start to END-POS.
Returns a list of message plists with :role and :content."
  (save-excursion
    (let* ((parse-tree (org-element-parse-buffer))
           (messages '())
           (current-user-content '())
           (in-response nil))

      ;; Iterate through all elements
      (org-element-map parse-tree '(headline paragraph src-block special-block)
        (lambda (element)
          (let ((elem-begin (org-element-property :begin element)))
            ;; Only process elements before END-POS
            (when (and elem-begin (<= elem-begin end-pos))
              (cond
               ;; Found a RESPONSE block - flush user content and add response
               ((org-ai-prompt--element-is-response-block-p element)
                ;; Flush any accumulated user content
                (when current-user-content
                  (push (list :role "user"
                             :content (string-trim
                                      (string-join (nreverse current-user-content) "\n\n")))
                        messages)
                  (setq current-user-content '()))
                ;; Add the assistant response
                (when-let ((content (org-ai-prompt--extract-block-content element)))
                  (push (list :role "assistant" :content content) messages)))

               ;; Found a PROMPT or other user content
               (t
                (when-let ((text (org-ai-prompt--element-to-text element)))
                  (unless (string-empty-p text)
                    (push text current-user-content)))))))))

      ;; Flush any remaining user content
      (when current-user-content
        (push (list :role "user"
                   :content (string-trim
                            (string-join (nreverse current-user-content) "\n\n")))
              messages))

      ;; Return messages in correct order
      (nreverse messages))))

;;; AI CLI Adapter System

(defun org-ai-prompt--format-messages-as-json (messages)
  "Format MESSAGES list as JSON string for CLI tools that accept JSON."
  (require 'json)
  (json-encode
   (mapcar (lambda (msg)
             (list (cons "role" (plist-get msg :role))
                   (cons "content" (plist-get msg :content))))
           messages)))

(defun org-ai-prompt--adapter-claude (messages system-prompt model temperature max-tokens)
  "Adapter for Claude CLI.
MESSAGES is the conversation history.
SYSTEM-PROMPT, MODEL, TEMPERATURE, MAX-TOKENS are config values."
  (let ((args '()))
    ;; Add system prompt if provided
    (when system-prompt
      (setq args (append args (list "--system" system-prompt))))

    ;; Add model if provided
    (when model
      (setq args (append args (list "--model" model))))

    ;; Add temperature if provided
    (when temperature
      (setq args (append args (list "--temperature" (number-to-string temperature)))))

    ;; Add max-tokens if provided
    (when max-tokens
      (setq args (append args (list "--max-tokens" (number-to-string max-tokens)))))

    ;; Add messages - Claude CLI typically accepts conversation as arguments
    ;; For now, we'll concatenate all messages and pass as a single prompt
    ;; This may need adjustment based on actual Claude CLI API
    (let ((conversation-text
           (mapconcat (lambda (msg)
                       (format "[%s]: %s"
                              (upcase (plist-get msg :role))
                              (plist-get msg :content)))
                     messages
                     "\n\n")))
      (append args (list conversation-text)))))

(defun org-ai-prompt--adapter-gemini (messages system-prompt model temperature max-tokens)
  "Adapter for Gemini CLI.
MESSAGES is the conversation history.
SYSTEM-PROMPT, MODEL, TEMPERATURE, MAX-TOKENS are config values."
  (let ((args '()))
    ;; Add model if provided
    (when model
      (setq args (append args (list "--model" model))))

    ;; Add temperature if provided
    (when temperature
      (setq args (append args (list "--temperature" (number-to-string temperature)))))

    ;; Add max-tokens if provided
    (when max-tokens
      (setq args (append args (list "--max-output-tokens" (number-to-string max-tokens)))))

    ;; Format conversation for Gemini
    (let ((conversation-text
           (concat
            (when system-prompt (format "System: %s\n\n" system-prompt))
            (mapconcat (lambda (msg)
                        (format "%s: %s"
                               (capitalize (plist-get msg :role))
                               (plist-get msg :content)))
                      messages
                      "\n\n"))))
      (append args (list conversation-text)))))

(defun org-ai-prompt--adapter-openai (messages system-prompt model temperature max-tokens)
  "Adapter for OpenAI CLI.
MESSAGES is the conversation history.
SYSTEM-PROMPT, MODEL, TEMPERATURE, MAX-TOKENS are config values."
  (let ((args '()))
    ;; Add model if provided
    (when model
      (setq args (append args (list "--model" model))))

    ;; Add temperature if provided
    (when temperature
      (setq args (append args (list "--temperature" (number-to-string temperature)))))

    ;; Add max-tokens if provided
    (when max-tokens
      (setq args (append args (list "--max-tokens" (number-to-string max-tokens)))))

    ;; Add system message if provided
    (when system-prompt
      (setq args (append args (list "--system" system-prompt))))

    ;; Format messages as conversation
    (let ((conversation-text
           (mapconcat (lambda (msg)
                       (format "%s: %s"
                              (capitalize (plist-get msg :role))
                              (plist-get msg :content)))
                     messages
                     "\n\n")))
      (append args (list conversation-text)))))

(defun org-ai-prompt--get-adapter (command)
  "Get the appropriate adapter function for COMMAND.
Returns a function or nil if using custom adapter."
  (cond
   ;; If custom adapter is set, return nil (will be handled separately)
   (org-ai-prompt-args-function nil)

   ;; Detect adapter based on command name
   ((string-match-p "claude" command) #'org-ai-prompt--adapter-claude)
   ((string-match-p "gemini" command) #'org-ai-prompt--adapter-gemini)
   ((string-match-p "openai\\|gpt" command) #'org-ai-prompt--adapter-openai)

   ;; Default to Claude adapter
   (t #'org-ai-prompt--adapter-claude)))

(defun org-ai-prompt--build-command-args (messages config)
  "Build command-line arguments for AI CLI.
MESSAGES is the conversation history.
CONFIG is a plist with :system, :model, :temperature, :max-tokens."
  (let* ((command (plist-get config :command))
         (system-prompt (plist-get config :system))
         (model (plist-get config :model))
         (temperature (plist-get config :temperature))
         (max-tokens (plist-get config :max-tokens))
         (adapter (or org-ai-prompt-args-function
                     (org-ai-prompt--get-adapter command))))

    (if adapter
        (funcall adapter messages system-prompt model temperature max-tokens)
      ;; Fallback if no adapter found
      (org-ai-prompt--adapter-claude messages system-prompt model temperature max-tokens))))

;;; Process Execution and Response Handling

(defun org-ai-prompt--insert-response (response marker)
  "Insert RESPONSE as a RESPONSE block at MARKER position."
  (when (and response (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        ;; Move to end of current block
        (org-end-of-meta-data t)
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

(defun org-ai-prompt--process-filter (process output)
  "Process filter for AI CLI PROCESS OUTPUT."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output))))

(defun org-ai-prompt--process-sentinel (process event)
  "Process sentinel for AI CLI PROCESS.
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
          (org-ai-prompt--error "AI CLI exited with code %d: %s"
                               exit-code
                               (if (buffer-live-p (process-buffer process))
                                   (with-current-buffer (process-buffer process)
                                     (buffer-string))
                                 "")))))

     ((eq status 'signal)
      (org-ai-prompt--error "AI CLI process killed: %s" event))

     (t
      (org-ai-prompt--message "AI CLI process status: %s" event)))))

(defun org-ai-prompt--execute-ai-command (command args marker)
  "Execute AI COMMAND with ARGS asynchronously.
Insert response at MARKER when complete."
  (let* ((executable (or org-ai-prompt-executable-path command))
         (process-buffer (generate-new-buffer " *org-ai-prompt-output*"))
         (process-name "org-ai-prompt"))

    ;; Start the async process
    (org-ai-prompt--message "Executing %s..." command)
    (condition-case err
        (let ((proc (apply #'start-process
                          process-name
                          process-buffer
                          executable
                          args)))
          ;; Store the marker in process properties
          (process-put proc 'org-ai-prompt-marker marker)
          (set-process-filter proc #'org-ai-prompt--process-filter)
          (set-process-sentinel proc #'org-ai-prompt--process-sentinel)
          (org-ai-prompt--message "AI request sent, waiting for response..."))
      (error
       (org-ai-prompt--error "Failed to start AI CLI: %s" (error-message-string err))
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

      ;; Get the end position of this prompt block
      (let* ((block-end (org-element-property :end block))
             (marker (point-marker))
             (config (org-ai-prompt--get-config))
             (messages (org-ai-prompt--build-conversation-context block-end))
             (command (plist-get config :command))
             (args (org-ai-prompt--build-command-args messages config)))

        ;; Validate that we have messages
        (if (null messages)
            (org-ai-prompt--error "No conversation context to send")

          ;; Execute the AI command
          (org-ai-prompt--execute-ai-command command args marker)
          t)))))  ; Return t to indicate we handled the C-c C-c

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode org-ai-prompt-mode
  "Minor mode for AI prompt integration in Org mode.

This mode adds support for PROMPT and RESPONSE blocks that enable
interactive AI conversations within org documents.

Key bindings:
  \\[org-ctrl-c-ctrl-c] - Execute prompt block at point

Configuration can be done through:
  - File properties: #+AI_SYSTEM:, #+AI_MODEL:, etc.
  - Customization variables: `org-ai-prompt-command', etc."
  :lighter " AI"
  :group 'org-ai-prompt
  (if org-ai-prompt-mode
      (org-ai-prompt--setup)
    (org-ai-prompt--teardown)))

(defun org-ai-prompt--setup ()
  "Set up org-ai-prompt-mode in the current buffer."
  ;; Add our ctrl-c-ctrl-c hook
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-ai-prompt--execute-block nil t)
  ;; Register our block types with org-mode
  (org-ai-prompt--register-blocks))

(defun org-ai-prompt--teardown ()
  "Clean up org-ai-prompt-mode in the current buffer."
  ;; Remove our ctrl-c-ctrl-c hook
  (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-ai-prompt--execute-block t))

(defun org-ai-prompt--register-blocks ()
  "Register PROMPT and RESPONSE block types with org-mode."
  ;; Org mode doesn't require explicit block registration for custom blocks,
  ;; but we can add language support for syntax highlighting if desired
  ;; For now, blocks will work with standard org-mode block syntax
  t)

(provide 'org-ai-prompt)

;;; org-ai-prompt.el ends here
