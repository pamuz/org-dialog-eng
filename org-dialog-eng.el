;;; org-dialog-eng.el --- Claude integration for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Pablo Munoz
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, ai, claude, prompts
;; URL: https://github.com/pmunoz/org-dialog-eng

;;; Commentary:

;; This package provides integration between Org mode and Claude AI.
;;
;; Usage:
;; 1. Create a #+BEGIN_PROMPT block with your question
;; 2. Position cursor inside the block
;; 3. Press C-c C-c
;; 4. Wait for Claude to respond
;; 5. Claude's response appears in a #+begin_assistant block below
;;
;; The mode sends all content before the prompt as context, enabling
;; natural conversations that reference your notes and code.
;;
;; Setup:
;;   (require 'org-dialog-eng)
;;   (add-hook 'org-mode-hook #'org-dialog-eng-mode)

;;; Code:

(require 'org)
(require 'org-element)

;;; Customization

(defgroup org-dialog-eng nil
  "Claude AI integration for Org mode."
  :group 'org
  :prefix "org-dialog-eng-")

(defcustom org-dialog-eng-command "claude"
  "Command to invoke Claude CLI."
  :type 'string
  :group 'org-dialog-eng)

(defcustom org-dialog-eng-executable-path nil
  "Path to Claude CLI executable.
If nil, the command will be searched in PATH."
  :type '(choice (const :tag "Use PATH" nil)
                 (file :tag "Executable path"))
  :group 'org-dialog-eng)

(defface org-dialog-eng-prompt-block
  '((t (:background "#2d3a55" :extend t)))
  "Face for PROMPT block content."
  :group 'org-dialog-eng)

(defface org-dialog-eng-assistant-block
  '((t (:background "#3d2f5b" :extend t)))
  "Face for ASSISTANT block content."
  :group 'org-dialog-eng)

;;; Utility Functions

(defun org-dialog-eng--message (format-string &rest args)
  "Display a message for org-dialog-eng with FORMAT-STRING and ARGS."
  (message "[org-dialog-eng] %s" (apply #'format format-string args)))

(defun org-dialog-eng--error (format-string &rest args)
  "Display an error message for org-dialog-eng with FORMAT-STRING and ARGS."
  (error "[org-dialog-eng] %s" (apply #'format format-string args)))

;;; Block Detection and Content Extraction

(defun org-dialog-eng--element-is-prompt-block-p (element)
  "Return non-nil if ELEMENT is a PROMPT block."
  (and (eq (org-element-type element) 'special-block)
       (string-equal (upcase (org-element-property :type element)) "PROMPT")))

(defun org-dialog-eng--element-is-response-block-p (element)
  "Return non-nil if ELEMENT is an ASSISTANT block."
  (and (eq (org-element-type element) 'special-block)
       (string-equal (upcase (org-element-property :type element)) "ASSISTANT")))

(defun org-dialog-eng--extract-block-content (element)
  "Extract the content from a special block ELEMENT."
  (let* ((begin (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element)))
    (when (and begin end)
      (string-trim (buffer-substring-no-properties begin end)))))

(defun org-dialog-eng--element-to-text (element)
  "Convert an org ELEMENT to text for context building."
  (let ((type (org-element-type element)))
    (cond
     ;; Source code blocks - format as markdown
     ((eq type 'src-block)
      (let* ((lang (org-element-property :language element))
             (value (org-element-property :value element)))
        (format "```%s\n%s```" (or lang "") (string-trim value))))

     ;; PROMPT blocks - include content
     ((org-dialog-eng--element-is-prompt-block-p element)
      (org-dialog-eng--extract-block-content element))

     ;; ASSISTANT blocks - include content
     ((org-dialog-eng--element-is-response-block-p element)
      (org-dialog-eng--extract-block-content element))

     ;; Other elements - extract text
     (t
      (let* ((begin (org-element-property :begin element))
             (end (org-element-property :end element)))
        (when (and begin end)
          (string-trim
           (buffer-substring-no-properties begin end))))))))

;;; Context Building

(defun org-dialog-eng--build-context (end-pos)
  "Build context from buffer start to END-POS.
Returns a string containing all relevant content before the prompt."
  (save-excursion
    (let* ((parse-tree (org-element-parse-buffer))
           (context-parts '()))

      ;; Collect all elements up to end-pos
      (org-element-map parse-tree '(headline paragraph src-block special-block plain-list)
        (lambda (element)
          (let ((elem-begin (org-element-property :begin element)))
            ;; Only process elements before END-POS
            (when (and elem-begin (< elem-begin end-pos))
              (when-let ((text (org-dialog-eng--element-to-text element)))
                (unless (string-empty-p text)
                  (push text context-parts)))))))

      ;; Join all parts with double newlines
      (if context-parts
          (string-join (nreverse context-parts) "\n\n")
        ""))))

;;; Response Insertion

(defun org-dialog-eng--strip-ansi-codes (text)
  "Remove ANSI escape codes and control characters from TEXT.
Preserves newlines and tabs."
  ;; Remove CSI sequences (ESC[...letter)
  (setq text (replace-regexp-in-string "\033\\[[0-9;?]*[a-zA-Z]" "" text))
  ;; Remove other escape sequences
  (setq text (replace-regexp-in-string "\033[^[]" "" text))
  ;; Clean up control characters but preserve newline (\n = \x0A) and tab (\t = \x09)
  (setq text (replace-regexp-in-string "[\x00-\x08\x0B-\x1F\x7F]" "" text))
  text)

(defun org-dialog-eng--ensure-blank-line-before ()
  "Ensure there is a blank line before point.
Only inserts newlines if needed to create a blank line."
  (let ((chars-before (buffer-substring-no-properties
                       (max (point-min) (- (point) 2))
                       (point))))
    (cond
     ;; Already have blank line (two newlines before point)
     ((string-match-p "\n\n\\'" chars-before) nil)
     ;; Have one newline, need one more
     ((string-match-p "\n\\'" chars-before) (insert "\n"))
     ;; At start of buffer or no newlines, need two
     (t (insert "\n\n")))))

(defun org-dialog-eng--ensure-blank-line-after ()
  "Ensure there is a blank line after point.
Only inserts newlines if needed to create a blank line."
  (let ((chars-after (buffer-substring-no-properties
                      (point)
                      (min (point-max) (+ (point) 2)))))
    (cond
     ;; Already have blank line (two newlines after point)
     ((string-match-p "\\`\n\n" chars-after) nil)
     ;; Have one newline, need one more
     ((string-match-p "\\`\n" chars-after) (insert "\n"))
     ;; At end of buffer or no newlines, need two
     ((= (point) (point-max)) (insert "\n"))
     ;; Content immediately after, need two newlines
     (t (insert "\n\n")))))

(defun org-dialog-eng--insert-response (response marker)
  "Insert RESPONSE as an ASSISTANT block at MARKER position."
  (when (and response (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        ;; Find the containing PROMPT block
        (let* ((element (org-element-at-point))
               (block (org-element-lineage element '(special-block) t)))
          (when block
            ;; Go to the end of the PROMPT block
            (goto-char (org-element-property :end block))
            ;; Ensure blank line before assistant block
            (org-dialog-eng--ensure-blank-line-before)
            (insert "#+begin_assistant\n")
            (insert (string-trim (org-dialog-eng--strip-ansi-codes response)))
            (insert "\n#+end_assistant\n")
            ;; Ensure blank line after assistant block
            (org-dialog-eng--ensure-blank-line-after)
            (org-dialog-eng--message "Response inserted successfully")))))))

;;; Process Execution

  (defun org-dialog-eng--process-filter (process output)
    "Process filter for Claude CLI PROCESS OUTPUT.
Accumulates output in process buffer."
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (goto-char (point-max))
        (insert output))))

(defun org-dialog-eng--process-sentinel (process event)
  "Process sentinel for Claude CLI PROCESS.
EVENT describes what happened to the process."
  (when-let ((buf (process-get process 'org-dialog-eng-buffer)))
    (when (buffer-live-p buf) (with-current-buffer buf (setq buffer-read-only nil))))
  (let ((status (process-status process))
	(marker (process-get process 'org-dialog-eng-marker)))
    (cond
     ((eq status 'exit)
      (let ((exit-code (process-exit-status process)))
        (if (= exit-code 0)
            ;; Success - insert the response
            (when (buffer-live-p (process-buffer process))
              (let ((response (with-current-buffer (process-buffer process)
				(buffer-string))))
                (org-dialog-eng--insert-response response marker)
                (kill-buffer (process-buffer process))))
          ;; Error - show message
          (org-dialog-eng--error "Claude CLI exited with code %d: %s"
				 exit-code
				 (if (buffer-live-p (process-buffer process))
                                     (with-current-buffer (process-buffer process)
                                       (buffer-string))
                                   "")))))

     ((eq status 'signal)
      (org-dialog-eng--error "Claude CLI process killed: %s" event))

     (t
      (org-dialog-eng--message "Claude CLI process status: %s" event)))))

(defun org-dialog-eng--execute-claude (prompt-content marker)
  "Execute Claude with PROMPT-CONTENT asynchronously.
Insert response at MARKER when complete."
  (let* ((executable (or org-dialog-eng-executable-path org-dialog-eng-command))
         (process-buffer (generate-new-buffer " *org-dialog-eng-output*"))
         (process-name "org-dialog-eng"))

    ;; Start the async process: claude -p "prompt content"
    (org-dialog-eng--message "Sending prompt to Claude...")
    (condition-case err
        (let ((proc (start-process
                     process-name
                     process-buffer
                     executable
                     "-p"
                     prompt-content)))
          ;; Store the marker in process properties
          (process-put proc 'org-dialog-eng-marker marker)
	  (process-put proc 'org-dialog-eng-buffer (marker-buffer marker))
	  (with-current-buffer (marker-buffer marker) (setq buffer-read-only t))
          (set-process-filter proc #'org-dialog-eng--process-filter)
          (set-process-sentinel proc #'org-dialog-eng--process-sentinel)
          (org-dialog-eng--message "Waiting for Claude's response..."))
      (error
       (org-dialog-eng--error "Failed to start Claude CLI: %s" (error-message-string err))
       (kill-buffer process-buffer)))))

;;; Block Execution

(defun org-dialog-eng--execute-block ()
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
               (org-dialog-eng--element-is-prompt-block-p block))

      ;; Extract the prompt content
      (let* ((prompt-content (org-dialog-eng--extract-block-content block))
             (block-begin (org-element-property :begin block))
             ;; Build context from start of buffer up to (but not including) this PROMPT block
             (context (org-dialog-eng--build-context block-begin))
             ;; Format the full message with context + emphasized prompt
             (full-message (if (string-empty-p context)
                               ;; No context, just send the prompt
                               prompt-content
                             ;; Has context, format with separator
                             (format "%s\n\n---\nPROMPT:\n%s" context prompt-content))))

        (if (or (null prompt-content) (string-empty-p prompt-content))
            (org-dialog-eng--error "PROMPT block is empty")

          ;; Execute Claude with the full message (context + prompt)
          (org-dialog-eng--execute-claude full-message (point-marker))
          t)))))  ; Return t to indicate we handled the C-c C-c

;;; Minor Mode Definition

      ;;;###autoload
(define-minor-mode org-dialog-eng-mode
  "Minor mode for Claude integration in Org mode.

    This mode adds support for PROMPT blocks that send questions to Claude.

    Key bindings:
      \\[org-ctrl-c-ctrl-c] - Execute prompt block at point (sends to Claude)

    Usage:
      1. Create a #+BEGIN_PROMPT block with your question
      2. Position cursor inside the block
      3. Press C-c C-c
      4. Wait for Claude to respond
      5. Claude's response appears in a #+begin_assistant block below"
  :lighter " AI"
  :group 'org-dialog-eng
  (if org-dialog-eng-mode
      (org-dialog-eng--setup)
    (org-dialog-eng--teardown)))

(defun org-dialog-eng--setup ()
  "Set up org-dialog-eng-mode in the current buffer."
  ;; Add our ctrl-c-ctrl-c hook
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-dialog-eng--execute-block nil t)

  ;; Add font-lock for block highlighting
  (font-lock-add-keywords nil
    			  '(("^\\([ \t]*#\\+begin_prompt\n\\(?:.*\n\\)*?[ \t]*#\\+end_prompt.*$\\)"
    			     (1 'org-dialog-eng-prompt-block t))
    			    ("^\\([ \t]*#\\+begin_assistant\n\\(?:.*\n\\)*?[ \t]*#\\+end_assistant\\)"
    			     (1 'org-dialog-eng-assistant-block t)))
    			  t))


(defun org-dialog-eng--teardown ()
  "Clean up org-dialog-eng-mode in the current buffer."
  ;; Remove our ctrl-c-ctrl-c hook
  (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-dialog-eng--execute-block t)
  (font-lock-remove-keywords nil
			     '(("^\\([ \t]*#\\+begin_prompt\n\\(?:.*\n\\)*?[ \t]*#\\+end_prompt.*$\\)"
				(1 'org-dialog-eng-prompt-block t))
			       ("^\\([ \t]*#\\+begin_assistant\n\\(?:.*\n\\)*?[ \t]*#\\+end_assistant\\)"
				(1 'org-dialog-eng-assistant-block t)))))

(provide 'org-dialog-eng)

      ;;; org-dialog-eng.el ends here
