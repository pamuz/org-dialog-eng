;;; org-dialog-eng.el --- LLM integration for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pablo Munoz

;; Author: Pablo Munoz
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, ai, llm, literate-programming
;; URL: https://github.com/pmunoz/org-dialog-eng

;;; Commentary:

;; This package enables interactive dialogues with Large Language Models
;; directly within Org mode documents.  It extends the literate programming
;; paradigm to include AI-assisted document development.
;;
;; Usage:
;;   1. Enable org-dialog-eng-mode in an Org buffer
;;   2. Create a #+begin_prompt block with your question
;;   3. Position cursor inside the block and press C-c C-c
;;   4. An ASSISTANT block with the response appears below
;;
;; The mode sends all document content before the prompt as context,
;; enabling context-aware conversations that reference your notes and code.
;;
;; Configuration:
;;   (require 'org-dialog-eng)
;;   (add-hook 'org-mode-hook #'org-dialog-eng-mode)
;;   (setq org-dialog-eng-executable "claude")  ; or your preferred LLM CLI

;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-dialog-eng nil
  "LLM integration for Org mode documents."
  :group 'org
  :prefix "org-dialog-eng-")

(defface org-dialog-eng-prompt-block
  '((t (:background "#2d3a55" :extend t)))
  "Face for PROMPT block content.
The background provides visual distinction for user-authored prompts."
  :group 'org-dialog-eng)

(defface org-dialog-eng-assistant-block
  '((t (:background "#3d2f5b" :extend t)))
  "Face for ASSISTANT block content.
A slightly different hue distinguishes LLM responses from user prompts."
  :group 'org-dialog-eng)

(defcustom org-dialog-eng-executable "claude"
  "Executable for LLM interaction.
This should be a command-line tool that accepts a prompt via the -p flag
and outputs the response to stdout. Examples: \"claude\", \"copilot\",
or a custom wrapper script."
  :type 'string
  :group 'org-dialog-eng)

(defun org-dialog-eng--element-is-prompt-block-p (element)
  "Return non-nil if ELEMENT is a PROMPT block.
ELEMENT should be an org-element as returned by `org-element-at-point'."
  (and (eq (org-element-type element) 'special-block)
       (string-equal (upcase (org-element-property :type element)) "PROMPT")))
(defun org-dialog-eng--element-is-assistant-block-p (element)
  "Return non-nil if ELEMENT is an ASSISTANT block."
  (and (eq (org-element-type element) 'special-block)
       (string-equal (upcase (org-element-property :type element)) "ASSISTANT")))

(defun org-dialog-eng--extract-block-content (element)
  "Extract the content from special block ELEMENT.
Returns the text between #+begin_XXX and #+end_XXX as a string,
with leading and trailing whitespace trimmed."
  (let* ((begin (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element)))
    (when (and begin end)
      (string-trim (buffer-substring-no-properties begin end)))))
(defun org-dialog-eng--find-next-assistant-block (prompt-element)
  "Find the ASSISTANT block immediately following PROMPT-ELEMENT.
Returns the element if found, nil otherwise.
Skips whitespace between the blocks."
  (save-excursion
    (goto-char (org-element-property :end prompt-element))
    ;; Skip any whitespace or blank lines
    (skip-chars-forward " \t\n")
    ;; Check if we're now at an ASSISTANT block
    (let ((next-elem (org-element-at-point)))
      (when (org-dialog-eng--element-is-assistant-block-p next-elem)
        next-elem))))

(defun org-dialog-eng--get-preceding-context ()
  "Extract all buffer content before the current PROMPT block.
Returns a string containing everything from the buffer start
up to (but not including) the current PROMPT block."
  (let* ((element (org-element-at-point))
         (block (org-element-lineage element '(special-block) t))
         (prompt-begin (org-element-property :begin block)))
    (when prompt-begin
      (string-trim
       (buffer-substring-no-properties (point-min) prompt-begin)))))
(defun org-dialog-eng--format-prompt (prompt-content context)
  "Format PROMPT-CONTENT and CONTEXT into a structured prompt.
Returns a string suitable for sending to the LLM executable."
  (format "
<instructions>
You are a helpful assistant helping write an Org mode document in Emacs,
potentially including code (literate programming style). I will provide
a prompt, and you should respond concisely.

If the prompt requests code, provide it in Org syntax (#+begin_src blocks),
not markdown. If it requests modifications, show only the changes needed.

I will also provide context: the content of the Org file we have written
so far, for better-informed responses.

When you see #+begin_prompt ... #+end_prompt delimiters, the content
inside is a prompt I gave you previously.

When you see #+begin_assistant ... #+end_assistant delimiters, the
content inside is a response you gave me previously.

CRITICAL: Since we are editing an Org file together, use Org syntax,
NOT markdown, unless explicitly requested otherwise.
</instructions>

<prompt>
%s
</prompt>

<context>
%s
</context>
" prompt-content context))

(defun org-dialog-eng--execute-prompt-block ()
  "Execute the PROMPT block at point.
Spawns an async LLM subprocess and inserts the response in an ASSISTANT block.
The buffer becomes read-only during execution to prevent concurrent edits."
  (interactive)
  (let* ((element (org-element-at-point))
         (prompt-content (org-dialog-eng--extract-block-content element))
         (preceding-context (org-dialog-eng--get-preceding-context))
         (formatted-prompt (org-dialog-eng--format-prompt prompt-content preceding-context)))

    (unless prompt-content
      (error "No prompt content found. Ensure cursor is inside a PROMPT block"))

    (let* ((block-end (org-element-property :end element))
           (output-buffer (generate-new-buffer " *llm-output*"))
           (source-buffer (current-buffer)))

      ;; Immediately insert a placeholder ASSISTANT block for feedback
      (save-excursion
        (goto-char block-end)
        (unless (looking-at-p "^[ \t]*$")
          (end-of-line)
          (insert "\n"))
        (insert "\n#+begin_assistant\nLLM is working...\n#+end_assistant\n"))

      ;; Make buffer read-only to prevent concurrent edits
      (setq buffer-read-only t)
      (message "Querying LLM...")

      ;; Spawn the async process
      (make-process
       :name "org-dialog-eng-process"
       :buffer output-buffer
       :command (list org-dialog-eng-executable "-p" formatted-prompt)
       :sentinel
       (lambda (process event)
         (when (string-match-p "finished" event)
           (if (buffer-live-p source-buffer)
               (with-current-buffer source-buffer
                 (let ((response (with-current-buffer output-buffer
                                   (buffer-substring-no-properties
                                    (point-min) (point-max)))))
                   (save-excursion
                     ;; Make buffer temporarily writable
                     (setq buffer-read-only nil)

                     ;; Find and remove any existing ASSISTANT block
                     (let ((next-assistant
                            (org-dialog-eng--find-next-assistant-block element)))
                       (when next-assistant
                         (delete-region
                          (org-element-property :begin next-assistant)
                          (org-element-property :end next-assistant))))

                     ;; Insert the new ASSISTANT block with actual response
                     (goto-char block-end)
                     (unless (looking-at-p "^[ \t]*$")
                       (end-of-line)
                       (insert "\n"))
                     (insert "\n#+begin_assistant\n")
                     (insert (string-trim response))
                     (insert "\n#+end_assistant\n"))

                   ;; Restore read-only status
                   (setq buffer-read-only nil)
                   (message "LLM response inserted")))
             (message "Source buffer no longer exists"))

           ;; Clean up output buffer
           (when (buffer-live-p output-buffer)
             (kill-buffer output-buffer))))

       :stderr (get-buffer-create "*org-dialog-eng-errors*")))))
(defun org-dialog-eng--execute-block ()
  "Hook function for `org-ctrl-c-ctrl-c-hook'.
Executes PROMPT blocks when point is inside one.
Returns non-nil if the block was executed, nil otherwise."
  (let* ((element (org-element-at-point))
         (block (org-element-lineage element '(special-block) t)))
    (when (and block (org-dialog-eng--element-is-prompt-block-p block))
      (org-dialog-eng--execute-prompt-block)
      t)))  ; Return t to stop hook processing

(defun org-dialog-eng--fontify-block (limit)
  "Fontify PROMPT and ASSISTANT blocks up to LIMIT.
This is a font-lock matcher function that properly handles multiline blocks."
  (let ((case-fold-search t))
    (when (re-search-forward
           "^[ \t]*#\\+begin_\\(prompt\\|assistant\\)\\>"
           limit t)
      (let* ((block-type (match-string 1))
             (face (if (string-equal (downcase block-type) "prompt")
                       'org-dialog-eng-prompt-block
                     'org-dialog-eng-assistant-block))
             (beg (match-beginning 0))
             (end-re (format "^[ \t]*#\\+end_%s\\>" block-type)))
        (when (re-search-forward end-re limit t)
          (let ((end (match-end 0)))
            (put-text-property beg end 'face face)
            (put-text-property beg end 'font-lock-multiline t)
            (goto-char end)
            t))))))

(defun org-dialog-eng--setup-font-lock ()
  "Add font-lock keywords for PROMPT and ASSISTANT blocks."
  ;; Use a function-based matcher for reliable multiline matching
  (font-lock-add-keywords nil
    '((org-dialog-eng--fontify-block))
    'append)
  ;; Enable multiline font-lock
  (setq-local font-lock-multiline t))

(defun org-dialog-eng--teardown-font-lock ()
  "Remove font-lock keywords for PROMPT and ASSISTANT blocks."
  (font-lock-remove-keywords nil
    '((org-dialog-eng--fontify-block))))
;;;###autoload
(define-minor-mode org-dialog-eng-mode
  "Minor mode for LLM integration in Org mode documents.

This mode enables interactive dialogues with Large Language Models
directly within your Org documents. Create PROMPT blocks and execute
them with \\[org-ctrl-c-ctrl-c] to receive ASSISTANT responses.

Key bindings:
  \\[org-ctrl-c-ctrl-c] - Execute PROMPT block at point

Usage:
  1. Create a #+begin_prompt block with your question
  2. Position cursor inside the block
  3. Press \\[org-ctrl-c-ctrl-c]
  4. Wait for the LLM to respond
  5. The response appears in a #+begin_assistant block below

All content before the prompt is sent as context, enabling
context-aware conversations about your document."
  :lighter " AI"
  :group 'org-dialog-eng
  (if org-dialog-eng-mode
      (org-dialog-eng--setup)
    (org-dialog-eng--teardown)))
(defun org-dialog-eng--setup ()
  "Set up org-dialog-eng-mode in the current buffer.
Adds hooks and font-lock keywords."
  ;; Hook into C-c C-c
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-dialog-eng--execute-block nil t)

  ;; Enable syntax highlighting
  (org-dialog-eng--setup-font-lock)

  ;; Refresh font-lock to apply immediately
  (when (fboundp 'font-lock-flush)
    (font-lock-flush)))

(defun org-dialog-eng--teardown ()
  "Clean up org-dialog-eng-mode in the current buffer.
Removes hooks and font-lock keywords."
  ;; Remove our C-c C-c hook
  (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-dialog-eng--execute-block t)

  ;; Remove syntax highlighting
  (org-dialog-eng--teardown-font-lock)

  ;; Refresh font-lock
  (when (fboundp 'font-lock-flush)
    (font-lock-flush)))

(provide 'org-dialog-eng)

;;; org-dialog-eng.el ends here
