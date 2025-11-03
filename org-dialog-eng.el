;;; org-dialog-eng-mode-v2.el --- LLM integration for Org mode -*- lexical-binding: t; -*-
(require 'org)
(require 'org-element)

(defface org-dialog-eng-prompt-block
  '((t (:background "#2d3a55" :extend t)))
  "Face for PROMPT block content."
  :group 'org-dialog-eng)

(defface org-dialog-eng-assistant-block
  '((t (:background "#3d2f5b" :extend t)))
  "Face for ASSISTANT block content."
  :group 'org-dialog-eng)

(defcustom org-dialog-eng-executable "claude"
  "Executable to spawn that can receive prompt as input and operates non-interactively."
  :type '(string)
  :group 'org-dialog-eng)

(defun org-dialog-eng--element-is-prompt-block-p (element)
  "Return non-nil if ELEMENT is a PROMPT block."
  (and (eq (org-element-type element) 'special-block)
       (string-equal (upcase (org-element-property :type element)) "PROMPT")))

(defun org-dialog-eng--extract-block-content (element)
  "Extract the content from a special block ELEMENT."
  (let* ((begin (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element)))
    (when (and begin end)
      (string-trim (buffer-substring-no-properties begin end)))))

(defun org-dialog-eng--element-is-assistant-block-p (element)
  "Return non-nil if ELEMENT is an ASSISTANT block."
  (and (eq (org-element-type element) 'special-block)
       (string-equal (upcase (org-element-property :type element)) "ASSISTANT")))
(defun org-dialog-eng--get-preceding-context ()
  "Extract all buffer content before the current PROMPT block."
  (let* ((element (org-element-at-point))
	 (block (org-element-lineage element '(special-block) t))
	 (prompt-begin (org-element-property :begin block)))
    (when prompt-begin
      (string-trim
       (buffer-substring-no-properties (point-min) prompt-begin)))))

(defun org-dialog-eng--find-next-assistant-block (prompt-element)
  "Find the next ASSISTANT block after PROMPT-ELEMENT, if it exists.
Returns the element or nil if no ASSISTANT block immediately follows."
  (save-excursion
    (goto-char (org-element-property :end prompt-element))
    (forward-line)
    ;; Skip whitespace
    (skip-chars-forward " \t\n")
    ;; Now check if we're at an ASSISTANT block
    (let ((next-elem (org-element-at-point)))
      (when (org-dialog-eng--element-is-assistant-block-p next-elem)
        next-elem))))
(defun org-dialog-eng--execute-prompt-block ()
  "Execute the PROMPT block at point by spawning LLM subprocess and inserting response."
  (interactive)
  (let* ((element (org-element-at-point))
	 (prompt-content (org-dialog-eng--extract-block-content element))
	 (preceding-context (org-dialog-eng--get-preceding-context))
	 (prompt (format "
<instructions>
You are a helpful assitant that is helping me write an org document in
Emacs that may contain code (literate programming style). I will provide
you with a prompt and you should respond to it in a very concise
manner. In case the prompt involves a request for code you shall not
edit any files but instead provide the code as output, if the request
involves modification of code you should provide just the changes
necessary (think of a git diff). I will also provide you with some
context (the content of the org file we have written so far) for you to
be better informed.

Whenever you see a #+begin_prompt ... #+end_prompt delimiters the
content inside is a prompt I have given you in the past.

Whenever you see a #+begin_assistant ... #+end_assistant delimiters, the
content inside is a response you have given me, to the preceding prompt,
in the past.

CRITICAL: Since we are editing an org file together, any structure
necessary for your response should be done in org syntax, NOT
markdown. Never respond with markdown UNLESS expclitly asked to in the
prompt.
</instructions>

<prompt>
%s
</prompt>

<context>
%s
</context>
" prompt-content preceding-context)))
    (unless prompt-content
      (error "No prompt content found"))
    
    ;; Get the end position of the current prompt block
    (let* ((block-end (org-element-property :end element))
	   (output-buffer (generate-new-buffer " *llm-output*"))
	   (source-buffer (current-buffer)))

      ;; Display ASSISTANT block with placeholder content for immediate feedback
      (save-excursion
	(goto-char block-end)
	(forward-line)
	(insert "\n#+begin_assistant\nLLM is working...\n#+end_assistant\n"))

      ;; Make buffer read-only while processing
      (setq buffer-read-only t)

      ;; Start the async process
      (make-process
       :name "org-dialog-eng-process"
       :buffer output-buffer
       :command (list org-dialog-eng-executable "-p" prompt)
       :sentinel
       (lambda (process event)
	 (when (string-match-p "finished" event)
	   (when (buffer-live-p source-buffer)
	     (with-current-buffer source-buffer
	       (save-excursion
		 ;; Move to the end of the prompt block
		 (goto-char block-end)
		 (forward-line)
		 ;; Get the response from the output buffer
		 (let ((response (with-current-buffer output-buffer
				   (buffer-substring-no-properties (point-min) (point-max)))))
		   
		   (setq buffer-read-only nil)
		   ;; Check if there's already an ASSISTANT block right after
		   ;; If so, we should replace it; otherwise insert new one
		   (let ((next-assistant (org-dialog-eng--find-next-assistant-block element)))
		     (when next-assistant
		       ;; Delete the existing ASSISTANT block
		       (delete-region (org-element-property :begin next-assistant)
				      (org-element-property :end next-assistant))))
		   
		   ;; Insert the new ASSISTANT block
		   (insert "\n#+begin_assistant\n")
		   (insert (string-trim response))
		   (insert "\n#+end_assistant\n"))))
	     (setq buffer-read-only t))
	   
	   ;; Clean up
	   (when (buffer-live-p source-buffer)
	     (with-current-buffer source-buffer
	       (setq buffer-read-only nil)))
	   (kill-buffer output-buffer)
	   (message "LLM response inserted")))
       
       :stderr (get-buffer-create "*org-dialog-eng-errors*")))))
(defun org-dialog-eng--find-next-assistant-block (prompt-element)
  "Find the next ASSISTANT block after PROMPT-ELEMENT, if it exists.
Returns the element or nil if no ASSISTANT block immediately follows."
  (save-excursion
    (goto-char (org-element-property :end prompt-element))
    ;; Skip whitespace
    (skip-chars-forward " \t\n")
    ;; Now check if we're at an ASSISTANT block
    (let ((next-elem (org-element-at-point)))
      (when (org-dialog-eng--element-is-assistant-block-p next-elem)
        next-elem))))
(defvar org-dialog-eng-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-dialog-eng--execute-prompt-block)
    map)
  "Keymap for org-dialog-eng-mode.")

(define-minor-mode org-dialog-eng-mode
  "Minor mode for LLM integration in Org documents."
  :lighter " OrgDialogEng"
  :keymap org-dialog-eng-mode-map)

(provide 'org-dialog-eng-mode-v2)

(defvar org-dialog-eng-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-dialog-eng--execute-prompt-block)
    map)
  "Keymap for org-dialog-eng-mode.")

(provide 'org-dialog-eng)
  (define-minor-mode org-dialog-eng-mode
    "Minor mode for LLM integration in Org mode.

      This mode adds support for PROMPT blocks that send questions to an LLM.

      Key bindings:
        \\[org-ctrl-c-ctrl-c] - Execute prompt block at point (sends to the LLM)

      Usage:
        1. Create a #+BEGIN_PROMPT block with your question
        2. Position cursor inside the block
        3. Press C-c C-c
        4. Wait for the LLM to respond
        5. The LLM's response appears in a #+begin_assistant block below"
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
