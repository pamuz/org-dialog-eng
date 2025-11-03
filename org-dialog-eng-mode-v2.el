;;; org-dialog-eng-mode-v2.el --- Claude integration for Org mode -*- lexical-binding: t; -*-
(require 'org)
(require 'org-element)

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

(defun org-dialog-eng--element-is-response-block-p (element)
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

(defun org-dialog-eng--execute-prompt-block-2 ()
  "Execute the PROMPT block at point by spawning Claude subprocess and inserting response."
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
           (output-buffer (generate-new-buffer " *claude-output*"))
           (source-buffer (current-buffer)))
      
      ;; Start the async process
      (message "Sending prompt to Claude...")
      (make-process
       :name "claude-prompt"
       :buffer output-buffer
       :command (list "claude" "-p" prompt)
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
		   
		   ;; Check if there's already an ASSISTANT block right after
		   ;; If so, we should replace it; otherwise insert new one
		   (let ((next-element (org-element-at-point)))
		     (when (org-dialog-eng--element-is-response-block-p next-element)
		       ;; Delete the existing ASSISTANT block
		       (delete-region (org-element-property :begin next-element)
				      (org-element-property :end next-element))))
		   
		   ;; Insert the new ASSISTANT block
		   (insert "\n#+begin_assistant\n")
		   (insert (string-trim response))
		   (insert "\n#+end_assistant\n")))))
           
           ;; Clean up
           (kill-buffer output-buffer)
           (message "Claude response inserted")))
       
       :stderr (get-buffer-create "*claude-errors*")))))
(defvar org-dialog-eng-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-dialog-eng--execute-prompt-block-2)
    map)
  "Keymap for org-dialog-eng-mode.")

(define-minor-mode org-dialog-eng-mode
  "Minor mode for Claude integration in Org documents."
  :lighter " OrgDialogEng"
  :keymap org-dialog-eng-mode-map)

(provide 'org-dialog-eng-mode-v2)

(defvar org-dialog-eng-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-dialog-eng--execute-prompt-block-2)
    map)
  "Keymap for org-dialog-eng-mode.")

(define-minor-mode org-dialog-eng-mode
  "Minor mode for Claude integration in Org documents."
  :lighter " DialogEng"
  :keymap org-dialog-eng-mode-map)

(provide 'org-dialog-eng-mode)
