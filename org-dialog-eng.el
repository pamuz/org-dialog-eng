;;-*- lexical-binding: t; -*- declaration

(require 'org)
(require 'org-element)

(defun org-dialog-eng--current-block-info ()
  "Get information about the current special block (prompt/assistant).
Returns a plist with :type, :begin, :end, and :end-pos, or nil if not in a block.
Uses org-element-lineage to properly handle nested elements."
  (let* ((element (org-element-at-point))
         (block (org-element-lineage element '(special-block) t)))
    (when block
      (let ((block-type (org-element-property :type block)))
        (when (member block-type '("prompt" "assistant"))
          (list :type (downcase block-type)
                :begin (org-element-property :contents-begin block)
                :end (org-element-property :contents-end block)
                :end-pos (org-element-property :end block)))))))

(defun org-dialog-eng--get-prompt-block-content ()
  "Get the content of current prompt block."
  (save-excursion
    (when-let* ((info (org-dialog-eng--current-block-info))
		  ((string= (plist-get info :type) "prompt"))
		  (begin (plist-get info :begin))
		  (end (plist-get info :end)))
	(buffer-substring-no-properties begin end))))
(defun org-dialog-eng--insert-assistant-block (content)
  "Insert an assitant block with CONTENT after the current prompt block.
Leaves a blank line between the prompt block and the new assistant block."
  (save-excursion
    (when-let* ((info (org-dialog-eng--current-block-info))
		((string= (plist-get info :type) "prompt"))
		(end-pos (plist-get info :end-pos)))
      (goto-char end-pos)
      (insert "#+begin_assistant\n"
	      content
	      (if (string-suffix-p "\n" content) "" "\n")
	      "#+end_assistant\n\n")

      ;; If inserting "Thinking.", set up animation
      (when (string= content "Thinking.")
	(setq org-dialog-eng--progress-marker
	      (copy-marker (- (point)
			      (length "#+end_assistant\n\n")
			      (if (string-suffix-p "\n" content) 0 1)
			      (length content))))
	;; Animate every 0.5 seconds, stop after 4 seconds
	(setq org-dialog-eng--progress-timer
	      (run-with-timer 0.5 0.5 #'org-dialog-eng--animate-progress))))))
(defvar org-dialog-eng--progress-timer nil
  "Timer for the progress indicator animation.")

(defvar org-dialog-eng--progress-marker nil
  "Marker pointing to the progress indicator content.")

(defun org-dialog-eng--animate-progress ()
  "Cycle through 'Thinking.' -> 'Thinking..' -> 'Thinking...'"
  (when (and org-dialog-eng--progress-marker
	     (marker-buffer org-dialog-eng--progress-marker))
    (save-excursion
      (goto-char org-dialog-eng--progress-marker)
      (let* ((line-end (line-end-position))
	     (current-text (buffer-substring-no-properties
			    org-dialog-eng--progress-marker
			    line-end)))
	(cond
	 ((string= current-text "Thinking.")
	  (delete-region org-dialog-eng--progress-marker line-end)
	  (insert "Thinking.."))
	 ((string= current-text "Thinking..")
	    (delete-region org-dialog-eng--progress-marker line-end)
	    (insert "Thinking..."))
	 ((string= current-text "Thinking...")
	    (delete-region org-dialog-eng--progress-marker line-end)
	    (insert "Thinking.")))))))

(defun org-dialog-eng--stop-progress-indicator ()
  "Stop and clean up the progress indicator."
  (when org-dialog-eng--progress-timer
    (cancel-timer org-dialog-eng--progress-timer)
    (setq org-dialog-eng--progress-timer nil))
  (when org-dialog-eng--progress-marker
    (set-marker org-dialog-eng--progress-marker nil)
    (setq org-dialog-eng--progress-marker nil)))
(defvar org-dialog-eng--claude-process nil
  "Current running Claude process.")

(defvar org-dialog-eng--claude-output nil
  "Accumulated output from Claude process.")

(defun org-dialog-eng--start-claude-process (prompt)
  "Start Claude process with PROMPT as input."
  (setq org-dialog-eng--claude-output "")
  (setq org-dialog-eng--claude-process
	(make-process
	 :name "claude-dialog"
	 :command (list "claude" "-p" prompt)
	 :filter #'org-dialog-eng--claude-filter
	 :sentinel #'org-dialog-eng--claude-sentinel)))

(defun org-dialog-eng--claude-filter (proc string)
  "Accumulate output from Claude process PROC."
  (setq org-dialog-eng--claude-output
	(concat org-dialog-eng--claude-output string))
  (message org-dialog-eng--claude-output))

(defun org-dialog-eng--claude-sentinel (proc event)
  "Handle Claude process PROC completion EVENT."
  (when (memq (process-status proc) '(exit signal))
    ;; Replace "Thinking..." with actual response
    (org-dialog-eng--replace-assistant-content
     org-dialog-eng--claude-output)

    ;; Clean up
    (setq org-dialog-eng--claude-process nil)
    (setq org-dialog-eng--claude-output nil)))

(defun org-dialog-eng--replace-assistant-content (new-content)
  "Replace the content of the most recently inserted assistant block."
  (when (and org-dialog-eng--progress-marker
	     (marker-buffer org-dialog-eng--progress-marker))
    (save-excursion
      (goto-char org-dialog-eng--progress-marker)
      (let ((block-end (save-excursion
			 (re-search-forward "^#\\+end_assistant" nil t)
			 (line-beginning-position))))
	(when block-end
	  (delete-region org-dialog-eng--progress-marker block-end)
	  (goto-char org-dialog-eng--progress-marker)
	  ;; Stop the progress animation
	  (org-dialog-eng--stop-progress-indicator)
	  ;; Insert content
	  (let ((start (point)))
	  (insert new-content
		  (if (string-suffix-p "\n" new-content) "" "\n"))
	  ;; Fill the inserted region
	  (fill-region start (point))))))))

(defun org-dialog-eng--get-document-context ()
  "Extract all content from buffer start up to the current prompt block.
Returns the content as a string, or nil if not in a prompt block."
  (save-excursion
    (when-let* ((info (org-dialog-eng--current-block-info))
		((string= (plist-get info :type) "prompt")))
      ;; Find the beginning of the current prompt block
      ;; (the line with #+begin_prompt)
      (goto-char (plist-get info :begin))
      (re-search-backward "^#\\+begin_prompt" nil t)
      (let ((prompt-start (line-beginning-position)))
	;; Extract from beginning of buffer to start of prompt block
	(buffer-substring-no-properties (point-min) prompt-start)))))
(defun org-dialog-eng--format-prompt (user-prompt context)
  "Fromat USER-PROMPT and CONTEXT into an XML-style string.
Returns a string with <instructions>...<instructions> and <context>...</context> blocks."
  (concat "<instructions>\n"
	  "You are a helpful assistant helping write an Org mode document in Emacs,\n"
	  "potentially including code (literate programming style). I will provide\n"
	  "a prompt, and you should respond concisely.\n\n"
	  "If the prompt contains code, provide it in Org syntax (#+begin_src blocks),\n"
	  "not markdown. If it request modification, only show the changes needed.\n\n"
	  "I will also provide context: the content of the Org file we have written\n"
	  "so far, for better informed respones.\n\n"
	  "When you see #+begin_prompt ... #+end_prompt delimiters, the content\n"
	  "inside is a prompt I gave you previously.\n\n"
	  "When you see #+begin_assistant ... #+end_assistant delimiters, the\n"
	  "content inside is a response you gave me previously.\n\n"
	  "<prompt>\n"
	  user-prompt
	  "</prompt>\n\n"
	  "<context>\n"
	  (or context "")
	  (if (or (not context) (string-suffix-p "\n" context)) "" "\n")
	  "</context>\n"))
(defun org-dialog-eng--replace-assistant-content (new-content)
  "Replace the content of the most recently inserted assistant block."
  (when (and org-dialog-eng--progress-marker
	     (marker-buffer org-dialog-eng--progress-marker))
    (save-excursion
      (goto-char org-dialog-eng--progress-marker)
      (let ((block-end (save-excursion
			 (re-search-forward "^#\\+end_assistant" nil t)
			 (line-beginning-position))))
	(when block-end
	  (delete-region org-dialog-eng--progress-marker block-end)
	  (goto-char org-dialog-eng--progress-marker)
	  ;; Stop the progress animation
	  (org-dialog-eng--stop-progress-indicator)
	  ;; Insert content
	  (let ((start (point)))
	    (insert new-content
		    (if (string-suffix-p "\n" new-content) "" "\n"))
	    ;; Fill the inserted region
	    (fill-region start (point))))))))
(defun org-dialog-eng--replace-assistant-content (new-content)
  "Replace the content of the most recently inserted assistant block."
  (when (and org-dialog-eng--progress-marker
	     (marker-buffer org-dialog-eng--progress-marker))
    (save-excursion
      (goto-char org-dialog-eng--progress-marker)
      (let ((block-end (save-excursion
			 (re-search-forward "^#\\+end_assistant" nil t)
			 (line-beginning-position))))
	(when block-end
	  (delete-region org-dialog-eng--progress-marker block-end)
	  (goto-char org-dialog-eng--progress-marker)
	  ;; Stop the progress animation
	  (org-dialog-eng--stop-progress-indicator)
	  ;; Insert content
	  (let ((start (point)))
	    (insert new-content
		    (if (string-suffix-p "\n" new-content) "" "\n"))
	    ;; Fill the inserted region
	    (fill-region start (point))))))))

(defun org-dialog-eng--execute-prompt-block ()
  "Execute the current prompt block using Claude.
Returns t if executed successfully, nil otherwise."
  (interactive)
  (when-let* ((info (org-dialog-eng--current-block-info))
	      (prompt-block-content (org-dialog-eng--get-prompt-block-content))
	      (context (org-dialog-eng--get-document-context))
	      (prompt (org-dialog-eng--format-prompt prompt-block-content context)))
    ;; Insert initial "Thinking." and start animation
    (org-dialog-eng--insert-assistant-block "Thinking.")

    ;; Launch the claude process
    (org-dialog-eng--start-claude-process prompt)
    t)) ; Return t to indicate we handled the command



(define-minor-mode org-dialog-eng-mode
  "Minor mode for org-dialog-eng functionality."
  :lighter " OrgDialog"
  (if org-dialog-eng-mode
      (add-hook 'org-ctrl-c-ctrl-c-hook #'org-dialog-eng--execute-prompt-block nil t)
    (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-dialog-eng--execute-prompt-block t)))

(provide 'org-dialog-eng)
