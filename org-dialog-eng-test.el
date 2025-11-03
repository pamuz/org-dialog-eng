(require 'ert)
(require 'org-dialog-eng-mode)

(ert-deftest org-dialog-eng-test-placeholder-replacement ()
  "Test that placeholder ASSISTANT block is replaced with LLM response."
  (with-temp-buffer
    (org-mode)
    (org-dialog-eng-mode 1)

    ;; Insert a PROMPT block
    (insert "#+begin_prompt\nTest response\n+#end_prompt\n")
    (goto-char (point-min))
    (forward-line 1)

    ;; Execute the prompt block
    (org-dialog-eng--execute-prompt-block)

    ;; Verify results
    (goto-char (point-min))
    (should (search-forward "#+begin_assistant" nil t))
    (should-not (search-forward "LLM is working..." nil t))
    (goto-char (point-min))
    (should (search-forward "Test response from LLM" nil t))))
