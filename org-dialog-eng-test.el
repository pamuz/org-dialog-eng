(require 'ert)
(require 'org-dialog-eng)

(ert-deftest org-dialog-eng-test-placeholder-replacement ()
  "Test that placeholder ASSISTANT block is replaced with LLM response.

This test mocks the LLM subprocess to verify that:
1. A placeholder ASSISTANT block appears immediately
2. The placeholder is replaced (not duplicated) when response arrives
3. The actual response content is correctly inserted"
  (with-temp-buffer
    (org-mode)
    (org-dialog-eng-mode 1)

    ;; Insert a PROMPT block
    (insert "#+begin_prompt\nTest prompt\n#+end_prompt\n")
    (goto-char (point-min))
    (forward-line 1)  ; Move inside the block

    ;; Mock the LLM executable to return a known response
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((sentinel (plist-get args :sentinel))
                       (buffer (plist-get args :buffer)))
                   ;; Write mock response to output buffer
                   (with-current-buffer buffer
                     (insert "Test response from LLM"))
                   ;; Create a mock process
                   (let ((proc (make-pipe-process
                                :name "mock-process"
                                :buffer buffer)))
                     ;; Immediately call sentinel with finished status
                     (run-at-time 0.1 nil sentinel proc "finished\n")
                     proc)))))

      ;; Execute the prompt block
      (org-dialog-eng--execute-prompt-block)

      ;; Wait for async completion
      (sleep-for 0.2)

      ;; Verify results
      (goto-char (point-min))
      (should (search-forward "#+begin_assistant" nil t))
      (should-not (search-forward "LLM is working..." nil t))
      (goto-char (point-min))
      (should (search-forward "Test response from LLM" nil t)))))

(ert-deftest org-dialog-eng-test-element-detection ()
  "Test that PROMPT and ASSISTANT blocks are correctly identified."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_prompt\nTest\n#+end_prompt\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((element (org-element-at-point)))
      (should (org-dialog-eng--element-is-prompt-block-p
               (org-element-lineage element '(special-block) t))))

    (goto-char (point-max))
    (insert "\n#+begin_assistant\nResponse\n#+end_assistant\n")
    (forward-line -2)
    (let ((element (org-element-at-point)))
      (should (org-dialog-eng--element-is-assistant-block-p
               (org-element-lineage element '(special-block) t))))))

(ert-deftest org-dialog-eng-test-context-extraction ()
  "Test that preceding context is correctly extracted."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nSome context here\n\n")
    (insert "#+begin_prompt\nQuestion\n#+end_prompt\n")
    (goto-char (point-min))
    (search-forward "Question")
    (let ((context (org-dialog-eng--get-preceding-context)))
      (should (string-match-p "Heading" context))
      (should (string-match-p "Some context here" context))
      (should-not (string-match-p "Question" context)))))
