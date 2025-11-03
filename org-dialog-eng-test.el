(require 'ert)
(require 'org-dialog-eng)

(ert-deftest org-dialog-eng--test-block-detection ()
  "Test that PROMPT blocks are detected correctly."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_prompt\nTest prompt\n#+end_prompt")
    (goto-char (point-min))
    (let ((element (org-element-at-point)))
      (should (org-dialog-eng--element-is-prompt-block-p element)))))

(ert-deftest org-dialog-eng--test-context-included-with-prompt ()
  "Test that the contents of the file up to the PROMPT block being executed
are fed into the AI."
  (with-temp-buffer
    (org-mode)
    (insert "* A test header\nSome test content before prompt\n#+begin_prompt\nTest prompt\n#+end_prompt")
    ;; Position cursor in the PROMPT block
    (goto-char (point-min))
    (search-forward "#+begin_prompt")
    (forward-line 1)

    ;; Get the element and its position
    (let* ((element (org-element-at-point))
	   (block (org-element-lineage element '(special-block) t))
	   (block-begin (org-element-property :begin block))
	   (context (org-dialog-eng--build-context block-begin)))

      ;; Verify the context contains expected content
      (should (string-match-p "A test header" context))
      (should (string-match-p "Some test content before prompt" context)))))
