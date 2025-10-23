# Contributing to org-ai-prompt-mode

Thank you for your interest in contributing to org-ai-prompt-mode! This document provides guidelines for contributing to the project.

## Ways to Contribute

- 🐛 **Report bugs** - Found an issue? Let us know!
- 💡 **Suggest features** - Have an idea? We'd love to hear it!
- 📝 **Improve documentation** - Help make the docs clearer
- 🔧 **Add AI CLI adapters** - Support for new AI tools
- 💻 **Submit code** - Bug fixes and new features
- 🧪 **Test and provide feedback** - Try it out and share your experience

## Getting Started

1. **Fork the repository**
2. **Clone your fork:**
   ```bash
   git clone https://github.com/yourusername/org-ai-prompt.git
   cd org-ai-prompt
   ```
3. **Create a branch:**
   ```bash
   git checkout -b feature/your-feature-name
   ```

## Development Setup

### Prerequisites

- Emacs 27.1 or later
- Org mode 9.0 or later
- Git
- At least one AI CLI for testing

### Testing Your Changes

1. **Load the modified file in Emacs:**
   ```elisp
   M-x load-file RET /path/to/org-ai-prompt.el
   ```

2. **Test in an org buffer:**
   - Open `test.org`
   - Enable the mode: `M-x org-ai-prompt-mode`
   - Test prompt execution with `C-c C-c`

3. **Check for errors:**
   - Enable debug mode: `M-x toggle-debug-on-error`
   - Check `*Messages*` buffer
   - Use `M-x checkdoc` to verify documentation style

## Coding Guidelines

### Emacs Lisp Style

- Follow [Emacs Lisp Coding Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html)
- Use descriptive function and variable names
- Prefix all functions with `org-ai-prompt--` (internal) or `org-ai-prompt-` (public)
- Maximum line length: 80 characters

### Documentation

- Every function must have a docstring
- Use proper docstring format:
  ```elisp
  (defun org-ai-prompt--example-function (arg1 arg2)
    "Brief description of what the function does.

  ARG1 is the first argument description.
  ARG2 is the second argument description.

  Returns description of return value."
    ...)
  ```

### Code Structure

```elisp
;;; Package Header
;; Copyright, author, version, etc.

;;; Commentary
;; High-level description

;;; Code

(require 'dependencies)

;;; Customization
(defgroup ...)
(defcustom ...)

;;; Internal Variables
(defvar ...)

;;; Utility Functions
(defun org-ai-prompt--helper ...)

;;; Main Functionality
(defun org-ai-prompt-main-feature ...)

;;; Minor Mode Definition
(define-minor-mode ...)

(provide 'org-ai-prompt)
;;; org-ai-prompt.el ends here
```

## Adding a New AI CLI Adapter

To add support for a new AI tool:

1. **Create the adapter function:**

```elisp
(defun org-ai-prompt--adapter-newtool (messages system-prompt model temperature max-tokens)
  "Adapter for NewTool CLI.
MESSAGES is the conversation history.
SYSTEM-PROMPT, MODEL, TEMPERATURE, MAX-TOKENS are config values."
  (let ((args '()))
    ;; Build command arguments based on NewTool's CLI format
    (when model
      (setq args (append args (list "--model" model))))

    ;; Format conversation
    (let ((conversation-text
           (mapconcat (lambda (msg)
                       (format "%s: %s"
                              (plist-get msg :role)
                              (plist-get msg :content)))
                     messages
                     "\n\n")))
      (append args (list conversation-text)))))
```

2. **Register it in the adapter selector:**

```elisp
(defun org-ai-prompt--get-adapter (command)
  "Get the appropriate adapter function for COMMAND."
  (cond
   ;; ... existing adapters ...
   ((string-match-p "newtool" command) #'org-ai-prompt--adapter-newtool)
   ;; ... rest ...
   ))
```

3. **Document it in README.md** under "Built-in Adapters"

4. **Add an example** to `example.org`

5. **Test it** thoroughly

## Submitting Changes

### Before Submitting

- [ ] Test your changes with at least one AI CLI
- [ ] Run `M-x checkdoc` on modified files
- [ ] Update documentation (README.md, CHANGELOG.md)
- [ ] Add examples if adding new features
- [ ] Verify no compilation warnings: `M-x byte-compile-file`

### Commit Messages

Use clear, descriptive commit messages:

```
Add support for NewTool AI CLI

- Implement org-ai-prompt--adapter-newtool function
- Add detection logic for newtool command
- Update documentation with NewTool examples
- Add test case in example.org
```

Format:
- First line: Brief summary (50 chars or less)
- Blank line
- Detailed description with bullet points

### Pull Request Process

1. **Update CHANGELOG.md** with your changes
2. **Push to your fork:**
   ```bash
   git push origin feature/your-feature-name
   ```
3. **Open a Pull Request** on GitHub
4. **Describe your changes:**
   - What does this PR do?
   - Why is this change needed?
   - How has it been tested?
   - Any breaking changes?

5. **Respond to review feedback**

## Bug Reports

When reporting bugs, please include:

1. **Emacs version:** `M-x emacs-version`
2. **Org version:** `M-x org-version`
3. **Package version:** Check org-ai-prompt.el header
4. **AI CLI and version:** `claude --version` (or equivalent)
5. **Configuration:** Relevant `setq` statements from your init file
6. **Steps to reproduce:**
   - What did you do?
   - What did you expect to happen?
   - What actually happened?
7. **Error messages:** From `*Messages*` buffer
8. **Minimal test case:** Simplest org file that reproduces the issue

### Bug Report Template

```markdown
**Describe the bug**
A clear description of what the bug is.

**To Reproduce**
Steps to reproduce:
1. Open org file with...
2. Add prompt block...
3. Press C-c C-c...
4. See error...

**Expected behavior**
What you expected to happen.

**Environment:**
- Emacs version:
- Org version:
- org-ai-prompt version:
- AI CLI:
- OS:

**Configuration:**
```elisp
(setq org-ai-prompt-command "...")
...
```

**Error messages:**
```
Paste from *Messages* buffer
```

**Additional context**
Any other relevant information.
```

## Feature Requests

When requesting features, please describe:

1. **The problem** - What are you trying to accomplish?
2. **Current workaround** - How do you currently solve this?
3. **Proposed solution** - How would you like this to work?
4. **Alternatives considered** - Other approaches you've thought about
5. **Additional context** - Why is this important to you?

## Questions?

- Open a discussion on GitHub
- Check existing issues and documentation
- Review example.org for usage patterns

## Code of Conduct

- Be respectful and inclusive
- Provide constructive feedback
- Focus on the code, not the person
- Help others learn and grow

## Recognition

Contributors will be:
- Listed in CHANGELOG.md for their contributions
- Mentioned in release notes
- Credited in the documentation

Thank you for contributing to org-ai-prompt-mode! 🎉
