# Installation Guide

This guide covers different methods to install org-ai-prompt-mode.

## Prerequisites

1. **Emacs 27.1 or later**
2. **Org mode 9.0 or later** (usually included with Emacs)
3. **An AI CLI tool** (Claude, Gemini, OpenAI, or custom)

### Installing an AI CLI

#### Claude
```bash
# Install Claude Code CLI
# Visit https://claude.com/code for instructions
```

#### Gemini
```bash
# Install Google's Gemini CLI
# Visit Google AI documentation for installation
```

#### OpenAI
```bash
# Install OpenAI CLI
pip install openai
# or use your preferred OpenAI CLI tool
```

## Installation Methods

### Method 1: Manual Installation

1. **Download the package:**
   ```bash
   git clone https://github.com/yourusername/org-ai-prompt.git ~/.emacs.d/site-lisp/org-ai-prompt
   ```

2. **Add to your init file** (`~/.emacs.d/init.el` or `~/.emacs`):
   ```elisp
   ;; Add to load path
   (add-to-list 'load-path "~/.emacs.d/site-lisp/org-ai-prompt")

   ;; Load the package
   (require 'org-ai-prompt)

   ;; Enable for all org files
   (add-hook 'org-mode-hook #'org-ai-prompt-mode)

   ;; Configure (optional)
   (setq org-ai-prompt-command "claude")
   (setq org-ai-prompt-default-model "claude-sonnet-4")
   ```

3. **Restart Emacs** or evaluate the configuration:
   ```
   M-x eval-buffer
   ```

### Method 2: Using use-package

If you use `use-package` for configuration:

```elisp
(use-package org-ai-prompt
  :load-path "~/.emacs.d/site-lisp/org-ai-prompt"
  :hook (org-mode . org-ai-prompt-mode)
  :custom
  (org-ai-prompt-command "claude")
  (org-ai-prompt-default-model "claude-sonnet-4")
  (org-ai-prompt-default-temperature 0.7))
```

### Method 3: Using straight.el

If you use `straight.el`:

```elisp
(straight-use-package
 '(org-ai-prompt :type git
                 :host github
                 :repo "yourusername/org-ai-prompt"))

(use-package org-ai-prompt
  :hook (org-mode . org-ai-prompt-mode)
  :custom
  (org-ai-prompt-command "claude"))
```

### Method 4: Using Doom Emacs

Add to `~/.doom.d/packages.el`:

```elisp
(package! org-ai-prompt
  :recipe (:host github
           :repo "yourusername/org-ai-prompt"))
```

Add to `~/.doom.d/config.el`:

```elisp
(use-package! org-ai-prompt
  :hook (org-mode . org-ai-prompt-mode)
  :config
  (setq org-ai-prompt-command "claude"))
```

Run:
```bash
doom sync
```

### Method 5: Using Spacemacs

Add to `~/.spacemacs` in `dotspacemacs-additional-packages`:

```elisp
(org-ai-prompt :location (recipe
                          :fetcher github
                          :repo "yourusername/org-ai-prompt"))
```

Add to `dotspacemacs/user-config`:

```elisp
(use-package org-ai-prompt
  :hook (org-mode . org-ai-prompt-mode)
  :config
  (setq org-ai-prompt-command "claude"))
```

## Configuration

### Basic Configuration

```elisp
;; Set your preferred AI command
(setq org-ai-prompt-command "claude")

;; Set default model
(setq org-ai-prompt-default-model "claude-sonnet-4")

;; Set default temperature
(setq org-ai-prompt-default-temperature 0.7)

;; Set default max tokens
(setq org-ai-prompt-default-max-tokens 2000)
```

### Advanced Configuration

```elisp
;; Use custom executable path
(setq org-ai-prompt-executable-path "/usr/local/bin/claude")

;; Use custom adapter function
(defun my-ai-adapter (messages system-prompt model temp max-tokens)
  ;; Custom implementation
  )

(setq org-ai-prompt-args-function 'my-ai-adapter)
```

### Per-Project Configuration

Use `.dir-locals.el` in your project directory:

```elisp
((org-mode . ((org-ai-prompt-command . "gemini")
              (org-ai-prompt-default-model . "gemini-pro"))))
```

## Verification

1. **Test the installation:**
   ```
   M-x org-ai-prompt-mode
   ```
   You should see " AI" in the mode line.

2. **Run the test file:**
   ```bash
   emacs test.org
   ```
   Follow the instructions in the file to verify everything works.

3. **Check configuration:**
   ```
   M-x describe-variable RET org-ai-prompt-command
   M-x describe-variable RET org-ai-prompt-mode
   ```

## Troubleshooting

### "Cannot find org-ai-prompt"

- Verify the package is in your load path
- Check the path in `add-to-list 'load-path` is correct
- Try `M-x locate-library RET org-ai-prompt`

### "AI command not found"

- Verify your AI CLI is installed: `which claude`
- Set the full path: `(setq org-ai-prompt-executable-path "/full/path/to/claude")`
- Check your `$PATH` includes the CLI location

### Mode not activating

- Check for errors: `M-x toggle-debug-on-error`
- Verify org-mode is active: `M-x org-mode`
- Manually activate: `M-x org-ai-prompt-mode`

### Response not appearing

- Check `*Messages*` buffer for errors
- Test AI CLI manually in terminal
- Enable debug mode and check backtrace

## Uninstallation

1. **Remove from init file**
2. **Delete the package directory:**
   ```bash
   rm -rf ~/.emacs.d/site-lisp/org-ai-prompt
   ```
3. **Restart Emacs**

## Updating

```bash
cd ~/.emacs.d/site-lisp/org-ai-prompt
git pull origin main
```

Restart Emacs or:
```
M-x eval-buffer (in org-ai-prompt.el)
```

## Getting Help

- Check the README.md for usage documentation
- Review example.org for usage examples
- Open an issue on GitHub
- Check the `*Messages*` buffer for errors

## Next Steps

After installation:

1. Read the [README.md](README.md) for usage instructions
2. Try the [test.org](test.org) file
3. Explore [example.org](example.org) for comprehensive examples
4. Configure your preferred AI tool and model
5. Start your first Dialog Engineering session!
