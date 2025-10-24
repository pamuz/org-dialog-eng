# org-ai-prompt.el

Simple Claude integration for Org mode - send prompts to Claude directly from your org files.

## What it does

- Create a `#+BEGIN_PROMPT` block with a question
- Press `C-c C-c` inside the block
- Get Claude's response in a `#+BEGIN_RESPONSE` block below

That's it!

## Installation

### Quick Setup

1. Add to your Emacs init file:

```elisp
;; Add to load path
(add-to-list 'load-path "/Users/pmunoz/my/projects/org-dialog-eng")

;; Load and enable
(require 'org-ai-prompt)
(add-hook 'org-mode-hook #'org-ai-prompt-mode)
```

2. Restart Emacs or eval the configuration

### Prerequisites

You need the [Claude CLI](https://claude.com/code) installed:

```bash
# Test if it's installed
which claude

# Test if it works
claude -p "Hello"
```

## Usage

### Basic Example

In any org file:

```org
* My Notes

#+BEGIN_PROMPT
What is the capital of France?
#+END_PROMPT
```

Position your cursor anywhere inside the PROMPT block and press `C-c C-c`.

The response appears automatically:

```org
#+BEGIN_PROMPT
What is the capital of France?
#+END_PROMPT

#+BEGIN_RESPONSE
The capital of France is Paris.
#+END_RESPONSE
```

### Real Example

```org
* Python Help

I'm writing this function:

#+BEGIN_SRC python
def greet(name):
    print(f"Hello {name}")
#+END_SRC

#+BEGIN_PROMPT
How can I add type hints to this function?
#+END_PROMPT

[Press C-c C-c here, response appears below]
```

## Configuration

### Default (uses `claude` from PATH)

```elisp
(setq org-ai-prompt-command "claude")
```

### Custom Path

If Claude is installed in a custom location:

```elisp
(setq org-ai-prompt-executable-path "/usr/local/bin/claude")
```

## How It Works

When you press `C-c C-c` in a PROMPT block:

1. Extracts the text from the PROMPT block
2. Runs: `claude -p "your prompt text"`
3. Waits for response (async, Emacs stays responsive)
4. Inserts response in a new RESPONSE block below

## Troubleshooting

### "C-c C-c cannot do anything useful here"

- Make sure org-ai-prompt-mode is enabled (you should see " AI" in the mode line)
- Try `M-x org-ai-prompt-mode` to toggle it on

### "Failed to start Claude CLI"

- Verify Claude is installed: `which claude`
- Test manually: `claude -p "test"`
- Check your path configuration

### No response appears

- Check the `*Messages*` buffer for errors (`C-h e`)
- Make sure Claude CLI is working
- Check for any error output

## Tips

### Check if mode is active

Look for " AI" in your mode line at the bottom of Emacs.

### Manual activation

If not enabled automatically:
```
M-x org-ai-prompt-mode
```

### Test installation

Create a simple test file:

```org
#+BEGIN_PROMPT
Say hello
#+END_PROMPT
```

Press `C-c C-c` and see if you get a response.

## FAQ

**Q: Can I use other AI tools besides Claude?**
A: Not in this version - it's intentionally simple and Claude-only. Fork it if you want to add others!

**Q: Does it send my whole document to Claude?**
A: No! It only sends the content of the specific PROMPT block you execute.

**Q: Can I have multiple prompts in one file?**
A: Yes! Each PROMPT block is independent.

**Q: Will it work with Claude Code or Claude CLI?**
A: It uses the Claude CLI (`claude` command) with the `-p` flag.

## Examples

See [example.org](example.org) for more usage examples.

## License

GPL-3.0

## Credits

Built with Emacs, Org mode, and Claude.
