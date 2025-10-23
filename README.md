# org-ai-prompt.el

An Emacs minor mode for integrating AI prompts into Org mode documents, enabling "Dialog Engineering" workflows.

## Overview

`org-ai-prompt-mode` allows you to have interactive conversations with AI tools directly within your org-mode documents. It adds support for `PROMPT` and `RESPONSE` blocks that work seamlessly with your existing org-mode workflow.

### Key Features

- 🤖 **AI-agnostic**: Works with Claude, Gemini, OpenAI, or any custom AI CLI
- 📝 **Native org-mode integration**: Uses familiar block syntax and `C-c C-c` execution
- 🔄 **Conversation context**: Automatically builds conversation history from your entire document
- ⚡ **Async execution**: Non-blocking AI requests
- 🎨 **Configurable**: Customize per-file or globally
- 🌳 **Branching conversations**: Re-execute earlier prompts to explore different paths

## Installation

### Manual Installation

1. Download `org-ai-prompt.el` to your Emacs load path
2. Add to your init file:

```elisp
(require 'org-ai-prompt)

;; Enable globally for all org files
(add-hook 'org-mode-hook #'org-ai-prompt-mode)

;; Or enable on-demand with M-x org-ai-prompt-mode
```

### Using use-package

```elisp
(use-package org-ai-prompt
  :load-path "/path/to/org-ai-prompt"
  :hook (org-mode . org-ai-prompt-mode))
```

## Prerequisites

You need an AI CLI tool installed and accessible. Supported tools:

- **Claude**: [Claude Code CLI](https://claude.com/code) - `claude` command
- **Gemini**: Google's Gemini CLI
- **OpenAI**: OpenAI CLI or compatible tool
- **Custom**: Any CLI that accepts text prompts

## Basic Usage

### 1. Create an org document

```org
#+TITLE: My AI Conversation
#+AI_SYSTEM: You are a helpful programming assistant.
#+AI_MODEL: claude-sonnet-4

* My Question

I have a Python function:

#+BEGIN_SRC python
def add(a, b):
    return a + b
#+END_SRC

#+BEGIN_PROMPT
Can you add type hints to this function?
#+END_PROMPT
```

### 2. Execute the prompt

1. Position your cursor anywhere in the `#+BEGIN_PROMPT` block
2. Press `C-c C-c`
3. Wait for the response to appear

### 3. Response appears automatically

```org
#+BEGIN_RESPONSE
Here's the function with type hints:

```python
def add(a: int, b: int) -> int:
    return a + b
```

For more flexibility, you could also use:
```python
from typing import Union

def add(a: Union[int, float], b: Union[int, float]) -> Union[int, float]:
    return a + b
```
#+END_RESPONSE
```

## Configuration

### File-Level Configuration

Configure AI behavior per-file using org properties at the top of your document:

```org
#+AI_SYSTEM: You are a helpful assistant who provides concise answers.
#+AI_MODEL: claude-sonnet-4
#+AI_TEMPERATURE: 0.7
#+AI_MAX_TOKENS: 2000
#+AI_COMMAND: claude
```

| Property | Description | Example |
|----------|-------------|---------|
| `#+AI_SYSTEM:` | System prompt to define AI behavior | `You are a helpful coding assistant` |
| `#+AI_MODEL:` | Model name to use | `claude-sonnet-4`, `gpt-4`, `gemini-pro` |
| `#+AI_TEMPERATURE:` | Creativity level (0.0-1.0) | `0.7` |
| `#+AI_MAX_TOKENS:` | Maximum response length | `2000` |
| `#+AI_COMMAND:` | AI CLI command override | `claude`, `gemini`, `/path/to/custom-ai` |

### Global Configuration

Configure defaults in your Emacs init file:

```elisp
;; Set default AI command
(setq org-ai-prompt-command "claude")

;; Or use a custom path
(setq org-ai-prompt-command "gemini")
(setq org-ai-prompt-executable-path "/usr/local/bin/gemini")

;; Set default model
(setq org-ai-prompt-default-model "claude-sonnet-4")

;; Set default temperature
(setq org-ai-prompt-default-temperature 0.7)

;; Set default max tokens
(setq org-ai-prompt-default-max-tokens 2000)
```

### Custom AI CLI Adapter

If your AI CLI has a different argument format, create a custom adapter:

```elisp
(defun my-custom-ai-adapter (messages system-prompt model temperature max-tokens)
  "Custom adapter for my-ai-tool CLI."
  (let ((args '()))
    ;; Build command arguments based on your CLI's format
    (when model
      (setq args (append args (list "--model" model))))
    (when temperature
      (setq args (append args (list "--temp" (number-to-string temperature)))))

    ;; Format conversation
    (let ((prompt (mapconcat
                   (lambda (msg)
                     (format "%s: %s"
                            (plist-get msg :role)
                            (plist-get msg :content)))
                   messages
                   "\n\n")))
      (append args (list prompt)))))

;; Use your custom adapter
(setq org-ai-prompt-args-function 'my-custom-ai-adapter)
```

## How It Works

### Conversation Context Building

When you execute a prompt block with `C-c C-c`, the mode:

1. **Parses the buffer** from beginning to the current prompt
2. **Categorizes content**:
   - **User messages**: Regular org content, headings, lists, code blocks, and PROMPT blocks
   - **Assistant messages**: RESPONSE blocks
3. **Concatenates** consecutive non-response content into single user messages
4. **Preserves** source code blocks as markdown fenced code blocks
5. **Sends** the full conversation history to the AI CLI

### Example Conversation Flow

```org
* Starting Point

I'm learning Python.

#+BEGIN_SRC python
print("Hello")
#+END_SRC

#+BEGIN_PROMPT
How can I make this more interesting?
#+END_PROMPT
```

**Sent to AI as:**
```
[USER]: * Starting Point

I'm learning Python.

```python
print("Hello")
```

How can I make this more interesting?
```

**AI responds, response inserted:**
```org
#+BEGIN_RESPONSE
You could add user input:

```python
name = input("What's your name? ")
print(f"Hello, {name}!")
```
#+END_RESPONSE
```

**Continue the conversation:**
```org
#+BEGIN_PROMPT
Can you add error handling?
#+END_PROMPT
```

**Sent to AI as:**
```
[USER]: * Starting Point

I'm learning Python.

```python
print("Hello")
```

How can I make this more interesting?

[ASSISTANT]: You could add user input:

```python
name = input("What's your name? ")
print(f"Hello, {name}!")
```

[USER]: Can you add error handling?
```

## Advanced Features

### Branching Conversations

Re-execute any earlier prompt to explore different responses:

```org
#+BEGIN_PROMPT
Suggest a data structure for storing user profiles.
#+END_PROMPT

#+BEGIN_RESPONSE
I'd suggest using a dictionary...
#+END_RESPONSE

* Alternative Approach

Let me try asking differently...

#+BEGIN_PROMPT
Suggest a data structure for storing user profiles.
[Moved cursor here and pressed C-c C-c again]
#+END_PROMPT
```

The second execution will only include conversation history up to that point, allowing you to branch the conversation.

### Multiple AI Tools

Use different AI tools in different files:

**research.org:**
```org
#+AI_COMMAND: claude
#+AI_MODEL: claude-sonnet-4
```

**creative-writing.org:**
```org
#+AI_COMMAND: gemini
#+AI_MODEL: gemini-pro
```

### Debugging

If something goes wrong:

1. Check the `*Messages*` buffer for org-ai-prompt messages
2. Verify your AI CLI is installed: `which claude`
3. Test manually: `claude "Hello"`
4. Check file properties are correctly formatted (no extra spaces)

## Built-in Adapters

### Claude Adapter

```elisp
(setq org-ai-prompt-command "claude")
```

Command format:
```bash
claude --system "SYSTEM" --model "MODEL" --temperature 0.7 "CONVERSATION"
```

### Gemini Adapter

```elisp
(setq org-ai-prompt-command "gemini")
```

Command format:
```bash
gemini --model "MODEL" --temperature 0.7 --max-output-tokens 2000 "CONVERSATION"
```

### OpenAI Adapter

```elisp
(setq org-ai-prompt-command "openai")
```

Command format:
```bash
openai --model "MODEL" --temperature 0.7 --max-tokens 2000 --system "SYSTEM" "CONVERSATION"
```

## Examples

See [example.org](example.org) for a comprehensive demonstration including:

- Basic question-answering
- Code review and improvement
- Multi-turn conversations
- Different programming languages
- Custom adapter examples

## Tips and Best Practices

### 1. Use Descriptive System Prompts

```org
#+AI_SYSTEM: You are an expert Python developer who writes clean, well-documented code following PEP 8 guidelines.
```

### 2. Structure Your Documents

```org
* Problem Statement
[Describe what you're working on]

* Initial Attempt
#+BEGIN_SRC python
[Your code]
#+END_SRC

* Questions
#+BEGIN_PROMPT
[Your question]
#+END_PROMPT

* Refinements
[Notes based on AI response]

* Final Solution
#+BEGIN_SRC python
[Improved code]
#+END_SRC
```

### 3. Keep Conversations Focused

Start a new document or section for unrelated topics to avoid context confusion.

### 4. Version Control

Everything is plain text - commit your `.org` files to git to track your thinking process over time.

### 5. Combine with Org Features

- Use `org-export` to create documentation
- Use `org-babel` to execute code blocks
- Use `org-agenda` to track TODOs from AI conversations

## Troubleshooting

### "No conversation context to send"

Your PROMPT block is at the very beginning with no other content. Add some context before the prompt.

### "Failed to start AI CLI"

- Verify the CLI is installed: `which claude` (or your command)
- Check `org-ai-prompt-executable-path` is correct
- Check `org-ai-prompt-command` matches your CLI command

### Response not appearing

- Check `*Messages*` buffer for errors
- Ensure the AI CLI is working: test it manually in terminal
- Check for trailing spaces in `#+AI_COMMAND:` property

### Wrong adapter being used

Explicitly set the adapter:

```elisp
(setq org-ai-prompt-args-function 'org-ai-prompt--adapter-claude)
```

## Contributing

Contributions welcome! To add support for new AI tools:

1. Create an adapter function following the pattern in existing adapters
2. Add detection logic to `org-ai-prompt--get-adapter`
3. Test with real AI CLI
4. Submit a PR

## License

This package is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3, or (at your option) any later version.

## Acknowledgments

Inspired by the need to combine literate programming with AI-assisted development in a seamless, version-control-friendly way.
