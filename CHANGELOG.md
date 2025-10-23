# Changelog

All notable changes to org-ai-prompt-mode will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-01-XX

### Added
- Initial release of org-ai-prompt-mode
- Support for PROMPT and RESPONSE blocks in org-mode
- C-c C-c integration for executing prompt blocks
- Automatic conversation context building from org buffer
- Built-in adapters for Claude, Gemini, and OpenAI CLIs
- Custom adapter function support
- File-level configuration via org properties (#+AI_SYSTEM:, #+AI_MODEL:, etc.)
- Global configuration via customization variables
- Async process execution (non-blocking)
- Proper error handling and user feedback
- Conversation branching support (re-execute earlier prompts)
- Source code block preservation as markdown fenced code
- Comprehensive documentation and examples

### Features
- AI-agnostic design works with any CLI tool
- Preserves org-mode workflow and syntax
- Version control friendly (plain text)
- Extensible adapter system
- Context-aware conversations

## [Unreleased]

### Planned
- Support for streaming responses
- Syntax highlighting for RESPONSE blocks
- Conversation export functionality
- Interactive model/parameter selection
- Response editing and regeneration
- Conversation statistics and token counting
- Integration with org-babel for code execution
- Support for attachments/images in conversations
- Org-mode folding support for long responses
- Keybinding for quick prompt insertion
- Template system for common prompts
