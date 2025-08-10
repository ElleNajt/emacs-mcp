#!/bin/bash

# Install Emacs MCP Server in Claude Code CLI

set -e

# Get the absolute path of the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Installing Emacs MCP Server in Claude Code CLI..."
claude mcp add emacs "$SCRIPT_DIR/mcp-proxy.sh"

echo "✅ Done! Now add init.el snippet to your Emacs configuration."