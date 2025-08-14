#!/usr/bin/env bash
# Ultra-thin MCP proxy - just pipes stdio to Emacs TCP server
# Gets the port from Emacs via emacsclient to ensure coordination

# Get port from Emacs
PORT=$(emacsclient --eval 'emacs-mcp-port' | tr -d '"')

exec nc 127.0.0.1 "$PORT"
