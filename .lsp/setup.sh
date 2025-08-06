#!/bin/bash
# Prolog LSP Setup Script

echo "Setting up Prolog LSP for multiple IDEs..."

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "Error: Node.js is required but not installed."
    echo "Please install Node.js from https://nodejs.org/"
    exit 1
fi

# Check if SWI-Prolog is installed
if ! command -v swipl &> /dev/null; then
    echo "Warning: SWI-Prolog not found in PATH."
    echo "Please install SWI-Prolog from https://www.swi-prolog.org/"
fi

echo "Available IDE configurations:"
for config in *.json; do
    if [ -f "$config" ]; then
        ide_name=$(basename "$config" .json)
        echo "  - $ide_name"
    fi
done

echo ""
echo "To use with your IDE:"
echo "1. Copy the appropriate configuration file"
echo "2. Follow the setup instructions in the documentation"
echo "3. Restart your IDE"

echo "Setup complete!"
