#!/bin/bash
# Test runner for exec sandbox tests

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=================================================="
echo "  Exec Sandbox Test Suite"
echo "=================================================="
echo ""

# Check if running as root
if [ "$EUID" -ne 0 ]; then
    echo -e "${YELLOW}WARNING: Not running as root${NC}"
    echo "Most exec tests require root privileges and will be skipped."
    echo "Run with: sudo -E $0"
    echo ""
fi

# Check for Odin compiler
if ! command -v odin &> /dev/null; then
    echo -e "${RED}ERROR: Odin compiler not found${NC}"
    echo "Please install Odin from https://odin-lang.org"
    exit 1
fi

echo "Running integration tests..."
echo ""

# Run integration tests (these don't require the full exec implementation)
if odin test integration_test.odin -file -out:integration_test.bin; then
    echo -e "${GREEN}✓ Integration tests passed${NC}"
    rm -f integration_test.bin
else
    echo -e "${RED}✗ Integration tests failed${NC}"
    exit 1
fi

echo ""
echo "=================================================="
echo "  Test Summary"
echo "=================================================="
echo ""
echo -e "${GREEN}All tests passed!${NC}"
echo ""
echo "Note: Unit tests in exec_test.odin require linking against"
echo "      the full squashd implementation and are skipped here."
echo "      Run them with: odin test exec_test.odin -file"
echo ""
