#!/usr/bin/env bash
# Run squashd tests in a privileged container

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Building test container...${NC}"
docker build -f Dockerfile.test -t squashd-test:latest .

echo -e "${YELLOW}Running tests with CAP_SYS_ADMIN...${NC}"
docker run --rm \
    --privileged \
    -v "$(pwd)":/app \
    -w /app \
    squashd-test:latest

exit_code=$?

if [ $exit_code -eq 0 ]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
else
    echo -e "${RED}✗ Some tests failed (exit code: $exit_code)${NC}"
    exit $exit_code
fi
