#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
GRAY='\033[1;30m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Map of example to expected exit code
declare -A examples=(
  ["examples/example.in"]=0
  ["examples/fib.in"]=0
  ["examples/group.in"]=0
  ["examples/logical.in"]=0
  ["examples/pointer.in"]=0
  ["examples/simple.in"]=3
  ["examples/rule110a.in"]=0
  ["examples/rule110b.in"]=0
  ["examples/fizzbuzz.in"]=0
  ["examples/anytype.in"]=0
  ["examples/varargs.in"]=0
  ["examples/address_of.in"]=0
)

# Warn if any file in examples/ is not included in the examples map
missing=()
for file in examples/*.in; do
  if [[ ! -v examples[$file] ]]; then
    missing+=("$file")
  fi
done

if [ ${#missing[@]} -ne 0 ]; then
  echo -e "${YELLOW}The following example files are not included in the tests:${NC}"
  for file in "${missing[@]}"; do
    echo -e "- $file"
  done
  echo
fi

# Function to print and execute a command
run_cmd() {
  echo -e "${GRAY}$*${NC}"
  "$@"
}

echo -e "${BOLD}${CYAN}Building project...${NC}"
mkdir -p bin
run_cmd go build -o bin/cubit ./cmd/cubit
if [ $? -ne 0 ]; then
  echo -e "${RED}Build failed!${NC}"
  exit 1
fi

echo -e "${GREEN}Build succeeded.${NC}\n"

all_passed=true
declare -A results=()

# Run go test and store result
echo -e "${BOLD}${CYAN}Running go test...${NC}"
run_cmd go tool gotestsum --format pkgname-and-test-fails --format-icons hivis ./...
go_test_exit_code=$?
if [ $go_test_exit_code -eq 0 ]; then
  results["go test"]="${GREEN}✓${NC}"
else
  all_passed=false
  results["go test"]="${RED}✗${NC}"
fi

# Iterate over the examples, run and check the exit code
echo -e "\n${BOLD}${CYAN}Running example tests...${NC}\n"

for example in "${!examples[@]}"; do
  expected_exit_code=${examples[$example]}
  echo -e "${YELLOW}==> Running $example...${NC}"

  # Run the command with the example input
  run_cmd ./bin/cubit -ast -ssa -run "$example"
  actual_exit_code=$?

  # Check the exit code
  if [ $actual_exit_code -ne $expected_exit_code ]; then
    echo -e "${RED}Test failed for $example: expected exit code $expected_exit_code, got $actual_exit_code${NC}\n"
    all_passed=false
    results[$example]="${RED}✗${NC}"
  else
    echo -e "${GREEN}Test passed for $example${NC}\n"
    results[$example]="${GREEN}✓${NC}"
  fi
done

# Print summary table
echo -e "${BOLD}${CYAN}Test Summary:${NC}"
printf "%-30s %s\n" "Test" "Result"
printf "%-30s %s\n" "------------------------------" "------"
for example in "${!results[@]}"; do
  printf "%-30s %b\n" "$example" "${results[$example]}"
done

if [ "$all_passed" = true ]; then
  echo -e "\n${BOLD}${GREEN}All tests passed!${NC}"
  exit 0
else
  echo -e "\n${BOLD}${RED}Some tests failed.${NC}"
  exit 1
fi
