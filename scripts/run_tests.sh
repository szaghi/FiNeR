#!/usr/bin/env bash
# Run the FiNeR test suite.
# Each finer_test_* binary exits 0 on success, 1 on failure.

if [[ -t 1 ]]; then
  RED=$'\033[0;31m'; GREEN=$'\033[0;32m'; BOLD=$'\033[1m'; RESET=$'\033[0m'
else
  RED=''; GREEN=''; BOLD=''; RESET=''
fi

pass=0; fail=0
tmpout=$(mktemp)
trap 'rm -f "$tmpout"' EXIT

for exe in exe/finer_test_*; do
  [[ -x "$exe" ]] || continue
  name=$(basename "$exe")
  if "$exe" > "$tmpout" 2>&1; then
    printf "  ${GREEN}PASS${RESET}  %s\n" "$name"
    pass=$((pass + 1))
  else
    printf "  ${RED}FAIL${RESET}  %s\n" "$name"
    sed 's/^/       /' "$tmpout"
    fail=$((fail + 1))
  fi
done

total=$((pass + fail))
printf "\n${BOLD}%d/%d passed${RESET}\n" "$pass" "$total"
exit $fail
