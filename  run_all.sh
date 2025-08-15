#!/usr/bin/env bash
set -euo pipefail

# 1) Recompile (if needed)
iverilog -g2012 -o tb_strict.out tb_mips32_pipeline_strict.v mips32_pipeline.v

# Optional: allow overriding finish condition via environment vars
EXTRA_ARGS=()
if [[ -n "${FINISH_ADDR:-}" ]]; then EXTRA_ARGS+=(+FINISH_ADDR="$FINISH_ADDR"); fi
if [[ -n "${FINISH_DATA:-}" ]]; then EXTRA_ARGS+=(+FINISH_DATA="$FINISH_DATA"); fi

# 2) Test list
tests=(
  "alu_basic.dat"
  "hazard_forward.dat"
  "load_use_stall.dat"
  "muldiv_hi_lo.dat"
  "div_hi_add.dat"
  "jr_indirect.dat"
  "branch_mix.dat"
  "zero_write_guard.dat"
  "jal_link_return.dat"
  "lui_andi_mix.dat"
  "branch_mispredict_stress.dat"
  "mem_sw_lw_use.dat"
  "slt_signed.dat"
  "sra_sign_arith.dat"
  "flush_store_guard_taken.dat"
)

pass=0; fail=0
printf "%-28s  %-6s  %-8s  %-8s  %-10s\n" "TEST" "RES" "CYCLES" "INST" "BR_PRED"

for t in "${tests[@]}"; do
  out=$(vvp tb_strict.out "${EXTRA_ARGS[@]}" +HEX="$t" 2>&1 || true)
  if grep -q "SUCCESS: Reached finish condition" <<<"$out"; then
    cyc=$(sed -nE 's~.*Total cycles:[[:space:]]+([0-9]+)~\1~p' <<<"$out")
    ins=$(sed -nE 's~.*Instructions completed:[[:space:]]+([0-9]+)~\1~p' <<<"$out")
    brc=$(sed -nE 's~.*Branch predictions:[[:space:]]+([0-9]+)[[:space:]]*/[[:space:]]*([0-9]+)~\1 / \2~p' <<<"$out")
    printf "%-28s  %-6s  %-8s  %-8s  %-10s\n" "$t" "PASS" "${cyc:-?}" "${ins:-?}" "${brc:-?-?}"
    pass=$((pass+1))
  else
    printf "%-28s  %-6s\n" "$t" "FAIL"
    echo "$out" | tail -n 5
    fail=$((fail+1))
  fi
done

echo "-----------------------------------------------"
echo "POSITIVE TOTAL: PASS=$pass  FAIL=$fail"

# 3) Negative tests (expecting ERROR from TB)
neg_tests=(
  "misaligned_store_err.dat"
  "misaligned_load_err.dat"
  "imem_overrun_err.dat"
  "dmem_overrun_store_err.dat"
  "dmem_overrun_load_err.dat"
)

echo "NEGATIVE TESTS (expecting ERROR from TB)"
neg_unexpected_pass=0
for t in "${neg_tests[@]}"; do
  out=$(vvp tb_strict.out "${EXTRA_ARGS[@]}" +HEX="$t" 2>&1 || true)
  if grep -q "ERROR:" <<<"$out"; then
    printf "  [%-28s] EXPECTED-FAIL ✓\n" "$t"
  else
    printf "  [%-28s] UNEXPECTED-PASS ✗\n" "$t"
    echo "$out" | tail -n 5
    neg_unexpected_pass=$((neg_unexpected_pass+1))
  fi
done
neg_expected_fail=$(( ${#neg_tests[@]} - neg_unexpected_pass ))

echo "-----------------------------------------------"
echo "NEGATIVE TOTAL: EXPECTED-FAIL=$neg_expected_fail  UNEXPECTED-PASS=$neg_unexpected_pass"

echo "-----------------------------------------------"
if [ "$fail" -eq 0 ] && [ "$neg_unexpected_pass" -eq 0 ]; then
  echo "ALL SUITES PASSED"
  exit 0
else
  echo "SOME TESTS FAILED"
  exit 1
fi
