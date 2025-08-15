#!/usr/bin/env bash
set -euo pipefail

# A) Unaligned Store (ERROR: Unaligned store)
cat > misaligned_store_err.dat <<'EOF'
340A000D  // ori  $t2,$zero,13
AC0A0002  // sw   $t2,2($zero)   ; 2-byte boundary -> 4B alignment violation
08000002  // j    2
EOF

# B) Unaligned Load (ERROR: Unaligned load)
cat > misaligned_load_err.dat <<'EOF'
8C090002  // lw   $t1,2($zero)   ; 2-byte boundary -> 4B alignment violation
08000001  // j    1
EOF

# C) IMEM out-of-range fetch (ERROR: Instruction fetch out of ROM range)
#   j 300 (index=300; ROM is 0..255 words, so out of range)
cat > imem_overrun_err.dat <<'EOF'
0800012C  // j 300
00000000  // nop
EOF

# D) DMEM out-of-range Store (ERROR: Store address out of range)
#   0x400 = index 256 (>255)
cat > dmem_overrun_store_err.dat <<'EOF'
340A000D  // ori  $t2,$zero,13
AC0A0400  // sw   $t2,0x400($zero)
08000002  // j    2
EOF

# E) DMEM out-of-range Load (ERROR: Load address out of range)
cat > dmem_overrun_load_err.dat <<'EOF'
8C090400  // lw   $t1,0x400($zero)
08000001  // j    1
EOF

echo "Done. Generated 5 negative HEX files."
