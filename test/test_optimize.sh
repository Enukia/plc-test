#!/bin/bash
# test_optimize.sh - 优化器专项验证
# 覆盖三种优化：常量折叠 / 死代码消除 / 尾递归优化

set +e

cd "$(dirname "$0")/.." || exit 1
COMPILER="$(pwd)/_build/default/bin/main.exe"
if command -v dune >/dev/null 2>&1; then
    DUNE_CMD=(dune)
elif command -v opam >/dev/null 2>&1; then
    DUNE_CMD=(opam exec -- dune)
else
    echo "Error: dune not found. Run this script inside an opam shell, or install dune." >&2
    exit 1
fi

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

PASS=0
FAIL=0
TOTAL=0

echo "Building project..."
"${DUNE_CMD[@]}" build || exit 1

check() {
    local desc="$1" code="$2" must="$3" mustnot="$4"
    TOTAL=$((TOTAL + 1))
    local out
    out=$(printf '%s' "$code" | "$COMPILER" -opt 2>&1)
    if echo "$out" | grep -q "$must" && ! echo "$out" | grep -q "$mustnot"; then
        echo -e "${GREEN}PASS${NC}: $desc"
        PASS=$((PASS + 1))
    else
        echo -e "${RED}FAIL${NC}: $desc"
        echo "$out" | sed 's/^/    /'
        FAIL=$((FAIL + 1))
    fi
}

echo ""
echo "========== CONSTANT FOLDING =========="

check "1+2*3 folded to li a0, 7" \
    'int main() { return 1 + 2 * 3; }' \
    'li a0, 7' '\.L_mul_loop'

check "cross-statement folding" \
    'int main() { int x = 2; int y = x * 5; return y - 1; }' \
    'li a0, 9' '\.L_mul_loop'

check "constant branch selects dead path away" \
    'int main() { if (2 > 1) return 10; else return 20; }' \
    'li a0, 10' 'li a0, 20'

check "cross-block equal constants fold after join" \
    'int f(int c) { int x = 0; if (c) { x = 4; } else { x = 4; } return x + 3; } int main() { return f(0); }' \
    'li a0, 7' 'lw a0'

check "loop predecessor constant reaches body return" \
    'int main() { int x = 7; int y = 0; while (y < 1) { return x + 2; } return x + 3; }' \
    'li a0, 9' 'lw a0'

echo ""
echo "========== DEAD CODE ELIMINATION =========="

check "unreachable call after return removed" \
    'void foo() {} int main() { return 1; foo(); }' \
    'li a0, 1' 'call foo'

check "dead local chain removed" \
    'int main() { int a = 1; int b = 2; int c = a + b; return a; }' \
    'li a0, 1' '\.L_mul_loop'

check "while(0) body removed" \
    'void foo() {} int main() { while (0) { foo(); } return 0; }' \
    'li a0, 0' 'call foo'

echo ""
echo "========== COPY PROPAGATION =========="

check "parameter copy chain collapsed" \
    'int id(int x) { int a = x; int b = a; return b; } int main() { return id(7); }' \
    'lw a0, -12(fp)' '\-16(fp)'

echo ""
echo "========== COMMON SUBEXPRESSION =========="

TOTAL=$((TOTAL + 1))
cse_out=$(printf '%s' \
    'int f(int a, int b) { int x = a * b; int y = a * b; return x + y; } int main() { return f(6, 7); }' \
    | "$COMPILER" -opt 2>&1)
f_body=$(printf '%s' "$cse_out" | awk '/^f:/,/^\.L_epilogue_f:/')
mul_count=$(printf '%s' "$f_body" | grep -c '^\.L_mul_loop')
if [ "$mul_count" -eq 1 ]; then
    echo -e "${GREEN}PASS${NC}: repeated multiply computed once"
    PASS=$((PASS + 1))
else
    echo -e "${RED}FAIL${NC}: repeated multiply computed once"
    echo "$f_body" | sed 's/^/    /'
    FAIL=$((FAIL + 1))
fi

echo ""
echo "========== ALGEBRAIC SIMPLIFICATION =========="

check "x*2 and x*0 avoid multiply helper" \
    'int f(int x) { return x * 2 + x * 0; } int main() { return f(9); }' \
    'add t0, t0, t1' '\.L_mul_loop'

echo ""
echo "========== LOOP OPTIMIZATION =========="

check "loop bound constant and x*2 simplified" \
    'int main() { int n = 20; int i = 0; int s = 0; while (i < n) { s = s + i * 2; i = i + 1; } return s; }' \
    'li t1, 20' '\.L_mul_loop'

echo ""
echo "========== BASIC COMBINED =========="

check "combined const copy cse algebra dead code" \
    'int f(int a, int b) { int scale = 4; int x = a * scale; int y = a * scale; int dead = b * b; return x + y + 0; } int main() { return f(3, 5); }' \
    'slli' '\.L_mul_loop'

echo ""
echo "========== ADVANCED GRAPH =========="

check "graph-style repeated global stride" \
    'const int N = 8; int edge_score(int u, int v) { int a = u * N + v; int b = u * N + v; return a == b; } int main() { return edge_score(2, 3); }' \
    'li a0, 1' '\.L_mul_loop'

echo ""
echo "========== ADVANCED MATRIX =========="

check "matrix-style row-major index" \
    'const int COLS = 16; int idx(int r, int c) { int a = r * COLS + c; int b = r * COLS + c; return a + b; } int main() { return idx(2, 3); }' \
    'slli' '\.L_mul_loop'

echo ""
echo "========== GLOBAL CONST PROP =========="

check "global const loaded as immediate" \
    'const int LIMIT = 500; int main() { return LIMIT + 1; }' \
    'li a0, 501' 'la .*LIMIT'

echo ""
echo "========== CONST EXPR CHAIN =========="

check "global const expression chain collapsed" \
    'const int A = 2; const int B = A * 3; const int C = B + 4; const int D = C * C; int main() { return D; }' \
    'li a0, 100' '\.L_mul_loop'

echo ""
echo "========== TAIL RECURSION =========="

TOTAL=$((TOTAL + 1))
tail_out=$(printf '%s' \
    'int fact(int n, int acc) { if (n <= 0) return acc; return fact(n - 1, acc * n); } int main() { return fact(5, 1); }' \
    | "$COMPILER" -opt 2>&1)
fact_body=$(printf '%s' "$tail_out" | awk '/^fact:/,/^\.L_epilogue_fact:/')
if ! echo "$fact_body" | grep -q 'call fact' && echo "$fact_body" | grep -q 'j L'; then
    echo -e "${GREEN}PASS${NC}: recursive call replaced by loop"
    PASS=$((PASS + 1))
else
    echo -e "${RED}FAIL${NC}: recursive call replaced by loop"
    echo "$fact_body" | sed 's/^/    /'
    FAIL=$((FAIL + 1))
fi

TOTAL=$((TOTAL + 1))
fib_out=$(printf '%s' \
    'int fib(int n) { if (n <= 1) return n; return fib(n - 1) + fib(n - 2); } int main() { return fib(10); }' \
    | "$COMPILER" -opt 2>&1)
if echo "$fib_out" | grep -q 'call fib'; then
    echo -e "${GREEN}PASS${NC}: non-tail recursion keeps calls"
    PASS=$((PASS + 1))
else
    echo -e "${RED}FAIL${NC}: non-tail recursion keeps calls"
    FAIL=$((FAIL + 1))
fi

echo ""
echo "Passed: $PASS / $TOTAL"
echo "Failed: $FAIL / $TOTAL"
