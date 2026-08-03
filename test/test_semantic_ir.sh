#!/bin/bash
# test_semantic_ir.sh — 使用绝对路径直接执行

set +e

PROJECT_DIR="/d/GitHub Project/26-Compiler/pl-c-Project"
cd "$PROJECT_DIR"

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

PASS=0
FAIL=0
TOTAL=0

# 编译器绝对路径
COMPILER="$PROJECT_DIR/_build/default/bin/main.exe"

# 构建
echo "Building project..."
dune build

# 验证
if [ ! -f "$COMPILER" ]; then
    echo "Error: Compiler not found at $COMPILER"
    read -p "Press any key to exit..." -n1 -s
    exit 1
fi

run_test() {
    local desc="$1"
    local code="$2"
    local expect="$3"
    
    TOTAL=$((TOTAL + 1))
    
    echo ""
    echo "========================================"
    echo "Test #$TOTAL: $desc"
    
    local tmpfile=$(mktemp /tmp/toyc_test.XXXXXX.tc)
    echo "$code" > "$tmpfile"
    
    # 直接执行绝对路径
    local output
    output=$("$COMPILER" < "$tmpfile" 2>&1)
    
    rm -f "$tmpfile"
    
    if [ "$expect" = "success" ]; then
        if ! echo "$output" | grep -q "Error:"; then
            echo -e "${GREEN}✓ PASS${NC}"
            PASS=$((PASS + 1))
        else
            echo -e "${RED}✗ FAIL${NC}"
            echo "Output:"
            echo "$output" | sed 's/^/  /'
            FAIL=$((FAIL + 1))
        fi
    else
        if echo "$output" | grep -q "Error:"; then
            echo -e "${GREEN}✓ PASS${NC}"
            PASS=$((PASS + 1))
        else
            echo -e "${RED}✗ FAIL${NC}"
            echo "Output:"
            echo "$output" | sed 's/^/  /'
            FAIL=$((FAIL + 1))
        fi
    fi
}

echo ""
echo "########################################"
echo "#     Semantic & IR Test Suite         #"
echo "########################################"

# ========== 功能测试 ==========
echo ""
echo "========== FUNCTIONAL TESTS =========="

run_test "Basic main with return" \
'int main() { return 42; }' \
"success"

run_test "Variable declaration" \
'int main() { int a = 10; return a; }' \
"success"

run_test "Arithmetic" \
'int main() { int a = 1; int b = 2; return a + b; }' \
"success"

run_test "If-else true" \
'int main() { int a = 1; if (a) return 1; else return 0; }' \
"success"

run_test "If-else false" \
'int main() { int a = 0; if (a) return 1; else return 0; }' \
"success"

run_test "While loop" \
'int main() { int i = 0; while (i < 5) i = i + 1; return i; }' \
"success"

run_test "Break in loop" \
'int main() { int i = 0; while (i < 10) { i = i + 1; if (i == 5) break; } return i; }' \
"success"

run_test "Continue in loop" \
'int main() { int sum = 0; int i = 0; while (i < 5) { i = i + 1; if (i == 3) continue; sum = sum + i; } return sum; }' \
"success"

run_test "Nested blocks" \
'int main() { int a = 1; { int a = 2; } return a; }' \
"success"

run_test "Function call" \
'int add(int a, int b) { return a + b; } int main() { return add(3, 4); }' \
"success"

run_test "Multiple functions" \
'int square(int x) { return x * x; } int main() { int a = 5; return square(a); }' \
"success"

run_test "Void function" \
'void foo() { return; } int main() { foo(); return 0; }' \
"success"

run_test "Const global" \
'const int PI = 3; int main() { int r = 2; return PI * r; }' \
"success"

run_test "Short circuit AND" \
'int main() { int a = 0; if (a && (1/0)) return 1; return 0; }' \
"success"

run_test "Short circuit OR" \
'int main() { int a = 1; if (a || (1/0)) return 1; return 0; }' \
"success"

run_test "Unary operators" \
'int main() { int a = 5; int b = -a; int c = !a; return b + c; }' \
"success"

run_test "Relational operators" \
'int main() { int a = 5; int b = 3; if (a > b) return 1; if (a < b) return 2; if (a == b) return 3; return 0; }' \
"success"

run_test "Complex expression" \
'int main() { int a = 1; int b = 2; int c = 3; int d = 4; return a + b * c - d / 2; }' \
"success"

# ========== 语义错误测试 ==========
echo ""
echo "========== SEMANTIC ERROR TESTS =========="

run_test "Undeclared variable" \
'int main() { x = 1; return 0; }' \
"error"

run_test "Undeclared in expr" \
'int main() { int a = b + 1; return a; }' \
"error"

run_test "Undeclared function" \
'int main() { return foo(); }' \
"error"

run_test "Const assignment" \
'const int x = 1; int main() { x = 2; return 0; }' \
"error"

run_test "Missing main" \
'int foo() { return 0; }' \
"error"

run_test "Main wrong return type" \
'void main() { }' \
"error"

run_test "Main with parameters" \
'int main(int argc) { return 0; }' \
"error"

run_test "Break outside loop" \
'int main() { break; return 0; }' \
"error"

run_test "Continue outside loop" \
'int main() { continue; return 0; }' \
"error"

run_test "Wrong arg count (too few)" \
'int add(int a, int b) { return a + b; } int main() { return add(1); }' \
"error"

run_test "Wrong arg count (too many)" \
'int add(int a, int b) { return a + b; } int main() { return add(1, 2, 3); }' \
"error"

run_test "Missing return" \
'int foo() { int a = 1; } int main() { return 0; }' \
"error"

run_test "Void returns value" \
'void foo() { return 1; } int main() { return 0; }' \
"error"

run_test "Redeclaration" \
'int main() { int a = 1; int a = 2; return 0; }' \
"error"

run_test "Function redeclaration" \
'int foo() { return 0; } int foo() { return 1; } int main() { return 0; }' \
"error"

run_test "Use before declare" \
'int main() { int a = b; int b = 1; return a; }' \
"error"

# ========== 边界测试 ==========
echo ""
echo "========== EDGE CASE TESTS =========="

run_test "Empty block" \
'int main() { {} return 0; }' \
"success"

run_test "Single stmt block" \
'int main() { { int a = 1; } return 0; }' \
"success"

run_test "Deeply nested" \
'int main() { int a = 1; { int b = 2; { int c = 3; return a + b + c; } } }' \
"success"

run_test "Zero init" \
'int main() { int a = 0; return a; }' \
"success"

run_test "Negative number" \
'int main() { int a = -5; return a; }' \
"success"

# ========== 总结 ==========
echo ""
echo "Passed: $PASS / $TOTAL"
echo "Failed: $FAIL / $TOTAL"

read -p "Press any key to exit..." -n1 -s
