#!/bin/bash
# Benchmark comparison: Crystal v2 vs Original Crystal
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_DIR"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}       Crystal v2 vs Original Crystal Benchmark            ${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

# Compile Crystal v2 driver first
echo -e "${YELLOW}Building Crystal v2 compiler driver...${NC}"
time_start=$(perl -MTime::HiRes=time -e 'printf "%.3f", time')
crystal build src/compiler/driver.cr -o bin/crystal_v2_driver --release 2>/dev/null
time_end=$(perl -MTime::HiRes=time -e 'printf "%.3f", time')
driver_build_time=$(echo "$time_end - $time_start" | bc)
echo -e "Driver build time: ${GREEN}${driver_build_time}s${NC}"
echo ""

# Benchmark: Fibonacci(35)
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Benchmark: Fibonacci(35) - ~9M recursive calls${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"

# Compile with v2
echo -e "${YELLOW}[Crystal v2] Compiling...${NC}"
time_start=$(perl -MTime::HiRes=time -e 'printf "%.3f", time')
./bin/crystal_v2_driver examples/bench_fib35.cr -o /tmp/bench_fib35_v2 2>/dev/null
time_end=$(perl -MTime::HiRes=time -e 'printf "%.3f", time')
v2_compile_time=$(echo "$time_end - $time_start" | bc)
echo -e "Compile time: ${GREEN}${v2_compile_time}s${NC}"

# Run v2 binary
echo -e "${YELLOW}[Crystal v2] Running...${NC}"
time_start=$(perl -MTime::HiRes=time -e 'printf "%.3f", time')
v2_result=$(/tmp/bench_fib35_v2; echo $?)
time_end=$(perl -MTime::HiRes=time -e 'printf "%.3f", time')
v2_run_time=$(echo "$time_end - $time_start" | bc)
echo -e "Run time: ${GREEN}${v2_run_time}s${NC}"
echo -e "Result (exit code % 256): ${v2_result}"

# Compile with original Crystal
echo ""
echo -e "${YELLOW}[Original Crystal] Compiling with --release...${NC}"
time_start=$(perl -MTime::HiRes=time -e 'printf "%.3f", time')
crystal build examples/bench_fib35_crystal.cr -o /tmp/bench_fib35_orig --release 2>/dev/null
time_end=$(perl -MTime::HiRes=time -e 'printf "%.3f", time')
orig_compile_time=$(echo "$time_end - $time_start" | bc)
echo -e "Compile time: ${GREEN}${orig_compile_time}s${NC}"

# Run original Crystal binary
echo -e "${YELLOW}[Original Crystal] Running...${NC}"
time_start=$(perl -MTime::HiRes=time -e 'printf "%.3f", time')
orig_result=$(/tmp/bench_fib35_orig)
time_end=$(perl -MTime::HiRes=time -e 'printf "%.3f", time')
orig_run_time=$(echo "$time_end - $time_start" | bc)
echo -e "Run time: ${GREEN}${orig_run_time}s${NC}"
echo -e "Result: ${orig_result}"

# Comparison
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}                     Summary                                ${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "                   Crystal v2    Original Crystal"
echo -e "Compile time:      ${v2_compile_time}s          ${orig_compile_time}s"
echo -e "Run time:          ${v2_run_time}s          ${orig_run_time}s"

# Calculate speedups
if [ $(echo "$v2_run_time > 0" | bc) -eq 1 ]; then
    runtime_ratio=$(echo "scale=2; $orig_run_time / $v2_run_time" | bc)
    echo ""
    if [ $(echo "$runtime_ratio > 1" | bc) -eq 1 ]; then
        echo -e "${GREEN}Crystal v2 is ${runtime_ratio}x FASTER at runtime!${NC}"
    elif [ $(echo "$runtime_ratio < 1" | bc) -eq 1 ]; then
        inverse_ratio=$(echo "scale=2; $v2_run_time / $orig_run_time" | bc)
        echo -e "${YELLOW}Crystal v2 is ${inverse_ratio}x slower at runtime${NC}"
    else
        echo -e "Runtime performance is similar"
    fi
fi

# Binary sizes
echo ""
echo -e "${BLUE}Binary sizes:${NC}"
ls -lh /tmp/bench_fib35_v2 /tmp/bench_fib35_orig | awk '{print $9 ": " $5}'
