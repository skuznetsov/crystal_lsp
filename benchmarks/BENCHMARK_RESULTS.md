# Formatter Benchmark Results

Comparison between original Crystal formatter and CrystalV2 token-based formatter.

## Test File
- **File**: `src/compiler/crystal/syntax/parser.cr`
- **Size**: 191,439 bytes (191 KB)
- **Lines**: 6,479 lines

## Results (10 runs each)

### Original Crystal Formatter (AST-based)
- **Average**: 53.41 ms
- **Min**: 51.63 ms
- **Max**: 61.09 ms
- **Code size**: ~5,260 lines

### CrystalV2 Token-based Formatter
- **Average**: 34.78 ms
- **Min**: 33.08 ms
- **Max**: 38.08 ms
- **Code size**: ~280 lines

## Performance Comparison

| Metric | Original | CrystalV2 | Improvement |
|--------|----------|-----------|-------------|
| Average time | 53.41 ms | 34.78 ms | **1.54x faster** |
| Min time | 51.63 ms | 33.08 ms | **1.56x faster** |
| Max time | 61.09 ms | 38.08 ms | **1.60x faster** |
| Code size | 5,260 lines | 280 lines | **18.8x smaller** |
| Throughput | 3.58 MB/s | 5.50 MB/s | **+54% faster** |

## Key Advantages of Token-based Approach

1. **Performance**: 54% faster on large files
2. **Simplicity**: 18.8x less code to maintain
3. **Correctness**: Preserves original text via spans (quotes, comments, etc.)
4. **Architecture**: Ground-up implementation, no dependencies on old compiler

## Conclusion

The token-based formatter demonstrates that simpler approaches can outperform
complex AST-based solutions. By traversing tokens and calculating whitespace
based on token pairs, we achieve both better performance and simpler code.
