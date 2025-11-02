# Crystal V2 Language Server for VS Code

VS Code extension for CrystalV2 Language Server Protocol support.

## Implemented Features (21 LSP methods)

### ✅ Navigation
- **Go to Definition** (`F12`) - Jump to symbol definition
- **Find All References** (`Shift+F12`) - Find all symbol usages
- **Hover** - Show type information on hover
- **Document Symbols** (`Cmd+Shift+O`) - Document outline
- **Call Hierarchy** - Function call hierarchy (incoming/outgoing)

### ✅ Code Intelligence
- **Completion** (`Ctrl+Space`) - Code autocompletion
- **Signature Help** (`Cmd+Shift+Space`) - Function parameter hints

### ✅ Editing
- **Rename** (`F2`) - Rename symbols across files
- **Format Document** (`Shift+Alt+F`) - Code formatting (54% faster than original!)

### ✅ Visual Enhancements
- **Semantic Highlighting** - Semantic token coloring
- **Inlay Hints** - Inline type hints
- **Folding Ranges** - Code block folding

### ✅ Code Actions
- **Quick Fixes** (`Cmd+.`) - Quick fixes and refactorings

## Installation

### Step 1: Build LSP Server

```bash
cd /path/to/crystal_v2
./build_lsp.sh
```

This creates the executable `bin/crystal_v2_lsp`.

### Step 2: Install Extension Dependencies

```bash
cd vscode-extension
npm install
```

### Step 3: Install Extension in VS Code

**Option A: Debug Mode (for development)**

1. Open the `vscode-extension` folder in VS Code:
   ```bash
   code vscode-extension
   ```

2. Press **F5** (or Run → Start Debugging)

3. A new VS Code window opens with "[Extension Development Host]" title

4. In this window, open any `.cr` file

**Option B: Install in VS Code (for permanent use)**

1. Create .vsix package:
   ```bash
   cd vscode-extension
   npm install -g @vscode/vsce
   vsce package
   ```

2. Install the created .vsix file:
   - VS Code → Extensions → `...` (three dots) → Install from VSIX
   - Select `crystalv2-lsp-0.1.0.vsix`

3. Restart VS Code

## Testing

### Create Test File

```bash
cat > test.cr << 'EOF'
class Calculator
  def add(x : Int32, y : Int32) : Int32
    x + y
  end

  def multiply(x : Int32, y : Int32) : Int32
    x * y
  end
end

calc = Calculator.new
result = calc.add(5, 3)
puts result
EOF
```

### Try Features

1. **Hover** - Hover over `calc` → shows type `Calculator`
2. **Go to Definition** (`F12`) - Click on `add` → jumps to method definition
3. **Completion** (`Ctrl+Space`) - Start typing `calc.` → method suggestions
4. **Format** (`Shift+Alt+F`) - Format the code
5. **Rename** (`F2`) - Rename `calc` → updates everywhere
6. **Outline** (`Cmd+Shift+O`) - View document structure

## Debugging

If something doesn't work:

1. **Check LSP server is built:**
   ```bash
   ls -lh crystal_v2/bin/crystal_v2_lsp
   ```

2. **Run LSP server manually to verify:**
   ```bash
   cd crystal_v2
   ./bin/crystal_v2_lsp
   ```

   Send test input:
   ```json
   Content-Length: 95

   {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{},"rootUri":null}}
   ```

   Should respond with capabilities.

3. **Enable LSP tracing in VS Code:**
   - Settings → `crystalv2.lsp.trace.server` → `verbose`
   - View → Output → select "Crystal V2 Language Server"

4. **Check extension logs:**
   - View → Output → "Crystal V2 Language Server"
   - Help → Toggle Developer Tools → Console

## Performance

CrystalV2 Formatter:
- **35ms** average for 6479-line file
- **54% faster** than original Crystal formatter
- **18.8x less code** (280 vs 5260 lines)

## Future Plans

Planned additions:
- `textDocument/documentHighlight` - Highlight symbol occurrences
- `textDocument/onTypeFormatting` - Format on typing
- `workspace/symbol` - Global symbol search
- `textDocument/codeLens` - Reference counts
- **Debug Adapter Protocol** - Debugging support

See `../LSP_COVERAGE.md` for complete roadmap.
