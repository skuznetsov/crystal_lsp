# Quick Start - Testing Crystal V2 LSP

## 1. Build LSP Server

```bash
cd /path/to/crystal_v2
./build_lsp.sh
```

## 2. Install Extension Dependencies

```bash
cd vscode-extension
npm install
```

## 3. Launch in Debug Mode

1. Open `vscode-extension` folder in VS Code:
   ```bash
   code /path/to/crystal_v2/vscode-extension
   ```

2. Press **F5** (or Run → Start Debugging)

3. A new VS Code window opens with title **"[Extension Development Host]"**

4. In this window, open any `.cr` file or create a new one:
   - File → Open Folder → select project with `.cr` files
   - OR create new file: File → New File → select language "Crystal"

## 4. Quick Test

Create `test.cr`:

```crystal
class Calculator
  def add(x : Int32, y : Int32)
    x + y
  end
end

calc = Calculator.new
result = calc.add(5, 3)
```

**Try these features:**
- Hover over `calc` → should show type `Calculator`
- `F12` on `add` → jumps to definition
- `Ctrl+Space` after `calc.` → autocompletion
- `Shift+Alt+F` → format document
- `F2` on `calc` → rename

## 5. Verify LSP is Working

In the Extension Development Host window:
- View → Output → select "Crystal V2 Language Server"
- Should see LSP communication logs

## Troubleshooting

**If extension doesn't activate:**
1. Make sure file has `.cr` extension
2. Check file language is set to "Crystal" (bottom right corner of VS Code)

**If LSP doesn't start:**
1. Verify `bin/crystal_v2_lsp` exists and is executable:
   ```bash
   ls -lh ../bin/crystal_v2_lsp
   ./bin/crystal_v2_lsp --version  # Should start (Ctrl+C to exit)
   ```

2. Check VS Code logs:
   - Help → Toggle Developer Tools → Console
   - View → Output → "Crystal V2 Language Server"

**To restart LSP:**
1. Close Extension Development Host
2. Press F5 again

## Next Steps

After successful testing:
1. Install extension permanently (see README.md)
2. Return to implementation plan
3. Add Debug Adapter Protocol support
