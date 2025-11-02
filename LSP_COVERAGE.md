# LSP Feature Coverage

## ✅ Implemented Features

### Basic (Lifecycle)
- ✅ `initialize` - Server initialization
- ✅ `initialized` - Initialization confirmation
- ✅ `shutdown` - Shutdown request
- ✅ `exit` - Exit notification

### Documents (Text Synchronization)
- ✅ `textDocument/didOpen` - Document opened
- ✅ `textDocument/didChange` - Document changed (TODO: not fully implemented)
- ✅ `textDocument/didClose` - Document closed

### Navigation (Language Features)
- ✅ `textDocument/definition` - Go to Definition
- ✅ `textDocument/references` - Find All References
- ✅ `textDocument/hover` - Hover information (types, documentation)
- ✅ `textDocument/documentSymbol` - Document outline/structure

### Editing (Code Intelligence)
- ✅ `textDocument/completion` - Autocompletion
- ✅ `textDocument/signatureHelp` - Function signature hints
- ✅ `textDocument/rename` - Rename symbols
- ✅ `textDocument/prepareRename` - Check if rename is possible

### Formatting
- ✅ `textDocument/formatting` - Format entire document
- ✅ `textDocument/rangeFormatting` - Format range (MVP: formats entire file)

### Visual Enhancements
- ✅ `textDocument/semanticTokens/full` - Semantic highlighting
- ✅ `textDocument/inlayHint` - Inline hints (parameter types, return values)
- ✅ `textDocument/foldingRange` - Code folding

### Call Hierarchy
- ✅ `textDocument/prepareCallHierarchy` - Prepare call hierarchy
- ✅ `callHierarchy/incomingCalls` - Who calls this function
- ✅ `callHierarchy/outgoingCalls` - What this function calls

### Code Actions
- ✅ `textDocument/codeAction` - Quick fixes and refactorings

---

## 🔴 NOT Implemented (standard LSP methods)

### Navigation
- ❌ `textDocument/declaration` - Go to Declaration (differs from definition)
- ❌ `textDocument/typeDefinition` - Go to Type Definition
- ❌ `textDocument/implementation` - Go to Implementation (for interfaces)
- ❌ `textDocument/documentHighlight` - Highlight all occurrences of symbol

### Code Lens
- ❌ `textDocument/codeLens` - Show information above code (reference counts, "Run", "Debug")
- ❌ `codeLens/resolve` - Resolve code lens

### Formatting
- ❌ `textDocument/onTypeFormatting` - Format on typing (after `;`, `}`, etc)

### Diagnostics
- ❌ `textDocument/diagnostic` - Pull diagnostics (protocol 3.17)
- ❌ `workspace/diagnostic` - Workspace-wide diagnostics

### Workspace
- ❌ `workspace/symbol` - Search symbols across entire workspace
- ❌ `workspace/didChangeConfiguration` - Configuration changed
- ❌ `workspace/didChangeWatchedFiles` - Watched files changed

### Selection & Linking
- ❌ `textDocument/selectionRange` - Smart selection (expand/shrink selection)
- ❌ `textDocument/linkedEditingRange` - Simultaneous editing of related elements
- ❌ `textDocument/documentLink` - Links in document (clickable paths)
- ❌ `documentLink/resolve` - Resolve links

### Color
- ❌ `textDocument/documentColor` - Color literals
- ❌ `textDocument/colorPresentation` - Color picker

### Type Hierarchy (LSP 3.17)
- ❌ `textDocument/prepareTypeHierarchy` - Prepare type hierarchy
- ❌ `typeHierarchy/supertypes` - Supertypes
- ❌ `typeHierarchy/subtypes` - Subtypes

### Inline Values (LSP 3.17)
- ❌ `textDocument/inlineValue` - Show variable values during debugging

### Monikers (LSP 3.16)
- ❌ `textDocument/moniker` - Unique identifiers for cross-repository navigation

---

## 💡 Priority for Implementation

### High Priority
1. **`textDocument/documentHighlight`** - Very useful for highlighting usages
2. **`textDocument/onTypeFormatting`** - Auto-format on typing
3. **`workspace/symbol`** - Global symbol search
4. **`textDocument/codeLens`** - Show reference counts, run tests, etc

### Medium Priority
5. **`textDocument/typeDefinition`** - Go to type definition
6. **`textDocument/implementation`** - Go to implementation
7. **`textDocument/selectionRange`** - Smart selection
8. **`textDocument/documentLink`** - Clickable paths/URLs

### Low Priority
9. **`textDocument/linkedEditingRange`** - Linked editing
10. **Type Hierarchy** - Type hierarchy (if inheritance support needed)

---

## 📊 Statistics

- **Implemented**: 21 methods
- **Standard LSP methods**: ~40-50
- **Coverage**: ~50-60% of core features
- **Quality**: High (token-based formatter 54% faster than original)

---

## 🎯 Next Steps

Recommended implementation order:

1. **Improve `textDocument/didChange`** - Currently TODO
2. **`textDocument/documentHighlight`** - Quick to implement, very useful
3. **`textDocument/onTypeFormatting`** - Can reuse existing formatter
4. **`workspace/symbol`** - Global search across all files
5. **`textDocument/codeLens`** - Show reference counters

Other methods can be added as needed.
