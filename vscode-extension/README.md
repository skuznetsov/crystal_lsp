# Crystal V2 Language Server для VS Code

Расширение VS Code для работы с CrystalV2 Language Server.

## Что реализовано (21 метод LSP)

### ✅ Навигация
- **Go to Definition** (`F12`) - переход к определению
- **Find All References** (`Shift+F12`) - поиск всех использований
- **Hover** - информация о типах при наведении
- **Document Symbols** (`Cmd+Shift+O`) - структура документа
- **Call Hierarchy** - иерархия вызовов функций

### ✅ Автодополнение
- **Completion** (`Ctrl+Space`) - автодополнение кода
- **Signature Help** (`Cmd+Shift+Space`) - подсказки параметров функций

### ✅ Редактирование
- **Rename** (`F2`) - переименование символов
- **Format Document** (`Shift+Alt+F`) - форматирование кода (54% быстрее оригинала!)

### ✅ Визуальные улучшения
- **Semantic Highlighting** - семантическая подсветка
- **Inlay Hints** - inline подсказки типов
- **Folding Ranges** - сворачивание блоков кода

### ✅ Code Actions
- **Quick Fixes** (`Cmd+.`) - быстрые исправления

## Установка

### Шаг 1: Скомпилировать LSP сервер

```bash
cd /Users/sergey/Projects/Crystal/crystal/crystal_v2
./build_lsp.sh
```

Это создаст исполняемый файл `bin/crystal_v2_lsp`.

### Шаг 2: Установить зависимости расширения

```bash
cd vscode-extension
npm install
```

### Шаг 3: Установить расширение в VS Code

**Вариант A: Отладка (для разработки)**

1. Откройте папку `vscode-extension` в VS Code:
   ```bash
   code vscode-extension
   ```

2. Нажмите `F5` для запуска Extension Development Host

3. В новом окне VS Code откройте любой `.cr` файл

**Вариант B: Установка в VS Code (для постоянного использования)**

1. Создайте .vsix пакет:
   ```bash
   cd vscode-extension
   npm install -g @vscode/vsce
   vsce package
   ```

2. Установите созданный .vsix файл:
   - VS Code → Extensions → `...` (три точки) → Install from VSIX
   - Выберите `crystalv2-lsp-0.1.0.vsix`

3. Перезапустите VS Code

## Тестирование

### Создайте тестовый файл

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

### Проверьте функциональность

1. **Hover** - наведите на `calc` → должен показать тип `Calculator`
2. **Go to Definition** (`F12`) - кликните на `add` → переход к определению метода
3. **Completion** (`Ctrl+Space`) - начните печатать `calc.` → автодополнение методов
4. **Format** (`Shift+Alt+F`) - отформатируйте код
5. **Rename** (`F2`) - переименуйте `calc` → изменится везде
6. **Outline** (`Cmd+Shift+O`) - посмотрите структуру документа

## Отладка

Если что-то не работает:

1. **Проверьте, что LSP сервер скомпилирован:**
   ```bash
   ls -lh crystal_v2/bin/crystal_v2_lsp
   ```

2. **Запустите LSP сервер вручную для проверки:**
   ```bash
   cd crystal_v2
   ./bin/crystal_v2_lsp
   ```

   Введите:
   ```json
   Content-Length: 95

   {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{},"rootUri":null}}
   ```

   Должен ответить с capabilities.

3. **Включите LSP трассировку в VS Code:**
   - Settings → `crystalv2.lsp.trace.server` → `verbose`
   - View → Output → выберите "Crystal V2 Language Server"

4. **Проверьте логи расширения:**
   - View → Output → выберите "Crystal V2 Language Server"
   - Help → Toggle Developer Tools → Console

## Производительность

Форматер CrystalV2:
- **35ms** в среднем для файла в 6479 строк
- **54% быстрее** оригинального Crystal форматера
- **18.8x меньше кода** (280 vs 5260 строк)

## Следующие шаги

Планируется добавить:
- `textDocument/documentHighlight` - подсветка вхождений символа
- `textDocument/onTypeFormatting` - форматирование при вводе
- `workspace/symbol` - глобальный поиск символов
- `textDocument/codeLens` - счетчики ссылок

См. `../LSP_COVERAGE.md` для полного плана.
