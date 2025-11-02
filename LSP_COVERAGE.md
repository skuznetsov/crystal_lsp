# LSP Feature Coverage

## ✅ Реализованные возможности

### Базовые (Lifecycle)
- ✅ `initialize` - Инициализация сервера
- ✅ `initialized` - Подтверждение инициализации
- ✅ `shutdown` - Завершение работы
- ✅ `exit` - Выход

### Документы (Text Synchronization)
- ✅ `textDocument/didOpen` - Открытие документа
- ✅ `textDocument/didChange` - Изменение документа (TODO: не полностью)
- ✅ `textDocument/didClose` - Закрытие документа

### Навигация (Language Features)
- ✅ `textDocument/definition` - Go to Definition
- ✅ `textDocument/references` - Find All References
- ✅ `textDocument/hover` - Hover информация (типы, документация)
- ✅ `textDocument/documentSymbol` - Outline/Структура документа

### Редактирование (Code Intelligence)
- ✅ `textDocument/completion` - Автодополнение
- ✅ `textDocument/signatureHelp` - Подсказки сигнатур функций
- ✅ `textDocument/rename` - Переименование символов
- ✅ `textDocument/prepareRename` - Проверка возможности переименования

### Форматирование
- ✅ `textDocument/formatting` - Форматирование всего документа
- ✅ `textDocument/rangeFormatting` - Форматирование диапазона (MVP: форматирует весь файл)

### Визуальные улучшения
- ✅ `textDocument/semanticTokens/full` - Семантическая подсветка
- ✅ `textDocument/inlayHint` - Inline подсказки (типы параметров, возвращаемые значения)
- ✅ `textDocument/foldingRange` - Сворачивание блоков кода

### Call Hierarchy
- ✅ `textDocument/prepareCallHierarchy` - Подготовка иерархии вызовов
- ✅ `callHierarchy/incomingCalls` - Кто вызывает эту функцию
- ✅ `callHierarchy/outgoingCalls` - Что вызывает эта функция

### Code Actions
- ✅ `textDocument/codeAction` - Быстрые исправления и рефакторинги

---

## 🔴 НЕ реализовано (стандартные LSP методы)

### Навигация
- ❌ `textDocument/declaration` - Go to Declaration (отличается от definition)
- ❌ `textDocument/typeDefinition` - Go to Type Definition
- ❌ `textDocument/implementation` - Go to Implementation (для интерфейсов)
- ❌ `textDocument/documentHighlight` - Подсветка всех вхождений символа

### Code Lens
- ❌ `textDocument/codeLens` - Показывает информацию над кодом (счетчик ссылок, "Run", "Debug")
- ❌ `codeLens/resolve` - Резолв code lens

### Форматирование
- ❌ `textDocument/onTypeFormatting` - Форматирование при вводе (после `;`, `}`, etc)

### Диагностика
- ❌ `textDocument/diagnostic` - Pull diagnostics (новый протокол 3.17)
- ❌ `workspace/diagnostic` - Workspace-wide diagnostics

### Workspace
- ❌ `workspace/symbol` - Поиск символов по всему workspace
- ❌ `workspace/didChangeConfiguration` - Изменение конфигурации
- ❌ `workspace/didChangeWatchedFiles` - Изменения файлов

### Selection & Linking
- ❌ `textDocument/selectionRange` - Smart selection (expand/shrink selection)
- ❌ `textDocument/linkedEditingRange` - Одновременное редактирование связанных элементов
- ❌ `textDocument/documentLink` - Ссылки в документе (кликабельные пути)
- ❌ `documentLink/resolve` - Резолв ссылок

### Color
- ❌ `textDocument/documentColor` - Цветовые литералы
- ❌ `textDocument/colorPresentation` - Color picker

### Type Hierarchy (LSP 3.17)
- ❌ `textDocument/prepareTypeHierarchy` - Подготовка иерархии типов
- ❌ `typeHierarchy/supertypes` - Супертипы
- ❌ `typeHierarchy/subtypes` - Подтипы

### Inline Values (LSP 3.17)
- ❌ `textDocument/inlineValue` - Показывает значения переменных во время отладки

### Monikers (LSP 3.16)
- ❌ `textDocument/moniker` - Уникальные идентификаторы для кросс-репозиторной навигации

---

## 💡 Приоритетные для реализации

### Высокий приоритет
1. **`textDocument/documentHighlight`** - Очень полезно для подсветки использований
2. **`textDocument/onTypeFormatting`** - Автоформатирование при вводе
3. **`workspace/symbol`** - Глобальный поиск символов
4. **`textDocument/codeLens`** - Показывать количество ссылок, run tests, etc

### Средний приоритет
5. **`textDocument/typeDefinition`** - Go to type definition
6. **`textDocument/implementation`** - Go to implementation
7. **`textDocument/selectionRange`** - Smart selection
8. **`textDocument/documentLink`** - Кликабельные пути/URLs

### Низкий приоритет
9. **`textDocument/linkedEditingRange`** - Linked editing
10. **Type Hierarchy** - Иерархия типов (если нужна поддержка наследования)

---

## 📊 Статистика

- **Реализовано**: 21 метод
- **Стандартных LSP методов**: ~40-50
- **Покрытие**: ~50-60% основных возможностей
- **Качество**: Высокое (token-based formatter быстрее оригинала на 54%)

---

## 🎯 Следующие шаги

Рекомендуемый порядок реализации:

1. **Улучшить `textDocument/didChange`** - Сейчас TODO
2. **`textDocument/documentHighlight`** - Быстро реализуется, очень полезно
3. **`textDocument/onTypeFormatting`** - Можно использовать уже готовый formatter
4. **`workspace/symbol`** - Глобальный поиск по всем файлам
5. **`textDocument/codeLens`** - Показывать счетчики ссылок

Остальные методы можно добавлять по мере необходимости.
