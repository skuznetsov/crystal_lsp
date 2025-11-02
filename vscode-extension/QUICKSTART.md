# Быстрый старт - Тестирование Crystal V2 LSP

## 1. Скомпилировать LSP сервер

```bash
cd /Users/sergey/Projects/Crystal/crystal/crystal_v2
./build_lsp.sh
```

## 2. Установить зависимости расширения

```bash
cd vscode-extension
npm install
```

## 3. Запустить в режиме отладки

1. Откройте папку `vscode-extension` в VS Code:
   ```bash
   code /Users/sergey/Projects/Crystal/crystal/crystal_v2/vscode-extension
   ```

2. Нажмите **F5** (или Run → Start Debugging)

3. Откроется новое окно VS Code с заголовком **"[Extension Development Host]"**

4. В этом окне откройте любой `.cr` файл или создайте новый:
   - File → Open Folder → выберите проект с `.cr` файлами
   - ИЛИ создайте новый файл: File → New File → выберите язык "Crystal"

## 4. Быстрый тест

Создайте файл `test.cr`:

```crystal
class Calculator
  def add(x : Int32, y : Int32)
    x + y
  end
end

calc = Calculator.new
result = calc.add(5, 3)
```

**Попробуйте:**
- Наведите на `calc` → должен показать тип `Calculator`
- `F12` на `add` → переход к определению
- `Ctrl+Space` после `calc.` → автодополнение
- `Shift+Alt+F` → форматирование документа
- `F2` на `calc` → переименование

## 5. Проверка что LSP работает

В окне Extension Development Host:
- View → Output → выберите "Crystal V2 Language Server"
- Должны видеть логи взаимодействия с сервером

## Troubleshooting

**Если расширение не активировалось:**
1. Убедитесь что файл имеет расширение `.cr`
2. Проверьте что язык файла установлен как "Crystal" (в нижнем правом углу VS Code)

**Если LSP не запускается:**
1. Проверьте что `bin/crystal_v2_lsp` существует и исполняемый:
   ```bash
   ls -lh ../bin/crystal_v2_lsp
   ./bin/crystal_v2_lsp --version  # Должен запуститься (Ctrl+C для выхода)
   ```

2. Проверьте логи в VS Code:
   - Help → Toggle Developer Tools → Console
   - View → Output → "Crystal V2 Language Server"

**Если нужно перезапустить LSP:**
1. Закройте Extension Development Host
2. Нажмите F5 снова

## Следующие шаги

После успешного тестирования можно:
1. Установить расширение постоянно (см. README.md)
2. Вернуться к плану реализации оставшихся LSP методов
3. Добавить поддержку отладки (Debug Adapter Protocol)
