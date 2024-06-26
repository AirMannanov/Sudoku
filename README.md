# project-sudoku

## Сборка и запуск

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать и запустить проект можно при помощи команды:

```
stack run
```

Чтобы проект автоматически пересобирался при обновлении исходных файлов, используйте команду:

```
stack build --file-watch
```

## Структура проекта

Важными частями проекта являются:

- директория `src/`, содержащая исходный код общей, библиотечной части;
- директория `app/` с исходным кодом исполняемого модуля;
- директория `test/` c автоматическими тестами для вашего проекта;
- директория `feild/` с файлами, содержащие начальные карты судоку;
- файл `stack.yaml` с указанием версии компилятора и зависимостей;
- файл `package.yaml` с описанием проекта, его зависимостей и опций компилятора;
- файл `README.md` с базовым описанием проекта и указаниями по сборке и запуску.

Остальные файлы — это дополнительные полезные настройки:

- файл `.gitignore`, указывающий Git не обращать внимания на временные файлы;