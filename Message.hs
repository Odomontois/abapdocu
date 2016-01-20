module Message where

className, intfName, paramKind, mtdName :: String -> String
className = ("класс " ++)
intfName = ("интерфейс " ++)
mtdName = ("метод " ++)

paramKind "importing" = "Импорт"
paramKind "exporting" = "Экспорт"
paramKind "changing"  = "Изменение"
paramKind "returning" = "Возврат"
paramKind "tables"    = "Таблица"
paramKind _           = ""

static, public, protected, private::String
static = "статический"
public = "общий"
protected = "защищённый"
private = "личный"

eventHandler::String->String->String
eventHandler evt cls = "обработчик события " ++ evt ++ " класса " ++ cls
