module Message (
  className   ,
  intfName    ,
  mtdName     ,
  paramKind   ,
  static      ,
  public      ,
  protected   ,
  private     ,
  eventHandler) where

mkName::String->String->String
mkName caption name = caption ++ " " ++ name

className, intfName, paramKind, mtdName::String->String
className = mkName "класс"
intfName  = mkName "интерфейс"
mtdName   = mkName "метод"

paramKind "importing" = "Импорт"
paramKind "exporting" = "Экспорт"
paramKind "changing"  = "Изменение"
paramKind "returning" = "Возврат"
paramKind "tables"    = "Таблица"
paramKind _           = ""

static, public, protected, private::String
static    = "статический"
public    = "общий"
protected = "защищённый"
private   = "личный"

eventHandler::String->String->String
eventHandler evt cls = "обработчик события " ++ evt ++ " класса " ++ cls
