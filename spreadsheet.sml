structure SpreadSheet :> SPREADSHEET = struct
  structure RichString = struct
    type t = CT.CT_Rst

    fun reltToString (CT.CT_RElt {t, ...}) = t
    fun rstToString (CT.CT_Rst {t, r, ...}) =
          Option.getOpt (t, "") ^ concat (map reltToString r)
    val toString = rstToString
  end

  structure Formula = struct
    type t = CT.CT_CellFormula
  end

  structure CellValue = struct
    datatype t = Empty
               | Boolean of bool
               | Number of string
               | Error of string
               | String of RichString.t
               | Formula of Formula.t

    fun toString Empty = ""
      | toString (Boolean boolean) = Bool.toString boolean
      | toString (Number number) = number
      | toString (Error error) = error
      | toString (String string) = RichString.toString string
      | toString (Formula (CT.CT_CellFormula {content, ...})) = content

    fun toInt Empty = 0
      | toInt (Boolean false) = 0
      | toInt (Boolean true) = 1
      | toInt (Number number) =
          Option.getOpt (Int.fromString number, 0)
      | toInt (Error _) = 0
      | toInt (String string) =
          Option.getOpt (Int.fromString (RichString.toString string), 0)
      | toInt (Formula _) = 0

    fun toBool Empty = false
      | toBool (Boolean boolean) = boolean
      | toBool (Number "0") = false
      | toBool (Number _) = true
      | toBool (Error _) = false
      | toBool (String s) = RichString.toString s <> ""
      | toBool (Formula _) = false
  end

  structure Cell = struct
    type t = { value : CellValue.t }

    fun value {value} = value
    fun toString {value} = CellValue.toString value
  end

  structure SharedStrings = struct
    type t = CT.CT_Rst Vector.vector
    fun sub (sharedStrings, i) = Vector.sub (sharedStrings, i)
  end

  structure Border = struct
    type t = CT.CT_Border
  end

  structure Worksheet = struct
    type t = {
      nav : ZipNavigator.navigator,
      worksheet : CT.CT_Worksheet,
      sharedStrings : SharedStrings.t }

    type range = { base : t, r : Ref.t }

    fun range (base, r) = {base = base, r = r}

    fun fullRange (ws as {worksheet = CT.CT_Worksheet {dimension = SOME (CT.CT_SheetDimension {ref = r, ...}), ...}, ...} : t) =
          (case Ref.fromString r of
                NONE => raise Fail "invalid ref"
              | SOME r => { base = ws, r = r })
      | fullRange _ = raise Fail "dimension not found"

    fun fromNav (nav, sharedStrings) : t =
          let
            val bytes = ZipNavigator.getStream nav
            val doc = UXML.Path.fromDocument (UXML.parseBytes bytes)
            val worksheet = CT.worksheet doc
          in
            { nav = nav,
              worksheet = worksheet,
              sharedStrings = sharedStrings }
          end

    fun renumRows rows : (LargeWord.word * CT.CT_Row) list =
          let
            fun renum (row as CT.CT_Row {r = NONE, ...}, (num, rows)) =
                  (LargeWord.+ (num, 0w1), ((num, row)::rows))
              | renum (row as CT.CT_Row {r = SOME r, ...}, (num, rows)) =
                  (LargeWord.+ (r, 0w1), ((r, row)::rows))
            val (_, rows') = List.foldr renum (0w1, []) rows
          in
            rows'
          end

    fun renumCells cells : (ColumnName.t * CT.CT_Cell) list =
          let
            fun renum (cell as CT.CT_Cell {r = NONE, ...}, (num, cells)) =
                  (ColumnName.next num, ((num, cell)::cells))
              | renum (cell as CT.CT_Cell {r = SOME r, ...}, (num, cells)) =
                  case CellRef.fromString r of
                       NONE =>
                         (ColumnName.next num, ((num, cell)::cells))
                     | SOME cellRef =>
                         let
                           val column = CellRef.column cellRef
                         in
                           (ColumnName.next column, ((column, cell)::cells))
                         end
            val (_, cells') = List.foldr renum (ColumnName.fromInt 1, []) cells
          in
            cells'
          end

    fun makeCell (CT.CT_Cell {t, f, v, is, ...}, sharedStrings) =
          case t of
               ST_CellType.b =>
                 let
                   val v = Option.valOf v
                   val boolean = Option.valOf (CT.toBool v)
                 in
                   { value = CellValue.Boolean boolean }
                 end
             | ST_CellType.n =>
                 (case v of
                       SOME v => { value = CellValue.Number v }
                     | NONE => { value = CellValue.Empty })
             | ST_CellType.e =>
                 { value = CellValue.Error (Option.valOf v) }
             | ST_CellType.s =>
                 let
                   val v = Option.valOf v
                   val index = Option.valOf (Int.fromString v)
                 in
                   { value = CellValue.String (Vector.sub (sharedStrings, index)) }
                 end
             | ST_CellType.str =>
                 { value = CellValue.Formula (Option.valOf f) }
             | ST_CellType.inlineStr =>
                 { value = CellValue.String (Option.valOf is) }

    fun cell {nav, worksheet, sharedStrings} cellRef =
          let
            val rowRef = LargeWord.fromInt (CellRef.row cellRef)
            val columnRef = CellRef.column cellRef
            val CT.CT_Worksheet {sheetData = CT.CT_SheetData {row = rows, ...}, ...} = worksheet
            val row = List.find (fn (num, _) => num = rowRef) (renumRows rows)
          in
            case row of
                 NONE => { value = CellValue.Empty }
               | SOME (_, CT.CT_Row {c = cells, ...}) =>
                   let
                     val cell = List.find (fn (num, _) => num = columnRef) (renumCells cells)
                   in
                     case cell of
                          NONE => { value = CellValue.Empty }
                        | SOME (_, c) => makeCell (c, sharedStrings)
                   end
          end

    fun cellValue worksheet cellRef =
          let
            val {value} = cell worksheet cellRef
          in
            value
          end

    fun appRange f {base = {nav, worksheet, sharedStrings}, r} =
          let
            val range = r
            val topLeft = Ref.topLeft r
            val bottom = Ref.bottom r
            val right = Ref.right r
            fun appCell (cellRef, []) = (
                  f (cellRef, CellValue.Empty);
                  if CellRef.column cellRef = right then ()
                  else appCell (CellRef.right cellRef, []))
              | appCell (cellRef, cells as (cell as CT.CT_Cell {r, ...})::cells') =
                  let
                    val thisCellRef =
                      Option.getOpt (Option.mapPartial CellRef.fromString r, cellRef)
                    val next = CellRef.right cellRef
                  in
                    if cellRef = thisCellRef then (
                      f (cellRef, #value (makeCell (cell, sharedStrings)));
                      if Ref.has (range, next) then
                        appCell (next, cells')
                      else ())
                    else (
                      f (cellRef, CellValue.Empty);
                      if Ref.has (range, next) then
                        appCell (next, cells)
                      else ())
                  end
            fun appRow (cellRef, []) = (
                  appCell (cellRef, []);
                  if CellRef.row cellRef = bottom then ()
                  else appRow (CellRef.down cellRef, []))
              | appRow (cellRef, rows as CT.CT_Row {r, c, ...}::rows') =
                  let
                    val thisRow =
                      Option.getOpt (Option.map LargeWord.toInt r, CellRef.row cellRef)
                    val next = CellRef.down cellRef
                  in
                    if CellRef.row cellRef = thisRow then (
                      appCell (cellRef, c);
                      if Ref.has (range, next) then
                        appRow (next, rows')
                      else ())
                    else (
                      appCell (cellRef, []);
                      if Ref.has (range, next) then
                        appRow (next, rows)
                      else ())
                  end
            val CT.CT_Worksheet {sheetData = CT.CT_SheetData {row = rows, ...}, ...} =
                  worksheet
          in
            appRow (topLeft, rows)
          end
    fun app f ws = appRange f (fullRange ws)
  end

  structure Workbook = struct
    type t = {
      package : ZipPackage.package,
      nav : ZipNavigator.navigator,
      workbook : CT.CT_Workbook,
      sharedStrings : SharedStrings.t }

    fun loadSharedStrings [] = Vector.fromList []
      | loadSharedStrings (bytes::_) =
          let
            val doc = UXML.Path.fromDocument (UXML.parseBytes bytes)
            val CT.CT_Sst {si, ...} = CT.sst doc
          in
            Vector.fromList si
          end

    fun openIn fileName : t =
          let
            val package = ZipPackage.openIn fileName
            val nav = ZipNavigator.ofPackage package
            open ZipNavigator
            infix |>
            val officeDocument = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
            val officeDocument = nav |> typ officeDocument |> hd
                  handle Empty => raise Fail "main part not found"
            val bytes = getStream officeDocument
            val doc = UXML.Path.fromDocument (UXML.parseBytes bytes)
            val workbook = CT.workbook doc
            val sharedStrings = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings"
            val sharedStrings = officeDocument |> typ sharedStrings
                                               |> map getStream
                                               |> loadSharedStrings
          in
            { package = package,
              nav = officeDocument,
              workbook = workbook,
              sharedStrings = sharedStrings }
          end

    fun worksheetById (workbook : t) id' =
          let
            open ZipNavigator
            infix |>
            val nav = #nav workbook |> id id'
            val sharedStrings = #sharedStrings workbook
          in
            Worksheet.fromNav (nav, sharedStrings)
          end

    fun worksheets (workbook : t) =
          let
            val CT.CT_Workbook {sheets = CT.CT_Sheets {sheet, ...}, ...} = #workbook workbook
            fun getId (CT.CT_Sheet {id, ...}) = id
          in
            map (worksheetById workbook) (map getId sheet)
          end

    fun findWorksheet workbook f =
          let
            val CT.CT_Workbook {sheets = CT.CT_Sheets {sheet, ...}, ...} = #workbook workbook
          in
            case List.find f sheet of
                 NONE => NONE
               | SOME (CT.CT_Sheet {id, ...}) => SOME (worksheetById workbook id)
          end

    fun worksheetBySheetId workbook sheetId' =
          let
            fun f (CT.CT_Sheet {sheetId, ...}) = sheetId = sheetId'
          in
            findWorksheet workbook f
          end

    fun worksheetByName workbook name' =
          let
            fun f (CT.CT_Sheet {name, ...}) = name = name'
          in
            findWorksheet workbook f
          end

    fun closeIn (workbook : t) =
          ZipPackage.closeIn (#package workbook)
  end
end
