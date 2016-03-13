structure ColumnName :> sig
  eqtype t
  val toInt : t -> int
  val fromInt : int -> t
  val toString : t -> string
  val fromString : string -> t option
  val next : t -> t
  val < : t * t -> bool
  val <= : t * t -> bool
end = struct
  type t = int

  fun toInt v = v
  fun fromInt i =
        if i > 0 then i
        else raise Fail ("should be > 0 but got " ^ Int.toString i)

  val largeA = Char.ord #"A"
  val numAlphas = Char.ord #"Z" - Char.ord #"A" + 1

  fun toString v =
        let
          fun f (0, cs) = implode cs
            | f (v, cs) =
                let
                  val v = v - 1
                  val c = Char.chr (Int.rem (v, numAlphas) + largeA)
                in
                  f (Int.quot (v, numAlphas), c::cs)
                end
        in
          f (v, [])
        end

  fun fromString s =
        let
          fun f ([], sum) = SOME sum
            | f (c::cs, sum) =
                let
                  val i = Char.ord c - largeA + 1
                in
                  if i < 1 orelse i > numAlphas then
                    NONE
                  else
                    f (cs, sum * numAlphas + i)
                end
        in
          if s = "" then NONE
          else f (explode s, 0)
        end

  fun next v = v + 1

  fun op < (l, r) = Int.< (l, r)
  fun op <= (l, r) = Int.<= (l, r)
end

structure CellRef :> sig
  eqtype t
  val column : t -> ColumnName.t
  val row : t -> int
  val right : t -> t
  val down : t -> t
  val toString : t -> string
  val fromString : string -> t option
end = struct
  type t = {column : ColumnName.t, row : int}

  fun column {column, row} = column
  fun row {column, row} = row

  fun right {column, row} = {column = ColumnName.next column, row = row}
  fun down {column, row} = {column = column, row = row + 1}

  fun toString {column, row} =
        ColumnName.toString column ^ Int.toString row
  fun fromString s =
        let
          val (column, s') = Substring.splitl Char.isAlpha (Substring.full s)
          val (row, s'') = Substring.splitl Char.isDigit s'
        in
          if Substring.size s'' > 0 then NONE
          else
            let
              val column = ColumnName.fromString (Substring.string column)
              val row = Int.fromString (Substring.string row)
            in
              case (column, row) of
                   (SOME column, SOME row) =>
                     if row <= 0 then NONE
                     else SOME {column = column, row = row}
                 | _ => NONE
            end
        end
end

structure Ref :> sig
  eqtype t
  val top : t -> int
  val bottom : t -> int
  val left : t -> ColumnName.t
  val right : t -> ColumnName.t
  val topLeft : t -> CellRef.t
  val bottomRight : t -> CellRef.t
  val has : t * CellRef.t -> bool
  val toString : t -> string
  val fromString : string -> t option
end = struct
  type t = CellRef.t * CellRef.t

  fun top (topLeft, _) = CellRef.row topLeft
  fun bottom (_, bottomRight) = CellRef.row bottomRight
  fun left (topLeft, _) = CellRef.column topLeft
  fun right (_, bottomRight) = CellRef.column bottomRight

  fun topLeft (topLeft, _) = topLeft
  fun bottomRight (_, bottomRight) = bottomRight

  fun has (r, cellRef) =
        let
          val cellRow = CellRef.row cellRef
          val cellCol = CellRef.column cellRef
        in
                  top r   <= cellRow
          andalso cellRow <= bottom r
          andalso ColumnName.<= (left r, cellCol)
          andalso ColumnName.<= (cellCol, right r)
        end

  fun toString (topLeft, rightBottom) =
        CellRef.toString topLeft ^ ":" ^ CellRef.toString rightBottom
  fun fromString s =
        let
          val cellRefs = String.fields (fn c => c = #":") s
        in
          if length cellRefs <> 2 then NONE
          else
            let
              val topLeft = CellRef.fromString (List.nth (cellRefs, 0))
              val rightBottom = CellRef.fromString (List.nth (cellRefs, 1))
            in
              case (topLeft, rightBottom) of
                   (SOME topLeft, SOME rightBottom) =>
                     SOME (topLeft, rightBottom)
                 | _ => NONE
            end
        end
end

structure SpreadSheet :> sig
  structure RichString : sig
    type t
    val toString : t -> string
  end

  structure Formula : sig
    type t
  end

  structure CellValue : sig
    datatype t = Empty
               | Boolean of bool
               | Number of string
               | Error of string
               | String of RichString.t
               | Formula of Formula.t

    val toString : t -> string
    val toInt : t -> int
  end

  structure Cell : sig
    type t = { value : CellValue.t }

    val value : t -> CellValue.t
    val toString : t -> string
  end

  structure SharedStrings : sig
    type t
    val sub : t * int -> RichString.t
  end

  structure Worksheet : sig
    type t
    type range

    val fromNav : ZipNavigator.navigator * SharedStrings.t -> t
    val cell : t -> CellRef.t -> Cell.t
    val cellValue : t -> CellRef.t -> CellValue.t
    val range : t * Ref.t -> range
  end
  where type t = { nav : ZipNavigator.navigator,
                   worksheet : CT.CT_Worksheet,
                   sharedStrings : SharedStrings.t }

  structure Workbook : sig
    type t
    val openIn : string -> t
    val closeIn : t -> unit
    val worksheets : t -> Worksheet.t list
    val worksheetById : t -> string -> Worksheet.t
    val worksheetBySheetId : t -> LargeWord.word -> Worksheet.t option
    val worksheetByName : t -> string -> Worksheet.t option
  end
  where type t = { package : ZipPackage.package,
                   nav : ZipNavigator.navigator,
                   workbook : CT.CT_Workbook,
                   sharedStrings : SharedStrings.t }
end = struct
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

  structure Worksheet = struct
    type t = {
      nav : ZipNavigator.navigator,
      worksheet : CT.CT_Worksheet,
      sharedStrings : SharedStrings.t }

    type range = { base : t, r : Ref.t }

    fun range (base, r) = {base = base, r = r}

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
