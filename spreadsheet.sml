structure ColumnName :> sig
  type t
  val toInt : t -> int
  val fromInt : int -> t
  val toString : t -> string
  val fromString : string -> t
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
          fun f ([], sum) = sum
            | f (c::cs, sum) =
                let
                  val i = Char.ord c - largeA + 1
                in
                  f (cs, sum * numAlphas + i)
                end
        in
          f (explode s, 0)
        end
end

structure SpreadSheet = struct
  val officeDocument = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
  val sharedStrings = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings"
  val main = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
  val r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"

  fun reltToString (CT.CT_RElt {t, ...}) = t
  fun rstToString (CT.CT_Rst {t, r, ...}) =
        Option.getOpt (t, "") ^ concat (map reltToString r)

  structure Worksheet :> sig
    type t
    val fromNav : ZipNavigator.navigator * CT.CT_Rst Vector.vector -> t
  end
  where type t = {
      nav : ZipNavigator.navigator,
      worksheet : CT.CT_Worksheet,
      sharedStrings : CT.CT_Rst Vector.vector }
  = struct
    type t = {
      nav : ZipNavigator.navigator,
      worksheet : CT.CT_Worksheet,
      sharedStrings : CT.CT_Rst Vector.vector }

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
  end

  structure Workbook :> sig
    type t
    val openIn : string -> t
    val closeIn : t -> unit
    val worksheets : t -> Worksheet.t list
    val worksheetById : t -> string -> Worksheet.t
    val worksheetBySheetId : t -> string -> Worksheet.t option
    val worksheetByName : t -> string -> Worksheet.t option
  end
  where type t = { package : ZipPackage.package,
                   nav : ZipNavigator.navigator,
                   workbook : CT.CT_Workbook,
                   sharedStrings : CT.CT_Rst Vector.vector }
  = struct
    type t = {
      package : ZipPackage.package,
      nav : ZipNavigator.navigator,
      workbook : CT.CT_Workbook,
      sharedStrings : CT.CT_Rst Vector.vector }

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
            val officeDocument = nav |> typ officeDocument |> hd
                  handle Empty => raise Fail "main part not found"
            val bytes = getStream officeDocument
            val doc = UXML.Path.fromDocument (UXML.parseBytes bytes)
            val workbook = CT.workbook doc
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
