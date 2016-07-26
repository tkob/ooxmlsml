signature SPREADSHEET = sig
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
    val toBool : t -> bool
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

  structure Border : sig
    type t
  end

  structure StyleSheet : sig
    type t
    val borders : t -> Border.t Vector.vector
  end

  structure Worksheet : sig
    type t
    type range

    val fromNav : ZipNavigator.navigator * SharedStrings.t * StyleSheet.t -> t
    val cell : t -> CellRef.t -> Cell.t
    val cellValue : t -> CellRef.t -> CellValue.t
    val range : t * Ref.t -> range
    val fullRange : t -> range
    val appRange : (CellRef.t * CellValue.t -> unit) -> range -> unit
    val app : (CellRef.t * CellValue.t -> unit) -> t -> unit
  end
  where type t = { nav : ZipNavigator.navigator,
                   worksheet : CT.CT_Worksheet,
                   sharedStrings : SharedStrings.t,
                   styles : StyleSheet.t }

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
                   sharedStrings : SharedStrings.t,
                   styles : StyleSheet.t }
end
