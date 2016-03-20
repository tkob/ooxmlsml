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
