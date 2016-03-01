# Setup

```
- CM.make "spreadsheet.cm";
...
val it = true : bool
```

# toString

```
- ColumnName.toString (ColumnName.fromInt 1);
val it = "A" : string
- ColumnName.toString (ColumnName.fromInt 26);
val it = "Z" : string
- ColumnName.toString (ColumnName.fromInt 27);
val it = "AA" : string
- ColumnName.toString (ColumnName.fromInt 52);
val it = "AZ" : string
- ColumnName.toString (ColumnName.fromInt 53);
val it = "BA" : string
- ColumnName.toString (ColumnName.fromInt 16384);
val it = "XFD" : string
```

# fromString

```
- Option.map ColumnName.toInt (ColumnName.fromString "A");
...
val it = SOME 1 : int option
- Option.map ColumnName.toInt (ColumnName.fromString "Z");
val it = SOME 26 : int option
- Option.map ColumnName.toInt (ColumnName.fromString "AA");
val it = SOME 27 : int option
- Option.map ColumnName.toInt (ColumnName.fromString "AZ");
val it = SOME 52 : int option
- Option.map ColumnName.toInt (ColumnName.fromString "BA");
val it = SOME 53 : int option
- Option.map ColumnName.toInt (ColumnName.fromString "XFD");
val it = SOME 16384 : int option
```
