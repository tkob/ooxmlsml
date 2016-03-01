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
