# Setup

```
- CM.make "spreadsheet.cm";
...
val it = true : bool
- val wb = SpreadSheet.Workbook.openIn "excel3.xlsx";
...
- val ws = SpreadSheet.Workbook.worksheetBySheetId wb 0w1;
...
```

# read

```
- SpreadSheet.Worksheet.cell (Option.valOf ws) (Option.valOf (CellRef.fromString "A1"));
[autoloading]
[autoloading done]
val it =
  {value=String (CT_Rst {dummy=(),phoneticPr=NONE,r=[#,#,#],rPh=[],t=NONE})}
  : SpreadSheet.Cell.t
```

# read

```
- SpreadSheet.Worksheet.cell (Option.valOf ws) (Option.valOf (CellRef.fromString "C2"));
val it =
  {value=String (CT_Rst {dummy=(),phoneticPr=SOME #,r=[],rPh=[],t=SOME #})}
  : SpreadSheet.Cell.t
```

# read

```
- SpreadSheet.Worksheet.cell (Option.valOf ws) (Option.valOf (CellRef.fromString "B4"));
val it =
  {value=String (CT_Rst {dummy=(),phoneticPr=SOME #,r=[],rPh=[],t=SOME #})}
  : SpreadSheet.Cell.t
```

# read

```
- SpreadSheet.Worksheet.cell (Option.valOf ws) (Option.valOf (CellRef.fromString "D2"));
val it =
  {value=String (CT_Rst {dummy=(),phoneticPr=SOME #,r=[],rPh=[],t=SOME #})}
  : SpreadSheet.Cell.t
```
