# Setup

```
- CM.make "spreadsheet.cm";
...
val it = true : bool
- val f = (Option.map CellRef.toString) o  CellRef.fromString;
...
val f = fn : string -> string option
```

# valid cell names

```
- f "A1";
val it = SOME "A1" : string option
- f "Z1";
val it = SOME "Z1" : string option
- f "AA1";
val it = SOME "AA1" : string option
- f "AZ1";
val it = SOME "AZ1" : string option
- f "BA1";
val it = SOME "BA1" : string option
- f "XFD1";
val it = SOME "XFD1" : string option
```

# invalid cell names

```
- f "";
val it = NONE : string option
- f "A";
val it = NONE : string option
- f "1";
val it = NONE : string option
- f "a1";
val it = NONE : string option
- f "A0";
val it = NONE : string option
- f "A 1";
val it = NONE : string option
- f " A1";
val it = NONE : string option
- f "A1 ";
val it = NONE : string option
```
