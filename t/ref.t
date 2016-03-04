# Setup

```
- CM.make "spreadsheet.cm";
...
val it = true : bool
- val f = (Option.map Ref.toString) o Ref.fromString;
...
val f = fn : string -> string option
```

# valid ref

```
- f "A1:B2";
val it = SOME "A1:B2" : string option
```

# invalid ref

```
- f "";
val it = NONE : string option
- f "A";
val it = NONE : string option
- f "1";
val it = NONE : string option
- f "A1";
val it = NONE : string option
- f "A1:";
val it = NONE : string option
- f ":A1";
val it = NONE : string option
- f " A1:B2";
val it = NONE : string option
- f "A1:B2 ";
val it = NONE : string option
- f "A1 : B2";
val it = NONE : string option
- f "a1 : b2";
val it = NONE : string option
```
