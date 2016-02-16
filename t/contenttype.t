# Setup

```
- CM.make "opc.cm";
...
val it = true : bool
```

# application/xml

```
- ContentType.fromString "application/xml";
val it = {parameters=[],subtype="xml",typ="application"}
  : {parameters:(string * string) list, subtype:string, typ:string}
```

# application/vnd.openxmlformats-officedocument.spreadsheetml.printerSettings

```
- ContentType.fromString "application/vnd.openxmlformats-officedocument.spreadsheetml.printerSettings";
val it =
  {parameters=[],
   subtype="vnd.openxmlformats-officedocument.spreadsheetml.printersettings",
   typ="application"}
  : {parameters:(string * string) list, subtype:string, typ:string}
```
