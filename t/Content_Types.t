# Setup

```
- CM.make "opc.cm";
...
val it = true : bool
- fun parse fileName = ContentTypeStream.fromReader TextIO.StreamIO.input1 ((TextIO.getInstream o TextIO.openIn) fileName);
...
val parse = fn : string -> ContentTypeStream.t
```

# Example 10–7. Content Types stream markup

```
- val types = parse "t/fixture/Content_Types-10-7.xml";
val types = fn : ContentTypeStream.t
- types (PartName.fromString "/a/b/sample1.txt");
val it = SOME {parameters=[],subtype="plain",typ="text"}
  : ContentType.t option
- types (PartName.fromString "/a/b/sample2.jpeg");
val it = SOME {parameters=[],subtype="jpeg",typ="image"}
  : ContentType.t option
- types (PartName.fromString "/a/b/sample3.picture");
val it = SOME {parameters=[],subtype="gif",typ="image"} : ContentType.t option
- types (PartName.fromString "/a/b/sample4.picture");
val it = SOME {parameters=[],subtype="jpeg",typ="image"}
  : ContentType.t option
```
