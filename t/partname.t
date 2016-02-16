# Setup

```
- CM.make "opc.cm";
...
val it = true : bool
```

# Tests

```
- PartName.fromString "";

uncaught exception Fail [Fail: [M1.1]]
...
```

```
- PartName.fromString "a";

uncaught exception Fail [Fail: [M1.4]]
...
```

```
- PartName.fromString "a/b";

uncaught exception Fail [Fail: [M1.4]]
...
```

```
- PartName.fromString "/";

uncaught exception Fail [Fail: [M1.5]]
...
```

```
- PartName.fromString "/a/";

uncaught exception Fail [Fail: [M1.5]]
...
```

```
- PartName.fromString "/a/b/";

uncaught exception Fail [Fail: [M1.5]]
...
```

```
- PartName.fromString "//b";

uncaught exception Fail [Fail: [M1.3]]
...
```

```
- PartName.fromString "/a//b";

uncaught exception Fail [Fail: [M1.3]]
...
```

```
- PartName.fromString "/.";

uncaught exception Fail [Fail: [M1.10]]
...
```

```
- PartName.fromString "/..";

uncaught exception Fail [Fail: [M1.10]]
...
```

```
- PartName.fromString "/a.";

uncaught exception Fail [Fail: [M1.9]]
...
```

```
- PartName.fromString "//xml/.";

uncaught exception Fail [Fail: [M1.3]]
...
```

```
- PartName.fromString "/a";
val it = ["a"] : string list
```

```
- PartName.fromString "/a/b";
val it = ["a","b"] : string list
```

```
- PartName.fromString "/a/%D1%86.xml";
val it = ["a","%D1%86.xml"] : string list
```

```
- PartName.fromString "/xml/item1.xml";
val it = ["xml","item1.xml"] : string list
```
