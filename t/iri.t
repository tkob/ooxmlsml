# Setup

```
- CM.make "opc.cm";
...
val it = true : bool
- val base : IRI.iri = { scheme = NONE, authority = NONE, path = ["", "b", "c", "d;p"], query = NONE, fragment = NONE };
val base =
  {authority=NONE,fragment=NONE,path=["","b","c","d;p"],query=NONE,
   scheme=NONE} : IRI.iri
```

# Remove dot segments /a/b/c/./../../g

```
- IRI.normalize (IRI.parseIrelativeRef "/a/b/c/./../../g");
val it =
  {authority=NONE,fragment=NONE,path=["","a","g"],query=NONE,scheme=NONE}
  : IRI.iri
```

# Remove dot segments mid/content=5/../6

```
- IRI.normalize (IRI.parseIrelativeRef "mid/content=5/../6");
val it =
  {authority=NONE,fragment=NONE,path=["mid","6"],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + g

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "g"};
val it =
  {authority=NONE,fragment=NONE,path=["","b","c","g"],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + ./g

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "./g"};
val it =
  {authority=NONE,fragment=NONE,path=["","b","c","g"],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + g/

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "g/"};
val it =
  {authority=NONE,fragment=NONE,path=["","b","c","g",""],query=NONE,
   scheme=NONE} : IRI.iri
```

# /b/c/d;p + /g

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "/g"};
val it = {authority=NONE,fragment=NONE,path=["","g"],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + ;x

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef ";x"};
val it =
  {authority=NONE,fragment=NONE,path=["","b","c",";x"],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + g;x

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "g;x"};
val it =
  {authority=NONE,fragment=NONE,path=["","b","c","g;x"],query=NONE,
   scheme=NONE} : IRI.iri
```

# /b/c/d;p + ""

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef ""};
val it =
  {authority=NONE,fragment=NONE,path=["","b","c","d;p"],query=NONE,
   scheme=NONE} : IRI.iri
```

# /b/c/d;p + .

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "."};
val it =
  {authority=NONE,fragment=NONE,path=["","b","c",""],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + ./

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "./"};
val it =
  {authority=NONE,fragment=NONE,path=["","b","c",""],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + ..

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef ".."};
val it =
  {authority=NONE,fragment=NONE,path=["","b",""],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + ../

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../"};
val it =
  {authority=NONE,fragment=NONE,path=["","b",""],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + ../g

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../g"};
val it =
  {authority=NONE,fragment=NONE,path=["","b","g"],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + ../..

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../.."};
val it = {authority=NONE,fragment=NONE,path=["",""],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + ../../

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../../"};
val it = {authority=NONE,fragment=NONE,path=["",""],query=NONE,scheme=NONE}
  : IRI.iri
```

# /b/c/d;p + ../../g

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../../g"};
val it = {authority=NONE,fragment=NONE,path=["","g"],query=NONE,scheme=NONE}
  : IRI.iri
```

# too many ..'s

```
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../../../g"};
val it = {authority=NONE,fragment=NONE,path=["","g"],query=NONE,scheme=NONE}
  : IRI.iri
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../../../../g"};
val it = {authority=NONE,fragment=NONE,path=["","g"],query=NONE,scheme=NONE}
  : IRI.iri
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../../../"};
val it = {authority=NONE,fragment=NONE,path=["",""],query=NONE,scheme=NONE}
  : IRI.iri
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../../.."};
val it = {authority=NONE,fragment=NONE,path=["",""],query=NONE,scheme=NONE}
  : IRI.iri
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../../../../"};
val it = {authority=NONE,fragment=NONE,path=["",""],query=NONE,scheme=NONE}
  : IRI.iri
- IRI.resolve {relativeTo = base, iri = IRI.parseIrelativeRef "../../../.."};
val it = {authority=NONE,fragment=NONE,path=["",""],query=NONE,scheme=NONE}
  : IRI.iri
```
