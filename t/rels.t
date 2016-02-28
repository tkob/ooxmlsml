# Setup

```
- CM.make "opc.cm";
...
val it = true : bool
- fun parse fileName = Relationship.fromReader TextIO.StreamIO.input1 ((TextIO.getInstream o TextIO.openIn) fileName);
...
val parse = fn : string -> Relationship.relationship list
```

# Example 9–4. Sample relationships and associated markup

```
- parse "t/fixture/rels001.xml";
val it =
  [{id="A5FFC797514BC",
    target={authority=NONE,fragment=NONE,path=[".","Signature.xml"],
            query=NONE,scheme=NONE},targetMode=Internal,
    typ="http://schemas.openxmlformats.org/package/2006/relationships/digital-s#"}]
  : Relationship.relationship list
```

# Example 9–5. Targeting resources

```
- parse "t/fixture/rels002.xml";
val it =
  [{id="A9EFC627517BC",
    target={authority=SOME "www.custom.com",fragment=NONE,
            path=["","images","pic1.jpg"],query=NONE,scheme=SOME "http"},
    targetMode=External,typ="http://www.custom.com/external-resource"},
   {id="A5EFC797514BC",
    target={authority=NONE,fragment=NONE,path=[".","images","my_house.jpg"],
            query=NONE,scheme=NONE},targetMode=External,
    typ="http://www.custom.com/external-resource"}]
  : Relationship.relationship list
```

# Example 9–6. Re-using attribute values

```
- parse "t/fixture/rels003.xml";
val it =
  [{id="A5FFC797514BC",
    target={authority=NONE,fragment=NONE,path=[".","Signature.xml"],
            query=NONE,scheme=NONE},targetMode=Internal,
    typ="http://schemas.openxmlformats.org/package/2006/relationships/digital-s#"},
   {id="B5F32797CC4B7",
    target={authority=NONE,fragment=NONE,path=[".","Signature.xml"],
            query=NONE,scheme=NONE},targetMode=Internal,
    typ="http://www.custom.com/internal-resource"}]
  : Relationship.relationship list
```
