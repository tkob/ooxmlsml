structure PartIRI = struct
  type segment = string
  type iri = segment list

  fun isIpchar c = c <> #"/" (* TODO: tentative implementation *)
  fun onlyIpchars s = List.all isIpchar (explode s)

  fun fromString s =
        let
          fun isEmpty s = String.size s = 0 
          val _ = if isEmpty s then raise Fail "[M1.1]" else ()

          fun delimiter c = c = #"/"
          val prefix::segments = String.fields delimiter s
          val _ = if not (isEmpty prefix) then raise Fail "[M1.4]" else ()

          fun onlyDots s = List.all (fn c => c = #".") (explode s)
          fun endsWithDot s = String.sub (s, String.size s - 1) = #"."
        in
          if isEmpty (List.last segments) then raise Fail "[M1.5]"
          else if List.exists isEmpty segments then raise Fail "[M1.3]"
          else if not (List.all onlyIpchars segments) then raise Fail "[M1.6]"
          (* TODO: [M1.7] and [M1.8] not implemented yet *)
          else if List.exists onlyDots segments then raise Fail "[M1.10]"
          else if List.exists endsWithDot segments then raise Fail "[M1.9]"
          else segments
        end

  val toString = String.concatWith "/"

  (* unit tests *)
  datatype result = Right of iri | Left of string
  fun fs_ s = Right (fromString s) handle Fail s => Left s
  val Left "[M1.1]" = fs_ ""
  val Left "[M1.4]" = fs_ "a"
  val Left "[M1.4]" = fs_ "a/b"
  val Left "[M1.5]" = fs_ "/"
  val Left "[M1.5]" = fs_ "/a/"
  val Left "[M1.5]" = fs_ "/a/b/"
  val Left "[M1.3]" = fs_ "//b"
  val Left "[M1.3]" = fs_ "/a//b"
  val Left "[M1.10]" = fs_ "/."
  val Left "[M1.10]" = fs_ "/.."
  val Left "[M1.9]" = fs_ "/a."
  val Left _ = fs_ "//xml/."
  val Right ["a"] = fs_ "/a"
  val Right ["a", "b"] = fs_ "/a/b"
  (* val Right ["a", "%D1%86.xml"] = fs_ "/a/%D1%86.xml" *)
  val Right ["xml", "item1.xml"] = fs_ "/xml/item1.xml"
end

structure ContentType = struct
  type t = {
    typ : string,
    subtype : string,
    parameters : (string * string) list }

  val toLower = implode o (map Char.toLower) o explode

  (* separators from RFC2616 2.2 *)
  fun isSeparator #"(" = true
    | isSeparator #")" = true
    | isSeparator #"<" = true
    | isSeparator #">" = true
    | isSeparator #"@" = true
    | isSeparator #"," = true
    | isSeparator #";" = true
    | isSeparator #":" = true
    | isSeparator #"\\" = true
    | isSeparator #"\"" = true
    | isSeparator #"/" = true
    | isSeparator #"[" = true
    | isSeparator #"]" = true
    | isSeparator #"?" = true
    | isSeparator #"=" = true
    | isSeparator #"{" = true
    | isSeparator #"}" = true
    | isSeparator #" " = true
    | isSeparator #"\t" = true
    | isSeparator _ = false

  fun isTokenChar c =
        Char.isAscii c andalso not (Char.isCntrl c) andalso not (isSeparator c)

  fun parseMediaType s =
        let
          val (typ, s') = parseToken [] s
          val s'' = parseChar #"/" s'
          val (subtype, s''') = parseToken [] s''
          val parameters = parseParameters [] s'''
        in
          { typ = toLower typ,
            subtype = toLower subtype,
            parameters = parameters }
        end
  and parseChar c s =
        case Substring.getc s of
             NONE => raise Fail (String.str c ^ "expected but EOS")
           | SOME (c', s') =>
               if c = c' then s'
               else raise Fail (String.str c ^ "expected but " ^ String.str c')
  and parseToken cs s =
        case Substring.getc s of
             NONE => (implode (rev cs), s)
           | SOME (c, s') =>
               if isTokenChar c then parseToken (c::cs) s'
               else (implode (rev cs), s)
  and parseParameters params s =
        if Substring.isEmpty s then rev params
        else
          let
            val (param, s') = parseParameter s
          in
            parseParameters (param::params) s'
          end
  and parseParameter s =
        let
          val (attribute, s') = parseToken [] s
          val s'' = parseChar #"=" s'
          val (value, s''') = parseValue [] s
        in
          ((toLower attribute, value), s''')
        end
  and parseValue cs s = parseToken cs s (* TODO: tentative *)

  fun fromString s = parseMediaType (Substring.full s)

  fun toString (t : t) =
        let
          fun paramToString (attr, value) = ";" ^ attr ^ "=" ^ value
          val params = String.concat (map paramToString (#parameters t))
        in
          #typ t ^ #subtype t ^ params
        end

  (* unit tests *)
  val { typ = "application", subtype = "xml", parameters = [] } =
    fromString "application/xml"
  val { typ = "application",
        subtype = "vnd.openxmlformats-officedocument.spreadsheetml.printersettings",
        parameters = [] } =
    fromString "application/vnd.openxmlformats-officedocument.spreadsheetml.printerSettings"
end

structure Opc = struct
  (* 9.1 Parts
   * A part is a stream of bytes with properties *)
  type part = {
    stream : Word8Vector.vector,
    (* [M1.1] *)
    name : PartIRI.iri,
    (* [M1.2] *)
    contentType : ContentType.t }
end

structure Relationship = struct
  datatype targetmode = Internal | External
  type uri = string
  type relationship = {
    targetMode : targetmode option,
    target : uri,
    typ : string,
    id : string }

  fun fromReader input1 instream : relationship list =
        let
          open UXML.Path
          infix |>
          val doc = UXML.Path.fromDocument (UXML.parseDocument input1 instream)
          val ns =
                "http://schemas.openxmlformats.org/package/2006/relationships"
          fun toRelationship node =
                let
                  fun stringToTargetMode "Internal" = Internal
                    | stringToTargetMode "External" = External
                    | stringToTargetMode s =
                        raise Fail ("Invalid TargetMode: " ^ s)
                  val targetMode = node |> getAttr "TargetMode"
                                        |> Option.map stringToTargetMode
                  val target = node |> getAttr "Target" |> Option.valOf
                  val typ = node |> getAttr "Type" |> Option.valOf
                  val id = node |> getAttr "Id" |> Option.valOf
                in
                  { targetMode = targetMode,
                    target = target,
                    typ = typ,
                    id = id }
                end
        in
          doc |> childNS (ns, "Relationships")
              |> childNS (ns, "Relationship")
              |> map toRelationship
        end
end

structure ContentTypeStream = struct
  type t = PartIRI.iri -> ContentType.t option

  fun fromReader input1 instream : t =
        let
          open UXML.Path
          infix |>
          val doc = UXML.Path.fromDocument (UXML.parseDocument input1 instream)
          val ns =
                "http://schemas.openxmlformats.org/package/2006/content-types"
          type default = { extension : string, contentType : ContentType.t }
          fun toDefault node : default =
                let
                  val extension = node |> getAttr "Extension"
                                       |> Option.valOf
                                       |> String.map Char.toLower
                  val contentType = node |> getAttr "ContentType"
                                         |> Option.valOf
                                         |> ContentType.fromString
                in
                  { extension = extension,
                    contentType = contentType }
                end
          type override = { partName : PartIRI.iri, contentType : ContentType.t }
          fun toOverride node : override =
                let
                  val partName = node |> getAttr "PartName"
                                      |> Option.valOf
                                      |> String.map Char.toLower
                                      |> PartIRI.fromString
                  val contentType = node |> getAttr "ContentType"
                                         |> Option.valOf
                                         |> ContentType.fromString
                in
                  { partName = partName,
                    contentType = contentType }
                end
          val defaults = doc |> childNS (ns, "Types")
                             |> childNS (ns, "Default")
                             |> map toDefault
          val overrides = doc |> childNS (ns, "Types")
                              |> childNS (ns, "Override")
                              |> map toOverride
          (* 10.1.2.4 Getting the Content Type of a Part [M2.9] *)
          fun getContentType partName =
                let
                  val partName = List.map (String.map Char.toLower) partName
                  fun matchPartName (override : override) =
                        partName = #partName override
                in
                  case List.find matchPartName overrides of
                       SOME override => SOME (#contentType override)
                     | NONE =>
                         case OS.Path.ext (List.last partName) of
                              NONE => NONE
                            | SOME extension =>
                                let
                                  fun matchExtension (default : default) =
                                        extension = #extension default
                                in
                                  Option.map
                                    #contentType
                                    (List.find matchExtension defaults)
                                end
                end
        in
          getContentType
        end
end
