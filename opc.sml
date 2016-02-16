structure IRI :> sig
  type segment
  type path = segment list
  type iri = {
    scheme    : string option,
    authority : string option,
    path      : path,
    query     : string option,
    fragment  : string option }

  exception IRI

  type rule

  val irelativeRef : rule
  val parse : rule -> string -> iri

  val normalize : iri -> iri
  val resolve : {iri : iri, relativeTo : iri } -> iri
end where type segment = string = struct
  type segment = string
  type path = segment list
  type iri = {
    scheme    : string option,
    authority : string option,
    path      : path,
    query     : string option,
    fragment  : string option }

  exception IRI

  type rule = Substring.substring -> iri * Substring.substring

  fun isUnreserved #"-" = true
    | isUnreserved #"." = true
    | isUnreserved #"_" = true
    | isUnreserved #"~" = true
    | isUnreserved c = Char.isAlpha c orelse Char.isDigit c

  fun isGenDelims #":" = true
    | isGenDelims #"/" = true
    | isGenDelims #"?" = true
    | isGenDelims #"#" = true
    | isGenDelims #"[" = true
    | isGenDelims #"]" = true
    | isGenDelims #"@" = true
    | isGenDelims _ = false

  fun isSubDelims #"!" = true
    | isSubDelims #"$" = true
    | isSubDelims #"&" = true
    | isSubDelims #"'" = true
    | isSubDelims #"(" = true
    | isSubDelims #")" = true
    | isSubDelims #"*" = true
    | isSubDelims #"+" = true
    | isSubDelims #"," = true
    | isSubDelims #";" = true
    | isSubDelims #"=" = true
    | isSubDelims _ = false

  fun isReserved c = isGenDelims c orelse isSubDelims c

  fun isDelimiter #"/" = true
    | isDelimiter _ = false

  (* parse scheme and colon or raise IRI *)
  fun scheme s =
        case Substring.getc s of
             NONE => raise IRI
           | SOME (car, s') =>
               if not (Char.isAlpha car) then raise IRI
               else
                 let
                   fun isSchemeChar c =
                                Char.isAlpha c
                         orelse Char.isDigit c
                         orelse c = #"+"
                         orelse c = #"-"
                         orelse c = #"."
                   val (cdr, s'') = Substring.splitl isSchemeChar s'
                 in
                   case Substring.getc s'' of
                        NONE => raise IRI
                      | SOME (colon, s''') =>
                          if colon <> #":" then raise IRI
                          else
                            (implode (car::(Substring.explode cdr)), s''')
                 end

  fun parseIpath s : path =
        if Substring.isEmpty s then []
        else map Substring.string (Substring.fields isDelimiter s)

  fun parseIrelativePart s =
        let
          val (iauthority, s') =
            if Substring.isPrefix "//" s then
              let
                fun notDelimiter c = not (isDelimiter c)
                val (iauthority, s') =
                  Substring.splitl notDelimiter (Substring.dropl isDelimiter s)
              in
                (SOME (Substring.string iauthority), s')
              end
            else (NONE, s)
          val ipath = parseIpath s'
        in
          (iauthority, ipath)
        end

  fun irelativeRef s : iri * Substring.substring =
        let
          (* irelative-ref  = irelative-part [ "?" iquery ] [ "#" ifragment ] *)
          fun notQuestionOrHash c = c <> #"?" andalso c <> #"#"
          val (irelativePart, s') = Substring.splitl notQuestionOrHash s
          val (iauthority, ipath) = parseIrelativePart irelativePart
          val iri = { scheme = NONE,
                      authority = iauthority,
                      path = ipath,
                      query = NONE,
                      fragment = NONE }
        in
          (iri ,s')
        end

  fun parse rule s =
        let
          val (iri, s') = rule (Substring.full s)
        in
          if Substring.isEmpty s' then iri
          else raise IRI
        end

  fun merge ([], r) = ""::r
    | merge (base, r) = List.revAppend (tl (rev base), r)

  fun removeDotSegments input =
        let
              (* A and D *)
          fun aAndD (".."::input) = aAndD input
            | aAndD ("."::input) = aAndD input
            | aAndD input = input
          fun loop [] output = rev output
              (* B *)
            | loop ("."::[])    output = loop [] (""::output) (* /. *)
            | loop ("."::input) output = loop input output    (* /./ *)
              (* C *)
            | loop (".."::[])    []          = loop []      []           (* C /.. *)
            | loop (".."::[])    (""::[])    = loop []      (""::""::[]) (* C /.. *)
            | loop (".."::[])    (_::output) = loop []      (""::output) (* C /.. *)
            | loop (".."::input) []          = loop input   []           (* C /../ *)
            | loop (".."::input) (""::[])    = loop input   (""::[])     (* C /../ *)
            | loop (".."::input) (_::output) = loop input   output       (* C /../ *)
              (* E *)
            | loop (segment::input) output = loop input (segment::output)
        in
          loop (aAndD input) []
        end

  (* unit tests *)
  val ["", "a", "g"] = removeDotSegments (String.fields isDelimiter "/a/b/c/./../../g")
  val ["mid", "6"] = removeDotSegments (String.fields isDelimiter "mid/content=5/../6")

  fun normalize {scheme, authority, path, query, fragment} =
        { scheme    = scheme,
          authority = authority,
          path      = removeDotSegments path,
          query     = query,
          fragment  = fragment }

  fun resolve {iri = iri as {scheme = SOME _, ...}, ...} =
        normalize iri
    | resolve {iri = iri as {authority = SOME _, ...}, ...} =
        normalize iri
    | resolve {iri = {path = [], query = NONE, fragment, ...}, relativeTo} =
        { scheme    = #scheme    relativeTo,
          authority = #authority relativeTo,
          path      = #path      relativeTo,
          query     = #query     relativeTo,
          fragment  = fragment }
    | resolve {iri = {path = [], query, fragment, ...}, relativeTo} =
        { scheme    = #scheme    relativeTo,
          authority = #authority relativeTo,
          path      = #path      relativeTo,
          query     = query,
          fragment  = fragment }
    | resolve {iri = {path = path as ""::_, query, fragment, ...}, relativeTo} =
        { scheme    = #scheme    relativeTo,
          authority = #authority relativeTo,
          path      = removeDotSegments path,
          query     = query,
          fragment  = fragment }
    | resolve {iri = {path, query, fragment, ...}, relativeTo : iri} =
        { scheme    = #scheme    relativeTo,
          authority = #authority relativeTo,
          path      = removeDotSegments (merge (#path relativeTo, path)),
          query     = query,
          fragment  = fragment }
end

structure PartName = struct
  type segment = string
  type name = segment list

  fun fromIRI ({scheme = SOME _, ...} : IRI.iri) = raise Fail "[M1.4]"
    | fromIRI ({authority = SOME _, ...}) = raise Fail "[M1.3]"
    | fromIRI ({path = [], ...} : IRI.iri) = raise Fail "[M1.1]"
    | fromIRI ({path = ""::segments, ...}) =
        let
          fun isEmpty s = String.size s = 0 
          fun onlyDots s = List.all (fn c => c = #".") (explode s)
          fun endsWithDot s = String.sub (s, String.size s - 1) = #"."
        in
          if isEmpty (List.last segments) then raise Fail "[M1.5]"
          else if List.exists isEmpty segments then raise Fail "[M1.3]"
          (* TODO: [M1.7] and [M1.8] not implemented yet *)
          else if List.exists onlyDots segments then raise Fail "[M1.10]"
          else if List.exists endsWithDot segments then raise Fail "[M1.9]"
          else segments
        end
    | fromIRI ({path = _::_, ...}) = raise Fail "[M1.4]"

  fun toIRI segments =
        { scheme = NONE,
          authority = NONE,
          path = ""::segments,
          query = NONE,
          fragment = NONE }

  fun fromString s = fromIRI (IRI.parse IRI.irelativeRef s)
  fun toString segments = "/" ^ String.concatWith "/" segments
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
end

structure Opc = struct
  (* 9.1 Parts
   * A part is a stream of bytes with properties *)
  type part = {
    stream : Word8Vector.vector,
    (* [M1.1] *)
    name : PartName.name,
    (* [M1.2] *)
    contentType : ContentType.t }
end

structure Relationship = struct
  datatype targetmode = Internal | External
  type uri = string
  type relationship = {
    targetMode : targetmode,
    target : uri,
    typ : string,
    id : string }

  fun findById ([], id) : relationship option = NONE
    | findById (rel::rels, id) =
        if id = #id rel then SOME rel
        else findById (rels, id)

  fun findByType ([], typ) : relationship option = NONE
    | findByType (rel::rels, typ) =
        if typ = #typ rel then SOME rel
        else findByType (rels, typ)

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
                                        |> (fn x => Option.getOpt (x, Internal))
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

  fun fromString s = fromReader Substring.getc (Substring.full s)
  fun fromBytes bytes = fromString (Byte.bytesToString bytes)
end

structure ContentTypeStream = struct
  type t = PartName.name -> ContentType.t option

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
          type override = { partName : PartName.name, contentType : ContentType.t }
          fun toOverride node : override =
                let
                  val partName = node |> getAttr "PartName"
                                      |> Option.valOf
                                      |> String.map Char.toLower
                                      |> PartName.fromString
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
                         let
                           fun matchExtension (default : default) =
                                 let
                                   val ext = "." ^ #extension default
                                 in
                                   String.isSuffix ext (List.last partName)
                                 end
                         in
                           Option.map
                             #contentType
                             (List.find matchExtension defaults)
                         end
                end
        in
          getContentType
        end
        handle e => raise Fail ("Error while reading Content Type stream: ["
                                ^ exnName e ^ ": " ^ exnMessage e ^ "]")

  fun fromString s = fromReader Substring.getc (Substring.full s)
  fun fromBytes bytes = fromString (Byte.bytesToString bytes)
end
