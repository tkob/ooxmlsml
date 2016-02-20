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

  val iri : rule
  val irelativeRef : rule
  val iriReference : rule
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

  val parseIhierPart = parseIrelativePart

  fun notQuestionOrHash c = c <> #"?" andalso c <> #"#"

  fun irelativeRef s : iri * Substring.substring =
        (* irelative-ref  = irelative-part [ "?" iquery ] [ "#" ifragment ] *)
        let
          val (irelativePart, s') = Substring.splitl notQuestionOrHash s
          val (iauthority, ipath) = parseIrelativePart irelativePart
          val iri = { scheme = NONE,
                      authority = iauthority,
                      path = ipath,
                      query = NONE,
                      fragment = NONE }
        in
          (iri, s')
        end

  fun iri s =
        (* IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ] *)
        let
          val (scheme, s') = scheme s
          val (ihierPart, s'') = Substring.splitl notQuestionOrHash s'
          val (iauthority, ipath) = parseIhierPart ihierPart
          val iri = { scheme = SOME scheme,
                      authority = iauthority,
                      path = ipath,
                      query = NONE,
                      fragment = NONE }
        in
          (iri, s'')
        end

  fun iriReference s =
        (* IRI-reference  = IRI / irelative-ref *)
        iri s handle IRI => irelativeRef s

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
    target : IRI.iri,
    typ : string,
    id : string }

  fun findById ([], id) : relationship option = NONE
    | findById (rel::rels, id) =
        if id = #id rel then SOME rel
        else findById (rels, id)

  fun findByType ([], typ) : relationship list = []
    | findByType (rel::rels, typ) =
        if typ = #typ rel then rel::findByType (rels, typ)
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
                  val parseURL = if targetMode = Internal
                                 then IRI.parse IRI.irelativeRef
                                 else IRI.parse IRI.iriReference
                  val target = node |> getAttr "Target"
                                    |> Option.valOf
                                    |> parseURL
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

structure LogicalItemName = struct
  type suffix = { pieceNumber : int, last : bool }
  type t = { prefix : string, suffix : suffix option }
end

signature PACKAGE = sig
  type package
  val openIn : string -> package
  val closeIn : package -> unit
  val getPart : package * PartName.name -> Opc.part option
  val getPackageRels : package -> Relationship.relationship list
  val getRels : package * PartName.name -> Relationship.relationship list
end

structure ZipPackage :> PACKAGE = struct
  type package = {
    pkzip : Pkzip.infile,
    contentTypes : ContentTypeStream.t }

  fun openIn fileName =
        let
          val pkzip = Pkzip.openIn fileName
        in
          case Pkzip.findEntry (pkzip, "[Content_Types].xml") of
               NONE =>
                 raise Fail ("[Content_Types].xml not found in " ^ fileName)
             | SOME entry =>
                 let
                   val bytes = Pkzip.readEntry (pkzip, entry)
                   val contentTypes = ContentTypeStream.fromBytes bytes
                 in
                   { pkzip = pkzip,
                     contentTypes = contentTypes }
                 end
          handle e => (Pkzip.closeIn pkzip; raise e)
        end

  fun closeIn ({pkzip, ...} : package) =
        Pkzip.closeIn pkzip

  fun partNameToZipItemName partName = String.concatWith "/" partName

  fun getPart ({pkzip, contentTypes, ...} : package, partName) : Opc.part option =
        let
          val entries = Pkzip.entries pkzip
          val fileName = partNameToZipItemName partName
        in
          case Pkzip.findEntry (pkzip, fileName) of
               NONE => NONE
             | SOME entry =>
                 let
                   val stream = Pkzip.readEntry (pkzip, entry)
                   val contentType = Option.valOf (contentTypes partName)
                 in
                   SOME { stream = stream,
                          name = partName,
                          contentType = contentType }
                 end
        end

  fun getPackageRels package =
        let
          val partName = PartName.fromString "/_rels/.rels"
        in
          case getPart (package, partName) of
               NONE => []
             | SOME part => Relationship.fromBytes (#stream part)
        end

  fun getRels (package, partName) =
        let
          val baseIri = PartName.toIRI partName
          val relativeIri : IRI.iri =
            IRI.parse IRI.irelativeRef ("_rels/" ^ List.last partName ^ ".rels")
          val targetIri : IRI.iri =
            IRI.resolve {relativeTo = baseIri, iri = relativeIri}
        in
          case getPart (package, PartName.fromIRI targetIri) of
               NONE => []
             | SOME part => Relationship.fromBytes (#stream part)
        end
end

functor PackageNavigator(P : PACKAGE) :> sig
  type navigator

  exception PackageNavigator

  val |> : 'a * ('a -> 'b) -> 'b

  val ofPackage : P.package -> navigator

  val id : string -> navigator -> navigator
  val typ : string -> navigator -> navigator list
  val target : navigator -> IRI.iri option
  val getStream : navigator -> Word8Vector.vector
end = struct
  datatype pointer = Package of Relationship.relationship list
                   | Part of IRI.iri * Relationship.relationship list
  type navigator = P.package * pointer

  exception PackageNavigator

  infix |>
  fun a |> b = b a

  fun ofPackage package = (package, Package (P.getPackageRels package))

  fun rels (Package rels) = rels
    | rels (Part (_, rels)) = rels

  fun id id (package, pointer) =
        case Relationship.findById (rels pointer, id) of
             NONE => raise Fail ("id " ^ id ^ "not found")
           | SOME {target, ...} =>
               let
                 val partName = PartName.fromIRI target
                 val rels = P.getRels (package, partName)
               in
                 (package, Part (target, rels))
               end

  fun typ typ (package, pointer) =
        let
          val rels = Relationship.findByType (rels pointer, typ)
          fun f ({target, ...} : Relationship.relationship) =
                let
                  val baseName =
                    case pointer of
                         Package _ => IRI.parse IRI.irelativeRef  "/"
                       | Part (iri, _) => iri
                  val partIRI =
                    IRI.resolve {iri = target, relativeTo = baseName}
                  val partName = PartName.fromIRI partIRI
                  val rels = P.getRels (package, partName)
                in
                  (package, Part (partIRI, rels))
                end
        in
          map f rels
        end

  fun target (_, Package _) = NONE
    | target (_, Part (uri, _)) = SOME uri

  fun getStream (package, Package _) =
        raise Fail "getStream on a package not supported"
    | getStream (package, Part (uri, _)) =
        #stream (Option.valOf (P.getPart (package, PartName.fromIRI uri)))

end

structure ZipNavigator = PackageNavigator(ZipPackage)
