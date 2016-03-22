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

  val parseIri : string -> iri
  val parseIrelativeRef : string -> iri
  val parseIriReference : string -> iri

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

  fun parseIri s = parse iri s
  fun parseIrelativeRef s = parse irelativeRef s
  fun parseIriReference s = parse iriReference s

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
