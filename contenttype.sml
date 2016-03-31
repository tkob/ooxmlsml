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

