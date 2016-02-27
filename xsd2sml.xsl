<xsl:stylesheet
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:xsd="http://www.w3.org/2001/XMLSchema"
        version="1.0">
        <xsl:output method="text" /> 

        <xsl:template match="/xsd:schema">
                <xsl:apply-templates select="xsd:simpleType"/>
                <xsl:choose>
                        <xsl:when test="xsd:complexType">
                                <xsl:text>structure CT = struct&#10;</xsl:text>
                                <xsl:text>  open UXML.Path&#10;</xsl:text>
                                <xsl:text>  infix |>&#10;</xsl:text>
                                <xsl:text>  val main = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"&#10;</xsl:text>
                                <xsl:text>  fun flatMap f NONE = NONE | flatMap f (SOME v) = f v&#10;</xsl:text>
                                <xsl:text>&#10;</xsl:text>
                                <xsl:apply-templates select="xsd:complexType"/>
                                <xsl:text>&#10;</xsl:text>
                                <xsl:apply-templates select="xsd:complexType" mode="fromNode"/>
                                <xsl:text>end&#10;</xsl:text>
                        </xsl:when>
                </xsl:choose>
        </xsl:template>
        
        <xsl:template match="xsd:simpleType">
                <xsl:text>structure </xsl:text>
                <xsl:value-of select="@name"/>
                <xsl:text> = struct&#10;</xsl:text>
                <xsl:apply-templates select="*">
                        <xsl:with-param name="typeName" select="@name"/>
                </xsl:apply-templates>
                <xsl:text>end&#10;&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:restriction">
                <xsl:param name="typeName"/>
                <xsl:variable name="t">
                        <xsl:choose>
                                <xsl:when test="contains(@base, ':')">
                                        <xsl:value-of select="substring-after(@base, ':')"/>
                                </xsl:when>
                                <xsl:otherwise>
                                        <xsl:value-of select="@base"/>
                                </xsl:otherwise>
                        </xsl:choose>
                </xsl:variable>
                <xsl:choose>
                        <xsl:when test="xsd:enumeration">
                                <xsl:text>  datatype t = </xsl:text>
                                <xsl:apply-templates select="*"/>
                                <xsl:text>&#10;</xsl:text>
                                <xsl:apply-templates select="*" mode="fromString"/>
                                <xsl:text>    | fromString s'' = raise Fail (s'' ^ " not allowed for </xsl:text>
                                <xsl:value-of select="$typeName"/>
                                <xsl:text>")&#10;</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:string'">
                                <xsl:text>  type t = string (* xsd:string *)&#10;</xsl:text>
                                <xsl:text>  fun fromString (s : string) : t = s&#10;</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:hexBinary'">
                                <xsl:text>  type t = string (* xsd:hexBinary *)&#10;</xsl:text>
                                <xsl:text>  fun fromString (s : string) : t = s&#10;</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:token'">
                                <xsl:text>  type t = string (* xsd:token *)&#10;</xsl:text>
                                <xsl:text>  fun fromString (s : string) : t = s&#10;</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:NCName'">
                                <xsl:text>  type t = string (* xsd:NCName *)&#10;</xsl:text>
                                <xsl:text>  fun fromString (s : string) : t = s&#10;</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:integer'">
                                <xsl:text>  type t = string (* xsd:integer *)&#10;</xsl:text>
                                <xsl:text>  fun fromString (s : string) : t = s&#10;</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:unsignedInt'">
                                <xsl:text>  type t = string (* xsd:unsignedInt *)&#10;</xsl:text>
                                <xsl:text>  fun fromString (s : string) : t = s&#10;</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:unsignedLong'">
                                <xsl:text>  type t = string (* xsd:unsignedLong *)&#10;</xsl:text>
                                <xsl:text>  fun fromString (s : string) : t = s&#10;</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:text>  type t = </xsl:text>
                                <xsl:value-of select="$t"/>
                                <xsl:text>.t&#10;</xsl:text>
                                <xsl:text>  val fromString = </xsl:text>
                                <xsl:value-of select="$t"/>
                                <xsl:text>.fromString&#10;</xsl:text>
                        </xsl:otherwise>
                </xsl:choose>
        </xsl:template>

        <xsl:template match="xsd:union">
                <xsl:text>type t = string (* xsd:union *)&#10;</xsl:text>
                <xsl:text>fun fromString s = s&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:list">
                <xsl:text>type t = </xsl:text>
                <xsl:value-of select="@itemType"/>
                <xsl:text>.t list&#10;</xsl:text>
                <xsl:text>fun fromString s = raise Fail "unimplemented"&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:enumeration">
                <xsl:choose>
                        <xsl:when test="position() != 1">
                                <xsl:text> | </xsl:text>
                        </xsl:when>
                </xsl:choose>
                <xsl:choose>
                        <xsl:when test="@value = ''">
                                <xsl:text>Blank</xsl:text>
                        </xsl:when>
                        <xsl:when test="number(substring(@value, 1, 1))">
                                <xsl:text>C_</xsl:text>
                                <xsl:value-of select="@value"/>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:value-of select="@value"/>
                        </xsl:otherwise>
                </xsl:choose>
        </xsl:template>

        <xsl:template match="xsd:enumeration" mode="fromString">
                <xsl:choose>
                        <xsl:when test="position() = 1">
                                <xsl:text>  fun fromString </xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:text>    | fromString </xsl:text>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:text>"</xsl:text>
                <xsl:value-of select="@value"/>
                <xsl:text>" = </xsl:text>
                <xsl:choose>
                        <xsl:when test="@value = ''">
                                <xsl:text>Blank</xsl:text>
                        </xsl:when>
                        <xsl:when test="number(substring(@value, 1, 1))">
                                <xsl:text>C_</xsl:text>
                                <xsl:value-of select="@value"/>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:value-of select="@value"/>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:text>&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:complexType">
                <xsl:choose>
                        <xsl:when test="position() = 1">
                                <xsl:text>  datatype </xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:text>  and </xsl:text>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:value-of select="@name"/>
                <xsl:text> = </xsl:text>
                <xsl:value-of select="@name"/>
                <xsl:text> of {&#10;</xsl:text>
                <xsl:apply-templates select="xsd:attribute"/>
                <xsl:apply-templates select="xsd:sequence"/>
                <xsl:text>    dummy : unit }&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:attribute">
                <xsl:variable name="t">
                        <xsl:choose>
                                <xsl:when test="contains(@type, ':')">
                                        <xsl:value-of select="substring-after(@type, ':')"/>
                                </xsl:when>
                                <xsl:otherwise>
                                        <xsl:value-of select="@type"/>
                                </xsl:otherwise>
                        </xsl:choose>
                </xsl:variable>
                <xsl:variable name="fieldName">
                        <xsl:choose>
                                <xsl:when test="starts-with(@ref, 'r:')">
                                        <xsl:value-of select="substring-after(@ref, ':')"/>
                                </xsl:when>
                                <xsl:when test="@name = 'val'">
                                        <xsl:text>value</xsl:text>
                                </xsl:when>
                                <xsl:when test="@name = 'type'">
                                        <xsl:text>typ</xsl:text>
                                </xsl:when>
                                <xsl:when test="@name = 'and' or @name = 'local' or @name = 'in' or @name = 'end'">
                                        <xsl:value-of select="@name"/>
                                        <xsl:text>'</xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                        <xsl:value-of select="@name"/>
                                </xsl:otherwise>
                        </xsl:choose>
                </xsl:variable>
                <xsl:text>    </xsl:text>
                <xsl:value-of select="$fieldName"/>
                <xsl:text> : </xsl:text>
                <xsl:choose>
                        <xsl:when test="starts-with(@ref, 'r:')">
                                <xsl:text>string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:string'">
                                <xsl:text>string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:boolean'">
                                <xsl:text>bool</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:base64Binary'">
                                <xsl:text>string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:token'">
                                <xsl:text>string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:int'">
                                <xsl:text>string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:unsignedInt'">
                                <xsl:text>string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:unsignedShort'">
                                <xsl:text>string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:unsignedByte'">
                                <xsl:text>string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:double'">
                                <xsl:text>string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:dateTime'">
                                <xsl:text>string</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:value-of select="$t"/>
                                <xsl:text>.t</xsl:text>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:choose>
                        <xsl:when test="(count(@use) = 0 or @use != 'required') and count(@default) = 0">
                                <xsl:text> option</xsl:text>
                        </xsl:when>
                </xsl:choose>
                <xsl:text>,</xsl:text>
                <xsl:text>&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:sequence">
                <xsl:apply-templates select="xsd:element[@name != '']"/>
        </xsl:template>

        <xsl:template match="xsd:element">
                <xsl:variable name="t">
                        <xsl:choose>
                                <xsl:when test="@type = 'xsd:string'">
                                        <xsl:text>string</xsl:text>
                                </xsl:when>
                                <xsl:when test="contains(@type, ':')">
                                        <xsl:value-of select="substring-after(@type, ':')"/>
                                </xsl:when>
                                <xsl:otherwise>
                                        <xsl:value-of select="@type"/>
                                </xsl:otherwise>
                        </xsl:choose>
                </xsl:variable>
                <xsl:variable name="fieldName">
                        <xsl:choose>
                                <xsl:when test="@name = 'val'">
                                        <xsl:text>value</xsl:text>
                                </xsl:when>
                                <xsl:when test="@name = 'type'">
                                        <xsl:text>typ</xsl:text>
                                </xsl:when>
                                <xsl:when test="@name = 'and' or @name = 'local' or @name = 'in' or @name = 'end' or @name = 'tupleCache' or @name = 'odxf' or @name = 'securityDescriptor'">
                                        <xsl:value-of select="@name"/>
                                        <xsl:text>'</xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                        <xsl:value-of select="@name"/>
                                </xsl:otherwise>
                        </xsl:choose>
                </xsl:variable>
                <xsl:text>    </xsl:text>
                <xsl:value-of select="$fieldName"/>
                <xsl:text>: </xsl:text>
                <xsl:value-of select="$t"/>
                <xsl:choose>
                        <xsl:when test="starts-with($t, 'ST_')">
                                <xsl:text>.t</xsl:text>
                        </xsl:when>
                </xsl:choose>
                <xsl:choose>
                        <xsl:when test="@minOccurs = '0' and @maxOccurs = '1'">
                                <xsl:text> option</xsl:text>
                        </xsl:when>
                </xsl:choose>
                <xsl:choose>
                        <xsl:when test="@minOccurs = '0' and @maxOccurs != '1'">
                                <xsl:text> list</xsl:text>
                        </xsl:when>
                </xsl:choose>
                <xsl:text>,</xsl:text>
                <xsl:text>&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:complexType" mode="fromNode">
                <xsl:choose>
                        <xsl:when test="position() = 1">
                                <xsl:text>  fun make_</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:text>  and make_</xsl:text>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:value-of select="@name"/>
                <xsl:text> node = &#10;</xsl:text>
                <xsl:text>        </xsl:text>
                <xsl:value-of select="@name"/>
                <xsl:text> {&#10;</xsl:text>
                <xsl:apply-templates select="xsd:attribute" mode="fromNode"/>
                <xsl:apply-templates select="xsd:sequence" mode="fromNode"/>
                <xsl:text>          dummy = () }&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:attribute" mode="fromNode">
                <xsl:variable name="t">
                        <xsl:choose>
                                <xsl:when test="contains(@type, ':')">
                                        <xsl:value-of select="substring-after(@type, ':')"/>
                                </xsl:when>
                                <xsl:otherwise>
                                        <xsl:value-of select="@type"/>
                                </xsl:otherwise>
                        </xsl:choose>
                </xsl:variable>
                <xsl:variable name="fieldName">
                        <xsl:choose>
                                <xsl:when test="starts-with(@ref, 'r:')">
                                        <xsl:value-of select="substring-after(@ref, ':')"/>
                                </xsl:when>
                                <xsl:when test="@name = 'val'">
                                        <xsl:text>value</xsl:text>
                                </xsl:when>
                                <xsl:when test="@name = 'type'">
                                        <xsl:text>typ</xsl:text>
                                </xsl:when>
                                <xsl:when test="@name = 'and' or @name = 'local' or @name = 'in' or @name = 'end'">
                                        <xsl:value-of select="@name"/>
                                        <xsl:text>'</xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                        <xsl:value-of select="@name"/>
                                </xsl:otherwise>
                        </xsl:choose>
                </xsl:variable>
                <xsl:text>          </xsl:text>
                <xsl:value-of select="$fieldName"/>
                <xsl:text> = node |> getAttrNS (main, "</xsl:text>
                <xsl:value-of select="$fieldName"/>
                <xsl:text>")</xsl:text>
                <xsl:choose>
                        <xsl:when test="(count(@use) = 0 or @use != 'required') and count(@default) > 0">
                                <xsl:text> |> (fn x => Option.getOpt (SOME x, SOME "</xsl:text>
                                <xsl:value-of select="@default"/>
                                <xsl:text>"))</xsl:text>
                        </xsl:when>
                </xsl:choose>
                <xsl:choose>
                        <xsl:when test="starts-with(@ref, 'r:')">
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:string'">
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:boolean'">
                                <xsl:text> |> flatMap Bool.fromString</xsl:text>
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:base64Binary'">
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:token'">
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:int'">
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:unsignedInt'">
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:unsignedShort'">
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:unsignedByte'">
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:double'">
                        </xsl:when>
                        <xsl:when test="@type = 'xsd:dateTime'">
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:text> |> Option.map </xsl:text>
                                <xsl:value-of select="$t"/>
                                <xsl:text>.fromString</xsl:text>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:choose>
                        <xsl:when test="(count(@use) = 0 or @use != 'required') and count(@default) > 0">
                                <xsl:text> |> Option.valOf</xsl:text>
                        </xsl:when>
                        <xsl:when test="@use = 'required'">
                                <xsl:text> |> Option.valOf</xsl:text>
                        </xsl:when>
                </xsl:choose>
                <xsl:text>,</xsl:text>
                <xsl:text>&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:sequence" mode="fromNode">
                <xsl:apply-templates select="xsd:element[@name != '']" mode="fromNode"/>
        </xsl:template>

        <xsl:template match="xsd:element" mode="fromNode">
                <xsl:variable name="t">
                        <xsl:choose>
                                <xsl:when test="@type = 'xsd:string'">
                                        <xsl:text>string</xsl:text>
                                </xsl:when>
                                <xsl:when test="contains(@type, ':')">
                                        <xsl:value-of select="substring-after(@type, ':')"/>
                                </xsl:when>
                                <xsl:otherwise>
                                        <xsl:value-of select="@type"/>
                                </xsl:otherwise>
                        </xsl:choose>
                </xsl:variable>
                <xsl:variable name="fieldName">
                        <xsl:choose>
                                <xsl:when test="@name = 'val'">
                                        <xsl:text>value</xsl:text>
                                </xsl:when>
                                <xsl:when test="@name = 'type'">
                                        <xsl:text>typ</xsl:text>
                                </xsl:when>
                                <xsl:when test="@name = 'and' or @name = 'local' or @name = 'in' or @name = 'end' or @name = 'tupleCache' or @name = 'odxf' or @name = 'securityDescriptor'">
                                        <xsl:value-of select="@name"/>
                                        <xsl:text>'</xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                        <xsl:value-of select="@name"/>
                                </xsl:otherwise>
                        </xsl:choose>
                </xsl:variable>
                <xsl:text>          </xsl:text>
                <xsl:value-of select="$fieldName"/>
                <xsl:text> = node |> getChildNS (main, "</xsl:text>
                <xsl:value-of select="$fieldName"/>
                <xsl:text>")</xsl:text>
                <xsl:choose>
                        <xsl:when test="$t = 'string'">
                                <xsl:text> |> map getText |> map concat</xsl:text>
                        </xsl:when>
                        <xsl:when test="starts-with($t, 'ST_')">
                                <xsl:text> |> map getText |> map concat |> map </xsl:text>
                                <xsl:value-of select="$t"/>
                                <xsl:text>.fromString</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:text> |> map make_</xsl:text>
                                <xsl:value-of select="$t"/>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:choose>
                        <xsl:when test="@minOccurs = '0' and @maxOccurs = '1'">
                                <xsl:text> |> (fn (x::_) => SOME x | _ => NONE)</xsl:text>
                        </xsl:when>
                        <xsl:when test="@minOccurs = '0' and @maxOccurs != '1'">
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:text> |> hd</xsl:text>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:text>,</xsl:text>
                <xsl:text>&#10;</xsl:text>
        </xsl:template>

</xsl:stylesheet>
