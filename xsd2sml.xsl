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
                                <xsl:apply-templates select="xsd:complexType"/>
                                <xsl:text>end&#10;</xsl:text>
                        </xsl:when>
                </xsl:choose>
        </xsl:template>
        
        <xsl:template match="xsd:simpleType">
                <xsl:text>structure </xsl:text>
                <xsl:value-of select="@name"/>
                <xsl:text> = struct&#10;</xsl:text>
                <xsl:apply-templates select="*"/>
                <xsl:text>end&#10;&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:restriction">
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
                                <xsl:text>datatype t = </xsl:text>
                                <xsl:apply-templates select="*"/>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:string'">
                                <xsl:text>type t = string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:hexBinary'">
                                <xsl:text>type t = string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:token'">
                                <xsl:text>type t = string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:NCName'">
                                <xsl:text>type t = string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:integer'">
                                <xsl:text>type t = string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:unsignedInt'">
                                <xsl:text>type t = string</xsl:text>
                        </xsl:when>
                        <xsl:when test="@base = 'xsd:unsignedLong'">
                                <xsl:text>type t = string</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:text>type t = </xsl:text>
                                <xsl:value-of select="$t"/>
                                <xsl:text>.t</xsl:text>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:text>&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:union">
                <xsl:text>type t = string&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:list">
                <xsl:text>type t = </xsl:text>
                <xsl:value-of select="@itemType"/>
                <xsl:text>.t list&#10;</xsl:text>
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

        <xsl:template match="xsd:complexType">
                <xsl:choose>
                        <xsl:when test="position() = 1">
                                <xsl:text>datatype </xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:text>and </xsl:text>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:value-of select="@name"/>
                <xsl:text> = </xsl:text>
                <xsl:value-of select="@name"/>
                <xsl:text> of {&#10;</xsl:text>
                <xsl:apply-templates select="xsd:attribute"/>
                <xsl:apply-templates select="xsd:sequence"/>
                <xsl:text>    dummy : unit&#10;</xsl:text>
                <xsl:text>  }&#10;</xsl:text>
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
                <xsl:text>,</xsl:text>
                <xsl:text>&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:sequence">
                <xsl:apply-templates select="xsd:element[@name != '']"/>
        </xsl:template>

        <xsl:template match="xsd:element">
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

</xsl:stylesheet>
