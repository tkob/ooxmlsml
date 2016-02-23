<xsl:stylesheet
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:xsd="http://www.w3.org/2001/XMLSchema"
        version="1.0">
        <xsl:output method="text" /> 

        <xsl:template match="/xsd:schema">
                <xsl:apply-templates select="xsd:simpleType"/>
        </xsl:template>
        
        <xsl:template match="xsd:simpleType">
                <xsl:text>structure </xsl:text>
                <xsl:value-of select="@name"/>
                <xsl:text> = struct&#10; </xsl:text>
                <xsl:apply-templates select="*"/>
                <xsl:text>end&#10;&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:restriction">
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
                                <xsl:value-of select="@base"/>
                                <xsl:text>.t</xsl:text>
                        </xsl:otherwise>
                </xsl:choose>
                <xsl:text>&#10;</xsl:text>
        </xsl:template>

        <xsl:template match="xsd:union">
                <xsl:text>type t = string&#10;</xsl:text>
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
                        <xsl:otherwise>
                                <xsl:value-of select="@value"/>
                        </xsl:otherwise>
                </xsl:choose>
        </xsl:template>
</xsl:stylesheet>
