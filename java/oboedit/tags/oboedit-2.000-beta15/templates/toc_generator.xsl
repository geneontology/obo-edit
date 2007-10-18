<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:param name="mapfile" select="'/home/jrichter/workspace/OBO-Edit/docs/OBO-Edit.jhm'"/>

<xsl:template match="toc">
  <html>
  <head><title>Table of Contents</title></head>
  <body>
  <small><font face="Arial"><p><center><strong>Contents</strong></center></p>

  <xsl:apply-templates/>

  </font></small>
  </body>
  </html>
</xsl:template>

<xsl:template match="tocitem">
  <table border="0" cellpadding="0" cellspacing="0" width="100%">
    <tr><td valign="baseline" width="15"><img src="page.gif" width="11" height="11" hspace="3"/></td>
        <td valign="top" width="100%">
        <font face="Arial" size="2">

  <xsl:choose>
  <xsl:when test="string-length(document($mapfile)/map/mapID[@target=current()/@target]/@url) > 0">
   <xsl:element name="a">
     <xsl:attribute name="href">
       <xsl:value-of select="document($mapfile)/map/mapID[@target=current()/@target]/@url"/>
     </xsl:attribute>
     <xsl:attribute name="target">
       <xsl:value-of select="'bodyframe'"/>
     </xsl:attribute>
     <xsl:value-of select="@text"/>
   </xsl:element>
  </xsl:when>
  <xsl:otherwise>
     <xsl:value-of select="@text"/>
  </xsl:otherwise>
  </xsl:choose>

          <xsl:apply-templates/>

        </font>
        </td>
    </tr>
  </table>
</xsl:template>

</xsl:stylesheet>