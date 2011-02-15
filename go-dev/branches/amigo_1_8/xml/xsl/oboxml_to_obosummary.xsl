<?xml version = "1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output indent="yes" method="text"/>

  <xsl:key name="terms" match="term" use="id"/>

  <xsl:template match="/">
    <xsl:text>format-version: 1.0</xsl:text>
    <xsl:apply-templates select="obo/term"/>
  </xsl:template>

  <xsl:template match="term">
    <xsl:text>[Term]</xsl:text>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>id: </xsl:text>
    <xsl:value-of select="id"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>name: </xsl:text>
    <xsl:value-of select="id"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>namespace: </xsl:text>
    <xsl:value-of select="namespace"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates select="def"/>
    <xsl:apply-templates select="synonym"/>
    <xsl:apply-templates select="alt_id"/>
    <xsl:apply-templates select="xref_analog"/>
    <xsl:apply-templates select="is_a"/>
    <xsl:apply-templates select="relationship"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="is_a">
    <xsl:text>is_a: </xsl:text>
    <xsl:value-of select="."/>
    <xsl:text> </xsl:text>
    <xsl:text> ! </xsl:text>
    <xsl:value-of select="key('terms', .)/name"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="relationship">
    <xsl:text>relationship: </xsl:text>
    <xsl:value-of select="type"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="to"/>
    <xsl:text> ! </xsl:text>
    <xsl:value-of select="key('terms', to)/name"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="synonym">
    <xsl:text>synonym: "</xsl:text>
    <xsl:value-of select="synonym_text"/>
    <xsl:text>"</xsl:text>
    <xsl:text> []&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template match="def">
    <xsl:text>def: "</xsl:text>
    <xsl:value-of select="defstr"/>
    <xsl:text>" </xsl:text>
    <xsl:text>[</xsl:text>
    <xsl:apply-templates select="dbxref"/>
    <xsl:text>]&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template match="dbxref">
    <xsl:value-of select="dbname"/>
    <xsl:text>:</xsl:text>
    <xsl:value-of select="acc"/>
  </xsl:template>
  
  <xsl:template match="xref_analog">
    <xsl:text>xref_analog: </xsl:text>
    <xsl:value-of select="dbname"/>
    <xsl:text>:</xsl:text>
    <xsl:value-of select="acc"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template match="alt_id">
    <xsl:text>alt_id: </xsl:text>
    <xsl:value-of select="alt_id"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template match="text()|@*">
  </xsl:template>

</xsl:stylesheet>



