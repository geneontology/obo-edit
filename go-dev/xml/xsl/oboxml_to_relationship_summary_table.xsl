<?xml version = "1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- transforms OBO XML format to a 3 column CHILD-REL-PARENT table

       cjm 2004

       -->

    <xsl:key name="terms" match="term" use="id"/>

  <xsl:output indent="yes" method="text"/>

  <xsl:template match="/">
    <xsl:text>#CHILD&#9;REL&#9;PARENT&#10;</xsl:text>
    <xsl:apply-templates select="*/term/is_a"/>
    <xsl:apply-templates select="*/term/relationship"/>
  </xsl:template>

  <xsl:template match="relationship">
    <xsl:value-of select="../id"/>
    <xsl:text> / "</xsl:text>
    <xsl:value-of select="key('terms', ../id)/name"/>
    <xsl:text>"&#9;</xsl:text>
    <xsl:value-of select="type"/>
    <xsl:text>&#9;</xsl:text>
    <xsl:value-of select="to"/>
    <xsl:text> / "</xsl:text>
    <xsl:value-of select="key('terms', to)/name"/>
    <xsl:text>"&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="is_a">
    <xsl:value-of select="../id"/>
    <xsl:text> / "</xsl:text>
    <xsl:value-of select="key('terms', ../id)/name"/>
    <xsl:text>"&#9;is_a&#9;</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text> / "</xsl:text>
    <xsl:value-of select="key('terms', .)/name"/>
    <xsl:text>"&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="text()|@*">
  </xsl:template>


</xsl:stylesheet>



