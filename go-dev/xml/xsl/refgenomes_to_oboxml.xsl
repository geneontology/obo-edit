<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="html"/>

  <xsl:template match="/">
    <obo>
      <xsl:apply-templates select="//homologset"/>
    </obo>
  </xsl:template>

  <xsl:template match="homologset">
    <!-- we create a term for the disease/homology set/human gene -->
    <instance>
      <id>
        <xsl:value-of select="concat('refgenomes:homolannot-',@id)"/>
      </id>
      <instance_of>oban:Annotation</instance_of>
    </instance>
    <term>
      <id>
        <xsl:value-of select="@id"/>
      </id>
      <name>
        <xsl:value-of select="tagval[@type='HS_gene_symbol']"/>
      </name>
      <namespace>
        <xsl:text>reference_genome</xsl:text>
      </namespace>
      <def>
        <defstr>
          <xsl:value-of select="tagval[@type='OMIM_Disease_name']"/>
        </defstr>
      </def>
      <is_a>
        <xsl:text>REFGENOMES:1</xsl:text>
      </is_a>
      <xsl:for-each select="member">
        <term>
          <id>
            <xsl:value-of select="@ref"/>
          </id>
          <relationship>
            <xsl:attribute name="id">
              <xsl:value-of select="concat('refgenomes:homolannot-',../@id)"/>
            </xsl:attribute>
            <type>
              <xsl:text>oban:evoluationary_ancestor_of</xsl:text>
            </type>
            <to>
              <xsl:value-of select="../@id"/>
            </to>
          </relationship>
        </term>
      </xsl:for-each>
    </term>
  </xsl:template>

</xsl:stylesheet>
