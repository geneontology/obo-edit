<?xml version = "1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- chadodb_prestore xml can be loaded into a chado db using DBIx::DBStag -->

<!-- does not build transitive closure -->

  <xsl:output indent="yes" method="xml"/>

  <xsl:key name="terms" match="term" use="id"/>

  <xsl:template match="/">
    <chadodb_prestore>
      <dbstag_metadata>
        <map>
          type/cvterm_relationship.type_id=cvterm.id
        </map>
        <map>
          type/cvtermsynonym.type_id=cvterm.id
        </map>
        <map>
          parentfk:cvterm_relationship.subject_id
        </map>
      </dbstag_metadata>
      <!-- source can appear in different obo file types -->
      <xsl:apply-templates select="*/source"/>
      <!-- terms can appear in different obo file types -->
      <xsl:apply-templates select="*/term"/>
      <!-- assocs only appear with root 'assocs' -->
      <xsl:apply-templates select="assocs/dbset/prod"/>
    </chadodb_prestore>
  </xsl:template>

  <xsl:template match="term">
    <cvterm>
      <xsl:apply-templates select="id" mode="dbxref"/>
      <name>
        <xsl:value-of select="name"/>
      </name>
      <cv>
        <name>
          <xsl:value-of select="namespace"/>
        </name>
      </cv>
      <xsl:if test="is_obsolete">
        <is_obsolete>1</is_obsolete>
      </xsl:if>
      <xsl:apply-templates select="def"/>
      <xsl:apply-templates select="comment"/>
      <xsl:apply-templates select="synonym"/>
      <xsl:apply-templates select="alt_id"/>
      <xsl:apply-templates select="xref_analog"/>
      <xsl:apply-templates select="is_a"/>
      <xsl:apply-templates select="relationship"/>
    </cvterm>
  </xsl:template>

  <xsl:template match="typedef">
    <cvterm>
      <xsl:apply-templates select="id" mode="dbxref"/>
      <name>
        <xsl:value-of select="name"/>
      </name>
      <cv>
        <name>
          <xsl:value-of select="namespace"/>
        </name>
      </cv>
      <xsl:if test="is_obsolete">
        <is_obsolete>1</is_obsolete>
      </xsl:if>
      <xsl:if test="def">
        <definition>
          <xsl:value-of select="defstr"/>
        </definition>
      </xsl:if>
      <xsl:apply-templates select="synonym"/>
      <xsl:apply-templates select="alt_id"/>
      <xsl:apply-templates select="xref_analog"/>
      <xsl:apply-templates select="is_a"/>
    </cvterm>
  </xsl:template>

  <xsl:template match="*" mode="dbxref">
    <dbxref>
      <db>
        <name>
          <xsl:value-of select="substring-before(.,':')"/>    
        </name>
      </db>
      <accession>
        <xsl:value-of select="substring-after(.,':')"/>    
      </accession>
    </dbxref>
  </xsl:template>

  <xsl:template match="is_a">
    <cvterm_relationship>
      <type>
        <cvterm>
          <cv>
            <name>relationship</name>
          </cv>
          <name>is_a</name>
        </cvterm>
      </type>
      <object>
        <cvterm>
          <xsl:apply-templates select="." mode="dbxref"/>
        </cvterm>
      </object>
    </cvterm_relationship>
    </xsl:template>

  <xsl:template match="relationship">
    <cvterm_relationship>
      <type>
        <cvterm>
          <cv>
            <name>relationship</name>
          </cv>
          <name>
            <xsl:value-of select="type"/>
          </name>
        </cvterm>
      </type>
      <object>
        <cvterm>
          <xsl:apply-templates select="to" mode="dbxref"/>
        </cvterm>
      </object>
    </cvterm_relationship>
  </xsl:template>

  <xsl:template match="synonym">
    <cvtermsynonym>
      <synonym>
        <xsl:value-of select="synonym_text"/>
      </synonym>
      <xsl:if test="type">
        <type>
          <cvterm>
            <cv>
              <name>synonym_type</name>
            </cv>
            <name>
              <xsl:value-of select="type"/>
            </name>
          </cvterm>
        </type>
      </xsl:if>
    </cvtermsynonym>
  </xsl:template>
    
  <xsl:template match="comment">
    <cvtermprop>
      <type>
        <cvterm>
          <cv>
            <name>cvterm_property</name>
          </cv>
          <name>
            comment
          </name>
        </cvterm>
      </type>
      <value>
        <xsl:value-of select="."/>
      </value>
    </cvtermprop>
  </xsl:template>
    
  <xsl:template match="xref_analog">
    <cvterm_dbxref>
      <dbxref>
        <db>
          <name>
            <xsl:value-of select="dbname"/>
          </name>
        </db>
        <accession>
            <xsl:value-of select="acc"/>
        </accession>
      </dbxref>
    </cvterm_dbxref>
  </xsl:template>
    
  <xsl:template match="dbxref" mode="is_for_definition">
    <cvterm_dbxref>
      <dbxref>
        <db>
          <name>
            <xsl:value-of select="dbname"/>
          </name>
        </db>
        <accession>
            <xsl:value-of select="acc"/>
        </accession>
      </dbxref>
      <is_for_definition>1</is_for_definition>
    </cvterm_dbxref>
  </xsl:template>
    
  <xsl:template match="alt_id">
    <cvterm_dbxref>
      <xsl:apply-templates select="." mode="dbxref"/>
    </cvterm_dbxref>
  </xsl:template>
  
  <xsl:template match="def">
    <definition>
      <xsl:value-of select="defstr"/>
    </definition>
    <xsl:apply-templates select="dbxref" mode="is_for_definition"/>
  </xsl:template>

  <xsl:template match="prod">
    <feature>
      <dbxref>
        <db>
          <name>
            <xsl:value-of select="../proddb"/>
          </name>
        </db>
        <accession>
          <xsl:value-of select="prodacc"/>
        </accession>
      </dbxref>
      <name>
        <xsl:value-of select="prodsymbol"/>
      </name>
      <uniquename>
        <xsl:value-of select="prodsymbol"/>
      </uniquename>
      <type>
        <cvterm>
          <name>
            <xsl:value-of select="prodtype"/>
          </name>
          <cv>
            <name>
              sequence
            </name>
          </cv>
        </cvterm>
      </type>
      <organism>
        <dbxref>
          <db>
            <name>
              ncbi_taxononmy
            </name>
          </db>
          <accession>
            <xsl:value-of select="prodtaxa"/>
          </accession>
        </dbxref>
      </organism>
      <xsl:apply-templates select="assoc"/>
    </feature>
  </xsl:template>

  <xsl:template match="assoc">
    <feature_cvterm>
      <cvterm>
        <xsl:apply-templates select="termacc" mode="dbxref"/>
      </cvterm>
      <xsl:apply-templates select="evidence"/>
    </feature_cvterm>
  </xsl:template>

  <xsl:template match="evidence">
    <feature_cvtermprop>
    </feature_cvtermprop>
  </xsl:template>

  <xsl:template match="text()|@*">
  </xsl:template>


</xsl:stylesheet>



