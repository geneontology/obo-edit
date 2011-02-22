<?xml version = "1.0"?>
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:obo="http://www.geneontology.org/formats/obo#"
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" 
  xmlns:owl="http://www.w3.org/2002/07/owl#"
  >
  
  <xsl:output indent="yes" method="xml"/>

  <xsl:key name="terms" match="term" use="id"/>

  <xsl:template match="/">
    <rdf:RDF
      xmlns="http://www.bioontologies.org/2006/02/obo#"
      xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
      xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
      xmlns:owl="http://www.w3.org/2002/07/owl#"
      xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
      xmlns:obo="http://www.geneontology.org/formats/obo#"
      xml:base="http://www.bioontologies.org/2006/02/obo">
      
      <!--  define OBODescriptor and synonym scope-->
  <owl:Class rdf:ID="OBODescriptor">
    <rdfs:comment> a class to collect the various symbols (instances)
      used to define synonym scope etc </rdfs:comment>
  </owl:Class>
  
  <!-- synonym scope and other OBO trailing modifiers can be organised
       into subclasses of OBODescriptor -->

  <owl:Class rdf:ID="SynonymScopeValue">
    <owl:equivalentClass>
      <owl:Class>
        <owl:oneOf rdf:parseType="Collection">
          <OBODescriptor rdf:ID="exact"/>
          <OBODescriptor rdf:ID="broad"/>
          <OBODescriptor rdf:ID="narrow"/>
        </owl:oneOf>
      </owl:Class>
    </owl:equivalentClass>
    <rdfs:subClassOf>
      <owl:Class rdf:ID="OBODescriptor"/>
    </rdfs:subClassOf>
</owl:Class>

<!-- organise named subsets in subsetdef into instances of OntologySubsetValue -->
  <owl:Class rdf:ID="OntologySubsetValue">
    <rdfs:subClassOf>
      <owl:Class rdf:ID="OBODescriptor"/>
    </rdfs:subClassOf>
  </owl:Class>

<!-- annotations that apply to the Ontology / OBO Header -->
<!-- from the XML DTD -->
  <owl:AnnotationProperty rdf:ID="format-version"/>
  <owl:AnnotationProperty rdf:ID="date"/> <!-- use dc ? -->
  <owl:AnnotationProperty rdf:ID="saved-by"/>
  <owl:AnnotationProperty rdf:ID="auto-generated-by"/>
  <owl:AnnotationProperty rdf:ID="default-namespace"/>
<!-- remark [rdfs:comment] 
  <owl:AnnotationProperty rdf:ID="remark"/> -->
  <rdf:Property rdf:ID="subsetdef">
    <rdfs:range rdf:resource="OntologySubsetValue"/>
  </rdf:Property>

<!-- annotations that apply to the term: id [URIRef], name[rdfs:label] -->
  <owl:AnnotationProperty rdf:ID="namespace"/>
<!-- def -->
  <owl:AnnotationProperty rdf:ID="has_definition"/>  
<!-- is_a [rdfs:subClassOf]-->
  <owl:AnnotationProperty rdf:ID="alt_id"/>
  <rdf:Property rdf:ID="subset">
    <rdfs:range rdf:resource="OntologySubsetValue"/>
  </rdf:Property>
<!-- comment [rdfs:comment], is_anonymous [ignore], is_obsolete [ignore], is_root [ignore]-->
  <owl:AnnotationProperty rdf:ID="has_xref_analog"/>
  <owl:AnnotationProperty rdf:ID="has_xref_unknown"/>
  <owl:AnnotationProperty rdf:ID="has_synonym"/>
<!-- relationship, intersection_of, union_of [all map to OWL constructs] -->
  <owl:AnnotationProperty rdf:ID="lexical_category"/>

<!-- define the dbxref rdf:Description -->
 <owl:AnnotationProperty rdf:ID="has_dbxref"/>
 <rdf:Property rdf:ID="acc"/>
 <rdf:Property rdf:ID="dbname"/>
 <rdf:Property rdf:ID="name"/>
 <rdf:Property rdf:ID="scope">
   <rdfs:range rdf:resource="SynonymScopeValue"/>
 </rdf:Property>
 <rdf:Property rdf:ID="synonym_category"/>

<!-- elements for OBO 1.2 header, not included in XML DTD as yet 
  <owl:AnnotationProperty rdf:ID="data-version"/>
  <owl:AnnotationProperty rdf:ID="import"/>
  <owl:AnnotationProperty rdf:ID="synonymtypedef"/> 
  <owl:AnnotationProperty rdf:ID="idspace"/> 
  <owl:AnnotationProperty rdf:ID="default-relationship-id-prefix"/> 
  <owl:AnnotationProperty rdf:ID="id-mapping"/> 
-->

<!-- elements for OBO 1.2 terms, not included in XML DTD as yet 
  <owl:AnnotationProperty rdf:ID="has_xref"/> 
  <owl:AnnotationProperty rdf:ID="replaced_by"/> 
  <owl:AnnotationProperty rdf:ID="consider"/> 
  <owl:AnnotationProperty rdf:ID="use_term"/> 
-->

      <xsl:apply-templates select="obo/header"/>
      <xsl:apply-templates select="obo/term[not(is_obsolete='1')]"/>
      <xsl:apply-templates select="obo/typedef"/>
      <xsl:apply-templates select="obo/synonym_category"/>
    </rdf:RDF>
  </xsl:template>


  <xsl:template match="header">
    <!-- create an instance for each subsetdef -->
    <xsl:for-each select="subsetdef">
      <xsl:element name="obo:OntologySubsetValue">
	<xsl:apply-templates mode="rdf-id" select="id"/>
	<rdfs:label xml:lang="en">
	  <xsl:value-of select="name"/>
	</rdfs:label>
	<xsl:for-each select="dbxref">
	  <obo:has_dbxref>
	    <rdf:Description>
	      <xsl:apply-templates select="."/>
	    </rdf:Description>
	  </obo:has_dbxref>
	</xsl:for-each>
      </xsl:element>
    </xsl:for-each>
    
<!-- supply the Ontology metadata -->
<owl:Ontology rdf:about="">
  <xsl:if test="format-version">
    <obo:format-version>
      <xsl:value-of select="format-version"/>
    </obo:format-version>
  </xsl:if>

  <xsl:if test="date">
    <obo:date>
      <xsl:value-of select="date"/>
    </obo:date>
  </xsl:if>

  <xsl:if test="saved-by">
    <obo:saved-by>
      <xsl:value-of select="saved-by"/>
    </obo:saved-by>
  </xsl:if>

  <xsl:if test="auto-generated-by">
    <obo:auto-generated-by>
      <xsl:value-of select="auto-generated-by"/>
    </obo:auto-generated-by>
  </xsl:if>

  <xsl:if test="default-namespace">
    <obo:default-namespace>
      <xsl:value-of select="default-namespace"/>
    </obo:default-namespace>
  </xsl:if>

  <xsl:if test="remark">
    <rdfs:comment rdf:datatype="http://www.w3.org/2001/XMLSchema#string">
      <xsl:value-of select="remark"/>
    </rdfs:comment>
  </xsl:if>

  <xsl:for-each select="subsetdef">
    <obo:subsetdef>
      <xsl:apply-templates mode="rdf-resource" select="id"/>
    </obo:subsetdef>
  </xsl:for-each>
      
</owl:Ontology>

  </xsl:template>
  

  <xsl:template match="synonym_category">
    <xsl:element name="OBODescriptor">
      <xsl:apply-templates mode="rdf-id" select="id"/>
      <rdfs:label xml:lang="en">
        <xsl:value-of select="name"/>
      </rdfs:label>
      <xsl:apply-templates select="namespace"/>
      </xsl:element>
  </xsl:template>

  <xsl:template match="term">
    <xsl:element name="owl:Class">
      <xsl:apply-templates mode="rdf-id" select="id"/>
      <rdfs:label xml:lang="en">
        <xsl:value-of select="name"/>
      </rdfs:label>
      <xsl:apply-templates select="comment"/>
      <xsl:apply-templates select="subset"/>
      <xsl:apply-templates select="def"/>
      <xsl:apply-templates select="synonym"/>
      <xsl:apply-templates select="namespace"/>
      <xsl:apply-templates select="alt_id"/>
      <xsl:for-each select="xref_analog">
	<obo:has_xref_analog>
	  <rdf:Description>
	    <xsl:for-each select=".">
	      <xsl:apply-templates select="."/>
	    </xsl:for-each>
	  </rdf:Description>
	</obo:has_xref_analog> 
      </xsl:for-each>
      <xsl:for-each select="xref_unknown">
	<obo:has_xref_unknown>
	  <rdf:Description>
	    <xsl:for-each select=".">
	      <xsl:apply-templates select="."/>
	    </xsl:for-each>
	  </rdf:Description>
	</obo:has_xref_unknown>
      </xsl:for-each>
      <xsl:apply-templates select="lexical_category"/>
      <xsl:apply-templates select="is_a"/>
      <xsl:apply-templates select="relationship"/>

      <!-- logical definitions -->
      <xsl:if test="count(intersection_of)>0">
        <owl:equivalentClass>
          <owl:Class>
            <owl:intersectionOf>
              <xsl:attribute name="rdf:parseType">Collection</xsl:attribute>
              <xsl:apply-templates select="intersection_of"/>
            </owl:intersectionOf>
          </owl:Class>
        </owl:equivalentClass>
      </xsl:if>

      <xsl:if test="count(union_of)>0">
        <owl:equivalentClass>
          <owl:Class>
            <owl:unionOf>
              <xsl:attribute name="rdf:parseType">Collection</xsl:attribute>
              <xsl:apply-templates select="union_of"/>
            </owl:unionOf>
          </owl:Class>
        </owl:equivalentClass>
      </xsl:if>

    </xsl:element>
  </xsl:template>

  <xsl:template match="typedef">
    <xsl:choose>
      <xsl:when test="is_transitive=1">
        <xsl:element name="owl:TransitiveProperty">
          <xsl:apply-templates mode="detail" select="."/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="owl:ObjectProperty">
          <xsl:apply-templates mode="detail" select="."/>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template mode="detail" match="typedef">
      <xsl:apply-templates mode="rdf-id" select="id"/>
      <rdfs:label>
        <xsl:value-of select="name"/>
      </rdfs:label>
      <xsl:for-each select="is_a">
        <rdfs:subPropertyOf>
          <xsl:apply-templates mode="rdf-resource" select="."/>
        </rdfs:subPropertyOf>
      </xsl:for-each>
  </xsl:template>

  <xsl:template match="instance">
    <xsl:element name="rdf:Description">
      <xsl:apply-templates mode="rdf-id" select="id"/>
      <xsl:apply-templates mode="rdf-about" select="instance_of"/>
      <xsl:if test="name">
        <rdfs:label xml:lang="en">
          <xsl:value-of select="name"/>
        </rdfs:label>
      </xsl:if>
      <xsl:apply-templates select="property_value"/>
    </xsl:element>
  </xsl:template>

  <!-- ============================================================ -->
  <!-- tags inside stanzas -->
  <!-- ============================================================ -->

  <xsl:template match="is_a">
    <!-- by default we exclude inferred is_a relations -->
    <xsl:if test="not(@novel_inferred) and not(@problematic_inferred)">
      <rdfs:subClassOf>
        <xsl:apply-templates mode="rdf-resource" select="."/>
      </rdfs:subClassOf>
    </xsl:if>
  </xsl:template>

  <xsl:template match="relationship">
    <rdfs:subClassOf>
      <xsl:apply-templates mode="restriction" select="."/>      
    </rdfs:subClassOf>
  </xsl:template>
    
  <xsl:template match="intersection_of|union_of">
    <xsl:choose>
      <!-- genus -->
      <!-- some species of obo use is_a(X) in intersection list -->
      <xsl:when test="type='is_a' or not(type)">
        <owl:Class>
          <xsl:apply-templates mode="rdf-about" select="to"/>
        </owl:Class>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates mode="restriction" select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
    
  <xsl:template mode="restriction" match="*">
    <owl:Restriction>
      <owl:onProperty>
        <owl:ObjectProperty>
          <xsl:apply-templates mode="rdf-about" select="type"/>
        </owl:ObjectProperty>
      </owl:onProperty>
      <owl:someValuesFrom>
        <xsl:apply-templates mode="rdf-resource" select="to"/>
      </owl:someValuesFrom>
    </owl:Restriction>
  </xsl:template>

  <xsl:template match="property_value">
    <xsl:element name="{property}">
      <xsl:apply-templates mode="rdf-about" select="value"/>
    </xsl:element>
  </xsl:template>
    
  
  <xsl:template match="text()|@*">
  </xsl:template>

  <xsl:template match="def">
    <obo:has_definition>
      <rdf:Description>
        <rdfs:label>
          <xsl:value-of select="defstr"/>
        </rdfs:label>
        <xsl:for-each select="dbxref">
            <obo:has_dbxref>
	      <rdf:Description>
		<xsl:apply-templates select="."/>
	      </rdf:Description>
            </obo:has_dbxref>
        </xsl:for-each>
      </rdf:Description>
    </obo:has_definition>
  </xsl:template>

  <xsl:template match="synonym">
    <obo:has_synonym>
      <rdf:Description>
        <xsl:if test="@scope">
	  <obo:scope>  
	    <xsl:attribute name="rdf:resource">
	      <xsl:value-of select="@scope"/>
	    </xsl:attribute>
          </obo:scope>
        </xsl:if>        
	<rdfs:label>
          <xsl:value-of select="synonym_text"/>
        </rdfs:label>
        <xsl:for-each select="dbxref">
            <obo:has_dbxref>
	      <rdf:Description>
		<xsl:apply-templates select="."/>
	      </rdf:Description>
            </obo:has_dbxref>
        </xsl:for-each>
      </rdf:Description>
    </obo:has_synonym>
  </xsl:template>
  
  <xsl:template match="comment">
    <rdfs:comment rdf:datatype="http://www.w3.org/2001/XMLSchema#string">
      <xsl:value-of select="."/>
    </rdfs:comment>
  </xsl:template>


  <xsl:template match="namespace">
    <obo:namespace>
      <xsl:value-of select="."/>
    </obo:namespace>
  </xsl:template>
  
  <xsl:template match="alt_id">
    <obo:alt_id>
      <xsl:value-of select="."/>
    </obo:alt_id>
  </xsl:template>
  
<!--
  <xsl:template match="xref_analog">
    <obo:has_xref_analog>
      <rdf:Description>
	<xsl:for-each select=".">
	  <xsl:apply-templates select="."/>
	</xsl:for-each>
      </rdf:Description>
    </obo:has_xref_analog>
  </xsl:template>

  <xsl:template match="xref_unknown">
    <obo:has_xref_unknown>
      <rdf:Description>
	<xsl:apply-templates select="."/>
      </rdf:Description>
    </obo:has_xref_unknown>
  </xsl:template>
-->
  <xsl:template match="lexical_category">
    <obo:lexical_category>
      <xsl:value-of select="."/>
    </obo:lexical_category>
  </xsl:template>

  <xsl:template match="subset">
    <obo:subset>
      <xsl:apply-templates mode="rdf-resource" select="."/>
    </obo:subset>
  </xsl:template>

  <xsl:template match="acc">
    <obo:acc>
      <xsl:value-of select="."/>
    </obo:acc>
  </xsl:template>

  <xsl:template match="dbname">
    <obo:dbname>
      <xsl:value-of select="."/>
    </obo:dbname>
  </xsl:template>

<!-- added 0.4 SA -->
  <xsl:template match="description">
    <rdfs:comment rdf:datatype="http://www.w3.org/2001/XMLSchema#string">
      <xsl:value-of select="."/>
    </rdfs:comment>
  </xsl:template>

  <xsl:template match="name">
    <obo:name>
      <xsl:value-of select="."/>
    </obo:name>
  </xsl:template>

  <!-- RDF stuff -->
  <xsl:template mode="rdf-id" match="*">
    <xsl:attribute name="rdf:ID">
      <xsl:value-of select="translate(.,':','/')"/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template mode="rdf-about" match="*">
    <xsl:attribute name="rdf:about">
      <xsl:value-of select="translate(.,':','/')"/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template mode="rdf-resource" match="*">
    <xsl:attribute name="rdf:resource">
      <xsl:value-of select="translate(.,':','/')"/>
    </xsl:attribute>
  </xsl:template>

  </xsl:stylesheet>



