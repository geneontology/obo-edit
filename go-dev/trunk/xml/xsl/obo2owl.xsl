<?xml version = "1.0"?>
<!DOCTYPE stylesheet [
  <!ENTITY oboInOwl "http://www.geneontology.org/formats/oboInOwl#">
  <!ENTITY oboContent "http://purl.org/obo/owl/">
  <!ENTITY xref "http://purl.org/obo/owl/">
  <!ENTITY xsd "http://www.w3.org/2001/XMLSchema#">
  ]>
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" 
  xmlns:owl="http://www.w3.org/2002/07/owl#"
  xmlns:oboInOwl="&oboInOwl;"
  xmlns:oboContent="&oboContent;"
  >

  <!-- *********************************************** -->
  <!-- Imports -->
  <!-- *********************************************** -->

  
  <!-- *********************************************** -->
  <!-- Parameters -->
  <!-- *********************************************** -->
  <xsl:param name="localid_prefix"/>

  <!-- *********************************************** -->
  <!-- XML -->
  <!-- *********************************************** -->

  <xsl:output indent="yes" method="xml"/>

  <!-- *********************************************** -->
  <!-- Indexes -->
  <!-- *********************************************** -->
  <xsl:key name="k_idspace" match="obo/header/idspace" use="local"/>

  <!-- *********************************************** -->
  <!-- block passthru -->
  <!-- *********************************************** -->
  <xsl:template match="text()|@*">
  </xsl:template>


  <!-- *********************************************** -->
  <!-- Top level -->
  <!-- *********************************************** -->

  <xsl:template match="/">
    <rdf:RDF
      xmlns="&oboContent;"
      xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
      xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
      xmlns:owl="http://www.w3.org/2002/07/owl#"
      xmlns:xsd="&xsd;"
      xmlns:oboInOwl="&oboInOwl;"
      xmlns:oboContent="&oboContent;"
      xml:base="&oboContent;"
      >


      <xsl:comment>
        <xsl:text>oboInOwl meta-model - we must declare this here to avoid falling into owl-full</xsl:text>
      </xsl:comment>
      <owl:AnnotationProperty rdf:about="&oboInOwl;hasAlternativeId"/>
      <owl:AnnotationProperty rdf:about="&oboInOwl;hasDate"/>
      <owl:AnnotationProperty rdf:about="&oboInOwl;hasDbXref"/>
      <owl:AnnotationProperty rdf:about="&oboInOwl;hasDefaultNamespace"/>
      <owl:AnnotationProperty rdf:about="&oboInOwl;hasNamespace"/>
      <owl:AnnotationProperty rdf:about="&oboInOwl;hasDefinition"/>
      <owl:AnnotationProperty rdf:about="&oboInOwl;hasExactSynonym"/>
      <owl:AnnotationProperty rdf:about="&oboInOwl;hasRelatedSynonym"/>
      <owl:AnnotationProperty rdf:about="&oboInOwl;hasSubset"/>
      <owl:AnnotationProperty rdf:about="&oboInOwl;inSubset"/>
      <owl:AnnotationProperty rdf:about="&oboInOwl;savedBy"/>
      <owl:Class rdf:about="&oboInOwl;DbXref"/>
      <owl:Class rdf:about="&oboInOwl;Definition"/>
      <owl:Class rdf:about="&oboInOwl;Subset"/>
      <owl:Class rdf:about="&oboInOwl;Synonym"/>
      <owl:Class rdf:about="&oboInOwl;SynonymType"/>
      <owl:Class rdf:about="&oboInOwl;ObsoleteClass"/>
      <owl:ObjectProperty rdf:about="&oboInOwl;ObsoleteProperty"/>
      
      <xsl:apply-templates select="obo/header"/>
      <xsl:apply-templates select="obo/term"/>
      <xsl:apply-templates select="obo/typedef"/>
      <xsl:apply-templates select="obo/synonym_category"/>
    </rdf:RDF>
  </xsl:template>

  <!-- *********************************************** -->
  <!-- Identifiers -->
  <!-- *********************************************** -->

  <!-- RDF stuff -->
  <xsl:template match="id">
    <xsl:attribute name="rdf:about">
      <xsl:apply-templates mode="translate-id" select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template mode="resource" match="*">
    <xsl:attribute name="rdf:resource">
      <xsl:apply-templates mode="translate-id" select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template mode="about" match="*">
    <xsl:attribute name="rdf:about">
      <xsl:apply-templates mode="translate-id" select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template mode="translate-id" match="*">
    <xsl:choose>
      <xsl:when test="key('k_idspace',substring-before(.,':'))">
        <xsl:value-of select="key('k_idspace',substring-before(.,':'))/global"/>
        <xsl:if test="$localid_prefix">
          <xsl:value-of select="$localid_prefix"/>
        </xsl:if>
        <xsl:value-of select="substring-after(.,':')"/>
      </xsl:when>
      <xsl:when test="substring-before(.,':')">
        <xsl:text>&oboContent;</xsl:text>
        <xsl:value-of select="substring-before(.,':')"/>
        <xsl:text>#</xsl:text>
        <xsl:value-of select="substring-before(.,':')"/>
        <xsl:text>_</xsl:text>
        <xsl:if test="$localid_prefix">
          <xsl:value-of select="$localid_prefix"/>
        </xsl:if>
        <xsl:value-of select="substring-after(.,':')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>&oboContent;</xsl:text>
        <xsl:text>_global/</xsl:text>
        <xsl:if test="$localid_prefix">
          <xsl:value-of select="$localid_prefix"/>
        </xsl:if>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- *********************************************** -->
  <!-- GENERAL PURPOSE -->
  <!-- *********************************************** -->

  <!-- obo names correspond to rdfs labels -->
  <xsl:template match="name">
    <!-- all names are presumed to be in english -->
    <rdfs:label xml:lang="en">
      <xsl:value-of select="."/>
    </rdfs:label>
  </xsl:template>

  <xsl:template mode="label" match="*|@*">
    <!-- all names are presumed to be in english -->
    <rdfs:label xml:lang="en">
      <xsl:value-of select="."/>
    </rdfs:label>
  </xsl:template>

  <!-- added 0.4 SA -->
  <xsl:template match="description">
    <rdfs:comment rdf:datatype="&xsd;string">
      <xsl:value-of select="."/>
    </rdfs:comment>
  </xsl:template>

  <!-- dbxrefs -->

  <!-- in obo-xml, both dbxref and xref_analog refer to the same thing.
       note that the tag now used in obo-text-1.2 is just 'xref' -->
  <xsl:template match="dbxref|xref_analog|xref">
    <oboInOwl:hasDbXref>
      <oboInOwl:DbXref>
        <xsl:attribute name="rdf:about">
          <xsl:choose>
            <!-- typically OBO xrefs are in the form DB:ID - eg EC:1.1.1.1
                 on occasion the xref is also a URI. This this case it may be
                 mistakenly mis-parsed as DB=http, ID=//xxx.yyy.zzz/foo
                 we correct for that here -->
            <xsl:when test="dbname='URL'">
              <oboInOwl:hasURI rdf:datatype="&xsd;anyuri">
                <xsl:value-of select="acc"/>
              </oboInOwl:hasURI>
            </xsl:when>
            <xsl:otherwise>
              <rdfs:label>
                <xsl:text>&xref;</xsl:text>
                <xsl:value-of select="dbname"/>
                <xsl:text>#</xsl:text>
                <xsl:value-of select="dbname"/>
                <xsl:text>_</xsl:text>
                <xsl:value-of select="acc"/>
              </rdfs:label>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
      </oboInOwl:DbXref>
    </oboInOwl:hasDbXref>
  </xsl:template>

  <!-- *********************************************** -->
  <!-- Synonmyms -->
  <!-- *********************************************** -->

  <xsl:template match="synonym[@scope='exact']">
    <oboInOwl:hasExactSynonym>
      <xsl:apply-templates mode="synonym" select="."/>
    </oboInOwl:hasExactSynonym>
  </xsl:template>

  <xsl:template match="synonym[@scope='narrow']">
    <oboInOwl:hasNarrowSynonym>
      <xsl:apply-templates mode="synonym" select="."/>
    </oboInOwl:hasNarrowSynonym>
  </xsl:template>

  <xsl:template match="synonym[@scope='broad']">
    <oboInOwl:hasBroadSynonym>
      <xsl:apply-templates mode="synonym" select="."/>
    </oboInOwl:hasBroadSynonym>
  </xsl:template>

  <xsl:template match="synonym">
    <oboInOwl:hasRelatedSynonym>
      <xsl:apply-templates mode="synonym" select="."/>
    </oboInOwl:hasRelatedSynonym>
  </xsl:template>

  <xsl:template mode="synonym" match="*">
    <oboInOwl:Synonym>
      <xsl:apply-templates mode="label" select="synonym_text"/>
      <xsl:apply-templates select="dbxref"/>
    </oboInOwl:Synonym>
  </xsl:template>
  
  <xsl:template match="synonym_category">
    <oboInOwl:OBODescriptor>
      <xsl:apply-templates select="id"/>
      <xsl:apply-templates select="name"/>
      <xsl:apply-templates select="namespace"/>
    </oboInOwl:OBODescriptor>
  </xsl:template>

  <!-- *********************************************** -->
  <!-- ONTOLOGY METADATA -->
  <!-- *********************************************** -->

  <!-- A subset is a view over an ontology -->
  <xsl:template match="subsetdef">
    <oboInOwl:hasSubset>
      <oboInOwl:Subset>
        <xsl:apply-templates select="id"/>
        <rdfs:comment rdf:datatype="&xsd;string">
          <xsl:value-of select="name"/>
        </rdfs:comment>
        <!-- subsetdefs can in theory have dbxrefs; no existing examples of this -->
        <xsl:apply-templates select="dbxref"/>
      </oboInOwl:Subset>
    </oboInOwl:hasSubset>
  </xsl:template>

  <xsl:template match="idspace">
    <oboInOwl:IDSpace>
      <oboInOwl:local>
        <xsl:value-of select="local"/>
      </oboInOwl:local>
      <oboInOwl:global>
        <xsl:value-of select="global"/>
      </oboInOwl:global>
      <xsl:apply-templates select="comment"/>
    </oboInOwl:IDSpace>
  </xsl:template>


  <xsl:template match="format-version">
  </xsl:template>

  <xsl:template match="date">
    <oboInOwl:hasDate>
      <xsl:value-of select="."/>
    </oboInOwl:hasDate>
  </xsl:template>

  <xsl:template match="saved-by">
    <oboInOwl:savedBy>
      <xsl:value-of select="."/>
    </oboInOwl:savedBy>
  </xsl:template>

  <xsl:template match="auto-generated-by">
  </xsl:template>

  <xsl:template match="default-namespace">
    <oboInOwl:hasDefaultNamespace>
      <xsl:value-of select="."/>
    </oboInOwl:hasDefaultNamespace>
  </xsl:template>

  <xsl:template match="remark">
    <rdfs:comment>
      <xsl:value-of select="."/>
    </rdfs:comment>
  </xsl:template>

  <!-- *********************************************** -->
  <!-- MAIN TEMPLATES -->
  <!-- *********************************************** -->

  <xsl:template match="header">

    <!-- supply the Ontology metadata -->
    <owl:Ontology rdf:about="">
      <!-- most of these are not relevant in the OWL
           transform but are required for round-tripping -->
      <xsl:apply-templates select="format-version"/>
      <xsl:apply-templates select="date"/>
      <xsl:apply-templates select="saved-by"/>
      <xsl:apply-templates select="auto-generated-by"/>
      <xsl:apply-templates select="default-namespace"/>
      <xsl:apply-templates select="remark"/>
      <!-- create an instance on OntologySubset for each subsetdef -->
      <!-- OntologySubsets can be applied to multiple ontologies
           - they are not specific to the ontology -->
      <xsl:apply-templates select="subsetdef"/>
    </owl:Ontology>

    
  </xsl:template>

  <!-- *********************************************** -->
  <!-- Terms/classes -->
  <!-- *********************************************** -->

  <xsl:template match="term">
    <owl:Class>
      <xsl:apply-templates select="id"/>
      <xsl:apply-templates select="name"/>
      <xsl:apply-templates select="comment"/>
      <xsl:apply-templates select="subset"/>
      <xsl:apply-templates select="def"/>
      <xsl:apply-templates select="synonym"/>
      <xsl:apply-templates select="namespace"/>
      <xsl:apply-templates select="alt_id"/>
      <xsl:apply-templates select="xref_analog|xref"/>
      <xsl:apply-templates select="lexical_category"/>
      <xsl:apply-templates select="is_a"/>
      <xsl:apply-templates select="relationship"/>

      <!-- we treat obsoletes as subclasses of the obsolete class;
           although they are not truly classes, we must treat them as
           such to avoid falling into OWL full when we have documents
           including out-of-date annotations to obsolete terms -->
      <xsl:if test="is_obsolete='1'">
        <rdfs:subClassOf rdf:resource="&oboInOwl;ObsoleteClass"/>
      </xsl:if>

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
    </owl:Class>
  </xsl:template>

  <xsl:template match="is_a">
    <!-- by default we exclude inferred is_a relations -->
    <!-- this is mostly used for obol output -->
    <xsl:if test="not(@novel_inferred) and not(@problematic_inferred)">
      <rdfs:subClassOf>
        <xsl:apply-templates mode="resource" select="."/>
      </rdfs:subClassOf>
    </xsl:if>
  </xsl:template>

  <xsl:template match="relationship">
    <rdfs:subClassOf>
      <xsl:apply-templates mode="restriction" select="."/>      
    </rdfs:subClassOf>
  </xsl:template>
    
  <!-- *** DL Constructs *** -->
  <!-- currently we support intersection_of, union_of -->
  <!-- TODO: disjoint_from, complement_of -->

  <xsl:template match="intersection_of|union_of">
    <xsl:choose>
      <!-- genus -->
      <!-- some species of obo use is_a(X) in intersection list -->
      <xsl:when test="type='is_a' or not(type)">
        <owl:Class>
          <xsl:apply-templates mode="about" select="to"/>
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
          <xsl:apply-templates mode="about" select="type"/>
        </owl:ObjectProperty>
      </owl:onProperty>
      <!-- TODO: For now we make the assumption that all relations
           are existential (this is the case for all OBO relations)
           may not be the case for non-foundry ontologies -->
      <owl:someValuesFrom>
        <xsl:apply-templates mode="resource" select="to"/>
      </owl:someValuesFrom>
    </owl:Restriction>
  </xsl:template>

  <xsl:template match="def">
    <oboInOwl:hasDefinition>
      <oboInOwl:Definition>
        <xsl:apply-templates mode="label" select="defstr"/>
        <xsl:apply-templates select="dbxref"/>
      </oboInOwl:Definition>
    </oboInOwl:hasDefinition>
  </xsl:template>

  <xsl:template match="comment">
    <rdfs:comment rdf:datatype="&xsd;string">
      <xsl:value-of select="."/>
    </rdfs:comment>
  </xsl:template>

  <xsl:template match="namespace">
    <oboInOwl:hasNamespace>
      <xsl:value-of select="."/>
    </oboInOwl:hasNamespace>
  </xsl:template>
  
  <xsl:template match="alt_id">
    <oboInOwl:hasAlternativeId>
      <xsl:value-of select="."/>
    </oboInOwl:hasAlternativeId>
  </xsl:template>
  
  <xsl:template match="lexical_category">
    <oboInOwl:hasLexicalCategory>
      <xsl:value-of select="."/>
    </oboInOwl:hasLexicalCategory>
  </xsl:template>

  <xsl:template match="subset">
    <oboInOwl:inSubset>
      <xsl:apply-templates mode="resource" select="."/>
    </oboInOwl:inSubset>
  </xsl:template>


  <!-- *********************************************** -->
  <!-- Relations -->
  <!-- *********************************************** -->

  <!-- In obo-xml, relations and datatype properties are confusingly
       called "typedef" - the reasons for this are historical -->
  <!-- These all map to owl properties -->

  <!-- TODO:
       Relation properties:

       symmetric, anti_symmetric, reflexive

       Transitive over (not yet supported in OWL 1.0 but supported in obo -->
  
  <xsl:template match="typedef">
    <xsl:choose>
      <xsl:when test="is_transitive=1">
        <xsl:element name="owl:TransitiveProperty">
          <xsl:apply-templates mode="detail" select="."/>
        </xsl:element>
      </xsl:when>
      <xsl:when test="is_symmetric=1">
        <xsl:element name="owl:SymmetricProperty">
          <xsl:apply-templates mode="detail" select="."/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <!-- TODO: datatype properties -->
        <xsl:element name="owl:ObjectProperty">
          <xsl:apply-templates mode="detail" select="."/>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template mode="detail" match="typedef">
    <!-- most of the following are the same as for terms/classes -->
    <xsl:apply-templates select="id"/>
    <xsl:apply-templates select="name"/>
    <xsl:apply-templates select="inverse_of"/>
    <xsl:apply-templates select="comment"/>
    <xsl:apply-templates select="subset"/>
    <xsl:apply-templates select="def"/>
    <xsl:apply-templates select="synonym"/>
    <xsl:apply-templates select="namespace"/>
    <xsl:apply-templates select="alt_id"/>
    <xsl:apply-templates select="xref_analog|xref"/>
    <xsl:apply-templates select="lexical_category"/>
    <!-- is_a is used for both subClassOf and subPropertyOf -->
    <xsl:for-each select="is_a">
      <rdfs:subPropertyOf>
        <xsl:apply-templates mode="resource" select="."/>
      </rdfs:subPropertyOf>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="inverse_of">
    <owl:inverseOf>
      <xsl:apply-templates mode="resource" select="."/>
    </owl:inverseOf>
  </xsl:template>


  <!-- *********************************************** -->
  <!-- Instances -->
  <!-- *********************************************** -->

  <!-- Yes, obo supports instances too -->

  <xsl:template match="instance">
    <xsl:element name="rdf:Description">
      <xsl:apply-templates select="id"/>
      <xsl:apply-templates select="instance_of"/>
      <xsl:apply-templates select="name"/>
      <xsl:apply-templates select="namespace"/>
      <xsl:apply-templates select="property_value"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="instance_of">
    <rdf:type>
      <xsl:apply-templates mode="resource" select="."/>
    </rdf:type>
  </xsl:template>

  <!-- TODO -->
  <xsl:template match="property_value">
    <xsl:element name="{property}">
      <xsl:apply-templates mode="resource" select="value"/>
    </xsl:element>
  </xsl:template>
    
  <!-- *********************************************** -->


</xsl:stylesheet>

