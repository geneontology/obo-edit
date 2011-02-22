<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE stylesheet [
  <!ENTITY owl "http://www.w3.org/2002/07/owl#">
  <!ENTITY oboInOwl "http://www.geneontology.org/formats/oboInOwl#">
  <!ENTITY xsd "http://www.w3.org/2001/XMLSchema#">
  ]>
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" 
  xmlns:owl="http://www.w3.org/2002/07/owl#"
  xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
  xmlns:oboInOwl="http://www.geneontology.org/formats/oboInOwl#"
  >

  <xsl:output method="xml"/>
  
  <!-- The OBO metamodel : oboInOwl -->
  <!-- Q: Should this be included in every ontology export, or
       exist as a separate file? -->
  <xsl:template name="obo_in_owl_model">

    <xsl:comment>
      <xsl:text>The OBO Metamodel</xsl:text>
    </xsl:comment>
    <owl:Class rdf:about="&oboInOwl;OBODescriptor">
      <rdfs:comment> a class to collect the various symbols (instances)
      used to define synonym scope etc </rdfs:comment>
    </owl:Class>
    
    <owl:Class rdf:about="&oboInOwl;DbXref">
      <owl:intersectionOf rdf:parseType="Collection">
        <owl:Restriction>
          <owl:cardinality rdf:datatype="&xsd;nonNegativeInteger">1</owl:cardinality>
          <owl:onProperty rdf:resource="&oboInOwl;dbname"/>
        </owl:Restriction>
        <owl:Restriction>
          <owl:cardinality rdf:datatype="&xsd;nonNegativeInteger">1</owl:cardinality>
          <owl:onProperty rdf:resource="&oboInOwl;acc"/>
        </owl:Restriction>
      </owl:intersectionOf>
    </owl:Class>

    <owl:Class rdf:about="&oboInOwl;Definition"/>
    <owl:Class rdf:about="&oboInOwl;Synonym"/>
    <owl:Class rdf:about="&oboInOwl;IDSpace"/>

    <owl:Class rdf:about="&oboInOwl;SynonymScopeValue">
      <!-- synonym scope and other OBO trailing modifiers can be organised
           into subclasses of OBODescriptor -->
      <owl:equivalentClass>
        <owl:Class>
          <owl:oneOf rdf:parseType="Collection">
            <oboInOwl:OBODescriptor rdf:about="&oboInOwl;exact"/>
            <oboInOwl:OBODescriptor rdf:about="&oboInOwl;broad"/>
            <oboInOwl:OBODescriptor rdf:about="&oboInOwl;narrow"/>
          </owl:oneOf>
        </owl:Class>
      </owl:equivalentClass>
    </owl:Class>

    <owl:Class rdf:about="&oboInOwl;SubsetDef">
      <!-- organise named subsets in subsetdef into instances of OntologySubsetValue -->
    </owl:Class>

    <!-- annotations that apply to the Ontology / OBO Header -->
    <!-- from the XML DTD -->

    <!-- comment [rdfs:comment], is_anonymous [ignore], is_obsolete [ignore], is_root [ignore]-->

    <owl:AnnotationProperty rdf:about="&oboInOwl;format-version"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;date"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;saved-by"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;auto-generated-by"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;default-namespace">
      <rdfs:comment>
        All resources in this ontology fall into this obo-namespace by default.
      </rdfs:comment>
    </owl:AnnotationProperty>
    <owl:AnnotationProperty rdf:about="&oboInOwl;namespace">
      <rdfs:comment>
        Ontology name.
        an oboInOwl namespace should not be confused with an OWL namespace
      </rdfs:comment>
    </owl:AnnotationProperty>
    <owl:AnnotationProperty rdf:about="&oboInOwl;has_definition"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;alt_id"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;hasSubset"/>
    <owl:ObjectProperty rdf:about="&oboInOwl;has_dbxref"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;has_xref_analog"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;has_synonym"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;local"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;global"/>
    <owl:AnnotationProperty rdf:about="&oboInOwl;lexical_category"/>
    <owl:DatatypeProperty rdf:about="&oboInOwl;acc">
      <rdf:type rdf:resource="&owl;FunctionalProperty"/>
    </owl:DatatypeProperty>
    <owl:DatatypeProperty rdf:about="&oboInOwl;dbname">
      <rdf:type rdf:resource="&owl;FunctionalProperty"/>
    </owl:DatatypeProperty>
    <owl:DatatypeProperty rdf:about="&oboInOwl;name"/>
    <owl:ObjectProperty rdf:about="&oboInOwl;scope">
      <rdfs:range rdf:resource="&oboInOwl;SynonymScopeValue"/>
    </owl:ObjectProperty>
    <owl:ObjectProperty rdf:about="&oboInOwl;synonym_category"/>
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

  </xsl:template>

</xsl:stylesheet>
