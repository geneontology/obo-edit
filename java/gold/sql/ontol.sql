-- ****************************************
-- ontology
-- ****************************************

CREATE TABLE ontology (
       id VARCHAR PRIMARY KEY,
       label VARCHAR,
       versionIRI VARCHAR,
       creation_date VARCHAR
);
COMMENT ON TABLE ontology IS 'A collection of classes, relations and relationships.
MAPPINGS: 
 OBO: header.ontology (introduced in obof1.4)
 OWL: Ontology
 LEAD: n/a';

CREATE TABLE ontology_annotation (
       ontology VARCHAR,
       property VARCHAR,
       annotation_value VARCHAR
);
COMMENT ON TABLE ontology_annotation IS 'A property of an ontology
Example properties: saved_by
MAPPINGS: 
 OBO: header.<property>
 OWL: OntologyAnnotation(ontology property value)
 LEAD: n/a';

CREATE TABLE ontology_imports (
       ontology VARCHAR,
       imports_ontology VARCHAR
);

-- ****************************************
-- cls
-- ****************************************

CREATE TABLE cls (
       id VARCHAR PRIMARY KEY,

       label VARCHAR,

       ontology VARCHAR,
       obo_namespace VARCHAR,
       text_comment TEXT,
       text_definition TEXT,

       is_obsolete BOOLEAN -- TODO: use separate table?
);
COMMENT ON TABLE cls IS 'An ontology class.
MAPPINGS: 
 OBO: Term stanza.
 OWL: Class
 LEAD: term';

COMMENT ON COLUMN cls.id IS 'A unique identifier for this class.
Example: GO:0008150
MAPPINGS:
 OBO: Term.id
 OWL: URI for class, with obo2owl transform. E.g. GO_nnnnnnn -> GO:nnnnnnn
 LEAD: term.acc';

COMMENT ON COLUMN cls.label IS 'A descriptive label for this class.
Should be unique within the ontology, but this is not enforced.
Example: "lung development"
MAPPINGS:
 OBO: term.name
 OWL: rdfs:label
 LEAD: term.name
';
COMMENT ON COLUMN cls.ontology IS 'The ontology to which this class belongs.
Examples: "GO", "CL". References ontology.id
MAPPINGS:
 OBO: the idspace for the term id
 OWL: ontology
 LEAD: n/a
';

COMMENT ON COLUMN cls.obo_namespace IS 'An obo namespace is similar to an ontology.
The GO is split into 3 namespaces. Most ontologies have a 1:1 association between
ontology and obo_namespace.
MAPPINGS:
 OBO: term.namespace
 OWL: n/a
 LEAD: term.term_type
Example: "biological_process".
';

COMMENT ON COLUMN cls.text_definition IS 'A textual definition uniquely defining the class.
MAPPINGS:
 OBO: term.definition
 OWL: see obo2owl guide
 LEAD: term_definition.term_definition
 API: getDef()
';

COMMENT ON COLUMN cls.is_obsolete IS 'True if this is an obsolete/deprecated class
MAPPINGS:
 OBO: term.is_obsolete
 OWL: AnnotationProperty(owl:deprecated cls true) [TBD: obo obsoletion may be stronger]
 LEAD: term.is_obsolete
';

-- ****************************************
-- relation
-- ****************************************
CREATE TABLE relation (
       id VARCHAR PRIMARY KEY,
       label VARCHAR,
       ontology VARCHAR,
       obo_namespace VARCHAR,
       text_comment TEXT,
       text_definition TEXT,
       is_transitive BOOLEAN,
       is_symmetric BOOLEAN,
       is_reflexive BOOLEAN,

       is_obsolete BOOLEAN -- TODO: use separate table?
);

COMMENT ON TABLE relation IS 'An ontology relation.
MAPPINGS: 
 OBO: Typedef stanza.
 OWL: ObjectProperty
 LEAD: term (the term table is overloaded)';

COMMENT ON COLUMN relation.id IS 'A unique identifier for this relation.
Example: part_of
MAPPINGS:
 OBO: Term.id
 OWL: URI for relation, with obo2owl transform.
 LEAD: term.acc';

COMMENT ON COLUMN relation.label IS 'A descriptive label for this relation.
Should be unique within the ontology, but this is not enforced.
Example: "is part of"
MAPPINGS:
 OBO: term.name
 OWL: rdfs:label
 LEAD: term.name
';
COMMENT ON COLUMN relation.ontology IS 'The ontology to which this relation belongs.
Examples: "RO", "GO". References ontology.id
MAPPINGS:
 OBO: the idspace for the term id
 OWL: ontology
 LEAD: n/a
';

COMMENT ON COLUMN relation.obo_namespace IS 'See comments for cls.namespace
MAPPINGS:
 OBO: term.namespace
 OWL: n/a
 LEAD: term.term_type
Example: "biological_process".
';

COMMENT ON COLUMN relation.text_definition IS 'A textual definition uniquely defining the relation.
MAPPINGS:
 OBO: term.definition
 OWL: see obo2owl guide
 LEAD: term_definition.term_definition
';

-- ****************************************
-- Obo Terminological Properties
-- ****************************************

-- ****************************************
-- ontology_subset
-- ****************************************
CREATE TABLE ontology_subset (
       id VARCHAR PRIMARY KEY,
       label VARCHAR
);

COMMENT ON TABLE ontology_subset IS 'A named subset of the ontology
EXAMPLE: goslim_prok
MAPPINGS:
 OBO: header.subsetdef
 OWL: see obo2owl spec
 LEAD: term (term table is overloaded)
';

-- ****************************************
-- obj_subset
-- ****************************************
CREATE TABLE obj_subset (
       obj VARCHAR,
       ontology_subset VARCHAR
);

COMMENT ON TABLE obj_subset IS 'Relates an ontology class or relation to an ontology_subset.
MAPPINGS:
 OBO: term.subset
 OWL: see obo2owl spec
 LEAD: term_subset
';

-- ****************************************
-- obj_definition_xref
-- ****************************************
CREATE TABLE obj_definition_xref (
       obj VARCHAR,
       xref VARCHAR
);

COMMENT ON TABLE obj_definition_xref IS 'Provenance for a class or relation textual definition
MAPPINGS:
 OBO: term.def.xrefs
 OWL: see obo2owl spec
 LEAD: term_dbxref[is_for_definition=1]
';

COMMENT ON COLUMN obj_definition_xref.xref IS 'Identifier for def xref.
EXAMPLE: PMID:123456
EXAMPLE: http://en/wikipedia.org/wiki/Transcription
 OBO: term.def.xrefs
 OWL: see obo2owl spec
 LEAD: term_dbxref[is_for_definition=1].dbxref.{xref_dbname:xref_key}
';

-- ****************************************
-- obj_alternate_label
-- ****************************************
-- TODO - denormalize synonym_xrefs
CREATE TABLE obj_alternate_label (
       obj VARCHAR,
       label VARCHAR,
       synonym_scope VARCHAR,
       synonym_type VARCHAR,
       synonym_xref VARCHAR
);

COMMENT ON TABLE obj_alternate_label IS 'Synonyms and alternative labels.
MAPPINGS:
 OBO: synonym
 OWL: see obo2owl spec
 LEAD: term_synonym
';

COMMENT ON COLUMN obj_alternate_label.obj IS 'class or relation that owns the alternate label.
';

CREATE TABLE annotation_property (
       id VARCHAR PRIMARY KEY,
       label VARCHAR,
       ontology VARCHAR,
       obo_namespace VARCHAR,
       text_comment TEXT,
       text_definition TEXT,

       is_obsolete BOOLEAN -- TODO: use separate table?
);

COMMENT ON TABLE annotation_property IS 'A non-logical relation/tag that can connect objects or objects and values.
TYPICAL ENTRIES: consider, replaced_by
MAPPINGS:
 OBO: Typedef[is_metadata_tag=true]
 OWL: AnnotationProperty
 LEAD: term (overloaded)
';

-- ****************************************
-- obj_alternate_id
-- ****************************************
CREATE TABLE obj_alternate_id (
       obj VARCHAR,
       id VARCHAR
);

COMMENT ON TABLE obj_alternate_id IS 'An alternive identifier for a class or relation, typically arising from class merges.
MAPPINGS:
 OBO: alt_id
 OWL: see obo2owl spec
 LEAD: term_synonym (overloaded)
';

CREATE TABLE obj_xref (
       obj VARCHAR,
       xref VARCHAR,

       xref_description VARCHAR
);

CREATE TABLE annotation_assertion (
       relation VARCHAR,
       obj VARCHAR,
       target_obj VARCHAR,

       ontology VARCHAR
);
COMMENT ON TABLE annotation_assertion IS 'A non-logical relationship between two objects (classes or relations).
When an obo consider tag is being used, relation=consider
When an obo replaced_by tag is being used, relation=replaced_by
MAPPINGS:
 OBO: property_value (also: consider, replaced_by)
 OWL: AnnotationAssertion(relation obj target_obj)
 LEAD: term2term_metadata
';


-- ****************************************
-- Logical Relationships
-- ****************************************
-- the semantics of each of these is specified via
-- a mapping to OWL

-- holds iff: cls SubClassOf super_cls
-- 
CREATE TABLE subclass_of (
       cls VARCHAR,
       super_cls VARCHAR,

       ontology VARCHAR
);

-- holds iff: cls SubClassOf rel Some tgt
CREATE TABLE all_some_relationship (
       cls VARCHAR,
       relation VARCHAR,
       target_cls VARCHAR,

       ontology VARCHAR
);

-- holds iff: cls SubClassOf rel Only tgt
-- EXAMPLE: lactation only_in_taxon Mammalia ==> lactation SubClassOf in_taxon only Mammalia
CREATE TABLE all_only_relationship (
       cls VARCHAR,
       relation VARCHAR,
       target_cls VARCHAR,

       ontology VARCHAR
);

-- holds iff: cls SubClassOf ComplementOf(rel Some tgt)
-- EXAMPLE: odontogenesis never_in_taxon Aves ==> odontogenesis SubClassOf ComplementOf(in_taxon some Aves) [taxon_go_triggers]
CREATE TABLE never_some_relationship (
       cls VARCHAR,
       target_cls VARCHAR,
       relation VARCHAR,

       ontology VARCHAR
);

-- holds iff: cls SubObjectPropertyOf super_cls
-- 
CREATE TABLE subrelation_of (
       relation VARCHAR,
       super_relation VARCHAR,

       ontology VARCHAR
);

CREATE TABLE relation_chain (
       inferred_relation VARCHAR,
       relation1 VARCHAR,
       relation2 VARCHAR,
       is_bidirectional BOOLEAN
);

COMMENT ON TABLE relation_chain IS 'A rule defining how two relations are composed.
EXAMPLE: has_part o part_of --> overlaps
EXAMPLE: regulates o part_of --> regulates
MAPPINGS:
 OBO: typedef.holds_over_chain
 OWL: SubObjectPropertyOf(inferred_relation PropertyChain(relation1 relation2))
 LEAD: n/a
';

COMMENT ON COLUMN relation_chain.is_bidirectional IS 'True if the implication is bidirectional.
MAPPINGS:
 OBO: true for typedef.equivalent_to_chain, false for typedef.holds_over_chain
 OWL: n/a
 LEAD: n/a
';

CREATE TABLE disjoint_with (
       cls VARCHAR,
       disjoint_cls VARCHAR,

       ontology VARCHAR
);

COMMENT ON TABLE disjoint_with IS 'Two classes are disjoint if they share no instances or subclasses in common.
MAPPINGS:
 OBO: Term.disjoint_from
 OWL: DisjointClasses(cls disjoint_cls)
 LEAD: n/a
';

CREATE TABLE equivalent_to (
       cls VARCHAR,
       equivalent_cls VARCHAR,

       ontology VARCHAR
);

CREATE TABLE cls_intersection_of (
       cls VARCHAR,
       relation VARCHAR,
       target_cls VARCHAR,

       ontology VARCHAR
);

COMMENT ON TABLE cls_intersection_of IS 'A shorthand for stating necessary and sufficient definitions.
For any cls, the set of all_intersection_of tuples are collected. This constitutes a conjunctive expression that is equivalent to cls.
E.g.
[Term]
id: blue_car
intersection_of: car
intersection_of: has_color blue
==> [gold]
cls_intersection_of(blue_car,null,car)
cls_intersection_of(blue_car,has_color,blue)
==> [owl]
EquivalentTo (blue_car IntersectionOf(car SomeValuesFrom(has_color blue)))
//
Limitation: only a single such equivalence relation is allowed per class
//
Note that there should never be a cls that only has a single cls_intersection_of.
MAPPINGS:
 OBO: intersection_of
 OWL: EquivalentTo(cls IntersectionOf( {...} ) -- see obo2owl doc
 LEAD: term2term[completes=1]
';

CREATE TABLE cls_union_of (
       cls VARCHAR,
       target_cls VARCHAR,

       ontology VARCHAR
);
COMMENT ON TABLE cls_union_of IS 'A shorthand declaring a class to be equivalent to a union of other classes.
For any cls, the set of all_union_of tuples are collected. This constitutes a conjunctive expression that is equivalent to cls.
E.g.
[Term]
id: prokaryote
union_of: eubacteria
union_of: archaea
==> [gold]
cls_union_of(prokaryote,eubacteria)
cls_union_of(prokaryote,archaea)
==> [owl]
EquivalentTo (prokaryote UnionOf(eubacteria archaea))
//
Limitation: only a single such equivalence relation is allowed per class
//
Note that there should never be a cls that only has a single cls_union_of.
MAPPINGS:
 OBO: union_of
 OWL: EquivalentTo(cls UnionOf( {...} ) -- see obo2owl doc
 LEAD: n/a
';

-- ****************************************
-- Inferred Relationships
-- ****************************************

CREATE TABLE inferred_relationship (
       cls VARCHAR,
       target_cls VARCHAR,
       relation VARCHAR,
       is_direct BOOLEAN,
       is_reflexive BOOLEAN,

       ontology VARCHAR
);

COMMENT ON TABLE inferred_relationship IS 'A path between cls and target_cls
';
