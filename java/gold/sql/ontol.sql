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
       annotation_value VARCHAR,
       PRIMARY KEY (ontology, property, annotation_value)
);
COMMENT ON TABLE ontology_annotation IS 'A property of an ontology
Example properties: saved_by
MAPPINGS: 
 OBO: header.<property>
 OWL: OntologyAnnotation(ontology property value)
 LEAD: n/a';

CREATE TABLE ontology_imports (
       ontology VARCHAR,
       imports_ontology VARCHAR,
       PRIMARY KEY (ontology, imports_ontology)
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

       is_obsolete BOOLEAN 
);
COMMENT ON TABLE cls IS 'An ontology class.
TODO: use separate table?
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

       is_obsolete BOOLEAN 
);

COMMENT ON TABLE relation IS 'An ontology relation.
TODO: use separate table?
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
       ontology_subset VARCHAR,
       PRIMARY KEY (obj, ontology_subset)
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
       xref VARCHAR,
       PRIMARY KEY (obj, xref)
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
CREATE TABLE obj_alternate_label (
       obj VARCHAR,
       label VARCHAR,
       synonym_scope VARCHAR,
       synonym_type VARCHAR,
       synonym_xref VARCHAR,
       PRIMARY KEY (obj, label)
);

COMMENT ON TABLE obj_alternate_label IS 'Synonyms and alternative labels.
TODO - denormalize synonym_xrefs
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

       is_obsolete BOOLEAN 
);

COMMENT ON TABLE annotation_property IS 'A non-logical relation/tag that can connect objects or objects and values.
TYPICAL ENTRIES: consider, replaced_by
TODO: use separate table?
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
       id VARCHAR,
       PRIMARY KEY (obj, id)
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
       xref_description VARCHAR,

       PRIMARY KEY(obj,xref)
);

CREATE TABLE annotation_assertion (
       relation VARCHAR,
       obj VARCHAR,
       target_obj VARCHAR,

       ontology VARCHAR,
       PRIMARY KEY (relation, obj, target_obj, ontology)
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

CREATE TABLE subclass_of (
       cls VARCHAR,
       super_cls VARCHAR,

       ontology VARCHAR,
       PRIMARY KEY (cls, super_cls, ontology)
);
COMMENT ON TABLE subclass_of IS 'holds iff: cls SubClassOf super_cls';

CREATE TABLE all_some_relationship (
       cls VARCHAR,
       relation VARCHAR,
       target_cls VARCHAR,

       ontology VARCHAR,
       PRIMARY KEY (cls, relation, target_cls, ontology)
);
COMMENT ON TABLE all_some_relationship IS 'holds iff: cls SubClassOf rel Some tgt';

CREATE TABLE all_only_relationship (
       cls VARCHAR,
       relation VARCHAR,
       target_cls VARCHAR,

       ontology VARCHAR,
       PRIMARY KEY (cls, relation, target_cls, ontology)
       
);
COMMENT ON TABLE all_only_relationship IS 'holds iff: cls SubClassOf rel Only tgt
EXAMPLE: lactation only_in_taxon Mammalia ==> lactation SubClassOf in_taxon only Mammalia
';

CREATE TABLE never_some_relationship (
       cls VARCHAR,
       target_cls VARCHAR,
       relation VARCHAR,

       ontology VARCHAR,
       PRIMARY KEY (cls, relation, target_cls, ontology)
);
COMMENT ON TABLE never_some_relationship IS 'holds iff: cls SubClassOf ComplementOf(rel Some tgt)
EXAMPLE: odontogenesis never_in_taxon Aves ==> odontogenesis SubClassOf ComplementOf(in_taxon some Aves) [taxon_go_triggers]
';
 
CREATE TABLE subrelation_of (
       relation VARCHAR,
       super_relation VARCHAR,

       ontology VARCHAR,
       PRIMARY KEY (relation, super_relation, ontology)
);
COMMENT ON TABLE subrelation_of IS 'holds iff: cls SubObjectPropertyOf super_cls
'; 

CREATE TABLE relation_disjoint_with (
       relation VARCHAR,
       disjoint_relation VARCHAR,

       ontology VARCHAR,
       
       PRIMARY KEY (relation, disjoint_relation, ontology)
);

COMMENT ON TABLE relation_disjoint_with IS 'holds iff: DisjointObjectProperties(relation disjoint_relation)
';

CREATE TABLE relation_equivalent_to (
       relation VARCHAR,
       equivalent_relation VARCHAR,

       ontology VARCHAR,
       PRIMARY KEY (relation, equivalent_relation, ontology)
);
COMMENT ON TABLE relation_equivalent_to IS 'holds iff: EquivalentObjectProperties(relation equivalent_relation)
';

CREATE TABLE relation_chain (
       inferred_relation VARCHAR,
       relation1 VARCHAR,
       relation2 VARCHAR,
       is_bidirectional BOOLEAN,
       PRIMARY KEY (inferred_relation, relation1, relation2)
);

COMMENT ON TABLE relation_chain IS 'A rule defining how two relations are composed.
SEMANTICS: SubObjectPropertyOf(inferred_relation PropertyChain(relation1 relation2))
note this table cannot be used for property chains of length >2, GO should not have these.
if this is desired then the property chain should be broken into pairs.
EXAMPLE: SubObjectPropertOf(regulates PropertyChain(regulates part_of))
(in obof, this is encoded using transitive_over).
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

       ontology VARCHAR,
       PRIMARY KEY (cls, disjoint_cls, ontology)
       
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

       ontology VARCHAR,
       PRIMARY KEY (cls, equivalent_cls, ontology)
);

CREATE TABLE cls_intersection_of (
       cls VARCHAR,
       relation VARCHAR,
       target_cls VARCHAR,

       ontology VARCHAR,
       UNIQUE (cls, relation, target_cls, ontology)
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

       ontology VARCHAR,
       PRIMARY KEY (cls, target_cls, ontology)
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

CREATE TABLE inferred_subclass_of (
       cls VARCHAR,
       target_cls VARCHAR,
       is_direct BOOLEAN,
       is_reflexive BOOLEAN,
	   relation VARCHAR,
	   quantifier VARCHAR,
       ontology VARCHAR,
       PRIMARY KEY (cls, target_cls, ontology)
);


CREATE TABLE inferred_all_some_relationship (
       cls VARCHAR,
       relation VARCHAR,
       target_cls VARCHAR,
       is_direct BOOLEAN,
       is_reflexive BOOLEAN,
	   quantifier VARCHAR,
       ontology VARCHAR
--  TODO      PRIMARY KEY (cls, target_cls, ontology, relation)
       
);
COMMENT ON TABLE inferred_all_some_relationship IS 'holds iff: cls SubClassOf rel Some tgt';

CREATE TABLE inferred_all_only_relationship (
       cls VARCHAR,
       relation VARCHAR,
       target_cls VARCHAR,
       is_direct BOOLEAN,
       is_reflexive BOOLEAN,
	   quantifier VARCHAR,
       ontology VARCHAR,
        PRIMARY KEY (cls, target_cls, ontology, relation)
       
);
COMMENT ON TABLE inferred_all_only_relationship IS 'holds iff: cls SubClassOf rel Only tgt
EXAMPLE: lactation only_in_taxon Mammalia ==> lactation SubClassOf in_taxon only Mammalia
';


CREATE TABLE inferred_never_some_relationship (
       cls VARCHAR,
       target_cls VARCHAR,
       relation VARCHAR,
       is_direct BOOLEAN,
       is_reflexive BOOLEAN,
	   quantifier VARCHAR,
       ontology VARCHAR,
       PRIMARY KEY (cls, target_cls, ontology, relation)

);
COMMENT ON TABLE inferred_never_some_relationship IS 'holds iff: cls SubClassOf ComplementOf(rel Some tgt)
EXAMPLE: odontogenesis never_in_taxon Aves ==> odontogenesis SubClassOf ComplementOf(in_taxon some Aves) [taxon_go_triggers]
';


CREATE TABLE inferred_subrelation_of (
       relation VARCHAR,
       super_relation VARCHAR,
       is_direct BOOLEAN,
       is_reflexive BOOLEAN,

       ontology VARCHAR,
       PRIMARY KEY (relation, super_relation, ontology)
);
COMMENT ON TABLE inferred_subrelation_of IS 'holds iff: cls SubObjectPropertyOf super_cls
';

CREATE TABLE ontology_changes_history(
	table_name VARCHAR,
	col VARCHAR,
	ontology VARCHAR,
	versionIRI VARCHAR,
	new_value VARCHAR
);

COMMENT ON TABLE ontology_changes_history IS 'This table maintains history of chages of an ontology when the
ontology is updated preiodically.
This table is on experimental basis. Its structure can be changed in future. 
';
