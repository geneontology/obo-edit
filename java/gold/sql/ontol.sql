-- ****************************************
-- Core
-- ****************************************

CREATE TABLE ontology (
       id VARCHAR PRIMARY KEY,
       label VARCHAR
);

CREATE TABLE cls (
       id VARCHAR PRIMARY KEY,
       label VARCHAR,
       ontology VARCHAR,
       obo_namespace VARCHAR,
       text_definition VARCHAR
);

CREATE TABLE relation (
       id VARCHAR PRIMARY KEY,
       label VARCHAR,
       ontology VARCHAR,
       obo_namespace VARCHAR,
       text_definition VARCHAR,
       is_transitive BOOLEAN,
       is_symmetric BOOLEAN,
       is_reflexive BOOLEAN
);

-- ****************************************
-- Obo Terminological Properties
-- ****************************************

CREATE TABLE ontology_subset (
       id VARCHAR PRIMARY KEY,
       label VARCHAR
);

CREATE TABLE cls_subset (
       cls VARCHAR,
       ontology_subset VARCHAR
);

CREATE TABLE cls_definition_xref (
       cls VARCHAR,
       xref VARCHAR
);

CREATE TABLE cls_alternate_label (
       cls VARCHAR,
       label VARCHAR,
       synonym_scope VARCHAR,
       synonym_type VARCHAR
       synonym_xref VARCHAR
);

-- alt_id -- use id_mapping table?


-- ****************************************
-- Logical Relationships
-- ****************************************
-- the semantics of each of these is specified via
-- a mapping to OWL

-- holds iff: cls SubClassOf super_cls
-- 
CREATE TABLE subclass_of (
       cls VARCHAR,
       super_cls VARCHAR
);

-- holds iff: cls SubClassOf rel Some tgt
CREATE TABLE all_some_relationship (
       cls VARCHAR,
       target_cls VARCHAR,
       relation VARCHAR
);

-- holds iff: cls SubClassOf rel Only tgt
CREATE TABLE all_only_relationship (
       cls VARCHAR,
       target_cls VARCHAR,
       relation VARCHAR
);

-- holds iff: cls SubClassOf ComplementOf(rel Some tgt)
CREATE TABLE never_some_relationship (
       cls VARCHAR,
       target_cls VARCHAR,
       relation VARCHAR
);


-- ****************************************
-- Logical Relationships
-- ****************************************
