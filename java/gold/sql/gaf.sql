-- ****************************************
-- CORE GOLD SCHEMA
-- ****************************************

-- todo - better name
CREATE TABLE bioentity (
  id VARCHAR PRIMARY KEY, -- e.g. FB:FBgn00000001
  symbol VARCHAR NOT NULL,
  full_name VARCHAR NOT NULL,
  type VARCHAR NOT NULL, -- e.g. SO:nnnn
  ncbi_taxon_id INT
);

-- requirement:
--  support complexes?
CREATE TABLE gene_annotation (
  -- GAF: col1:col2
  bioentity VARCHAR NOT NULL,

  -- GAF: col4
  -- note: negative annotations to go in separate table
  qualifier_expression VARCHAR,

  -- ontology class
  -- GAF: col5
  cls VARCHAR NOT NULL,

  -- single identifier
  -- PMID is prioritized.
  -- put alternate identifiers in id_mapping table
  reference_id VARCHAR,

  -- col 7
  evidence_cls VARCHAR,

  -- col 8
  with_expression VARCHAR,

  -- col 14
  date_updated VARCHAR,

  -- col 15
  assigned_by VARCHAR,

  -- col16
  extension_expression VARCHAR,

  -- col17
  gene_product VARCHAR
);

-- qualifier expression
-- syntax: QE --> [ Q ]
CREATE TABLE qualifier (
  qualifier_expression VARCHAR,
  cls VARCHAR
);

-- expression representing sum total of evidence for this assignment
CREATE TABLE evidence_expression (
  evidence_cls VARCHAR,
  with_expression VARCHAR,
  UNIQUE (evidence_cls,with_expression),

  with_xref VARCHAR
);

CREATE TABLE bioentity_relationship (
       bioentity VARCHAR,
       relation VARCHAR,
       parent_bioentity VARCHAR
);

CREATE TABLE id_mapping (
  source_id VARCHAR NOT NULL,
  target_id VARCHAR NOT NULL,
  
  -- optional: relationship assumed to be equivalence
  relationship VARCHAR,

  mapping_source VARCHAR
);

