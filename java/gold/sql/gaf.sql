-- ****************************************
-- CORE GOLD SCHEMA
-- ****************************************

CREATE TABLE bioentity (
  id VARCHAR PRIMARY KEY, -- e.g. FB:FBgn00000001
  symbol VARCHAR NOT NULL,
  full_name VARCHAR NOT NULL,
  type_cls VARCHAR NOT NULL, -- e.g. SO:nnnn
  ncbi_taxon_id INT,

  db VARCHAR -- e.g. FB - must match prefix part of id
);

-- requirement:
--  support complexes?
CREATE TABLE gene_annotation (
  -- TBD:
  -- id = concat(bioentity,qualifier,cls,reference,evidence_cls)

  -- GAF: col1:col2
  bioentity VARCHAR NOT NULL,

  -- GAF: col4
  -- note: negative annotations to go in separate table
  qualifier_expression VARCHAR,

  -- TBD: add to qualifier column?
  is_contributes_to BOOLEAN,

  -- TBD: add to qualifier column?
  is_integral_to BOOLEAN,

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
  -- this should be identical to GAF. references with_info table.
  with_expression VARCHAR,

  -- col 13, card>1
  acts_on_taxon_id INT,

  -- col 14.
  last_update_date VARCHAR,

  -- col 15
  assigned_by VARCHAR,

  -- col16
  extension_expression VARCHAR,

  -- col17
  gene_product_form VARCHAR
);

-- qualifier expression
-- syntax: QE --> [ Q ]
CREATE TABLE qualifier (
  qualifier_expression VARCHAR,
  cls VARCHAR
);

-- expression representing sum total of evidence WITHs for this assignment
CREATE TABLE with_info (
  with_expression VARCHAR,
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

