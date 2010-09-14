-- ****************************************
-- CORE GOLD SCHEMA
-- ****************************************

CREATE TABLE annotated (
  id VARCHAR PRIMARY KEY, -- e.g. FB:FBgn00000001
  symbol VARCHAR NOT NULL,
  full_name VARCHAR NOT NULL,
  type VARCHAR NOT NULL, -- e.g. SO:nnnn
  ncbi_taxon_id INT
);


CREATE TABLE gene_annotation (
  -- GAF: col1:col2
  gene VARCHAR NOT NULL,

  -- GAF: col4
  qualifier_expression VARCHAR,

  -- ontology class
  -- GAF: col5
  cls VARCHAR NOT NULL,

  -- single identifier?
  reference_expression VARCHAR,

  -- col + col
  evidence_expression VARCHAR,

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
  evidence_expression VARCHAR,

  -- an ECO class identifier
  evidence_cls VARCHAR,
  with_xref VARCHAR
);

