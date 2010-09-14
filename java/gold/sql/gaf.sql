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
  gene VARCHAR NOT NULL,
  qualifier_expression VARCHAR,
  cls VARCHAR NOT NULL,
  reference_expression VARCHAR,
  evidence_expression VARCHAR,
  extension_expression VARCHAR,
  gene_product VARCHAR
);

CREATE TABLE qualifier (
  qualifier_expression VARCHAR,
  cls VARCHAR
);

CREATE TABLE evidence_expression (
  evidence_expression VARCHAR,
  evidence_cls VARCHAR,
  with_xref VARCHAR
);
