-- ****************************************
-- CORE GOLD SCHEMA
-- ****************************************

CREATE TABLE bioentity (
  id VARCHAR PRIMARY KEY, -- e.g. FB:FBgn00000001, UniProtKB:P12345
  symbol VARCHAR NOT NULL,
  full_name VARCHAR NOT NULL,
  type_cls VARCHAR NOT NULL, -- e.g. SO:nnnn
  ncbi_taxon_id INT,

  db VARCHAR -- e.g. FB - must match prefix part of id
);

COMMENT ON TABLE bioentity IS 'A gene, gene product, or collection of
gene products (i.e. macromolecular complex). A bioentity is capable of
performing some kind of biological function, either by itself, or as a
member of a larger object or complex.';

COMMENT ON COLUMN bioentity.id IS 'A unique identifier for the bioentity.
MAPPINGS:
 GAF: c1:c2
 LEAD: gene_product.dbxref.(xref_dbname+":"+xref_key)
';

COMMENT ON COLUMN bioentity.symbol IS 'A concise label identifying the
bioentity. This should be unique within the db, but this is not mandated.

MAPPINGS:
 GAF: c3
 LEAD: gene_product.symbol
';

COMMENT ON COLUMN bioentity.full_name IS 'A descriptive label describing the
bioentity. This should be unique within the db, but this is not mandated.

MAPPINGS:
 GAF: c10
 LEAD: gene_product.full_name
';

COMMENT ON COLUMN bioentity.type_cls IS 'An ontology identifier
referencing the cls table, providing the bioentity type. The
referenced ontology should be SO in the majority of cases, unless the
type is "complex" in which case GO should be used.

MAPPINGS:
 GAF: c12->class
 LEAD: gene_product.type_id
';

COMMENT ON COLUMN bioentity.ncbi_taxon_id IS 'Integer corresponding
directly to the NCBI Taxon ID. TODO: decide whether to retain this, or
keep the generic ontology reference mechanism.

MAPPINGS:
 GAF: c13
 LEAD: gene_product.species.ncbi_taxa_id
';

COMMENT ON COLUMN bioentity.db IS 'Database from which this bioentity
ID comes from. MUST be the same as the prefix part of the id
column. Note this is partly redundant with the id column.

MAPPINGS:
 GAF: c11
 LEAD: gene_product.dbxref.xref_dbname
';

-- requirement:
--  support complexes?
CREATE TABLE gene_annotation (
  -- TBD:
  -- id = concat(bioentity,qualifier,cls,reference,evidence_cls)

  -- GAF: col1:col2
  bioentity VARCHAR NOT NULL,

  -- GAF: col4
  -- the value of this column is identical to col 4 in the GAF.
  -- This can be viewed as a composite expression describing the relationship
  -- between the bioentity and the GO cls.
  -- Typically this will be a single qualifier (e.g. 'contributes_to'), but
  -- may be a pipe-separated list of qualifiers. These are decomposed
  -- in the composite_qualifier table.
  --
  -- note: negative annotations to go in separate table.
  composite_qualifier VARCHAR,

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
  -- EXAMPLE VALUE: "CGSC:pabA|CGSC:pabB"
  with_expression VARCHAR,

  -- col 13, card>1
  acts_on_taxon_id INT,

  -- col 14.
  -- TODO: change to datetime instead of VARCHAR?
  last_update_date VARCHAR,

  -- col 15
  assigned_by VARCHAR,

  -- col16
  -- this column gets filled in with the exact value from c16 in the GF.
  -- EXAMPLE: "occurs_in(CL:0000123)"
  -- EXAMPLE: "occurs_in(CL:0000123)|occurs_in(MA:9999999)"
  extension_expression VARCHAR,

  -- col17
  gene_product_form VARCHAR,

  -- e.g. filename
  gaf_document VARCHAR
);

-- qualifier expression
-- syntax: QE --> [ Q ]
CREATE TABLE composite_qualifier (
  -- composite pipe-separated ID
  id VARCHAR, 

  -- cls or relation
  qualifier_obj VARCHAR 
);

-- expression representing sum total of evidence WITHs for this assignment.
-- For example a with expression of "CGSC:pabA|CGSC:pabB" would have two
-- rows in this table, one with each xref
CREATE TABLE with_info (
  --- this is the exact value of the expression in col 8 of the GAF.
  --- EXAMPLE: CGSC:pabA|CGSC:pabB
  id VARCHAR,

  --- 
  --- EXAMPLE: CGSC:pabA
  with_xref VARCHAR
);

-- EXAMPLE: "occurs_in(CL:0000123)"
--  in this case there would be one row in the table
-- EXAMPLE: "occurs_in(CL:0000123)|occurs_in(MA:9999999)"
--  in this case there would be two rows in the table
CREATE TABLE extension_expression (
  -- composite expression
  id VARCHAR PRIMARY KEY,

  -- EXAMPLE: "occurs_in"
  relation VARCHAR,

  -- EXAMPLE: "CL:000123"
  cls VARCHAR
);

CREATE TABLE gaf_document (
  id VARCHAR PRIMARY KEY,

  document_path VARCHAR   
   
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

