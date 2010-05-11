CREATE TABLE load_gaf (
       -- following columns bulkloaded:
        proddb  VARCHAR(55), -- must match dbxref.xref_dbname
        prodacc  VARCHAR(255),
        prodsymbol  VARCHAR(255),
        qualifier  VARCHAR(255),
        termacc  VARCHAR(255),
        ref  VARCHAR(255),
        evcode  VARCHAR(255),
        evwith  VARCHAR(255),
        aspect  VARCHAR(255),
        prodname  VARCHAR(255),
        prodsyn  VARCHAR(8096),
        prodtype  VARCHAR(255),
        prodtaxa  VARCHAR(255),
        assocdate  VARCHAR(255),
	source_db  VARCHAR(255),
        properties  VARCHAR(255) DEFAULT '',
        gpform  VARCHAR(255) DEFAULT '',

       -- these are updated based on db contents:
        gene_product_dbxref_id  INT DEFAULT NULL,
        gene_product_id  INT DEFAULT NULL,
        association_id INT DEFAULT NULL,
        evidence_id INT DEFAULT NULL,
        term_id INT DEFAULT NULL
);

COPY load_gaf( proddb, prodacc, prodsymbol, qualifier, termacc, ref, evcode, evwith, aspect, prodname, prodsyn, prodtype, prodtaxa, assocdate, source_db)
FROM '/Users/cjm/cvs/go-dev/sql/load/load_gaf.txt';


CREATE INDEX load_gaf_ix1 ON load_gaf(proddb,prodacc);

-- Load: gene product dbxrefs --   
INSERT INTO dbxref (xref_dbname,xref_key) SELECT DISTINCT proddb,prodacc FROM load_gaf WHERE NOT EXISTS (SELECT id FROM dbxref WHERE xref_dbname=proddb AND xref_key=prodacc);
UPDATE load_gaf SET gene_product_dbxref_id = (SELECT id FROM dbxref WHERE xref_dbname=proddb AND xref_key=prodacc);

-- Load: gene products --   
-- TODO: species, names, synonyms
INSERT INTO gene_product (dbxref_id,symbol) SELECT DISTINCT gene_product_dbxref_id,prodsymbol FROM load_gaf
  WHERE NOT EXISTS (SELECT id FROM gene_product WHERE dbxref_id=gene_product_dbxref_id);
UPDATE load_gaf SET gene_product_id = (SELECT id FROM gene_product WHERE dbxref_id=gene_product_dbxref_id);

-- we assume terms already loaded
UPDATE load_gaf SET term_id = (SELECT id FROM term WHERE acc=termacc);

-- if any gaf line points to a non-existent GO ID, remove it
-- TODO: do we allow indirect lookup by alt_id, gene_product_id?
DELETE FROM load_gaf WHERE term_id IS NULL;

-- TODO: qualifiers; make sure we have a unique association for each (gene,term,assocdate,qualifier,db) 5-tuple.
-- This is highly incomplete as it stands: just for testing only
INSERT INTO association (gene_product_id,term_id) SELECT DISTINCT gene_product_id,term_id FROM load_gaf 
  WHERE NOT EXISTS (SELECT id FROM association WHERE association.gene_product_id=load_gaf.gene_product_id AND association.term_id=load_gaf.term_id);

-- TODO: evidence
