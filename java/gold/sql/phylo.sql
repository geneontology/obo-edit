-- Comments have been added by Sven in trying to understand the
-- schema, please correct if I got something wrong. 2010-11-09

-- Issues:

-- * Where do missing members go? I guess the seq2pthr loader could
--   add missing members to the bioentity table too. Then perhaps we
--   need a bioentity.data_source column too.

-- * I don't think the same family can have different trees, do we
--   want a single family to be able to have multiple trees?  Perhaps
--   an algorithm column added to family_branch & family_data_dump.

-- * As family_branch implies a direction, do we want to be able to
--   mark the tree unrooted?

-- One entry per family
CREATE TABLE family (
       id VARCHAR PRIMARY KEY, -- internal reference. Should be a standard ID of the form <DB>:<LocalID>
       label VARCHAR,          -- name to display? E.g. PHOSPHOSERINE PHOSPHATASE for PTHR10000.tree
       versionIRI VARCHAR,     -- whats this?
       creation_date VARCHAR   -- date created (should this be automated?)
);

-- A set of directed branches
CREATE TABLE family_branch (
       bioentity VARCHAR,        -- child
       parent_bioentity VARCHAR,
       branch_length FLOAT,

       family VARCHAR
);

-- Meta branches that may span arcoss multiple members, for
-- convenience?
CREATE TABLE family_ancestry (
       bioentity VARCHAR,          -- descent
       ancestor_bioentity VARCHAR,
       branch_length_sum FLOAT,

       family VARCHAR
);

-- A flat list of family members
CREATE TABLE family_member (
       family VARCHAR,
       bioentity VARCHAR
);

-- text representations of the entire family
CREATE TABLE family_data_dump (
       family VARCHAR, -- FK to family table
       data_source TEXT,
       data_dump TEXT,
       data_format VARCHAR -- e.g. nhx, phyloxml
);

-- See: ftp://ftp.pantherdb.org/ortholog/current/README
-- relationship_type: LDO (least-diverged ortholog), O (ortholog), P (within-species paralog).  LDO is the best 1:1 match of all orthologs between two species
-- 
CREATE TABLE homology_relationship (
       bioentity1 VARCHAR,
       bioentity2 VARCHAR,
       relationship_type VARCHAR,
       ancestor_taxon VARCHAR,
       family VARCHAR
);
