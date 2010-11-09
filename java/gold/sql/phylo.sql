-- Comments have been added by Sven in trying to understand the
-- schema, please correct if I got something wrong. 2010-11-09

-- Issues:

-- * Where do missing members go? I guess the seq2pthr loader could
--   add missing members to the bioentity table too. Then perhaps we
--   need a bioentity.data_source column too.

-- * I don't think the same family can have different trees, do we
--   want a single family to be able to have multiple trees?

-- * As family branch implies a direction, do we want to be able to
--   mark the thee unrooted?

-- One ontry per family
CREATE TABLE family (
       id VARCHAR PRIMARY KEY, -- internal reference
       label VARCHAR,          -- name to display?
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

-- meta branches that may span arcoss multiple members, for
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
       family VARCHAR,
       data_source TEXT,
       data_dump TEXT,
       data_format VARCHAR, -- e.g. nhx
);
