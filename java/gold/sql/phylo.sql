CREATE TABLE family (
       id VARCHAR PRIMARY KEY,
       label VARCHAR,
       versionIRI VARCHAR,
       creation_date VARCHAR
);

CREATE TABLE family_branch (
       bioentity VARCHAR,
       parent_bioentity VARCHAR,
       branch_length FLOAT,

       family VARCHAR
);

CREATE TABLE family_ancestry (
       bioentity VARCHAR,
       ancestor_bioentity VARCHAR,
       branch_length_sum FLOAT,

       family VARCHAR
);
