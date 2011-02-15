--- GO DATABASE TABLE CREATION SQL
--- contact: go-database@fruitfly.bdgp.berkeley.edu

--- Note: this file is the SOURCE sql; it is for defining
--- the schema in a RDBMS vendor independent way. to "compile"
--- this source into SQL that your DBMS (mysql/postgres/etc)
--- will understand, use the "configure" and "make" commands.
--- See the README file in this directory for more details

--- *******************************************************
--- ** go-general                                        **
--- **                                                   **
--- ** general purpose tables, not specific to GO or     **
--- ** biology per se                                    **
--- **                                                   **
--- *******************************************************


--- @@ dbxref
--- a unique bipartite identifier for a record typically
--- housed in an external database.
--- dbxrefs are of the form DB_ID:Local_ID.
--- the combination of DB and local identifier is unique: it
--- is up to the DB_ID authority to ensure Local_ID is unique within
--- their ID space
--- 
--- This table is referenced from a variety of other table:
---  (1) as an xref for a sequence (*seq_dbxref* table)
---  (2) as a primary identifier for a gene product (gene_product != seq except in the case of
---  GOA) (*gene_product.dbxref_id* column)
---  (3) as an xref for a term or term definition  (eg swissprot keywords) (*term_dbxref* table)
---  (4) as evidence for an association based on sequence evidence (*evidence* or *evidence_dbxref* table)
--- 
--- (docs: http://www.geneontology.org/cgi-bin/xrefs.cgi)
CREATE TABLE dbxref (

	id	serial PRIMARY KEY,

        --- @@ dbxref.xref_dbname
        --- The name of the database or ID-granting authority from which information concerning
        --- this dbxref can be retrieved. It is recommended that this comes from the set here:
        --- http://www.geneontology.org/cgi-bin/xrefs.cgi; however, this is not enforced at the
        --- schema level.
        --- There *may* be a corresponding entry in the *db* table, housing metadata on this dbname
        --- but this is not enforced as a foreign key reference
        --- (Example: FB)
        --- (Example: SGD)
        --- (Example: UniProt)
        --- (Example: PubMed)
	xref_dbname varchar(55) not null,

        --- @@ dbxref.xref_key
        --- The local identifier that is unique within xref_dbname
        --- (Example: FBgn0000001)
	xref_key  varchar(255) not null,

        --- @@ dbxref.xref_keytype
        --- DEPRECATED. Was in principle used for what "type" the xref was - eg symbol vs ID
	xref_keytype  varchar(32),

        --- @@ dbxref.xref_desc
        --- DEPRECATED
        --- optional description of dbxref
	xref_desc  varchar(255),

        -- bipartite ID is unique
	UNIQUE(xref_key, xref_dbname)
);

--- @@ db
--- metadata on the different database / accession granting
--- authorities. the data should come from the GO.xref_abbs
--- file.
--- (docs: http://www.geneontology.org/cgi-bin/xrefs.cgi)
--- the dbname is the abbreviation, and should match dbxref.xref_dbname
--- however we have no foreign key so not every dbxref.xref_dbname will
--- have an entry here
--- most columns will not be populated in the short term - the other
--- fields are for future expansion
CREATE TABLE db (
	id serial PRIMARY KEY,

        name varchar(55),
        fullname        varchar(255),
	datatype        varchar(255),
	generic_url     varchar(255),
        url_syntax      varchar(255),
        url_example     varchar(255),
        uri_prefix      varchar(255),

        UNIQUE(name)
);

CREATE UNIQUE INDEX dx0 on dbxref(id);
CREATE INDEX dx1 on dbxref(xref_dbname);
CREATE INDEX dx2 on dbxref(xref_key);
CREATE INDEX dx3 on dbxref(id, xref_dbname);
CREATE INDEX dx4 on dbxref(id, xref_key, xref_dbname);
CREATE INDEX dx5 on dbxref(id, xref_key);
CREATE UNIQUE INDEX dx6 on dbxref(xref_key, xref_dbname);
--- CREATE INDEX dx3 on dbxref(xref_keytype);

CREATE UNIQUE INDEX db0 on db(id);
CREATE INDEX db1 on db(name);
CREATE INDEX db2 on db(fullname);
CREATE INDEX db3 on db(datatype);
