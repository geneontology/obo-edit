--- GO DATABASE TABLE CREATION SQL
--- contact: go-database@fruitfly.bdgp.berkeley.edu
--- 
--- Note: this file is the SOURCE sql; it is for defining
--- the schema in a RDBMS vendor independent way. to "compile"
--- this source into SQL that your DBMS (mysql/postgres/etc)
--- will understand, use the "configure" and "make" commands.
--- See the README file in this directory for more details
--- 
--- *******************************************************
--- ** go-associations.sql                               **
--- **                                                   **
--- ** gene products and associations between            **
--- ** products and terms                                **
--- **                                                   **
--- *******************************************************
---
--- CORRESPONDING GO SOURCE FILE:
--- ftp://ftp.geneontology.org/pub/go/gene-associations/gene_association.*
---
--- for documentation, see
--- http://www.geneontology.org/doc/GO.annotation.shtml#file
--- http://www.geneontology.org/GO.format.annotation.shtml
---
--- MAPPING:
---
--- COL    NAME		     GO-DB SCHEMA
--- ===    ====              ============
--- 1	   DB		     gene_product x dbxref.xref_dbname
--- 2	   DB_Object_ID	     gene_product x dbxref.xref_xref_key
--- 3	   DB_Object_Symbol  gene_product.symbol
--- 4      NOT		     association.is_not
---        Qualifiers	     association_qualifier
--- 5      GOid              association x term.acc
--- 6      DB:Reference	     association x evidence.dbxref_id
--- 7      Evidence	     association x evidence.code
--- 8      With/From         evidence.seq_acc       [DENORMALIZED]
---                          evidence x evidence_dbxref x dbxref [NORMALIZED]
--- 9      Aspect            association x term.term_type
--- 10     DB_Object_Name    gene_product.full_name
--- 11     Synonym           gene_product x gene_product_synonym
--- 12     DB_Object_type    gene_product.type_id x term.name    [TBA]
--- 13     Taxon             gene_product x species.ncbi_taxa_id
--- 14     Date              association.assocdate
--- 15     Assigned_by       association.source_db_id x db.name


--- @@ species
--- Linnaean taxonomic information for an organism type. Modeled after NCBI Taxonomy
--- (Note: the name of the table is misleading, as it can model ANY node in Linnaen taxonomy)
--- (The table should be better called "taxon")
CREATE TABLE species (

	id	serial PRIMARY KEY,

        --- @@ species.ncbi_taxa_id
        --- identifier within the NCBI Taxonomy database. 
        --- (Example: Dmel=7227)
        --- (Example: S Cerevisae=4932)
	ncbi_taxa_id	integer,

	--- @@ species.common_name
        --- Non-scientific name
        --- (Example: fruitfly)
	common_name     varchar(255),

	--- @@ species.lineage_string
        --- denormalized list of taxon names as text.
        --- (Note: not currently populated)
	lineage_string	text,

	--- @@ species.genus
        --- If the row in the table is genuinely a species, this column is
        --- for storing the "genus" in the Linnaean system. If the row is a higher
        --- taxon, then this column is for the scientific name of that taxon.
        --- (Example: Drosophila -- for leaf node taxon) 
        --- (Example: Homo -- for leaf node taxon) 
        --- (Example: Metazoa -- for non-leaf node taxon) 
        --- unfortunately the name of this column is misleading. However, it will
        --- be retained for backwards compatibility
	genus		varchar(55),

	--- @@ species.species
        --- If the row in the table is genuinely a species, this column is
        --- for storing the "species" name in the Linnaean system. If the row is a higher
        --- taxon, this column is null
        --- (Example: sapiens)
        --- (Example: pombe)
        --- unfortunately the name of this column is misleading. However, it will
        --- be retained for backwards compatibility
        --- 
        --- note that (genus,species) is not declared unique
	species		varchar(255),

	--- @@ species.parent_id
        --- parent taxon in hierarchy (direct parent - for indirect parents see left_value and right_value)
        parent_id	integer,

	--- @@ species.left_value
        --- left_value, right_value implement a nested set model
        --- see http://www.oreillynet.com/pub/a/network/2002/11/27/bioconf.html
        --- or Joe Celko's "SQL for smarties" for more information.
        --- 
        left_value	        integer,

	--- @@ species.right_value
        --- see left_value
        right_value	integer,

	--- @@ species.taxonomic_rank
        --- eg species, family, phylum, ...
        taxonomic_rank	varchar(255),

        unique(ncbi_taxa_id)
        --- unique(genus, species)
);

--- @@ gene_product
--- Represents a gene or gene_product, typically at the species level.
--- GO allows for annotation of genes OR gene products. Annotation of a gene
--- is understood to be "proxy" for annotation of the corresponding 
--- gene products.
--- (docs: http://www.geneontology.org/GO.annotation.fields.shtml)
--- (docs: http://www.geneontology.org/GO.annotation.shtml#file)
--- (Example: human p53-gene)
--- (Example: human p53-protein)
CREATE TABLE gene_product (

	id	serial PRIMARY KEY,

	--- @@ gene_product.symbol
	--- concise label for this gene product
        --- (Example: p53)
        --- (Example: BRCA)
        --- (Example: PHO3)
	--- typically unique within an originating database authority, but not guaranteed;
	--- (an example of such as authority is FlyBase or UniProt)
	symbol	varchar(128)   not null,

	--- @@ gene_product.dbxref_id
        --- A globally unique identifier for this gene or gene product.
        --- All (non-GO) unique identifiers are stored as dbxrefs - they must consist
        --- of both a DB and a DB_Object_ID
        --- (Example: SGD:S000000296)
        --- (column 1 and 2 in gene_association file)
	dbxref_id	integer not null,
	foreign key (dbxref_id) references dbxref(id),

	--- @@ gene_product.species_id
        --- The species or taxon to which this gene product belongs
        --- (Note: in future we reserve the option to use gene_product to represent families at higher
        ---  levels in the taxonomic tree above species)
        --- (column 13 in gene_association file; if card>1, this is the first entry)
	species_id 	integer,
	foreign key (species_id) references species(id),

        --- @@ gene_product.type_id
        --- gene_product type (eg gene, transcript, protein, complex)
        --- (column 13 in the gene-association file)
        --- the type term may correspond to a SO ID, but typically SO
        --- is not loaded an an ontology into the GO database
	type_id 	integer,
	foreign key (type_id) references term(id),

        --- @@ gene_product.full_name
        --- symbol is typically a concise label, full_name may be
        --- more textual
        --- (column 10 in the gene-association file)
	full_name	text,

	unique(dbxref_id)

);

--- @@ gene_product_synonym
--- alternate label for the gene or gene product
--- (column 11 in the gene-association file)
CREATE TABLE gene_product_synonym (

	gene_product_id	integer not null,
	foreign key (gene_product_id) references gene_product(id),

        --- @@ gene_product_synonym.product_synonym
        --- alternate label. Typically NOT redundant with gene_product.symbol
        --- or gene_product.full_name, but this is not guaranteed
        --- (column 11 in the gene-association file)
	product_synonym		varchar(255) not null,

	unique(gene_product_id, product_synonym)
);

--- BEGIN EXPERIMENTAL
--- @@ gene_product_property
CREATE TABLE gene_product_property (

	gene_product_id	integer not null,
	foreign key (gene_product_id) references gene_product(id),

	property_key		varchar(64) not null,
        property_val            varchar(255)
---create named unique index to avoid index name clash (in pg)
);

CREATE INDEX gpp1 on gene_product_property(gene_product_id);
CREATE INDEX gpp2 on gene_product_property(property_key);
CREATE INDEX gpp3 on gene_product_property(property_val);
CREATE UNIQUE INDEX gppu4 on gene_product_property(gene_product_id, property_key, property_val);
--- END EXPERIMENTAL


--- @@ association
--- Annotation model: 
--- An association is a link between a gene product record and an ontology term,
--- with one or more pieces of evidence
--- *** IMPORTANT:
---     NOT all associations are positive: some posit negative links. THESE
---     SHOULD TYPICALLY BE FILTERED OUT FOR MOST ANALYSIS PURPOSES.
---     See the is_not column ***

CREATE TABLE association (

	id  	serial PRIMARY KEY,

        -- @@ association.term_id
        -- the (GO) term to which the gene_product is associated
	term_id	        integer not null,
	foreign key (term_id) references term(id),

        -- @@ association.gene_product_id
        -- the gene or gene_product to which the term is associated
	gene_product_id	integer not null,
	foreign key (gene_product_id) references gene_product(id),

        -- @@ association.is_not
        -- ** IMPORTANT **
	-- when this field is non-zero, the meaning of the annotation is
        -- that the gene_product does NOT have the role defined by the GO term
        -- (column 4 = NOT in the gene-association file)
        -- See also: association_qualifier table
	is_not          integer,

--- BEGIN EXPERIMENTAL
        role_group      integer,
--- "roles"
--- this table contains a "group" number which is optional
--- the idea is that associations could be seperated into groups
--- representing different roles/actions of the gene product
--- at first this will be unpopulated, as GO currently lumps together
--- all associations, but this allows the option of statements such as
--- "product X has function F in cell component C"
--- NOTE:
---  in future we may have more complex statements
---  about annotations which will require a schema
---  change, although the current table will probably
---  be retained as a [materialized] view
---
--- END EXPERIMENTAL

        --- @@ association.assocdate
        --- a date in YYYYMMDD format.
        --- This is the date the association was last checked
        ---  the source db providers
        --- (column 14 in the gene-association file)
	assocdate	integer,

        --- @@ association.source_db_id
        --- the source of the association; for instance, the association
        --- file may come from SwissProt, but the source of the association
        --- (Example: SGD)
        --- (Example: MGI)
        --- (column 15 = NOT in the gene-association file)
        --- (docs: http://www.geneontology.org/cgi-bin/xrefs.cgi)
	source_db_id	integer,
	foreign key (source_db_id) references db(id)

);

--- @@ association_qualifier
--- associations can have a number of qualifiers. These include,
--- but are not limited to the NOT qualifier (which technically is
--- not a qualifier at all as it fundamentally changes the semantics
--- of an association)
--- note that this table IS redundant with association.is_not
--- this is intentional - negation is important enough to go directly
--- in the association table. It also goes in this table for
--- consistency with the gene association file
--- (column 4 in the gene-association file)
--- (docs: http://www.geneontology.org/GO.format.annotation.shtml)
CREATE TABLE association_qualifier (

	id  	serial PRIMARY KEY,

	association_id	integer not null,
	foreign key (association_id) references association(id),

        --- @@ association_qualifier.term_id
        --- qualifiers come from their own terminology
	term_id	        integer not null,
	foreign key (term_id) references term(id),

        --- @@ association_qualifier.value
        --- qualifiers can potentialy be tag=value pairs.
        --- however, all qualifiers are currently boolean
        --- tags, so this column is always null
        value   varchar(255)
);

--- @@ association_property
--- (column 16 in the gene-association file)
--- (see http://wiki.geneontology.org/index.php/Annotation_Cross_Products)
CREATE TABLE association_property (

	id  	serial PRIMARY KEY,

	association_id	integer not null,
	foreign key (association_id) references association(id),

        --- @@ association_property.relationship_type_id
        --- e.g. the term.id that has acc='part_of'
	relationship_type_id	        integer not null,
	foreign key (relationship_type_id) references term(id),

        --- @@ association_property.term_id
        --- e.g. the term.id that has acc='CL:0000017'
	term_id	        integer not null,
	foreign key (term_id) references term(id)
);

--- @@ association_species_qualifier
--- (see http://www.geneontology.org/GO.annotation.shtml#manySpp)
--- (column 13 in gene_association file, ONLY WHEN card>1, this is the next entry)
--- to be used only in conjunction with terms that have the term 'interaction between organisms' as an ancestor.
--- gene_product.species_id is for the organism type encoding the gene or gene product,
--- association_species_qualifier.species_id should be that of the other organism in the interaction.
--- aka "dual taxon"
CREATE TABLE association_species_qualifier (

	id  	serial PRIMARY KEY,

	association_id	integer not null,
	foreign key (association_id) references association(id),

	--- @@ association_species_qualifier.species_id
        --- The species of the interacting organism (eg host)
        --- Example: in cytolysis of cells of another organism (GO:0051715)
        --- this would be the species playing the 'other organism' role
	species_id 	integer,
	foreign key (species_id) references species(id)
);

--- @@ evidence
--- each association can have one or more pieces of
--- evidence attached to it (the schema actually allows
--- zero or more, but with GO all annotation have at
--- least one piece of evidence)
--- (doc: http://www.geneontology.org/GO.evidence.shtml)
CREATE TABLE evidence (

	id  	serial PRIMARY KEY,

        --- @@ evidence.code
        --- a string code (typically 3-letter) corresponding to
        --- a GO evidence code.
        --- (column 7 in the gene-association file)
        --- (Example: IEA - inferred from electronic annotation)
        --- (Example: IMP - inferred from mutant phenotype)
        --- evidence codes may eventually become "ontologized", allowing
        --- us to take full advantage of the OBO evidence ontology:
        --- http://www.obofoundry.org/cgi-bin/detail.cgi?evidence_code
	code	varchar(8) not null,

	association_id	integer not null,
	foreign key (association_id) references association(id),

        --- @@ evidence.dbxref_id
        --- A reference for the annotation. Typically a pubmed ID
        --- (column 6 in the gene-association file)
	dbxref_id	integer not null,
	foreign key (dbxref_id) references dbxref(id),

        --- @@ evidence.seq_acc
        --- a denormalised field containing a "|" separated
        --- list of accession supporting the call. for
        --- the normalised data, use evidence_dbxref
        --- (column 8 in the gene-association file, copied identically)
	seq_acc	varchar(255), 

	UNIQUE (association_id, dbxref_id, code)

);

--- @@ evidence_dbxref
--- each piece of evidence can have multiple dbxrefs associated
--- with it; this is the *normalised* version of the "With" or "From"
--- field of the evidence
--- (column 8 in the gene-association file, normalized)
CREATE TABLE evidence_dbxref (
	evidence_id integer not null,
	foreign key (evidence_id) references evidence(id),

        --- @@ evidence_dbxref.dbxref_id
        --- globally unique identifier for the evidence 
        --- (Example: GO:0000346 - will be stored as DB=GO, Acc=0000346)
	dbxref_id integer not null,
	foreign key (dbxref_id) references dbxref(id)
);

--- @@ gene_product_subset
--- Not yet in production
CREATE TABLE gene_product_subset (

	gene_product_id	integer not null,
	foreign key (gene_product_id) references gene_product(id),
	subset_id	integer not null,
	foreign key (subset_id) references term(id)
);

--- @@ gene_product_dbxref
--- Not yet in production.
--- Will replace the existing convoluted means of fetching xrefs via seqs
CREATE TABLE gene_product_dbxref (

	gene_product_id	integer not null,
	foreign key (gene_product_id) references gene_product(id),
	dbxref_id	integer not null,
	foreign key (dbxref_id) references term(id)
);


--- BEGIN EXPERIMENTAL
--- @@ assoc_rel
---
--- NOT POPULATED YET
--- for "conjunctive" annotations, or "slot-based" annotations.
--- currently all GO annotations are "disjunctive" (ie unrelated)
--- this table provides a way of linking together annotations.
---
--- for example:
---
--- "gene foo has function F during process P in cell component C"
---     treated as 3 associations: foo/F, foo/P, foo/C
---     assocs 1 and 2 are related by the type "acts_during" (for example)
---     assocs 1 and 3 are related by the type "acts_in"
---
--- "gene foo has function biosynthesis, acting on interleukin-18"
---     treated as 2 associations: foo/biosynth, foo/il-18
---     assocs are related by the type "acts_on" (for example)
---
--- the from_id is for the subject (domain); the to_id is for the
--- object (range). if a gene has function F during process P, then
--- the from_id is the association to F, and the to_id is the
--- association to P
---
--- the nature of the relationship (aka slot) is a controlled term
--- in the ontology; for example "acts_on", "acts_during",...

CREATE TABLE assoc_rel (
	id  	serial PRIMARY KEY,

	from_id	integer not null,
	foreign key (from_id) references association(id),

	to_id	integer not null,
	foreign key (to_id) references association(id),

        relationship_type_id integer NOT NULL,
	foreign key (relationship_type_id) references term(id)		
);
--- END EXPERIMENTAL

CREATE UNIQUE INDEX sp0 on species(id);
CREATE INDEX sp1 on species(ncbi_taxa_id);
CREATE INDEX sp2 on species(common_name);
CREATE INDEX sp3 on species(genus);
CREATE INDEX sp4 on species(species);
CREATE INDEX sp5 on species(genus, species);
CREATE INDEX sp6 on species(id, ncbi_taxa_id);
CREATE INDEX sp7 on species(id, ncbi_taxa_id, genus, species);
CREATE INDEX sp8 on species(parent_id);
CREATE INDEX sp9 on species(left_value);
CREATE INDEX sp10 on species(right_value);
CREATE INDEX sp11 on species(left_value,right_value);
CREATE INDEX sp12 on species(id,left_value);
CREATE INDEX sp13 on species(genus,left_value,right_value);


CREATE UNIQUE INDEX a0 on association(id);
CREATE INDEX a1 on association(term_id);
CREATE INDEX a2 on association(gene_product_id);
CREATE INDEX a3 on association(term_id, gene_product_id);
CREATE INDEX a4 on association(id, term_id, gene_product_id);
CREATE INDEX a5 on association(id, gene_product_id);
CREATE INDEX a6 on association(is_not,term_id, gene_product_id);
CREATE INDEX a7 on association(assocdate);

CREATE INDEX aq1 on association_qualifier(association_id,term_id);

CREATE UNIQUE INDEX g0 on gene_product(id);
CREATE INDEX g1 on gene_product(symbol);
CREATE INDEX g2 on gene_product(dbxref_id);
CREATE INDEX g3 on gene_product(species_id);
CREATE INDEX g4 on gene_product(id, species_id);
CREATE INDEX g5 on gene_product(dbxref_id, species_id);
CREATE INDEX g6 on gene_product(id, dbxref_id);
CREATE INDEX g7 on gene_product(id, species_id);
CREATE INDEX g8 on gene_product(id, dbxref_id,species_id);

CREATE INDEX gs1 on gene_product_synonym(gene_product_id);
CREATE INDEX gs2 on gene_product_synonym(product_synonym);

CREATE UNIQUE INDEX ev0 on evidence(id);
CREATE INDEX ev1 on evidence(association_id);
CREATE INDEX ev2 on evidence(code);
CREATE INDEX ev3 on evidence(dbxref_id);
CREATE INDEX ev4 on evidence (association_id, code);    
CREATE UNIQUE INDEX ev5 on evidence (id,association_id);    
CREATE UNIQUE INDEX ev6 on evidence (id,code,association_id);    

CREATE INDEX evx1 on evidence_dbxref(evidence_id);
CREATE INDEX evx2 on evidence_dbxref(dbxref_id);
CREATE INDEX evx3 on evidence_dbxref(evidence_id, dbxref_id);

CREATE INDEX gps1 on gene_product_subset(gene_product_id);
CREATE INDEX gps2 on gene_product_subset(subset_id);
CREATE UNIQUE INDEX gps3 on gene_product_subset(gene_product_id,subset_id);


CREATE INDEX gpx1 on gene_product_dbxref(gene_product_id);
CREATE INDEX gpx2 on gene_product_dbxref(dbxref_id);
CREATE UNIQUE INDEX gpx3 on gene_product_dbxref(gene_product_id,dbxref_id);

