--- GO DATABASE TABLE CREATION SQL
--- contact: go-database@fruitfly.bdgp.berkeley.edu

--- Note: this file is the SOURCE sql; it is for defining
--- the schema in a RDBMS vendor independent way. to "compile"
--- this source into SQL that your DBMS (mysql/postgres/etc)
--- will understand, use the "configure" and "make" commands.
--- See the README file in this directory for more details

--- *******************************************************
--- ** go-optimisations.sql                              **
--- **                                                   **
--- ** these are OPTIONAL optimisations; they are only   **
--- ** suitable for a 'warehouse' style GO database;     **
--- ** this is because every time one of the main        **
--- ** tables is updated, an update here is forced       **
--- ** which would be expensive.                         **
--- **                                                   **
--- *******************************************************

--- @@ graph_path
--- A transitive or implied link between two terms
---
--- Example: if nuclear chromosome is_a chromosome,
--- AND chromosome is_a organelle, then nuclear chromosome
--- is_a organelle
---
--- this table states whether there exists a
--- path between a parent and a child, and the
--- distance between them. multiple paths mean
--- multiple entries in this table
---
--- [this table only used in "data warehouse mode"]
---
--- an entry also exists linking every term with itself
--- of distance 0. This is known as "reflexive transitive
--- closure". [See 
--- http://foldoc.doc.ic.ac.uk/foldoc/foldoc.cgi?query=transitive+closure]
--- [See also
--- http://foldoc.doc.ic.ac.uk/foldoc/foldoc.cgi?query=reflexive+transitive+closure]
---
--- note: use of this table is optional. but as most
--- relational dbs don't implement recursive queries, you
--- will have to incrementally calculate the transitive closure
--- via multiple SQL calls if you do not use it for
--- graph based queries
---
--- At this time, this table holds the general transitive closure
--- across *all* relations. In the future this table may house transitive
--- closure on a per-relation basis, taking into account the various
--- rules that follow from the definitions of the relation in question.
--- For example, if X is_a Y and Y part_of Z then X part_of Z.
CREATE TABLE graph_path (

	id	serial PRIMARY KEY,

        --- @@graph_path.term1_id
        --- the object node
        --- See docs for *term.term1_id* 
	term1_id	integer not null,
	foreign key (term1_id) references term(id),

        --- @@graph_path.term2_id
        --- the subject node
        --- See docs for *term.term2_id* 
	term2_id	integer not null,
	foreign key (term2_id) references term(id),

        --- @@ graph_path.relationship_type_id
        --- References an entry in the term table corresponding
        --- to the INFERRED relation that holds between term2 and term1.
        --- For future extension. See:
        --- http://wiki.geneontology.org/index.php/Transitive_closure
        relationship_type_id integer,
	foreign key (relationship_type_id) references term(id),

        --- @@graph_path.distance
        --- The distance in terms of the number of "hops" between
        --- nodes in the asserted graph (term2term).
        --- The relationship_type_id is ignored here.
        --- Example: if A part_of B is_a C part_of D, then
        --- distance=3 for A part_of D
        distance        integer,

        --- @@graph_path.relation_distance
        --- (added 2008-10-27)
        --- The distance in terms of the number of "hops" over
        --- relationship_type_id in the asserted graph (term2term).
        --- Example: if A part_of B is_a C part_of D, then
        --- relation_distance=2 for A part_of D
        relation_distance        integer

);

--- BEGIN EXPERIMENTAL
--- FOR FUTURE EXPANSION
--- *** not currently populated ***
--- this table only used in "data warehouse mode"
--- each entry in the path table can have multiple 
--- entries in here showing the path from parent to child
CREATE TABLE graph_path2term (

	graph_path_id	        integer not null,
	foreign key (graph_path_id) references graph_path(id),
	term_id	        integer not null,
	foreign key (term_id) references term(id),
        rank            integer not null
);
--- END EXPERIMENTAL



--- @@ gene_product_count
--- this table for use in "data warehouse mode" (will typically be populated
--- in publically available instantiations of the GO database).
--- 
--- caches recursive gene product counts
---  the number of DISTINCT gene product records at OR BELOW a term 
---  filtered by evidence
--- 
--- code refers to the evidence code used to filter this particular count;
--- if preceded by a ! it means exclude this evidence code;
--- typically this will be "!IEA" (ie exclude IEA)
---
--- speciesdbname corresponds to the dbname from gene_product.dbxref_id
--- and represents the authority that contributed the annotated gene product
--- being counted
--- 
--- the product count is partitioned by the speciesdbname
---
--- product_count is the number of DISTINCT gene product IDs owned by speciesdbname
--- at or below term_id in the DAG
---
--- note that the product_count is additive across speciesdbnames (because no two
--- speciesdbnames may contribute the same ID), but is NOT additive across
--- evidence codes (the same gene product can be associated more than once beneath
--- a term with different evidence codes)
---
--- this makes filtering by evidence code hard - we must include all combinations
--- which is a factorial!! to get round this we typically only include counts
--- for non-IEA associations
---
--- Equivalent to the query:
---  SELECT 
---        path.term1_id                      AS term_id,
---        count(DISTINCT a.gene_product_id) AS total
---  FROM 
---   association  AS a
---   INNER JOIN evidence AS e             ON (e.association_id=a.id)
---   INNER JOIN graph_path AS path        ON (path.term2_id=ae.term_id)
---  WHERE <<evidence constraint here>>
CREATE TABLE gene_product_count (

        --- @@ gene_product_count.term_id
        --- the term for which gene products are counted; also
        --- includes terms below this term in the graph
	term_id	integer not null,
	foreign key (term_id) references term(id),

        --- @@ gene_product_count.code
        --- evidence code over which this count hold.
        --- can include negation; eg "!IEA"
        --- note that not every combination will be pre-computed.
        --- typically just !IEA is stored. Also note that counts
        --- over evidence codes are NON-additive; this is because
        --- the same gene_product can be double-counted if it is
        --- anntated with >1 evidence code
        code            varchar(8),

        --- @@ gene_product_count.speciesdbname
        --- this should match the gene_product.dbxref
        --- (Examples: FlyBase, SGD, MGI, UniProt)
        --- (docs: http://www.geneontology.org/cgi-bin/xrefs.cgi)
        --- counts *ARE* additive across speciesdbnames - this is because
        --- each gene_product record belongs to a single speciesdbname,
        --- so double counting is not possible (although the same gene product
        --- in reality may be double annotated, for example by UniProt and by
        --- a Model Organism Database - these should be filtered out, but even
        --- in cases where this filtering fails we do not worry about double-counting
        --- when summing across speciesdbnames as we are technically counting distinct
        --- gene product *records*)
        --- (Note: there is no foreign key reference to the *db* table,
        ---  but their could in principle be a nullable link here)
	speciesdbname   varchar(55),

	--- @@ gene_product_count.species_id
        --- The species or taxon the count pertains to
        --- (Note: in future we reserve the option to use gene_product to represent families at higher
        ---  levels in the taxonomic tree above species)
	species_id 	integer,
	foreign key (species_id) references species(id),


        --- @@ gene_product_count.product_count
        --- total number of DISTINCT genes/gene product records annotated directly
        --- to or via transitivity to term_id 
        product_count   integer not null
);


CREATE UNIQUE INDEX graph_path0 on graph_path(id);
CREATE INDEX graph_path1 on graph_path(term1_id);
CREATE INDEX graph_path2 on graph_path(term2_id);
CREATE INDEX graph_path3 on graph_path(term1_id, term2_id);
CREATE INDEX graph_path4 on graph_path(term1_id, distance);
CREATE INDEX graph_path5 on graph_path(term1_id, term2_id, relationship_type_id);
CREATE INDEX graph_path6 on graph_path(term1_id, term2_id, relationship_type_id, distance, relation_distance);
CREATE INDEX graph_path7 on graph_path(term2_id, relationship_type_id);
CREATE INDEX graph_path8 on graph_path(term1_id, relationship_type_id);
-- CREATE INDEX graph_path9 on graph_path(term1_id, relationship_type_id, distance);

CREATE INDEX gpc1 on gene_product_count(term_id);
CREATE INDEX gpc2 on gene_product_count(code);
CREATE INDEX gpc3 on gene_product_count(speciesdbname);
CREATE INDEX gpc4 on gene_product_count(term_id, code, speciesdbname);
CREATE INDEX gpc5 on gene_product_count(term_id, species_id);
