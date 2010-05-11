
--- @@ homolset
--- A collection of genes or gene products from a common evolutionary lineage.
--- The purpose of this table is to group homologous sets of gene products 
--- to query for shared and divergent biological function.
--- The table is neutral with respect to the method used to determine evolutionary
--- relations; instead it represents the derived results of some kind of some analysis.
--- A homolset may also be derived from a collection of analyses.
--- The table is also neutral w.r.t questions of homology or orthology
--- REQUIRED FOR REFERENCE GENOMES PROJECT
CREATE TABLE homolset (
	id	serial PRIMARY KEY,

	--- @@ homolset.symbol
        --- A convenient human-assigned label for the homology set; this may
        --- be arbitarily chosen from one of the set members (frequently the
        --- target_gene_product); or it may be the name of the gene family.
        --- no guarantee is given to its usefulness
	symbol	varchar(128),

	--- @@ homolset.dbxref_id
        --- A globally unique identifier for this set, or some proxy for the set.
        --- For example, if the purpose of building homolsets is to examine disease genes,
        --- then an OMIM ID may be appropriate to use here, even though OMIM is not concerned with
        --- homology or orthology. If the homolset is derived 
	dbxref_id	integer,
	foreign key (dbxref_id) references dbxref(id),

        --- @@ homolset.target_gene_product_id
        --- A homolset may be constructed from a collection of pairwise
        --- homology assignments between individual gene products and a
        --- (possibly arbitrary) "target" gene_product. 
        --- this field is optional: for example, a target is not required
        --- for sets that are derived from a tree-based analysis
	target_gene_product_id	integer,
	foreign key (target_gene_product_id) references gene_product(id),

	--- @@ homolset.taxon_id
        --- The least common ancestor of all members of the homolset.
        --- (may not be populated)
	taxon_id 	integer,
	foreign key (taxon_id) references species(id),

        --- @@ homolset.type_id
        --- homolsets may fall into different categories - this field
        --- identifies the category. May not be populated
	type_id 	integer,
	foreign key (type_id) references term(id),

        --- @@ homolset.description
        --- for example: if the purpose of the homolset is to examine
        --- disease genes and model organism orthologs, the description
        --- could be a summary of the disease in human
	description	text,

	unique(dbxref_id)
);

--- @@ gene_product_homolset
--- a set-member relation between a gene product and the homolset to which it belongs.
--- the relation should, where possible, be supported by individual homology-based
--- relations [TODO]
--- REQUIRED FOR REFERENCE GENOMES PROJECT
CREATE TABLE gene_product_homolset (
	id	serial PRIMARY KEY,

	gene_product_id	integer not null,
	foreign key (gene_product_id) references gene_product(id),
	homolset_id	integer not null,
	foreign key (homolset_id) references homolset(id)
);

-- note: the following tables are not required for the refG project
-- they may eventually be used for more complex evo-ontology analyses
-- in which gene trees and individual pairwise homology relations must
-- be calculated

--- @@ phylotree
--- A phylogenetic tree.
--- This is a new table - this will be populated from PANTHER datasets.
--- Additional rows or property tables may be added at a future date; e.g.
--- to add bootstrap values and so on.
CREATE TABLE phylotree (
	id	serial PRIMARY KEY,
	name	varchar(255) not null default '',

	--- @@ phylotree.dbxref_id
        --- A globally unique identifier for this tree.
        --- e.g. PantherDB:PTHR11361
        --- make sure dbname is registered in GO.xrf_abbs
	dbxref_id	integer not null,
	foreign key (dbxref_id) references dbxref(id),

	unique(dbxref_id)
);

--- @@ phylotree_property
--- Example properties:
---   newick - ascii blob containing newick-format for tree
CREATE TABLE phylotree_property (
	id	serial PRIMARY KEY,

	phylotree_id	integer not null,
	foreign key (phylotree_id) references phylotree(id),

	property_key		varchar(64) not null,
        property_val            text
	
);

--- @@ gene_product_phylotree
--- a set-member relation between a gene product and the phylogenetic tree to which it belongs.
--- This is a new table - this will be populated from PANTHER datasets
CREATE TABLE gene_product_phylotree (
	id	serial PRIMARY KEY,

	gene_product_id	integer not null,
	foreign key (gene_product_id) references gene_product(id),
	phylotree_id	integer not null,
	foreign key (phylotree_id) references phylotree(id)
);


--- @@ gene_product_ancestor
--- connects two nodes in a phylogenetic tree
CREATE TABLE gene_product_ancestor (

	gene_product_id	integer not null,
	foreign key (gene_product_id) references gene_product(id),

	ancestor_id	integer not null,
	foreign key (ancestor_id) references gene_product(id),

	phylotree_id	integer not null,
	foreign key (phylotree_id) references phylotree(id),

        branch_length   float,

        is_transitive integer not null default 0,

        UNIQUE(gene_product_id,ancestor_id,phylotree_id)
);


--- @@ gene_product_homology
CREATE TABLE gene_product_homology (

	gene_product1_id	integer not null,
	foreign key (gene_product1_id) references gene_product(id),

	gene_product2_id	integer not null,
	foreign key (gene_product2_id) references gene_product(id),

        --- @@ gene_product_homology.relationship_type_id
        --- References an entry in the term table corresponding
        --- to the relation that holds between 1 and 2
        relationship_type_id integer NOT NULL,
	foreign key (relationship_type_id) references term(id)

);
