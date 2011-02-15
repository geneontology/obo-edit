--- GO DATABASE TABLE CREATION SQL
--- contact: go-database@fruitfly.bdgp.berkeley.edu

--- Note: this file is the SOURCE sql; it is for defining
--- the schema in a RDBMS vendor independent way. to "compile"
--- this source into SQL that your DBMS (mysql/postgres/etc)
--- will understand, use the "configure" and "make" commands.
--- See the README file in this directory for more details

--- *******************************************************
--- ** go-graph.sql                                      **
--- **                                                   **
--- ** generic tables for representing graphs/triples    **
--- ** the central concept in a GO/OBO style ontology.   **
--- ** Nodes are terms, arcs are relationships between   **
--- ** terms                                             **
--- *******************************************************

--- @@ term
--- Fundamental representational unit in a controlled vocabulary or 
--- ontology. *Terms* form the nodes in the ontology graph
--- (structured using the *term2term* table). An example of a term
--- in GO is GO:0009333 "cysteine synthase complex". Each entry in
--- the term table corresponds to a distinct type (kind, class) of
--- entity in reality.  Note that
--- the term table is also used for storing *relations*. 
--- The two fundamental relations in GO are "is_a" and "part_of".
--- Relations comprise the "labels" of the edges of the ontology
--- graph.
--- Note the term "term" is misleading in that this table is *not*
--- used for storing synonyms and alternate labels; only the
--- *preferred* terms, corresponding to nodes in the ontology graph
--- each representing a distinct type of entity.
--- In OBO-Format, both Term and Typedef (relation) stanzas are
--- housed in the term table.
--- (doc: http://www.geneontology.org/GO.format.obo-1_2.shtml)

CREATE TABLE term (

	id	serial PRIMARY KEY,

        --- @@ term.name
        --- A textual label for the term. Each term has a single
        --- such label (see *term_synonym* for alternate labels). 
        --- The name should be
        --- unique within an ontology (in fact uniqueness is encourage
        --- across ontologies - the principle of univocity. However, this
        --- is not enforced at the database schema level).
        --- the uniqueness recommendation is relaxed in the case of
        --- obsolete terms, which are also housed in this table: there
        --- can be many "ex-terms" with the same name.
        --- In some alternate systems, *term.name* is also known as the
        --- "preferred term"
        --- (OBO-Format: *name* tag)
        --- (Example: "cysteine biosnthetic process")
	name	varchar(255) not null default '',

        --- @@ term.term_type
        --- The ontology or namespace to which this term belongs
        --- (OBO-Format: *namespace* tag)
        --- (Example: biological_process)
        --- (Note: the column name is somewhat misleading, but is
        --- retained for historical reasons. It would be better named
        --- "namespace" or "ontology")
        --- The namespace for GO terms will always be
        --- molecular_function, biological_process or cellular_component.
        --- The relations defined in the main GO obo file (from which
        --- this table is populated) go into the gene_ontology
        --- namespace, with the exception of is_a, which has
        --- namespace "relationship" (taken from the obo relation
        --- ontology). is_a is a builtin relation as far as obo is 
        --- concerned, so it does not go in the gene_ontology namespace
	term_type	varchar(55) not null,

        --- @@ term.acc
        --- The unique identifier for this term.
        --- This should be in OBO bipartite ID format, and should be
        --- unique within OBO, but this is not enforced at the schema
        --- level. 
        --- (Example: GO:0019344)
        --- (OBO-Format: *id* tag)
	acc	varchar(255) not null,

        --- @@ term.is_obsolete        
        --- equals 1 if this row corresponds to an obsoleted "ex-term".
        --- Note that obsoletes are not terms in the true sense, but
        --- we house them in the same table as this is the most expedient
        --- for the kinds of queries people wish to perform.
        --- (OBO-Format: *is_obsolete* tag)
        --- valid values: 0 or 1
	is_obsolete	integer not null default 0,

        --- @@ term.is_root
        --- equals 1 if this term is the root term in the ontology graph.
        --- Note that in some instantiations of the GO database "fake" root
        --- nodes are added
        --- (OBO-Format: No correspoding tag)
	is_root		integer not null default 0,

        --- @@ term.is_relation
        --- equals 1 if this term is a relation (relationship type)
        --- (OBO-Format: Typedef stanzas)
	is_relation		integer not null default 0,

       	unique(acc)
);


--- @@ term2term
--- Each entry in this table corresponds to an arc/edge in
--- the ontology graph, which represents a relationship
--- that holds between two entities in reality.
--- Graphs are often thought of in terms of parent-child 
--- links; with this conception term1_id is the parent
--- and term2_id is the child.
--- However, it may be better to think of each edge as
--- a *statement*, each statement being about a subject
--- and it's relationship to some other entity.
--- For example, a part_of edge between terms "nucleus"
--- and "cell" is a statement about cell nuclei in general,
--- namely that all nuclei are part_of some cell.
--- (the statement is *not* a general statement about cells:
---  not all cells have a nucleus)
--- Here the "subject" of the statement corresponds to term2_id.
--- EXAMPLE:
---   if term1_id points to nucleic acid binding AND
---      term2_id points to DNA binding  AND
---      relationship_type points to "is_a"
---   THEN we have a statement 
---   "DNA binding is_a nucleic acid binding"
--- (OBO-Format: *is_a* tag or *relationship* tag)
CREATE TABLE term2term (

	id	serial PRIMARY KEY,


        --- @@ term2term.relationship_type_id
        --- References an entry in the term table corresponding
        --- to the relation that holds between term2 and term1
        --- (recall that the term table housed both the terms themselves,
        ---  and the relations)
        --- (Example: a reference to a row "part_of" in the term table)
        relationship_type_id integer NOT NULL,
	foreign key (relationship_type_id) references term(id),

        --- @@ term2term.term1_id
        --- the "parent" node of the edge. For example, in
        --- the edge corresponding to "nucleus part_of cell",
        --- (all nuclei are part_of some cell)
        --- term1_id is "cell"
	term1_id	integer not null,
	foreign key (term1_id) references term(id),

        --- @@ term2term.term2_id
        --- the "child" node of the edge. For example, in
        --- the edge corresponding to "nucleus part_of cell",
        --- (all nuclei are part_of some cell)
        --- term2_id is "nucleus"
	term2_id	integer not null,
	foreign key (term2_id) references term(id),
        
        --- @@ term2term.complete
	--- DEPRECATED - use intersection_of instead
        complete        integer not null default 0,

	UNIQUE (term1_id, term2_id, relationship_type_id)
);

--- @@intersection_of
--- this edge comprises an element of the *computable definition*,
--- a set of necessary and sufficient conditions.
--- Example: the term "cysteine metabolism" is completely defined by the edges
--- "is_a metabolism" and "has_participant cysteine" - which is to say anything
--- that satisfies these two conditions is by definition an instance of cysteine
--- metabolism.
--- 
--- intersection_of links/edges may be provided in
--- addition to normal links/edges, even though these may be partially
--- redundant.
--- 
--- Formally: An edge in intersection_of states a NECESSARY CONDITION for
--- term2_id.  The set of all edges in intersection_of
--- for any term2_id states the NECESSARY AND SUFFICIENT CONDITIONS
--- for that term.
--- 
--- a logical definition <"oocyte nucleus" = nucleus and part_of oocyte>
--- would be written in obo format as
---
---  name: oocyte nucleus
---  intersection_of: GO:nucleus
---  intersection_of: part_of CL:oocyte
---
--- we would have two rows in this table
--- row1 = <term2="GO:oocyte nucleus" term1="GO:nucleus" rel="is_a">
--- row1 = <term2="GO:oocyte nucleus" term1="CL:oocyte" rel="part_of">
--- 
--- This table uses the column names "term1_id" and "term2_id"
--- in order to be consistent with term2term, despite the fact that
--- this could be considered unintuitive for this table.
--- 
--- Currently unused in publically deployed GO DB instances.
---
--- Note: this table replaces the use of the 'complete' flag in term2term
CREATE TABLE intersection_of (

	id	serial PRIMARY KEY,

        --- @@ intersection_of.relationship_type_id
        --- References an entry in the term table corresponding
        --- to the relation that holds between term2 and term1
        --- (recall that the term table housed both the terms themselves,
        ---  and the relations)
        --- (Example: a reference to a row "part_of" in the term table)
        relationship_type_id integer NOT NULL,
	foreign key (relationship_type_id) references term(id),

        --- @@ intersection_of.term1_id
        --- the "parent" node of the edge. For example, in
	--- a logical definition "oocyte nucleus" = nucleus and part_of oocyte,
	--- we would have two edges with term1_id = {nucleus,oocyte}
	term1_id	integer not null,
	foreign key (term1_id) references term(id),

        --- @@ intersection_of.term2_id
        --- the "child" node of the edge. For example, in
	--- a logical definition "oocyte nucleus" = nucleus and part_of oocyte,
	--- we would have two edges with term2_id = "oocyte nucleus" in both.
	term2_id	integer not null,
	foreign key (term2_id) references term(id),
        
	UNIQUE (term1_id, term2_id, relationship_type_id)
);

--- @@relation_properties
CREATE TABLE relation_properties (

        --- @@ relation_properties.relationship_type_id
        --- The first relation in the pairwise composition.
        --- References an entry in the term table.
        --- (recall that the term table housed both the terms themselves,
        ---  and the relations)
        relationship_type_id integer NOT NULL,
	foreign key (relationship_type_id) references term(id),

        --- @@ relation_properties.is_transitive        
        --- equals 1 if this relation is transitive.
        --- IF [X R Y] AND [Y R Z] AND [R is_transitive] THEN [X R Z]
        --- (OBO-Format: *is_transitive* tag)
        --- valid values: 0 or 1
	is_transitive	integer,

        --- @@ relation_properties.is_symmetric        
        --- equals 1 if this relation is symmetric.
        --- IF [X R Y] AND [R is_symmetric] THEN [Y R X]
        --- (OBO-Format: *is_symmetric* tag)
        --- valid values: 0 or 1
	is_symmetric	integer,

        --- @@ relation_properties.is_anti_symmetric        
        --- equals 1 if this relation is anti_symmetric.
        --- IF [X R Y] AND [Y R X] AND [R is_anti_symmetric] THEN [X=Y]
        --- (OBO-Format: *is_anti_symmetric* tag)
        --- valid values: 0 or 1
	is_anti_symmetric	integer,

        --- @@ relation_properties.is_cyclic        
        --- equals 1 if this relation is cyclic.
        --- (OBO-Format: *is_cyclic* tag)
        --- valid values: 0 or 1
	is_cyclic	integer,

        --- @@ relation_properties.is_reflexive        
        --- equals 1 if this relation is reflexive.
        --- IF [R is_reflexive] THEN [X R X]
        --- (OBO-Format: *is_reflexive* tag)
        --- valid values: 0 or 1
	is_reflexive	integer,

        --- @@ relation_properties.is_metadata_tag        
        --- equals 1 if this relation is metadata_tag.
        --- IF [X R Y] AND [R is_metadata_tag] THEN [Y R X]
        --- (OBO-Format: *is_metadata_tag* tag)
        --- valid values: 0 or 1
	is_metadata_tag	integer,

	UNIQUE (relationship_type_id)

);

--- @@ relation_composition
--- (See http://wiki.geneontology.org/index.php/Relation_composition)
--- Stores rules of the form: r1 . r2 -> r
--- i.e. IF [ X r1 Y ] AND [ Y r2 Z ] THEN [ X r Z ]
--- Corresponds to "transitive_over" and "holds_over_chain" tags in obo-format.
CREATE TABLE relation_composition (

	id	serial PRIMARY KEY,

        --- @@ relation_composition.relation1_id
        --- The first relation in the pairwise composition.
        --- References an entry in the term table.
        --- (recall that the term table housed both the terms themselves,
        ---  and the relations)
        relation1_id integer NOT NULL,
	foreign key (relation1_id) references term(id),

        --- @@ relation_composition.relation2_id
        --- The second relation in the pairwise composition.
        --- References an entry in the term table.
        --- (recall that the term table housed both the terms themselves,
        ---  and the relations)
        relation2_id integer NOT NULL,
	foreign key (relation2_id) references term(id),

        --- @@ relation_composition.inferred_relation_id
        --- The inferred relation in the pairwise composition.
        --- References an entry in the term table.
        --- (recall that the term table housed both the terms themselves,
        ---  and the relations)
        inferred_relation_id integer NOT NULL,
	foreign key (inferred_relation_id) references term(id),

	UNIQUE (relation1_id, relation2_id, inferred_relation_id)
       
);

--- INDICES

CREATE UNIQUE INDEX t0 on term(id);
CREATE INDEX t1 on term(name);
CREATE INDEX t2 on term(term_type);
CREATE INDEX t3 on term(acc);
CREATE INDEX t4 on term(id,acc);
CREATE INDEX t5 on term(id,name);
CREATE INDEX t6 on term(id,term_type);
CREATE INDEX t7 on term(id,acc,name,term_type);

CREATE INDEX tt1 on term2term(term1_id);
CREATE INDEX tt2 on term2term(term2_id);
CREATE INDEX tt3 on term2term(term1_id, term2_id);
CREATE INDEX tt4 on term2term(relationship_type_id);

CREATE UNIQUE INDEX rp1 on relation_properties(relationship_type_id);

CREATE INDEX rc1 on relation_composition(relation1_id);
CREATE INDEX rc2 on relation_composition(relation2_id);
CREATE INDEX rc3 on relation_composition(inferred_relation_id);
CREATE INDEX rc4 on relation_composition(relation1_id,relation2_id,inferred_relation_id);
