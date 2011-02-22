--- GO DATABASE TABLE CREATION SQL
--- contact: go-database@fruitfly.bdgp.berkeley.edu

--- Note: this file is the SOURCE sql; it is for defining
--- the schema in a RDBMS vendor independent way. to "compile"
--- this source into SQL that your DBMS (mysql/postgres/etc)
--- will understand, use the "configure" and "make" commands.
--- See the README file in this directory for more details

--- *******************************************************
--- ** go-meta.sql                                       **
--- **                                                   **
--- ** meta data specific to the core GO model           **
--- ** that doesn't relate to graphs per se;             **
--- ** extra information about the nodes in the graph    **
--- **                                                   **
--- *******************************************************

--- @@ term_definition
--- In OBO, a term should where possible be defined. A term
--- can have only one definition.
--- Note: due to this cardinality constraint, it would be
--- possible to merge term_definition into the *term* table.
--- The decision was made not to do this early on, and the schema
--- has not been changed since.
--- (doc: http://www.geneontology.org/GO.format.obo-1_2.shtml)
--- (OBO-Format: *def* tag; and also the *comment* tag - see column documentation)
CREATE TABLE term_definition (

	term_id	integer not null,
	foreign key (term_id) references term(id),

        --- @@ term_definition.term_definition
        --- A textual definition for the term referenced
        --- in term_definition.term_id.
        --- The attentive user will have two criticisms;
        --- the first is that it is poor practice for a column
        --- to share a name with a table. The second is for more
        --- egregious:
        --- although this column is declared non-null, in fact
        --- some entries are in fact empty strings. This is
        --- due to the fact that term comments are housed in this table,
        --- rather than in the term table or their own term_comment table!
        --- We accept that this was a poor design decision. However, we have
        --- no plans to rectify this modeling error in the short term as this
        --- would break many pieces of 3rd party software not under our control.
        --- No fixes are likely until after MySQL has good efficient support
        --- for SQL Views.
        term_definition	text not null,

        --- @@ term_definition.dbxref_id
        --- SEVERELY DEPRECATED: see term_dbxref.is_for_definition
        dbxref_id integer,
	foreign key (dbxref_id) references dbxref(id),

        --- @@ term_definition.term_comment
        --- A free-text comment with non-definitional information that may
        --- be useful for end-users or curators.
        --- (OBO-Format: *comment* tag - each term has max 1 comment)
        --- Please see notes for term_definition column
        term_comment text,

        --- @@ term_definition.reference
        --- SEVERELY DEPRECATED
        reference varchar(255),

        -- each term has max 1 definition
	UNIQUE (term_id)
);

--- @@ term_synonym
--- In OBO, each term can have 0 or more synonyms
--- or alternate identifiers. A synonym is an alternate
--- label for a preferred term, intended for humans.
--- An alternate identifer is an OBO ID intended for
--- unique identification of the term in information systems.
--- Note: OBO Format has the concept of broad and narrow
--- synonyms - these might better be called broad and narrow
--- aliases or alternate labels, since technically the definition
--- of synonym is such that synonyms can replace words without
--- changing the meaning.
CREATE TABLE term_synonym (

	term_id	integer not null,
	foreign key (term_id) references term(id),

        --- @@ term_synonym.term_synonym
        --- A textual label typically intended for humans.
        --- (Example: "cysteine biosynthesis" is a synonym
        ---  for the term named "cysteine biosynthetic process")
        --- One wrinkle is that alternate identifiers are
        --- redundantly stored in this column, as well as in
        --- the acc_synonym column. This design decision can
        --- be rightfully criticised, but we retain this
        --- use for software compatibility reasons.
        --- alt_ids can ve discriminated from synonyms using
        --- synonym_type_id - this will be "alt_id" for alternate
        --- identifiers.
        --- Note: although we accept it is bad practice to
        --- name columns the same as tables, we retain this
        --- for software compatibility reasons
        --- (OBO-Format: *synonym* tag)
	term_synonym		varchar(996),

        --- @@ term_synonym.acc_synonym
        --- An alternate identifier.
        --- (OBO-Format: *alt_id* tag)
        --- See also docs for term_synonym column
	acc_synonym		varchar(255),

        --- @@ term_synonym.synonym_type_id
        --- actually corresponds to a synonym "scope" in
        --- OBO-Format - one of exact, broad, narrow, related
        --- for alternate identifiers we use the term "alt_id" -
        --- see notes above
	synonym_type_id integer not null,
	foreign key (synonym_type_id) references term(id),

        --- @@ term_synonym.synonym_category_id
        --- category/class to which this synonym belongs
        --- Correspinds to type OBO-Format 1.2
        --- Not currently used in GO
        --- Note: the synonym "scope" (eg exact, narrow) goes
        --- in synonym_type_id
	synonym_category_id integer,
	foreign key (synonym_category_id) references term(id),

	UNIQUE (term_id, term_synonym)
);

--- @@ term_dbxref
--- linking table between term and dbxref used where there is
--- some other information entity of relevance to the term in question;
--- it may be a dbxref for a publication defining the term, or it may
--- be a reference to an entity in another database, terminology or
--- ontology-like system with similar or identical semantics to the term.
CREATE TABLE term_dbxref (

	term_id	    integer not null,
	foreign key (term_id) references term(id),

	dbxref_id   integer not null,
	foreign key (dbxref_id) references dbxref(id),

        --- @@ term_dbxref.is_for_definition
        --- equals 1 if this dbxref references the source of the definition
        --- of the term in question. This includes but is not limited to
        --- PubMed IDs. It may also be so-called "GO Curator" references, of the
        --- form GOC:<curator-id>
        is_for_definition integer not null default 0,

	unique(term_id, dbxref_id, is_for_definition)
);

--- BEGIN EXPERIMENTAL
--- @@ term_property
--- Extensible metadata on a term
--- (OBO-Format: *propery_value* tag)
--- An example of this may be to represent the "rank" in a Linnaen-style taxonomy or
--- ontology-like system.
--- This table is not used in GO, but it may be used for other ontologies
--- that require additional tags beyond the core set provided for Terms
CREATE TABLE term_property (

	term_id	integer not null,
	foreign key (term_id) references term(id),
	property_key		varchar(64) not null,
        property_val            varchar(255)
);
--- END EXPERIMENTAL

--- @@ term_subset
--- (aka goslims). Each subsetdef (slim) is stored as a term in the database (with term_type = 'subset')
--- The subset_id links to this term
--- (OBO-Format: *subset* tag. term_id references a term housing the *subsetdef*)
CREATE TABLE term_subset (

	term_id	integer not null,
	foreign key (term_id) references term(id),
	subset_id	integer not null,
	foreign key (subset_id) references term(id)
);

--- @@ term2term_metadata
--- a metadata link between two terms
--- this is primarily to support the "consider"
--- and "replaced_by" tags in OBO Format 1.2.
--- It could also be used for other metadata links
--- we may want to include in the future.
---
--- The main difference between term2term and this
--- table is that term2term is for encoding the
--- relationships that hold between types of biological
--- entity, whereas this table is for relationships
--- between the units in the ontology. Different  rules
--- apply to both.
--- Eg consider/replaced_by would never
--- propagate over the is_a relation
--  open question: do we also want to include disjointness axioms here?
CREATE TABLE term2term_metadata (

	id	serial PRIMARY KEY,


        --- @@ term2term_metadata.relationship_type_id
        --- "consider" or "replaced_by"
        --- may in future be extended to other tags
        relationship_type_id integer NOT NULL,
	foreign key (relationship_type_id) references term(id),

        --- @@ term2term_metadata.term1_id
        --- the "parent" node of the edge. For example, in
        --- the edge corresponding to
        --- for "GO:0005696 telomere" consider "GO:0000781 chromosome, telomeric region"
        --- term1_id is "GO:0005696"
	term1_id	integer not null,
	foreign key (term1_id) references term(id),

        --- @@ term2term_metadata.term2_id
        --- the "child" node of the edge. For example, in
        --- the edge corresponding to
        --- for "GO:0005696 telomere" consider "GO:0000781 chromosome, telomeric region"
        --- term2_id is "GO:0000781"
	term2_id	integer not null,
	foreign key (term2_id) references term(id),
        

	UNIQUE (term1_id, term2_id)
);



CREATE INDEX td1 on term_definition(term_id);
--- CREATE INDEX td2 on term_definition(term_definition);

CREATE INDEX ts1 on term_synonym(term_id);
CREATE INDEX ts2 on term_synonym(term_synonym);
CREATE INDEX ts3 on term_synonym(term_id, term_synonym);

CREATE INDEX tx0 on term_dbxref(term_id);
CREATE INDEX tx1 on term_dbxref(dbxref_id);
CREATE INDEX tx2 on term_dbxref(term_id, dbxref_id);

CREATE INDEX tss1 on term_subset(term_id);
CREATE INDEX tss2 on term_subset(subset_id);
CREATE INDEX tss3 on term_subset(term_id,subset_id);
