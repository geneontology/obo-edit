--- GO DATABASE TABLE CREATION SQL
--- contact: go-database@fruitfly.bdgp.berkeley.edu

--- Note: this file is the SOURCE sql; it is for defining
--- the schema in a RDBMS vendor independent way. to "compile"
--- this source into SQL that your DBMS (mysql/postgres/etc)
--- will understand, use the "configure" and "make" commands.
--- See the README file in this directory for more details

--- *******************************************************
--- ** go-sequence.sql                                   **
--- **                                                   **
--- ** storing of sequences in a GO database is          **
--- ** optional.                                         **
--- **                                                   **
--- **                                                   **
--- *******************************************************


--- @@ seq
--- A representative DNA or amino acid sequence for a gene_product;
--- will typically be amino acid. This table is modeled after
--- the BioPerl Bio::PrimarySeq model
CREATE TABLE seq (

	id  	serial PRIMARY KEY,

        --- @@ seq.display_id
        --- the primary label used for identifying the sequence for
        --- humans. Not guaranteed to be globally unique.
        --- typically corresponds to the first part of a FASTA header
	display_id     varchar(64),

        --- @@ seq.description
        --- textual information for humans concerning this sequence.
        --- typically corresponds to the part after the ID in the FASTA header
	description    varchar(255),

        --- @@ seq.seq
        --- residue sequence: standard IUPAC alphabetic codes are used
	seq	       text,

        --- @@ seq.seq_len
        --- number of residues in sequence. should always correspond to 
        --- length(seq), where seq is populated
	seq_len	       integer,

        --- @@ seq.md5checksum
        --- result of md5(seq), where md5 is the standard MD5 checksum algorithm.
        --- see GO::Model::Seq for calculation
        --- almost 100% guaranteed to be unique for any sequence of symbols
        --- representing the biopolymer
	md5checksum    varchar(32),

        --- @@ seq.moltype
        --- DNA or AA
	moltype	       varchar(25),

	timestamp      integer,

	unique(display_id, md5checksum)
);

--- @@ seq_property
--- seq can have various properties attached to it.
--- not currently used
CREATE TABLE seq_property (

	id  	        serial PRIMARY KEY,
	seq_id	        integer not null,
	foreign key (seq_id) references seq(id),
        property_key    varchar(64) not null,
        property_val    varchar(255) not null,
	unique(seq_id, property_key, property_val)
);

--- @@ seq_dbxref
--- linking table for external identifiers for a sequence
---
--- seq_dbxref is derived from the dbxrefs in the source
--- sequence file. For example, if the source is a UniProt
--- file, this table will reflect the DR lines
--- if the source is a FASTA file from UniProt, the DB:ACC
--- parts of the fasta header will be used to populate this
--- table
---
--- typically a seq entry will be something like a protein/
--- polypeptide - ie something that can be annotated with GO;
--- the dbxref could be for entities in other databases that
--- may only be tangentially related to the protein
--- for example: mRNA records, genomic records, OMIM IDs, etc
CREATE TABLE seq_dbxref (

	seq_id	    integer not null,
	foreign key (seq_id) references seq(id),
	dbxref_id   integer not null,
	foreign key (dbxref_id) references dbxref(id),

	unique(seq_id, dbxref_id)
);

--- @@ gene_product_seq
--- relationship between gene_product and seq is
--- potentially many to many, although in practice
--- each gene product may only have one seq, depending
--- on data population method. If this link is for the
--- representatibe sequence for a gene product, is_primary_seq
--- should be set to true (=1)
--- (a gene_product should only have one seq that
---  is marked is_primary_seq, although this is not
---- enforced. the idea is that there may be different
---  seqs for a product - allelic variations, variant
---  spliceforms, but only one is the "representative"
---  seq)
--- This table is typically sourced from the gp2protein
--- contributed data file: http://www.geneontology.org/gp2protein
CREATE TABLE gene_product_seq (

	gene_product_id	integer not null,
	foreign key (gene_product_id) references gene_product(id),

	seq_id	integer not null,
	foreign key (seq_id) references seq(id),

        --- @@ gene_product_seq.is_primary_seq
        --- If this link is for the
        --- representatibe sequence for a gene product, is_primary_seq
        --- should be set to true (=1)
	is_primary_seq	     integer

);

CREATE UNIQUE INDEX seq0 on seq(id);
CREATE INDEX seq1 on seq(display_id);
CREATE INDEX seq2 on seq(md5checksum);

CREATE INDEX seqx0 on seq_dbxref(seq_id);
CREATE INDEX seqx1 on seq_dbxref(dbxref_id);
CREATE INDEX seqx2 on seq_dbxref(seq_id, dbxref_id);

CREATE INDEX seqp0 on seq_property(seq_id);
CREATE INDEX seqp1 on seq_property(property_key);
CREATE INDEX seqp2 on seq_property(property_val);

CREATE INDEX gpseq1 on gene_product_seq(gene_product_id);
CREATE INDEX gpseq2 on gene_product_seq(seq_id);
CREATE INDEX gpseq3 on gene_product_seq(seq_id, gene_product_id);
