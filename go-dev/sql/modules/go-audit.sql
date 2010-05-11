--- GO DATABASE TABLE CREATION SQL
--- contact: go-database@fruitfly.bdgp.berkeley.edu

--- Note: this file is the SOURCE sql; it is for defining
--- the schema in a RDBMS vendor independent way. to "compile"
--- this source into SQL that your DBMS (mysql/postgres/etc)
--- will understand, use the "configure" and "make" commands.
--- See the README file in this directory for more details

--- *******************************************************
--- ** go-audit.sql                                      **
--- **                                                   **
--- ** tracks loading times                              **
--- **                                                   **
--- ** all times are unixtimes (seconds since 1970)      **
--- ** use standard unix/perl funcs to convert these     **
--- **                                                   **
--- **                                                   **
--- *******************************************************


--- @@ term_audit
--- not in use
CREATE TABLE term_audit (

	term_id	integer not null,
	foreign key (term_id) references term(id),

        term_loadtime integer,

	UNIQUE (term_id)
);

--- @@ source_audit
--- time of last modification of data source (usually type:file)
--- source_path is a file path or name
CREATE TABLE source_audit (

        source_id varchar(255),
        source_fullpath varchar(255),
        source_path varchar(255),
	source_type varchar(255),
	source_md5  char(32),
        source_parsetime integer,
        source_mtime integer
);

--- @@ instance_data
--- metadata on this particular instance/build of the GO database
CREATE TABLE instance_data (
	
        --- @@ instance_data.release_name
        --- Typically named by date/version in YYYY-MM-DD format
	release_name	varchar(255),

        --- @@ instance_data.release_type
        --- One of: 
        --- termdb (ontology only)
        --- assocdb (termdb + associations)
        --- seqdb (assocdb + sequences)
        --- seqdblite (seqdb - IEAs)
	release_type	varchar(255),

        --- @@ instance_data.release_notes
        --- notes specific to this release. Will typically be null unless this release is unusual in some way
	release_notes	text,

        -- @@ instance_data.ontology_data_version
        -- data-version tag from the header of the obo file
        -- (not yet implemented)
        ontology_data_version   varchar(255),

	UNIQUE (release_name)
);

CREATE INDEX ta1 on term_audit(term_id);
CREATE INDEX fa1 on source_audit(source_path);

