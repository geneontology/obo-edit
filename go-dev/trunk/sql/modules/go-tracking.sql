--- GO DATABASE TABLE CREATION SQL
--- contact: go-database@fruitfly.bdgp.berkeley.edu

--- Note: this file is the SOURCE sql; it is for defining
--- the schema in a RDBMS vendor independent way. to "compile"
--- this source into SQL that your DBMS (mysql/postgres/etc)
--- will understand, use the "configure" and "make" commands.
--- See the README file in this directory for more details

--- *******************************************************
--- ** go-tracking.sql                                   **
--- **                                                   **
--- ** tables for tracking changes to the go graph and   **
--- ** model. based on John Richter's DAG Edit model     **
--- ** (may be out of sync with the tables DAG Edit      **
--- ** is using)                                         **
--- **                                                   **
--- *******************************************************

--- History tracking tables
CREATE TABLE edit_session (
  id   serial PRIMARY KEY,
--- contains a database user name
  person varvarchar(55) not null,
  mod_time integer not null,
  session_comment text
);

CREATE TABLE godb_meta_data (
  date_created integer not null,
  creation_comment text,
  version varvarchar(64) not null,
  created_by varvarchar(64) not null,
  current_id integer not null
);

CREATE TABLE node_operation (
  id serial PRIMARY KEY,
  type integer,
  term_id integer not null,
  edit_session_id integer not null,
  foreign key (edit_session_id) references edit_session(id),
  foreign key (term_id) references term(id)
);

CREATE TABLE structure_edit (
  id serial PRIMARY KEY,
  operation_id integer not null,
  term_id integer,
  term_output_id integer,
  foreign key (term_id) references term(id),
  foreign key (term_output_id) references term(id),
  foreign key (operation_id) references operation(id)
);

CREATE TABLE structure_edit_src (
  structure_edit_id integer,
  source_parent_id integer,
  source_term_id integer,
  source_type integer,
  implied integer not null,
  foreign key (structure_edit_id) references structure_edit(id),
  foreign key (source_parent_id) references term(id),
  foreign key (source_term_id) references term(id)
);

CREATE TABLE synonym_edit (
  operation_id integer,
  synonym_text varchar(255),
  acc integer,
  new_synonym varchar(255),
  new_acc integer,
  edit_type varchar(25) not null,
  foreign key (operation_id) references node_operation(id)
);

CREATE TABLE term_category_edit (
  operation_id integer not null,
  category_id integer not null,
  is_del integer not null,
  foreign key (operation_id) references node_operation(id),
  foreign key (category_id) references category(id)
);

CREATE TABLE term_dbxref_edit (
  operation_id integer not null,
  xref_key varchar(55),
  xref_dbname varchar(55),
  xref_type varchar(55),
  new_xref_key varchar(55),
  new_xref_dbname varchar(55),
  new_xref_type varchar(55),
  edit_type varchar(25) not null,
  foreign key (operation_id) references node_operation(id)
);

CREATE TABLE term_text_edit (
  operation_id integer not null,
  name varchar(255),
  term_definition text,
  comment_text text,
  foreign key (operation_id) references node_operation(id)
);


--- e.g. go_slim
CREATE TABLE category (
  id   serial PRIMARY KEY,
  name varchar(55) not null,
  description text
);

CREATE TABLE term_category (
  term_id integer not null,
  category_id integer not null,
  foreign key (category_id) references category(id),
  foreign key (term_id) references term(id),
  unique(term_id, category_id)
);

CREATE TABLE term_comment (
  term_id integer not null,
  comment_text text not null,
  foreign key (term_id) references term_comment(term_id)
);



