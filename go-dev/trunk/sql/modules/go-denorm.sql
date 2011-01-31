--- GO DATABASE TABLE CREATION SQL
--- contact: go-database@fruitfly.bdgp.berkeley.edu

--- Note: this file is the SOURCE sql; it is for defining
--- the schema in a RDBMS vendor independent way. to "compile"
--- this source into SQL that your DBMS (mysql/postgres/etc)
--- will understand, use the "configure" and "make" commands.
--- See the README file in this directory for more details

--- *******************************************************
--- ** go-denorm.sql                                     **
--- **                                                   **
--- ** bulk-load tables (experiemntal)                   **
--- *******************************************************

CREATE TABLE uniprot_idmapping (
       src_acc   VARCHAR(64) NOT NULL,
       dbname    VARCHAR(32) NOT NULL,
       acc       VARCHAR(64) NOT NULL,

       UNIQUE(src_acc,dbname,acc)
);

CREATE INDEX uniprot_idmapping_ix1 ON uniprot_idmapping(src_acc);
CREATE INDEX uniprot_idmapping_ix2 ON uniprot_idmapping(src_acc,dbname,acc);
