--- GO DATABASE TABLE CREATION SQL
--- contact: go-database@fruitfly.bdgp.berkeley.edu

--- Note: this file is the SOURCE sql; it is for defining
--- the schema in a RDBMS vendor independent way. to "compile"
--- this source into SQL that your DBMS (mysql/postgres/etc)
--- will understand, use the "configure" and "make" commands.
--- See the README file in this directory for more details

--- EXPERIMENTAL MODULE

--- index of terms by word
--- FOR FUTURE EXPANSION
--- *** not currently populated ***
CREATE TABLE wordunit2term (

	term_id	        integer not null,
	foreign key (term_id) references term(id),
--- set to true (1) if this word came from the synonym table
        is_synonym      integer not null,
--- set to true (1) if this word came from the definitions table
        is_definition   integer not null,

--- type of word (eg noun, verb etc) - UNPOPULATED
        wordtype        varchar(32),
--- in mysql we can use soundex function to get the word 'sound'
        wordsound       varchar(32),
	wordunit	varchar(128)
);

CREATE INDEX tut1 on wordunit2term(term_id);
CREATE INDEX tut2 on wordunit2term(wordunit);
CREATE INDEX tut3 on wordunit2term(wordsound);
CREATE INDEX tut4 on wordunit2term(wordunit, wordsound);


