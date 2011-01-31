#!/usr/local/bin/perl -wnp
s/serial/integer AUTO_INCREMENT not null/g;
s/ text/ mediumtext/g;
# comments look nicer with # in mysql
s/^\-\-\-/\#/;
