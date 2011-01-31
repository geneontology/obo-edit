#!/usr/local/bin/perl -w
my $dbname = shift @ARGV || die;
print `echo "drop database $dbname" | dbaccess -e `;
print `echo "create database $dbname" in cjmspace4 with buffered log | dbaccess -e `;
print `dbaccess -e $dbname < go_tables.sql`;
print `dbaccess -e $dbname < go_views.sql`;
print `dbaccess -e $dbname < go_indices.sql`;
print `echo "grant connect to public" | dbaccess -e $dbname`;

if (@ARGV) {
    require "load-data.pl";
    load_data($dbname);
}

print `echo "update statistics" | dbaccess -e $dbname`;
