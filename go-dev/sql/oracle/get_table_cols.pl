#!/usr/bin/perl -w
# Extract column names from a table definition and transfrom them to a
# field specification for sql loader. Field sizes may have to be
# adjusted.
# Author: Frank Schacherer <bioinformatics@schacherer.de>
# $Logfile: $
# $Date: 2002/08/08 19:05:34 $
# $Revision: 1.1 $
# $History: $
use strict;
my $table = shift || die "Usage $0 tablename tablefile\n";
my $file  = shift || die "Usage $0 tablename tablefile\n";
my (@cols, $in_table, $open_pars);
my @stopwords = qw( unique foreign );

open (FH, "<$file");
while(<FH>) {
    if (/^create table\s+$table/i) { #beginnig of table
        $in_table = 1;
        $open_pars = 0; 
    }
    $in_table and $open_pars = $open_pars 
        + length(join("", /\(/g)) 
        - length(join("", /\)/g));
    if ($in_table and /^\s+(\w+)\s+(\w+)(?:\((\d+)\))?/) {
        my $col = lc($1);
        my $type = lc($2);
        my $len = $3;
        # sql*loader otherwise truncates at 255 chars
        # don't use varchar, since then  the first 2 bytes are
        # expected to contain the string length
        $col .= " char(17000)" if $type eq "long"; 
        $col .= " char(4000)"  if (defined $len and $len > 255);
        push(@cols, $col) unless scalar grep {/$col/i} @stopwords;
    }
    if ($in_table and $open_pars == 0 and /;/) { #end of table
        print "( ", join (", ", @cols), " )\n";
        close FH;
        exit;
    }
}
close FH;
