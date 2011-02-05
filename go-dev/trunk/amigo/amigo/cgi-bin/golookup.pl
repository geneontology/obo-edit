#!/usr/local/bin/perl

BEGIN {

    if (-f "config.pl") {
    	require "config.pl";
    }

    # find go perl libraries pre compile time
    #hmm, use lib "$ENV{GO_DEV_ROOT}/go-perl" imports /go-perl !!!
    if (defined($ENV{GO_DEV_ROOT})) {
        ;
    } elsif (-f "../cvs/go-dev/") {
		$ENV{GO_DEV_ROOT} = "../cvs/go-dev";
    }
}

use lib "$ENV{GO_DEV_ROOT}/go-perl";
use lib "$ENV{GO_DEV_ROOT}/go-db-perl";
use lib "$ENV{GO_DEV_ROOT}/amigo/perl";

use strict;
use CGI qw(:standard escape center *table *dl *TR *td);
use GO::CGI::Session;
use GO::SqlWrapper qw(:all);

#
# Set up the relevant objects.
#

my $q = new CGI;
my $session = new GO::CGI::Session('-q'=>$q);
my @names = $q->param;

print header(-type=>"text/html");
print "<html><body>";
my @a = ();
my $query = $q->param('query');
$query =~ s/[%*]//g;
if (length($query)) {
    my %item_h = ();
    #default is search term!!!
    if ($q->param('search_constraint') eq 'gp') {
        %item_h = map{$_=>1}
          (@{select_vallist($session->apph->dbh,"gene_product", "symbol like '%$query%'", "symbol") || []},
           @{select_vallist($session->apph->dbh,"gene_product_synonym", "product_synonym like '%$query%'", "product_synonym") || []});
    } else {
        %item_h = map{$_=>1}
          (@{select_vallist($session->apph->dbh,"term", "name like '%$query%'", "name") || []},
           @{select_vallist($session->apph->dbh,"term_synonym", "term_synonym like '%$query%'", "term_synonym") || []});
    }
    @a = sort{$a cmp $b}keys %item_h;
}
unshift @a, '';

my $size = scalar(@a);
$size = 10 if ($size >= 10);
my $i = 0;

printf "<select name=\"mySelect\" id=\"mySelect\" multiple=\"yes\" size=$size>%s</select>",join("\n", map{$i++;"<option value=\"$i\">$_</option>"}@a);

print "</body></html>";
exit;
