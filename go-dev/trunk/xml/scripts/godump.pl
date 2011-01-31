#!/usr/local/bin/perl

$ENV{'DBMS'}='mysql';
use lib "/users/bradmars/cvs/go-dev/perl-api";
use lib "/users/bradmars/cvs/software/perl-modules";
use GO::AppHandle;  

use GO::IO::XML;
use GeneralUtils::XML::Generator;
use CGI;

my $dbname = $ENV{"GO_DBNAME"} || 'go';
my $dbhost = $ENV{"GO_DBHOST"} || 'sin';

my $apph = GO::AppHandle->connect(-dbname=>$dbname, -dbhost=>$dbhost); 
my $gen = new GO::IO::XML;

my @ts = localtime($apph->timestamp);
my $ts = '20'.@ts->[5];
# There's probably a less pedestrian way to do this...
if (@ts->[4] < 10) {
  $ts .= '0'.@ts->[4];
} else {
  $ts .= @ts->[4];
}
if (@ts->[3] < 10) {
  $ts .= '0'.@ts->[3];
} else {
  $ts .= @ts->[3];
}

$gen->start_document(-timestamp=>$ts);

my $terms = $apph->get_terms('*', {acc=>1});
foreach my $term(sort by_acc @{$terms}) {
  if ($term->acc =~ m/^GO:/ &&
      $term->acc ne 'GO:isa' &&
      $term->acc ne 'GO:partof') {
    my $graph = $apph->get_node_graph(-acc=>$term->acc, -depth=>0);
    $gen->draw_term(-term=>$term, 
		    -graph=>$graph, 
		    -focus=>'no',
		    -show_associations=>'yes',
		    -show_terms=>'yes',
		    -show_xrefs=>'yes'
		   );
  }
}
$gen->end_document;

sub by_acc {
  $a->acc cmp $b->acc;

}


sub make_go_from_acc {
  my $acc = shift;

  my $go_name = 'GO:';
  return sprintf "GO:%07d", $acc;
}
