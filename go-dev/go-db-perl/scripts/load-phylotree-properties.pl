#!/usr/bin/perl -w
####
#### Load Newick tree blobs into GO db from named files in directory
#### on fs. Temporary hack until these are in the pipeline.
####
#### Usage:
####
####    GO_DBNAME='go_latest_lite' perl -I /home/sjcarbon/local/src/svn/geneontology/gobo-dbic ../go-db-perl/scripts/load-phylotree-properties.pl /srv/www/htdocs/amigo/panther
####
#### TODO:
####
####    *) Uses a very API-ish setup--maybe add the command line bits that
####       Chris uses (vs. the env in GODBModel that we use here).
####

use File::Basename;
use Data::Dumper;
use GOBO::DBIC::GODBModel::Query;
use GOBO::DBIC::GODBModel::Modify;

sub ll {
  my $arg = shift || '';
  print STDERR "$arg\n";
}

##
ll("Starting to load phylotree properties...");

## Get PANTHER Newick tree directory path and pull in all the trees.
my $pdir = shift @ARGV || '.';
my @full_tree_files = glob("$pdir/*.tree");
die "Couldn\'t find trees, please check path." if ! scalar(@full_tree_files);

## Draw all of the data in from the tree files out on disk somewhere.
my $data = {};
foreach my $tfile (@full_tree_files){

  ## Get key.
  my $file_bit = [fileparse($tfile)]->[0];
  my $db_key = substr($file_bit, 0, -5);

  ## Get blob.
  open (FILE, $tfile)
    or die "Cannot open $tfile: $!";
  my @buf = ();
  while (<FILE>) {
    push @buf, $_;
  }
  close FILE;
  my $blob = join '', @buf;
  ## Remove any gunk at beginning and end.
  $blob =~ s/^[\r\s\n]+//;
  $blob =~ s/[\r\s\n]+$//;

  my $db_name = 'PantherDB';
  my $xref = $db_name . ':' . $db_key;

  #ll("xref: " . $xref);
  #ll("blob: " . $blob);

  $data->{$db_key} =
    {
     xref => $xref,
     blob => $blob,
    };
}

## Get ids to phylotree if possible...
my $query = GOBO::DBIC::GODBModel::Query->new({type=>'phylotree_lazy'});
my $p_mod = GOBO::DBIC::GODBModel::Modify->new({type=>'PhylotreeProperty'});

my $counter = 0;
ll("Starting to load into database...");
foreach my $key (keys %$data){

  my $xref = $data->{$key}{xref};
  my $blob = $data->{$key}{blob};

  my $pts = $query->get_all_results({'me.name' => $xref});
  if( $pts && @$pts ){
    if( scalar(@$pts) == 0 ){
      die "Nothing found for $key";
    }elsif( scalar(@$pts) > 1 ){
      die "Too many found for $key";
    }else{

      my $res = $$pts[0];
      my $ptree_id = $res->id;

      my $insert =
	{
	 phylotree_id => $ptree_id,
	 property_key => $xref,
	 property_val => $blob,
	};

      ##
      #ll("Insert: " . Dumper($insert));
      $p_mod->add($insert);
      $counter++;
    }
  }
}

##
ll("Done loading ($counter) phylotree properties.");
