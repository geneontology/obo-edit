#!/usr/bin/perl -w
####
#### Convert the .tree files we get from PANTHER to a JS consumable
#### JSON format. Uses BioPerl for parsing the tree.
####
#### Usage:
####    ./tree2json.pl PTHR10004.tree
####    ./tree2json.pl *.tree
####
#### NOTE: The PTHR files a appear to be NHX: http://phylosoft.org/NHX/
####

use Bio::TreeIO;
use File::Temp;
use Data::Dumper;
use Data::UUID;
use JSON::PP;


sub ll {
  my $str = shift || '';
  print $str . "\n";
}


##
foreach my $ifn (@ARGV){

  my $tree_struct =
    {
     nodes => {}, # props keyed by node id
     edges => [], # store as list pf props
     meta => {}  # 
    };

  my $title_line = undef;
  my $tree_line = undef;
  my $gp_list = [];

  ## Read in file.
  #ll($ifn);
  open(my $fh, "<", $ifn)
    or die "bad $ifn: $!";
  while(<$fh>){
    chomp $_;
    if( ! defined $title_line ){
      $title_line = $_;
    }elsif( ! defined $tree_line ){
      $tree_line = $_;
    }else{
      push @$gp_list, $_;
    }
  }
  close $fh;

  ## Make new filename.
  my $new_fname = $ifn . '.json';
  if( $ifn =~ /^(.*?)\.tree$/ ){
    $new_fname = $1 . '.json';
  }

  ## Check in.
  #ll('Processing: '. $title_line .' with '. scalar(@$gp_list) .' in list.');

  ###
  ### Process title.
  ###

  $title_line =~ /^\[title\:(.*?)\]$/;
  $tree_struct->{meta}{title} = $1;

  ###
  ### Process tree.
  ###

  ## Spin out the tree part into a temp file.
  my($fh_tree, $fname_tree) =  File::Temp::tempfile(UNLINK => 1);
  open($fht, ">", $fname_tree)
    or die "cannot open $fname_tree to write: $!";
  #ll('file: ' . $fname_tree);
  print $fht $tree_line . "\n"; # needs the newline to parse properly
  close $fht;

  ## Parse tree.
  my $treeio = Bio::TreeIO->new(-format => 'nhx', -file => $fname_tree);
  my $tree = $treeio->next_tree();

  ## We're assuming that completely unnamed nodes can be arbitrarily
  ## labeled if they don't collide.
  sub extract_node_id {
    my $cnode = shift || die "badness";
    my $retval = undef;

    ## First see if we have an id atthe ready.
    if( $cnode->id() ){
      $retval = $cnode->id();
    }elsif( $cnode->has_tag('ID') ){
      my @tvals = $cnode->get_tag_values('ID');
      $retval = $tvals[0];
    }else{
      ## TODO: attempts to name have failed--die?
      my $uuer = Data::UUID->new();
      $retval = '_anonymous_' . $uuer->to_string( $uuer->create() );
    }

    return $retval;
  }

  ## Extract all NHX tags from the node, return as hashref.
  sub extract_tags {
    my $cnode = shift || die "badness";

    my $tags = undef;

    my @tag_keys = $cnode->get_all_tags();
    foreach my $tkey (@tag_keys){
      if( ! defined $tags ){ $tags = {}; }

      my @tvals = $cnode->get_tag_values($tkey);
      if( $tkey eq 'S' ){
	$tags->{species} = $tvals[0];
      }elsif( $tkey eq 'ID' ){
	$tags->{id} = $tvals[0];
      }elsif( $tkey eq 'Ev' ){
	my @evs = split(/\>/, $tvals[0]);
	#ll('TVALS: ' . $tvals[0]);
	#ll('TVALS#: ' . scalar(@evs));
	$tags->{duplications} = $evs[0]
	  if scalar(@evs) > 0;
	$tags->{speciations} = $evs[1]
	  if scalar(@evs) > 1;
	$tags->{losses} = $evs[2]
	  if scalar(@evs) > 2;
	$tags->{event_type} = $evs[3]
	  if scalar(@evs) > 3;
	$tags->{duplication_type} = $evs[4]
	  if scalar(@evs) > 4;
      }else{
	$tags->{$tkey} = \@tvals;
      }
    }

    return $tags;
  }

  ## Best guess at length, 0 if we can't figure it out.
  sub extract_branch_length {
    my $cnode = shift || die "badness";
    my $retval = $cnode->branch_length() || 0;
    return $retval;
  }

  sub node_walk {
    my $curr_node = shift || die "bad recursion";
    #my $parent_node = shift || undef;

    ## TODO: add node to graph.
    my $all_tags = extract_tags($curr_node);
    my $n_id = extract_node_id($curr_node);
    $all_tags->{node_id} = $n_id;
    $tree_struct->{nodes}{$n_id} = $all_tags;

    my @child_nodes = $curr_node->each_Descendent();
    for my $child_node (@child_nodes){
      ## TODO: add edge to graph.
      #ll("in kid loop");

      my $k_id = extract_node_id($child_node);
      my $dist = extract_branch_length($child_node);

      push @{$tree_struct->{edges}},
	{
	 source_id => $n_id,
	 sink_id => $k_id,
	 #length => $dist
	 #length => sprintf("%f", $dist)
	 length => $dist . ''
	};

      ## Walk down.
      #node_walk($child_node, $curr_node);
      node_walk($child_node);
    }
  }
  my $root_node = $tree->get_root_node();
  node_walk($root_node);

  ###
  ### GP list.
  ### TODO: Figure out what they are.
  ###

  foreach my $gp_line (@$gp_list){
    chop $gp_line; # get trailing semicolon
    my @all = split(/\|/, $gp_line);
    my $droppable_name = $all[0];
    my $gene_part = $all[1];
    my $protein_part = $all[2];

    ## Easy.
    @droppable_name_all = split(/\:/, $droppable_name);
    $droppable_name_a = $droppable_name_all[0];
    #$droppable_name_b = $droppable_name_all[1];

    ## Annoying--there can be multiple equals.
    my $index_2 = index($gene_part, '=');
    my $gene_part_a = substr($gene_part, 0, $index_2);
    my $gene_part_b = substr($gene_part, $index_2 +1, length($gene_part));
    ## Any remaining '=' (PANTHER bug) go back to ':'.
    $gene_part_b =~ s/\=/\:/;

    ## Again.
    my $index_3 = index($protein_part, '=');
    my $protein_part_a = substr($protein_part, 0, $index_3);
    my $protein_part_b = substr($protein_part,$index_3+1,length($protein_part));
    ## Any remaining '=' (PANTHER bug) go back to ':'.
    $protein_part_b =~ s/\=/\:/;

    if( defined $tree_struct->{nodes}{$droppable_name_a} ){
      ## Unecessary according to Chris.
      # $tree_struct->{nodes}{$droppable_name_a}{internal_species_symbol} =
      # 	$droppable_name_b;
      $tree_struct->{nodes}{$droppable_name_a}{gene_id} =
	$gene_part_a . ':' . $gene_part_b;
      $tree_struct->{nodes}{$droppable_name_a}{protein_id} =
	$protein_part_a . ':' . $protein_part_b;
    }else{
      die "what am I doing with this thing then: $gp_line";
    }
  }

  ## Done.
  my $jse = JSON::PP->new();
  $jse->allow_bignum(1);
  #my $final_js = $jse->encode($tree_struct);
  my $final_js = $jse->pretty->encode($tree_struct);

  #ll($final_js);
  #ll($new_fname);
  open($nfh, ">", $new_fname)
    or die "cannot open $new_fname to write: $!";
  print $nfh $final_js . "\n";
  close $nfh;
}
