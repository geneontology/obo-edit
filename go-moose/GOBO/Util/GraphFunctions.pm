package GOBO::Util::GraphFunctions;
use strict;

use Moose;
use Carp;
use Data::Dumper;

use GOBO::Graph;
use GOBO::Parsers::OBOParserDispatchHash;
use GOBO::Writers::OBOWriter;
use GOBO::Parsers::QuickGAFParser;
use GOBO::InferenceEngine::CustomEngine;
use GOBO::InferenceEngine;
use Class::MOP;


use GOBO::DataArray;

#use Storable qw(dclone);

=head2 go_slim_annotations

expects a graph, a subset, options{ga_input} => GAF file

 input:  hash of the form
          graph => $graph
          subset => hashref with subset IDs as keys
          options =>
          { ga_input => '/path/to/GAF_file',
            mode => 'rewrite' } ## optional; the script doesn't create extra
                                ## indexes with this option on
 output: hashref containing { graph => $graph, assoc_data => $assoc_data }

The graph will have the following indexes:
 - direct_annotations
 - all_annotations
 - direct_ontology_links
 - transitive_closure
 - transitive_reduction

=cut

sub go_slim_annotations {
	my %args = (@_);

	my $options = $args{options};
	my $graph = $args{graph};
	my $subset = $args{subset};

	## get the GA data
	my $gaf_parser = GOBO::Parsers::QuickGAFParser->new(fh=>$options->{ga_input});

	my $assoc_data = $gaf_parser->parse;
	#print STDERR "assoc data: " . Dumper($assoc_data) . "\n\n";

	if (! $assoc_data )
	{	die "No annotations were found! Dying";
	}

	print STDERR "Finished parsing annotations!\n" if $options->{verbose};

=cut
my $annot = new GOBO::RelationNode( id => 'annotated_to', label => 'annotated to' );
my $not_annot = new GOBO::RelationNode( id => 'annotated_to_NOT', label => 'annotated to NOT' );
$graph->add_relation($annot);
$graph->add_relation($not_annot);
## should there be any other relationships for annotated_to?
$annot->transitive_over( $graph->relation_noderef('part_of') );
$not_annot->propagates_over_is_a(0);
=cut

	## this file contains the 'annotated_to' and 'annotated_to_NOT' relations
	my $parser = new GOBO::Parsers::OBOParserDispatchHash(file=>'t/data/annotation_relations.obo',
		graph => new GOBO::Graph,
		body => { parse_only => { typedef => '*' } },
	);
	$parser->parse;
	foreach (@{$parser->graph->relations})
	{	$graph->add_relation($_);
	}

	## check that we have an overlap between the terms in the graph and those in
	## the GAF file
	my @terms = map {$_->id} @{$graph->terms};
	my @errs;
	foreach my $t (keys %{$assoc_data->{by_t}})
	{	next if grep { $t eq $_ } @terms;
		## otherwise, this term was not in the graph.
		push @errs, $t;
	}

	if (@errs)
	{	
		if (scalar @errs == scalar keys %{$assoc_data->{by_t}})
		{	## crap! No matching terms!
			die "None of the terms in the annotation file matched those in the ontology file. Dying";
		}
		else
		{	warn "The following terms were not found in the ontology file: " . join(", ", sort @errs);
		}
	}
	
	
	

# we're going to do a slight hack here because we didn't bother to parse the
# annotations properly; we're adding the association data as DataArray objects,
# a subclass of GOBO::Gene (to trick the InfEng into thinking we're dealing with
# proper annotations).

	undef @errs;
	my $a_ids;
	foreach my $a (keys %{$assoc_data->{by_a}})
	{	my $a_node = new GOBO::DataArray( id => $a, data_arr => $assoc_data->{by_a}{$a}{arr} );
		$graph->add_node($a_node);
		foreach my $t (@{$assoc_data->{by_a}{$a}{terms}})
		{	## check the term exists in the graph
			my $t_obj = $graph->get_term($t);
			if (! $t_obj)
			{	push @errs, $t unless grep { $_ eq $t } @errs;
				next;
			}
			elsif ($t_obj->obsolete)
			{	push @errs, $t unless grep { $_ eq $t } @errs;
				next;
			}
			
			$a_ids->{$a}++;
			my $link;
			# check for a NOT annotation
			if ($assoc_data->{by_a}{$a}{arr}[4] && $assoc_data->{by_a}{$a}{arr}[4] =~ /NOT/)
			{	$link = new GOBO::Annotation(node=>$a_node, relation=>$graph->get_relation('annotated_to_NOT'), target=>$graph->get_term($t));
			}
			else
			{	$link = new GOBO::Annotation(node=>$a_node, relation=>$graph->get_relation('annotated_to'), target=>$graph->get_term($t));
			}
			$graph->add_annotation($link);
		}
	}

	$graph->update_graph;
	

	if (@errs)
	{	if (scalar @errs == scalar keys %{$assoc_data->{by_t}})
		{	die "None of the terms in the annotation file matched those in the ontology file. Dying";
		}
		else
		{	warn "The following terms were not found in the ontology file: " . join(", ", @errs);
		}
	}

	print STDERR "Added associations to the graph!\n" if $options->{verbose};

	$graph->duplicate_statement_ix('statements', 'asserted_links');

	## find the root nodes in the graph
	my $roots = $graph->get_connected_roots;

	## OK, we have our graph with the annotations attached. Let's get slimming!
	# the input set is any terms with an annotation connected to them
	# get the links between the nodes
	my $ie = new GOBO::InferenceEngine::CustomEngine( graph => $graph );

	$ie->get_closest_and_ancestral( subset_ids => [ keys %$subset ], input_ids => [ keys %$subset, keys %$a_ids ], from_ix => 'asserted_links', all_ix => 'transitive_closure', closest_ix => 'transitive_reduction' );

	print STDERR "Finished slimming; performing error checks\n" if $options->{verbose};

#	print STDERR "graph terms: " . join(", ", @{$ie->graph->terms}) . "\n";
#	print STDERR "statements:\n" . join("\n", @{$ie->graph->statements}) . "\n";
	
#	print STDERR "Closest links:\n" . join("\n", @{$ie->graph->get_all_statements_in_ix('transitive_reduction')} )
#	. "\nAll links\n" . join("\n", @{$ie->graph->get_all_statements_in_ix('transitive_closure')} ) . "\n\n";

	## lots and lots of error checks!

	## check for missing annotations, and transfer annotation info
	undef @errs;
	my $n_annots = scalar keys %$a_ids;

	my $n_ix = $ie->graph->statement_ix_node_index('transitive_closure');
	my $t_ix = $ie->graph->statement_ix_target_index('transitive_closure');
	my $ol_ix = [ @{$ie->graph->statement_ix_target_index('ontology_links')}, @{$ie->graph->statement_ix_node_index('ontology_links')} ];

	## Do some checks.
	## Look for missing terms:
	foreach my $t (keys %$subset)
	{	if (! grep { $t eq $_ } @$t_ix)
		{	# this term has been lost from the graph!
			# shall we just forget about it?
			push @errs, $t;
			print STDERR "Looking at $t... no links at all!\n";
		}
		elsif (! grep { $t eq $_ } @$ol_ix)
		{	# this term is not attached to any other ontology terms!
			push @errs, $t;
			print STDERR "Looking at $t... no ontology links!\n";
			## delete the term unless we are keeping orphans
			if (! $options->{keep_orphans})
			{	print STDERR "deleting node $t\n";
				$ie->graph->remove_node($t, 1);
			}
		}
		else
		{	#print STDERR "Looking at $t... found some links!\n";
		}
	}

	if (@errs)
	{	if (scalar @errs == scalar keys %$subset)
		{	die "All terms were lost during slimming! Dying";
		}
		else
		{	warn "The following terms were lost during slimming:\n" . join(", ", sort @errs);
		}
	}
	
	## look for missing annotations
	foreach my $a (keys %$a_ids)
	{	if (! grep { $a eq $_ } @$n_ix)
		{	# this annotation has been lost from the graph!
			push @errs, $a;
		}
	}

	if (@errs)
	{	if (scalar @errs == $n_annots)
		{	die "All annotations were lost during slimming! Dying";
		}
		else
		{	warn "The following annotations were lost during slimming:\n" . join(", ", sort @errs);
		}
	}

	undef @errs;

	if ($options->{delete_new_roots})
	{	
		## check our roots... have we got some mysterious new root nodes?
		my $new_roots = $ie->graph->get_connected_roots_in_ix('transitive_closure');
		my $root_h;
		my $new;
		foreach (@$roots)
		{	$root_h->{$_->id} = $_;
		}
		foreach (@$new_roots)
		{	if ($root_h->{$_->id})
			{	delete $root_h->{$_->id};
				next;
			}
			## this is a new root!
			$new->{$_->id} = $_;
		}
	
		if (keys %$root_h)
		{	print STDERR "The following root nodes were lost:\n" . join(", ", keys %$root_h) . "\n";
		}
		if (keys %$new)
		{	print STDERR "The following terms are now root nodes:\n" . join(", ", keys %$new) . "\n";
		}

		my $to_delete;
		%$to_delete = %$new;
		foreach my $r (keys %$new)
		{	## get all the children of this term and delete the whole blimmin' lot!
			foreach (  # @{$ie->graph->get_outgoing_statements($r)}, 
			@{$ie->graph->get_incoming_statements($r)})
			{	# $to_delete->{$_->target->id} = 1;
				$to_delete->{$_->node->id} = 1;
			}
		}
		
		foreach (keys %$to_delete)
		{	$ie->graph->remove_node($_, 1);
		}
	}

	## if we're doing a rewrite of the GAF file, we don't need any more fancy
	## trimmings
	if ($options->{mode} && $options->{mode} eq 'rewrite')
	{	return { graph => $ie->graph, assoc_data => $assoc_data };
	}

	print STDERR "Creating extra indexes\n" if $options->{verbose};

	## otherwise, sort the statements into direct annotations, direct ontology
	## links and inferred annotations

	my $a_refs = $ie->graph->get_statement_ix_by_name('annotations')->get_all_references;
	my $o_refs = $ie->graph->get_statement_ix_by_name('ontology_links')->get_all_references;
	my $t_red = $ie->graph->get_statement_ix_by_name('transitive_reduction')->get_all_references;

	## sort out direct annotations and direct ontology links
	foreach my $r (@$t_red)
	{	if ( grep { $r eq $_ } @$a_refs )
		{	## direct annotation
			$ie->graph->add_statements_to_ix(ix=>'direct_annotations', statements=>[$$r]);
		}
		elsif (grep { $r eq $_ } @$o_refs)
		{	## ontology link
			$ie->graph->add_statements_to_ix(ix=>'direct_ontology_links', statements=>[$$r]);
		}
	}

	## add inferred annotations to 'all_annotations' and label them as 'inferred'
	foreach my $r (@{$ie->graph->get_statement_ix_by_name('transitive_closure')->get_all_references})
	{	if ( grep { $r eq $_ } @$a_refs) # && ! grep { $r eq $_ } @$t_red)
		{	$$r->inferred(1) if ! grep { $r eq $_ } @$t_red;
			$ie->graph->add_statements_to_ix(ix=>'all_annotations', statements=>[$$r]);
		}
	}

	return { graph => $ie->graph, assoc_data => $assoc_data };
}



sub new_go_slim_annotations {
	my %args = (@_);

	my $options = $args{options};
	my $graph = $args{graph};
	my $subset = $args{subset};

	## get the GA data
	my $gaf_parser = GOBO::Parsers::QuickGAFParser->new(fh=>$options->{ga_input});

	my $assoc_data = $gaf_parser->parse;
	#print STDERR "assoc data: " . Dumper($assoc_data) . "\n\n";

	if (! $assoc_data )
	{	die "No annotations were found! Dying";
	}

	print STDERR "Finished parsing annotations!\n" if $options->{verbose};

my $annot = new GOBO::RelationNode( id => 'annotated_to', label => 'annotated to' );
my $not_annot = new GOBO::RelationNode( id => 'annotated_to_NOT', label => 'annotated to NOT' );
$graph->add_relation($annot);
$graph->add_relation($not_annot);
## should there be any other relationships for annotated_to?
$annot->transitive_over( $graph->relation_noderef('part_of') );
$not_annot->propagates_over_is_a(0);

=insert
	## this file contains the 'annotated_to' and 'annotated_to_NOT' relations
	my $parser = new GOBO::Parsers::OBOParserDispatchHash(file=>'t/data/annotation_relations.obo',
		graph => new GOBO::Graph,
		body => { parse_only => { typedef => '*' } },
	);
	$parser->parse;
	foreach (@{$parser->graph->relations})
	{	$graph->add_relation($_);
	}
=cut
	## check that we have an overlap between the terms in the graph and those in
	## the GAF file
	my @term_ids = map {$_->id} grep { ! $_->obsolete } @{$graph->terms};
	die "No extant terms could be found in the graph! Dying" if ! @term_ids;

	my @errs;
	my $n_terms = scalar keys %{$assoc_data->{by_t}};
	foreach my $t (keys %{$assoc_data->{by_t}})
	{	next if grep { $t eq $_ } @term_ids;
		## otherwise, this term was not in the graph.
		push @errs, $t;
		delete $assoc_data->{by_t}{$t};
	}

	if (@errs)
	{	
		if (scalar @errs == $n_terms)
		{	## crap! No matching terms!
			die "None of the terms in the annotation file matched those in the ontology file. Dying";
		}
		warn "The following terms were not found in the ontology file: " . join(", ", sort @errs);
	}

	## copy the ontology links into a new index...
	$graph->duplicate_statement_ix('ontology_links', 'asserted_links');
	
	## now let's slim the graph down.
#	my $ie = new GOBO::InferenceEngine::CustomEngine( graph => $graph );
	
#	$ie->slim_graph( subset_ids => [ keys %$subset ], input_ids => [ @term_ids ], from_ix => 'asserted_links', save_ix => 'slimmed_links', options => { return_as_graph => 1 } );

#	$graph = $ie->graph;
#	$graph->duplicate_statement_ix('ontology_links', 'links_and_annots');

# we're going to do a slight hack here because we didn't bother to parse the
# annotations properly; we're adding the association data as DataArray objects,
# a subclass of GOBO::Gene (to trick the InfEng into thinking we're dealing with
# proper annotations).

	my $err_count;
	my $a_ids;
	my $ada_ids;
	my @statements;
	my $annotated_to = $graph->get_relation('annotated_to');
	my $annotated_to_NOT = $graph->get_relation('annotated_to_NOT');

	my $assoc_h;
	my $count = 1;
	foreach my $t (keys %{$assoc_data->{by_t}})
	{	## check the term exists in the graph
		my $t_obj = $graph->get_term($t);
		if (! $t_obj)
		{	$err_count++;
			next;
		}
		my ($annot, $not);
		map { 
#			$a_ids->{$_}++;
			## the association data is in $assoc_data->{by_a}{$a}{arr}
			## check for a NOT annotation
			if ($assoc_data->{by_a}{$_}{arr}[4] && $assoc_data->{by_a}{$_}{arr}[4] =~ /NOT/)
			{	push @$not, { id => $_, data => $assoc_data->{by_a}{$_}{arr} };
			}
			else
			{	push @$annot, { id => $_, data => $assoc_data->{by_a}{$_}{arr} };
			}
		} @{$assoc_data->{by_t}{$t}};
		
		if ($annot)
		{	my $a_node = new GOBO::DataArray( id=> "annot_data_$count", data_arr=>$annot );
			$graph->add_node( $a_node );
			push @statements, new GOBO::Annotation(node=>$a_node, relation=>$annotated_to, target=>$t_obj);
			$ada_ids->{"annot_data_$count"} = $a_node;
			$count++;
		}
		if ($not)
		{	my $a_node = new GOBO::DataArray( id=> "annot_data_$count" , data_arr=>$not);
			$graph->add_node( $a_node );
			push @statements, new GOBO::Annotation(node=>$a_node, relation=>$annotated_to_NOT, target=>$t_obj);
			$ada_ids->{"annot_data_$count"} = $a_node;
			$count++;
		}
	}

	$graph->add_statements_to_ix(ix=>'asserted_links', statements=>[@statements]);

	$graph->update_graph;

	if ($err_count)
	{	warn "$err_count annotations were not added to the graph (terms could not be found or were obsolete)";
	}

	print STDERR "Added associations to the graph!\n" if $options->{verbose};
	## OK, we have our graph with the annotations attached. Let's get slimming!
	# the input set is any terms with an annotation connected to them
	# get the links between the nodes
	my $ie = new GOBO::InferenceEngine::CustomEngine( graph => $graph );

	$ie->get_closest_and_ancestral( subset_ids => [ keys %$subset ], input_ids => [ keys %$subset, keys %$ada_ids ], from_ix => 'asserted_links', all_ix => 'transitive_closure', closest_ix => 'transitive_reduction' );

	print STDERR "Finished slimming; performing error checks\n" if $options->{verbose};

#	print STDERR "graph terms: " . join(", ", @{$ie->graph->terms}) . "\n";
#	print STDERR "statements:\n" . join("\n", @{$ie->graph->statements}) . "\n";
	
#	print STDERR "Closest links:\n" . join("\n", @{$ie->graph->get_all_statements_in_ix('transitive_reduction')} )
#	. "\nAll links\n" . join("\n", @{$ie->graph->get_all_statements_in_ix('transitive_closure')} ) . "\n\n";

	## lots and lots of error checks!

	## check for missing annotations, and transfer annotation info
	undef @errs;
	my $n_annots = scalar keys %$ada_ids;

	my $n_ix = $ie->graph->statement_ix_node_index('transitive_closure');
	my $t_ix = $ie->graph->statement_ix_target_index('transitive_closure');
	my $ol_ix = [ @{$ie->graph->statement_ix_target_index('ontology_links')}, @{$ie->graph->statement_ix_node_index('ontology_links')} ];

	## Do some checks.
	## Look for missing terms:

	foreach my $t (keys %$subset)
	{	if (! grep { $t eq $_ } @$t_ix)
		{	# this term has been lost from the graph!
			# shall we just forget about it?
			push @errs, $t;
			print STDERR "Looking at $t... no links at all!\n";
		}
		elsif (! grep { $t eq $_ } @$ol_ix)
		{	# this term is not attached to any other ontology terms!
			push @errs, $t;
			print STDERR "Looking at $t... no ontology links!\n";
			## delete the term unless we are keeping orphans
#			if (! $options->{keep_orphans})
#			{	print STDERR "deleting node $t\n";
#				$ie->graph->remove_node($t, 1);
#			}
		}
		else
		{	#print STDERR "Looking at $t... found some links!\n";
		}
	}

	if (@errs)
	{	if (scalar @errs == scalar keys %$subset)
		{	die "All terms were lost during slimming! Dying";
		}
		else
		{	warn "The following terms were lost during slimming:\n" . join(", ", sort @errs);
		}
	}

	$err_count = 0;
	## look for missing annotations
	foreach my $a (keys %$ada_ids)
	{	if (! grep { $a eq $_ } @$n_ix)
		{	# this annotation has been lost from the graph!
			$err_count += scalar @{$ada_ids->{$a}->data_arr};
		}
	}

	if (@errs)
	{	if (scalar @errs == $n_annots)
		{	die "All annotations were lost during slimming! Dying";
		}
		else
		{	warn "Some annotations ($err_count) were lost during slimming. Sorry!";
		}
	}

	undef @errs;

=cut
	if ($options->{delete_new_roots})
	{	
		## check our roots... have we got some mysterious new root nodes?
		my $new_roots = $ie->graph->get_connected_roots_in_ix('transitive_closure');
		my $root_h;
		my $new;
		foreach (@$roots)
		{	$root_h->{$_->id} = $_;
		}
		foreach (@$new_roots)
		{	if ($root_h->{$_->id})
			{	delete $root_h->{$_->id};
				next;
			}
			## this is a new root!
			$new->{$_->id} = $_;
		}
	
		if (keys %$root_h)
		{	print STDERR "The following root nodes were lost:\n" . join(", ", keys %$root_h) . "\n";
		}
		if (keys %$new)
		{	print STDERR "The following terms are now root nodes:\n" . join(", ", keys %$new) . "\n";
		}

		my $to_delete;
		%$to_delete = %$new;
		foreach my $r (keys %$new)
		{	## get all the children of this term and delete the whole blimmin' lot!
			foreach (  # @{$ie->graph->get_outgoing_statements($r)}, 
			@{$ie->graph->get_incoming_statements($r)})
			{	# $to_delete->{$_->target->id} = 1;
				$to_delete->{$_->node->id} = 1;
			}
		}
		
		foreach (keys %$to_delete)
		{	$ie->graph->remove_node($_, 1);
		}
	}
=cut

	## if we're doing a rewrite of the GAF file, we don't need any more fancy
	## trimmings
	if ($options->{mode} && $options->{mode} eq 'rewrite')
	{	return { graph => $ie->graph, assoc_data => $assoc_data };
	}

	print STDERR "Creating extra indexes\n" if $options->{verbose};

	## otherwise, sort the statements into direct annotations, direct ontology
	## links and inferred annotations

	my $a_refs = $ie->graph->get_statement_ix_by_name('annotations')->get_all_references;
	my $o_refs = $ie->graph->get_statement_ix_by_name('ontology_links')->get_all_references;
	my $t_red = $ie->graph->get_statement_ix_by_name('transitive_reduction')->get_all_references;

	## sort out direct annotations and direct ontology links
	foreach my $r (@$t_red)
	{	if ( grep { $r eq $_ } @$a_refs )
		{	## direct annotation
			$ie->graph->add_statements_to_ix(ix=>'direct_annotations', statements=>[$$r]);
		}
		elsif (grep { $r eq $_ } @$o_refs)
		{	## ontology link
			$ie->graph->add_statements_to_ix(ix=>'direct_ontology_links', statements=>[$$r]);
		}
	}

	## add inferred annotations to 'all_annotations' and label them as 'inferred'
	foreach my $r (@{$ie->graph->get_statement_ix_by_name('transitive_closure')->get_all_references})
	{	if ( grep { $r eq $_ } @$a_refs) # && ! grep { $r eq $_ } @$t_red)
		{	$$r->inferred(1) if ! grep { $r eq $_ } @$t_red;
			$ie->graph->add_statements_to_ix(ix=>'all_annotations', statements=>[$$r]);
		}
	}

	return { graph => $ie->graph, assoc_data => $assoc_data };
}




=head2 get_subset_nodes

get the subset nodes we want by whatever means, fair or foul
roots will be added to the subset after determining that there are other terms
in the subset unless options->{exclude_roots} is set to 1

input:  graph   => Graph object
        options => option_h
          options may be:
          get_all_subsets => 1
          subset => { subset_name => 1, subset_2_name => 1 }
          # subset_regexp => regular expression

          exclude_roots => 1  # set this is you DON'T want the root nodes included


output: data hash or death with an appropriate error
        data hash will be of the form
        data->{subset}{subset_name}{id of node in subset} = 1
        data->{roots}{node id} = 1

=cut

sub get_subset_nodes {
	my %args = (@_);
	my $graph = $args{graph};
	my $options = $args{options};
	my $data = $args{data};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless defined $graph && $options;

#	print STDERR "options: " . Dumper($options) . "\n";

	## create the subroutine to filter out the desired subset nodes
	my $sub_test;
	if ($options->{get_all_subsets})
	{	$sub_test = sub {
			my $node = shift;
			if ($node->subsets)
			{	map { $data->{subset}{$_->id}{$node->id} = $node } @{$node->subsets};
			}
		};
	}
	elsif ($options->{subset_regexp})
	{	$sub_test = sub {
			my $node = shift;
			if ($node->subsets)
			{	foreach (map { $_->id } @{$node->subsets})
				{	$data->{subset}{$_}{$node->id} = $node if /$options->{subset_regexp}/;
				}
			}
		};
	}
	else
	{	$sub_test = sub {
			my $node = shift;
			if ($node->subsets)
			{	foreach my $s (map { $_->id } @{$node->subsets})
				{	$data->{subset}{$s}{$node->id} = $node if defined $options->{subset}{$s};
				}
			}
		};
	}

	foreach ( @{$graph->terms} )
	{	next if $_->obsolete;
		my $n = $_;
		# make sure that we have all the root nodes
		if (!@{$graph->get_outgoing_statements(node=>$n, ix=>'ontology_links')}) {
			$data->{roots}{$n->id} = $n;
		}
		## if it's in a subset, save the mofo.
		else
		{	&$sub_test($n);
		}
	}

	#	check that we have nodes in our subsets
	if ($options->{subset})
	{	my $no_nodes;
		foreach (keys %{$options->{subset}})
		{	if (! $data->{subset}{$_})
			{	push @$no_nodes, $_;
			}
		}
		if ($no_nodes)
		{	if (scalar @$no_nodes == scalar keys %{$options->{subset}})
			{	die "Error: no nodes were found in any of the subsets specified. Dying";
			}
			else
			{	warn "Error: no nodes were found for the following subset(s): " . join(", ", @$no_nodes) . "\nDying";
			}
		}
	}
	else
	{	if (! $data->{subset} || ! values %{$data->{subset}})
		{	if ($options->{get_all_subsets})
			{	die "Error: no subsets were found! Dying";
			}
			else
			{	die "Error: no subsets were found matching the regular expression specified! Dying";
			}
		}
	}


	# merge the subsets into one if we want combined results
	if ($options->{combined})
	{	my @subs = keys %{$data->{subset}};
		map {
			my $s = $_;
			map {
				$data->{subset}{combined}{$_} = $data->{subset}{$s}{$_};
			} keys %{$data->{subset}{$s}};
			delete $data->{subset}{$s};
		} @subs;
	}


	## add the roots to the subsets unless we specifically don't want 'em
	unless (defined $options->{exclude_roots} && $options->{exclude_roots} == 1)
	{	foreach my $r (keys %{$data->{roots}})
		{	foreach my $s (keys %{$data->{subset}})
			{	$data->{subset}{$s}{$r} = $data->{roots}{$r};
			}
		}
	}
	return $data;
}




=head2 slim_graph

Concatenates the various sub-functions involved in slimming

input:  graph   => Graph object
        options => option_h
          return_as_graph => ## to return the results as a proper Graph object

output:
        with option 'return_as_graph' on: a slimmed Graph object
        otherwise, data hash in the form
        {graph}{ node_id }{ relation_id }{ target_id }


sub slim_graph {
	my %args = ( @_ );
	my $graph = $args{graph};
	my $subset = $args{subset};
	my $options = $args{options};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless defined $graph && $options && $subset;

	$graph->duplicate_statement_ix('ontology_links', 'asserted_links');
	my $inf_eng = $args{inf_eng} || new GOBO::InferenceEngine::CustomEngine(graph => $graph, from_ix => 'asserted_links', save_ix => 'inferred_links');


	# get the links between the nodes
#	my $node_data = $args{node_data} ||
#		get_graph_inferred_links( subset => $subset, graph => $graph, input => $args{input}, inf_eng => $inf_eng, options => $options );
	$ie->__create_edge_matrix( subset => $subset, input => $args{input}, options => $options );
	# populate the node look up hashes
	$ie->__populate_all_edge_matrices;

	print STDERR "Done \$ie->__create_edge_matrix!\n" if $options->{verbose};


	# get the relations from the graph
	my $relations = $args{relations} ||
		get_graph_relations( graph => $graph, options => $options );
		print STDERR "Done GOBO::Util::GraphFunctions::get_graph_relations!\n" if $options->{verbose};

	# remove redundant relationships between nodes
	$ie->remove_redundant_relationships;

	print STDERR "Done GOBO::Util::GraphFunctions::remove_redundant_relationships!\n" if $options->{verbose};

	if ($options->{verbose})
	{	$ie->dump_edge_matrix('node_rel_target');
	}

	# repopulate the node look up hashes
	$ie->__populate_all_edge_matrices;
	print STDERR "Done GOBO::Util::GraphFunctions::populate_lookup_hashes!\n" if $options->{verbose};

	if ($options->{return_as_graph})
	{	my $inf_graph = add_extra_stuff_to_graph( old_g => $graph, new_g => $inf_eng->inferred_graph, options => $options );
		return trim_inferred_graph( graph_data => $node_data, inf_graph => $inf_graph, options => $options );
	}
	else
	{	return $ie->trim_graph;
	}

=cut

=cut
	# slim down dem nodes
	my $slimmed = trim_graph( graph_data => $node_data, options => $options );
	print STDERR "Done GOBO::Util::GraphFunctions::trim_graph!\n" if $options->{verbose};

	if ($options->{return_as_graph})
	{	my $new_graph = add_nodes_and_links_to_graph( old_g => $graph, full_g => $inf_eng->inferred_graph, graph_data => $slimmed->{graph}, options => $options );
		print STDERR "Done GOBO::Util::GraphFunctions::add_nodes_and_links_to_graph!\n" if $options->{verbose};

		return add_extra_stuff_to_graph( old_g => $graph, new_g => $new_graph, options => $options );
		print STDERR "Done GOBO::Util::GraphFunctions::add_extra_stuff_to_graph!\n" if $options->{verbose};

	}
	return $slimmed;
}

=cut

=head2 get_graph_relations

Get the relations and their inter-relation from a graph

input:  graph   => Graph object
        options => option_h

output: rel_h containing the relations from graph in the form
             { rel_node_id }{ rel_relation_id }{ rel_target_id }
        and rel_h->{got_graph} = 1


sub get_graph_relations {
	my %args = (@_);
	my $graph = $args{graph};
	my $options = $args{options};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless $graph && $options;
	my $rel_h;

	# get the relations specified in the graph and see how they relate to each other...
	foreach (@{$graph->relations})
	{	if ($graph->get_outgoing_statements(node=>$_, ix=>'edges'))
		{	foreach (@{$graph->get_outgoing_statements(node=>$_, ix=>'edges')})
			{	$rel_h->{graph}{$_->node->id}{$_->relation->id}{$_->target->id}++;
			}
		}
	}
	print STDERR "Finished getting relationships\n" if $options->{verbose};

	$rel_h->{got_graph} = 1;
	return $rel_h;
}

=cut

=head2 get_graph_inferred_links

Find the links between the terms in $input and those in $subset

input:  input   => array of input node IDs; optional; uses all graph nodes
                   all graph nodes if not specified
        subset  => subset nodes in the form node_id => 1; optional; uses
                   all graph nodes if not specified
        graph   => Graph object
        inf_eng => inference engine (a new one will be created if not)

        options => hash of options, including options->{verbose}

output: node data in the form
             {graph}{ node_id }{ relation_id }{ target_id }


sub get_graph_inferred_links {
	my %args = (@_);
#	my $roots = $args{roots};
	my $graph = $args{graph};

	my $subset = $args{subset};
	my $input = $args{input};
	my $options = $args{options} || {};

	confess( (caller(0))[3] . ": missing required arguments" ) unless defined $graph;

	if (! $graph->exists_statement_ix('asserted_links'))
	{	$graph->duplicate_statement_ix('ontology_links', 'asserted_links');
	}

	my $ie = $args{inf_eng} || new GOBO::InferenceEngine::CustomEngine(from_ix => 'asserted_links', save_ix => 'inferred_links');
	$ie->graph($graph);

	# no input: use all the terms in the graph as input
	if (! $input || ! @$input)
	{	@$input = map { $_->id } @{$graph->terms};
		print STDERR "Using all terms as input set\n" if $options->{verbose};
	}

	confess( (caller(0))[3] . ": missing required arguments" ) unless $ie && $input;

	# get rid of any existing data
	my $node_data;

#print STDERR "subset: " . join(", ", sort keys %$subset) . "\n";

	if ($subset)
	{	# get all the links between the input nodes and those in the subset
		foreach my $t (@$input)
		{
			## asserted links
			foreach (@{ $graph->get_outgoing_links(node=>$t) })
			{	# skip it unless the target is a root or in the subset
				next unless $subset->{$_->target->id}; # || $roots->{$_->target->id} ;
				$node_data->{graph}{$t}{$_->relation->id}{$_->target->id} = 1;
#				print STDERR "ASS: $t " . $_->relation->id . " " . $_->target->id . "\n";
			}

=cut


=cut
			foreach (@{ $ie->get_inferred_outgoing_edges(node=>$t) })
			{	# skip it unless the target is a root or in the subset
#				print STDERR "looking at $_\n";
				if (! $subset->{$_->target->id})
				{	#print STDERR $_->target . " is not a subset term!\n";
					next;
				}
				next unless exists $subset->{$_->target->id}; # || $roots->{$_->target->id} ;
#				# skip it if we already have this link
#				next if defined $node_data->{graph}{$t}{$_->relation->id}{$_->target->id};
				## add to a list of inferred entries
				$node_data->{graph}{$t}{$_->relation->id}{$_->target->id}++;
			}
		}
	}
	else
	{	# get all the links involving the input nodes
		foreach my $t (@$input)
		{	## asserted links
#			foreach (@{ $graph->get_outgoing_links(node=>$t) })
#			{	$node_data->{graph}{$t}{$_->relation->id}{$_->target->id} = 1;
#			}

			foreach (@{ $ie->get_inferred_outgoing_edges(node=>$t) })
			{	# skip it if we already have this link
#				next if defined $node_data->{graph}{$t}{$_->relation->id}{$_->target->id};
				## add to a list of inferred entries
				$node_data->{graph}{$t}{$_->relation->id}{$_->target->id}++;
			}
		}
	}
	return $node_data;
}
=cut


=head2 remove_redundant_relationships

input:  node_data => hash of node data in the form
                     {graph}{ node_id }{ relation_id }{ target_id }
        rel_data  => relationship data hash (structure same as node_data)
        graph     => Graph object
        options   => option_h


output: node_data with redundant rels carefully removed

if we have relationships between relations -- e.g. positively_regulates is_a
regulates -- and two (or more) related relations are found between the same
two nodes, the less specific relationships are removed.

e.g.

A positively_regulates B
A regulates B

==> A regulates B will be removed


sub remove_redundant_relationships {
	my %args = (@_);
	my $node_data = $args{node_data};
	my $rel_data = $args{rel_data};
	my $graph = $args{graph};
	my $options = $args{options};

	# make sure we have the relation relationships
	if (! $rel_data->{got_graph} )
	{	$rel_data = get_graph_relations( graph => $graph, options => $options );
	}

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless $node_data && $rel_data && $graph;

	if (defined $rel_data->{graph})
	{	$ie->__populate_all_edge_matrices;

		my $slimmed = trim_graph( graph_data => $rel_data, options => $options );

		$ie->__populate_all_edge_matrices;

		## slim down the relationships
		## get rid of redundant relations
		# these are the closest to the root

		## this could probably be done more effectively / efficiently

		foreach my $r (keys %{$slimmed->{target_node_rel}})
		{	foreach my $r2 (keys %{$slimmed->{target_node_rel}{$r}})
			{	# if both exist...
				if ($node_data->{rel_node_target}{$r} && $node_data->{rel_node_target}{$r2})
				{
					# delete anything where we have the same node pairs with both relations
					foreach my $n (keys %{$node_data->{rel_node_target}{$r2}})
					{	if (defined $node_data->{graph}{$n}{$r})
					#	if ($data->{nodes}{rel_node_target}{$r}{$n})
						{
							foreach my $t (keys %{$node_data->{rel_node_target}{$r2}{$n}})
							{	if (defined $node_data->{graph}{$n}{$r}{$t})
								{
									delete $node_data->{graph}{$n}{$r}{$t};
									if (! values %{$node_data->{graph}{$n}{$r}})
									{	delete $node_data->{graph}{$n}{$r};
										if (! values %{$node_data->{graph}{$n}})
										{	delete $node_data->{graph}{$n};
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
#	return $node_data;
}

=cut

=head2 trim_graph

input:  graph_data => data hash with nodes and relations specified as
               {graph}{ node_id }{ relation_id }{ target_id }
          plus various rearrangements, with a hash key specifying the ordering
          e.g. {node_target_rel}
               {target_node_rel}
        options => option_h  # no options specified as yet

output: new data hash, slimmed down, with relations specified as
               {graph}{ node_id }{ relation_id }{ target_id }

For each term, finds the closest node for each relation and stores them in a hash


sub trim_graph {
	my %args = (@_);
	my $d = $args{graph_data};
	my $new_d;      # new data hash - woohoo!
	my $options = $args{options};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless values %{$d->{graph}};

	if (! $ie->edge_matrix->defined('target_node_rel') )
	{	$ie->__populate_all_edge_matrices;
	}

	# for each node with a link to a 'target' (closer to root) node
	foreach my $id (keys %{$d->{node_target_rel}})
	{	# only connected to one node: must be the closest!
		if (scalar keys %{$d->{node_target_rel}{$id}} == 1)
		{	$new_d->{graph}{$id} = $d->{graph}{$id};
			next;
		}
		foreach my $rel (keys %{$d->{node_rel_target}{$id}})
		{	# only one node connected by $rel
			if (scalar keys %{$d->{node_rel_target}{$id}{$rel}} == 1)
			{	$new_d->{graph}{$id}{$rel} = $d->{node_rel_target}{$id}{$rel};
				next;
			}

			#	list_by_rel contains all the nodes between it and the root(s) of $id
			my @list_by_rel = keys %{$d->{node_rel_target}{$id}{$rel}};

			REL_SLIMDOWN_LOOP:
			while (@list_by_rel)
			{	my $a = pop @list_by_rel;
				my @list2_by_rel = ();
				while (@list_by_rel)
				{	my $b = pop @list_by_rel;
					if ($d->{target_node_rel}{$a}{$b})
					{	#	b is node, a is target
						#	forget about a, go on to the next list item
						push @list_by_rel, $b;
						push @list_by_rel, @list2_by_rel if @list2_by_rel;
						next REL_SLIMDOWN_LOOP;
					}
					elsif ($d->{node_target_rel}{$a}{$b})
					{	#	a is node, b is target
						#	forget about b, look at the next in the list
						next;
					}
					else
					{	#a and b aren't related
						#	keep b
						push @list2_by_rel, $b;
						next;
					}
				}
				#	if a is still around, it must be a descendent of
				#	all the nodes we've looked at, so it can go on our
				#	descendent list
				$new_d->{graph}{$id}{$rel}{$a} = $d->{node_rel_target}{$id}{$rel}{$a};

				#	if we have a list2_by_rel, transfer it back to @list_by_rel
				push @list_by_rel, @list2_by_rel if @list2_by_rel;
			}
		}
	}
	return $new_d;
}

=cut


=head2 populate_lookup_hashes

input:  data hash with nodes and relations specified as
             {graph}{ node_id }{ relation_id }{ target_id }
output: rearrangements of the data with first key specifying the order:
             {node_target_rel}
             {target_node_rel}
             {node_rel_target}
             {target_rel_node}
             {rel_node_target}
             {rel_target_node}


sub populate_lookup_hashes {
	my %args = (@_);
	my $hash = $args{graph_data};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless values %{$hash->{graph}};

	foreach my $k qw(node_target_rel target_node_rel node_rel_target target_rel_node rel_node_target rel_target_node)
	{	delete $hash->{$k};
	}

	foreach my $n (keys %{$hash->{graph}})
	{	foreach my $r (keys %{$hash->{graph}{$n}})
		{	foreach my $t (keys %{$hash->{graph}{$n}{$r}})
			{	$hash->{node_target_rel}{$n}{$t}{$r} = #1;
				$hash->{target_node_rel}{$t}{$n}{$r} = #1;
				$hash->{node_rel_target}{$n}{$r}{$t} = #1;
				$hash->{target_rel_node}{$t}{$r}{$n} = #1;
				$hash->{rel_node_target}{$r}{$n}{$t} = #1;
				$hash->{rel_target_node}{$r}{$t}{$n} = #1;
				$hash->{graph}{$n}{$r}{$t};
			}
		}
	}
}

=cut




=head2 get_furthest_ancestral_nodes

input:  graph_data => data hash with nodes and relations specified as
               {graph}{ node_id }{ relation_id }{ target_id }
               nb: must already have had all that reasoning stuff done
          plus various rearrangements, with a hash key specifying the ordering
          e.g. {node_target_rel}
               {target_node_rel}
        id        => id of node to find the closest ancestral node of
        relation  => relation id, if wanted
        options   => option_h

output: new data hash, slimmed down, with relations specified as
               {graph}{ node_id }{ relation_id }{ target_id }

For a given term, finds the furthest node[s]

=cut

sub get_furthest_ancestral_nodes_from_matrix {
	my %args = (@_);
	my $matrix = $args{graph_data};
	my $id = $args{id};
	my $rel_wanted = $args{relation} || undef;
	my $options = $args{options};


	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless $id && values %$matrix;

	# if there are no links whatsoever involving this term, report and return undef
	if (! $matrix->{N_T_R}{$id} )
	{	print STDERR "No links from $id\n" if $options->{verbose};
		return undef;
	}

	if ( $rel_wanted )
	{	# if a relation is specified, but there are no relations involving this term
		# that match, return undef
		if ( ! values %{$matrix->{N_T_R}{$id}{$rel_wanted}} )
		{	print STDERR "No $rel_wanted links from $id\n" if $options->{verbose};
			return undef;
		}
		# if it is only connected to one node by the relation, it must be the closest!
		elsif (scalar keys %{$matrix->{N_T_R}{$id}{$rel_wanted}} == 1)
		{	return [ map { { node => $id, rel => $rel_wanted, target => $_ } } keys %{$matrix->{N_T_R}{$id}{$rel_wanted}} ];
		}
	}

	# make sure the look up hashes are populated
	if (! $matrix->{N_R_T} || ! $matrix->{T_N_R} )
	{	populate_lookup_hashes( graph_data => $matrix );
	}

	# only connected to one node: must be the closest!
	if (scalar keys %{$matrix->{N_T_R}{$id}} == 1)
	{	# we specified a relation
		if ($rel_wanted)
		{	return [ map { { node => $id, rel => $rel_wanted, target => $_ } } keys %{$matrix->{N_T_R}{$id}} ];
		}
		else
		{	my $target = (keys %{$matrix->{N_T_R}{$id}})[0];
			return [ map { { node => $id, rel => $_, target => $target } } keys %{$matrix->{N_T_R}{$id}{$target}} ];
		}
	}

	#TODO: add in a check for the root nodes



	my $new_d;
	foreach my $rel (keys %{$matrix->{N_R_T}{$id}})
	{	next if $rel_wanted && $rel ne $rel_wanted;

		#	list_by_rel contains all the nodes between it and the root(s) of $id
		my @list_by_rel = keys %{$matrix->{N_R_T}{$id}{$rel}};

		REL_SLIMDOWN_LOOP:
		while (@list_by_rel)
		{	my $a = pop @list_by_rel;
			my @list2_by_rel = ();
			while (@list_by_rel)
			{	my $b = pop @list_by_rel;
				if ($matrix->{T_N_R}{$a}{$b})
				{	#	b is node, a is target
					#	forget about b, look at the next in the list
					next;
				}
				elsif ($matrix->{N_T_R}{$a}{$b})
				{	#	a is node, b is target
					#	forget about a, go on to the next list item
					push @list_by_rel, $b;
					push @list_by_rel, @list2_by_rel if @list2_by_rel;
					next REL_SLIMDOWN_LOOP;
				}
				else
				{	#a and b aren't related
					#	keep b
					push @list2_by_rel, $b;
					next;
				}
			}
			#	if a is still around, it must be a descendent of
			#	all the nodes we've looked at, so it can go on our
			#	descendent list
			push @$new_d, { node => $id, rel => $rel, target => $a };
#			$new_d->{graph}{$id}{$rel}{$a} = $matrix->{N_R_T}{$id}{$rel}{$a};

			#	if we have a list2_by_rel, transfer it back to @list_by_rel
			push @list_by_rel, @list2_by_rel if @list2_by_rel;
		}
	}

	return $new_d;
}


=head2 topological_sort

input:  graph_data => data hash with nodes and relations specified as
               {graph}{ node_id }{ relation_id }{ target_id }
               nb: must already have had all that reasoning stuff done
          plus various rearrangements, with a hash key specifying the ordering
          e.g. {node_target_rel}
               {target_node_rel}
        id        => id of node to do the topological sort on
        relation  => relation id, if wanted
        options   => option_h

output: topo sorted list

For a given term, finds the furthest node[s]

=cut

sub topological_sort {
	my %args = (@_);
	my $d = $args{graph_data};
	my $id = $args{id};
#	my $rel_wanted = $args{relation} || undef;
	my $options = $args{options};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless $id && values %{$d->{graph}};

	if (! $d->{graph}{$id})
	{	print STDERR "$id is a root node... sob!\n";
		return;
	}

	my @sorted;   # Empty list that will contain the sorted nodes
	my @leafy;    # Set of all nodes with no incoming edges

=algorithm:
while leafy is non-empty do
	remove a node n from leafy
	insert n into sorted
	for each node m with an edge e from n to m do
		remove edge e from the graph
		if m has no other incoming edges then
			insert m into leafy
if graph has edges then
	output error message (graph has at least one cycle)
else
	output message (proposed topologically sorted order: sorted)
=cut

#	print STDERR "input data: " . Dumper($d);

	@leafy = ( $id );
	my $graph;  # this will be all the nodes related to $id

	foreach my $r (keys %{$d->{graph}{$id}})
	{	foreach (keys %{$d->{graph}{$id}{$r}})
		{	$graph->{node_target}{$id}{$_} = 1;
			$graph->{target_node}{$_}{$id} = 1;
		}
	}

	## get all relations involving terms listed as keys in $graph->{target_node}
	foreach my $t (keys %{$graph->{target_node}})
	{	if ($d->{graph}{$t})
		{	foreach my $r (keys %{$d->{graph}{$t}})
			{	foreach (keys %{$d->{graph}{$t}{$r}})
				{	if ($graph->{target_node}{$_})
					{	$graph->{node_target}{$t}{$_} = 1;
						$graph->{target_node}{$_}{$t} = 1;
					}
				}
			}
		}
	}

	while (@leafy)
	{	my $n = pop @leafy;
		push @sorted, $n;
		if (defined $graph->{node_target}{$n} && defined values %{$graph->{node_target}{$n}})
		{	foreach my $m (keys %{$graph->{node_target}{$n}})
			{	undef $graph->{node_target}{$n}{$m};
				undef $graph->{target_node}{$m}{$n};

				my $none;
				if (defined $graph->{target_node}{$m} && defined values %{$graph->{target_node}{$m}})
				{	foreach (keys %{$graph->{target_node}{$m}})
					{	if (defined $graph->{target_node}{$m}{$_})
						{	$none++;
							last;
						}
					}
					if (! $none )
					{	push @leafy, $m;
					}
				}
			}
		}
	}

	return [ @sorted ];

}


## Graph cloning stuff ##


=head2 add_referenced_nodes_to_graph

input:  old_g => old Graph object
        graph_data => hash of data in the familiar format
             { node_id }{ relation_id }{ target_id }
        new_g => new Graph object (created if does not exist)

output: new graph with all nodes from old Graph object added

=cut

sub add_referenced_nodes_to_graph {
	my %args = (@_);
	my $old_g = $args{old_g};
	my $new_g = $args{new_g} || new GOBO::Graph; # if ! $new_g;
	my $graph_data = $args{graph_data}{graph};

	my $nodes;
	# add the nodes to the graph
	foreach my $n ( keys %$graph_data )
	{	$nodes->{$n}++;
		foreach my $r ( keys %{$graph_data->{$n}} )
		{	$nodes->{$r}++;
			foreach my $t ( keys %{$graph_data->{$n}{$r}} )
			{	$nodes->{$t}++;
			}
		}
	}

	delete $nodes->{is_a} if $nodes->{is_a};

#	print STDERR "node list: " . join("\n", sort keys %$nodes ) . "\n\n";

	NODE_LOOP:
	foreach my $x (keys %$nodes)
	{	next if $new_g->get_node($x);
		warn "Could not find $x in old graph" if ! $old_g->get_node($x);
		foreach my $type qw( term relation instance )
		{	my $add = "add_" . $type;
			my $get = "get_" . $type;
			if ($old_g->$get($x))
			{	$new_g->$add($x);
			#	print STDERR "$x is a $type! Adding...\n";
				next NODE_LOOP;
			}
		}
		$new_g->add_node( $old_g->get_node($x) );
	}

	return $new_g;
}



=head2 add_all_nodes_to_graph

input:  old_g => old Graph object
        new_g => new Graph object (created if does not exist)

output: new graph with all nodes from old Graph object added

=cut

sub add_all_nodes_to_graph {
	my %args = (@_);
	my $old_g = $args{old_g};
	my $new_g = $args{new_g} || new GOBO::Graph; # if ! $new_g;

	my $node_h;

	foreach my $type qw( term relation instance )
	{	my $has = "has_" . $type . "s";
		my $all = $type . "s";
		my $add = "add_" . $type;
		my $get = "get_" . $type;
		next unless $old_g->$has;
		foreach (@{$old_g->$all})
		{	$new_g->$add($_) if ! $new_g->$get;
		}
	}

	foreach (@{$old_g->nodes})
	{	next if $new_g->get_node($_);
		$new_g->add_node($_);
	}

	return $new_g;
}



=head2 add_all_relations_to_graph

input:  old_g => old Graph object
        new_g => new Graph object (created if does not exist)
        no_rel_links => 1  if links should be NOT added (default is to add them)

output: new graph with relations from the old graph added

=cut

sub add_all_relations_to_graph {
	my %args = (@_);
	my $old_g = $args{old_g};
	my $new_g = $args{new_g} || new GOBO::Graph; # if ! $new_g;
	my $no_rel_links = $args{no_rel_links} || undef;

	confess( (caller(0))[3] . ": missing required argument old_g. Dying" ) unless $old_g && defined $new_g;

	sub check_for_relation {
		my ($graph, $r) = @_;
		return 1 if $r->id ne 'is_a' && ! $graph->get_relation($r);
		return;
	}

	if ($no_rel_links)
	{	$new_g->add_relation($old_g->noderef($_)) foreach @{$old_g->relations};
	}
	else
	{	# add all the relations from the other graph
		foreach (@{$old_g->relations})
		{	$new_g->add_relation($old_g->noderef($_)) if check_for_relation($new_g, $_);

			if ($old_g->get_outgoing_links(node=>$_))
			{	foreach (@{$old_g->get_outgoing_links(node=>$_)})
				{
					$new_g->add_relation( $old_g->noderef( $_->relation ) ) if check_for_relation($new_g, $_->relation);
					$new_g->add_relation( $old_g->noderef( $_->target ) ) if check_for_relation($new_g, $_->target);

					$new_g->add_link( new GOBO::LinkStatement(
						node => $new_g->noderef($_->node),
						relation => $new_g->noderef($_->relation),
						target => $new_g->noderef($_->target)
					) );
				}
			}
		}
	}

	return $new_g;
}


=head2 add_all_terms_to_graph

input:  old_g => old Graph object
        new_g => new Graph object (created if does not exist)
        no_term_links => 1  if links between terms should NOT be added
                            (default is to add them)
        no_rel_links  => 1  if links between relations should NOT be added
                            (default is to add them; only matters if no_term_links
                             has been specified)

output: new graph with terms from the old graph added

=cut

sub add_all_terms_to_graph {
	my %args = (@_);
	my $old_g = $args{old_g};
	my $new_g = $args{new_g} || new GOBO::Graph; # if ! $new_g;
	my $no_term_links = $args{no_term_links} || undef;

	confess( (caller(0))[3] . ": missing required argument old_g. Dying" ) unless $old_g && defined $new_g;

	if ($no_term_links)
	{	$new_g->add_term($old_g->get_term($_)) foreach @{$old_g->terms};
	}
	else
	{	# add all the relations from the other graph
		$new_g = add_all_relations_to_graph(%args);

		foreach (@{$old_g->terms})
		{	$new_g->add_term($old_g->get_term($_));
			if ($old_g->get_outgoing_links(node=>$_))
			{	foreach (@{$old_g->get_outgoing_links(node=>$_)})
				{	# add the target term if it isn't there
					$new_g->add_node($old_g->noderef($_->target)) if ! $new_g->get_node($_->target);
					$new_g->add_link( new GOBO::LinkStatement(
						node => $new_g->noderef($_->node),
						relation => $new_g->noderef($_->relation),
						target => $new_g->noderef($_->target)
					) );
				}
			}
		}
	}

	return $new_g;
}


=head2 add_extra_stuff_to_graph

input:  old_g => old Graph object
        new_g => new Graph object (created if does not exist)

output: new graph has various attributes from the old graph added


## TODO : This method was a bit of a hack done in the early days of go-moose
## and could probably do with updating!

sub add_extra_stuff_to_graph {
	my %args = (@_);
	my $old_g = $args{old_g};
	my $new_g = $args{new_g};

	copy_attributes(%args, ignore => [ qw( term_h relation_h instance_h ) ] );
#	return $new_g;
}
=cut

=head2 copy_attributes

Duplicate attributes from one Object to another. The 'to' object should be of the
same class or a subclass of the 'from' object.

If 'include' is specified, only the attributes in the 'include' array will be
copied.
If 'ignore' is specified, all attributes except those in the 'ignore' array will
be copied.
If neither are specified, all attributes will be duplicated. Specifying both
'ignore' and 'include' is an error.

WARNING: this is a bit of a hack!!

input:  hash containing
        from => Object to copy attribute(s) from
        to   => Object to copy attribute(s) to
        ## The following are optional; only ONE can be specified
        include => [ attrib, attrib, ... ]  ## only copy attribs in this array
        ignore  => [ attrib, attrib, ... ]  ## do not copy attribs in this array
output: the 'to' object will have values

=cut

sub copy_attributes {
	my %args = (@_);
	my $from = $args{from};
	my $to = $args{to};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless defined $from && defined $to;
	confess( (caller(0))[3] . ": cannot specify 'ignore' and 'include'! Dying" ) if $args{include} && $args{ignore};
	confess( (caller(0))[3] . ": \$args{to} must be the same class or a subclass of \$args{from}. Dying" ) unless $to->isa( ref $from );

	## get the object attributes and compare them
	my $meta = Class::MOP::Class->initialize( ref($from) );
	my @attrs = $meta->get_all_attributes;

#	print STDERR "attributes: " . join(", ", sort map { $_->name } @attrs ) . "\n";

	my $todo;

	foreach my $a (@attrs)
	{	if ($args{include})
		{	next unless grep { $a->name eq $_ } @{$args{include}};
		}
		if ($args{ignore})
		{	next if grep { $a->name eq $_ } @{$args{ignore}};
		}
		$todo->{$a->name} = $a;
	}

	## try to minimise the hackiness...
	if ($from->isa('GOBO::Graph'))
	{	if ($todo->{term_h} && $todo->{relation_h} && $todo->{instance_h} && $todo->{node_index})
		{	## they're all there, so no need to do anything
		}
		else
		{	## term_h, instance_h or relation_h affect the content of the node_index,
			## so generate them separately
			if ($todo->{term_h})
			{	delete $todo->{term_h};
				foreach (@{$from->terms})
				{	$to->add_term($_);
				}
			}
			if ($todo->{relation_h})
			{	delete $todo->{relation_h};
				foreach (@{$from->relations})
				{	$to->add_relation($_);
				}
			}
			if ($todo->{instance_h})
			{	delete $todo->{instance_h};
				foreach (@{$from->instances})
				{	$to->add_instance($_);
				}
			}
			delete $todo->{node_index};
		}
	}


#	print STDERR "todo: " . join(", ", map { $_->name } sort { $a->name cmp $b->name } values %$todo) . "\n";

	foreach my $x (sort { $a->name cmp $b->name } values %$todo) {
		my $r = $x->get_read_method;
		my $w = $x->get_write_method;
		warn ref($from) . " lacks read AND write methods for " . $x->name && next unless defined $r && defined $w;

		my $from_val = $from->$r;
		$to->$w( $from_val ) if defined $from_val;
	}
}


=head2 add_nodes_and_links_to_graph

input:  graph_data => data hash with nodes and relations specified as
             { node_id }{ relation_id }{ target_id }
        old_g      => old graph, containing nodes and relations that appear
                      in the data hash
        new_g      => preferably the new graph, containing relations
                      (created if does not exist)

output: new graph, containing all the nodes and relations specified in

=cut

sub add_nodes_and_links_to_graph {
	my %args = (@_);
	my $graph_data = $args{graph_data}{graph};
	my $old_g = $args{old_g};
	my $new_g = $args{new_g};
	my $full_g = $args{full_g};

	$new_g = new GOBO::Graph if ! $new_g;

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless defined $old_g && defined $new_g && $graph_data;

	## add the relations to the graph if absent
	if (! @{$new_g->relations} )
	{	$new_g = add_all_relations_to_graph(%args);
	}

	# add the nodes to the graph
	foreach my $n ( keys %$graph_data )
	{	# add the nodes to the graph
		$new_g->add_node( $old_g->noderef( $n ) ) if ! $new_g->get_node($n);

		foreach my $r ( keys %{$graph_data->{$n}} )
		{	foreach my $t ( keys %{$graph_data->{$n}{$r}} )
			{	$new_g->add_node( $old_g->noderef( $t ) ) if ! $new_g->get_node($t);

				$new_g->add_link( new GOBO::LinkStatement(
					node => $new_g->noderef($n),
					relation => $new_g->noderef($r),
					target => $new_g->noderef($t)
				) );
			}
		}
	}
	return $new_g;
}


=head2 trim_inferred_graph

input:  an inferred graph (with every link under the sun in it)
        a hash containing the links that *should* be in the graph
        option hash, containing
          remove_unlinked_terms => 1  ## to get rid of any terms with no links
output: a trimmed graph, containing only the links that should be in the graph

=cut

sub trim_inferred_graph {
	my %args = (@_);
	my $d = $args{graph_data};
	my $options = $args{options};
	my $graph = $args{inf_graph};

	$graph->update_graph;

	if ($options->{verbose})
	{	print STDERR "n links: " . scalar (@{$graph->links}) . "\n" . join("\n", @{$graph->links}) . "\n\n\n";
	}

	$graph->remove_all_links;

	if ($options->{verbose})
	{	print STDERR "n links: " . scalar (@{$graph->links}) . "\n" . join("\n", sort { $a->node->id cmp $b->node->id | $a->target->id cmp $b->target->id } @{$graph->links}) . "\n\n\n";
	}

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless values %{$d->{graph}};

	if (! $d->{node_rel_target} || ! $d->{target_node_rel} )
	{	populate_lookup_hashes( graph_data => $d );
	}

	my @links;
	# for each node with a link to a 'target' (closer to root) node
	foreach my $id (keys %{$d->{node_target_rel}})
	{	# only connected to one node: must be the closest!
		if (scalar keys %{$d->{node_target_rel}{$id}} == 1)
		{	foreach my $rel (keys %{$d->{node_rel_target}{$id}})
			{	foreach my $t (keys %{$d->{node_rel_target}{$id}{$rel}})
				{	my $l = new GOBO::LinkStatement( node => $graph->get_term($id), relation => $graph->get_relation($rel), target => $graph->get_term($t) );
					push @links, $l;
				}
			}
			next;
		}
		foreach my $rel (keys %{$d->{node_rel_target}{$id}})
		{	# only one node connected by $rel
			if (scalar keys %{$d->{node_rel_target}{$id}{$rel}} == 1)
			{	#$new_d->{graph}{$id}{$rel} = $d->{node_rel_target}{$id}{$rel};
				foreach my $t (keys %{$d->{node_rel_target}{$id}{$rel}})
				{	my $l = new GOBO::LinkStatement( node => $graph->get_term($id), relation => $graph->get_relation($rel), target => $graph->get_term($t) );
					push @links, $l;
				}
				next;
			}

			#	list_by_rel contains all the nodes between it and the root(s) of $id
			my @list_by_rel = keys %{$d->{node_rel_target}{$id}{$rel}};

			REL_SLIMDOWN_LOOP:
			while (@list_by_rel)
			{	my $a = pop @list_by_rel;
				my @list2_by_rel = ();
				while (@list_by_rel)
				{	my $b = pop @list_by_rel;
					if ($d->{target_node_rel}{$a}{$b})
					{	#	b is node, a is target
						#	forget about a, go on to the next list item
						push @list_by_rel, $b;
						push @list_by_rel, @list2_by_rel if @list2_by_rel;
						next REL_SLIMDOWN_LOOP;
					}
					elsif ($d->{node_target_rel}{$a}{$b})
					{	#	a is node, b is target
						#	forget about b, look at the next in the list
						next;
					}
					else
					{	#a and b aren't related
						#	keep b
						push @list2_by_rel, $b;
						next;
					}
				}
				#	if a is still around, it must be a descendent of
				#	all the nodes we've looked at, so it can go on our
				#	descendent list
#				$new_d->{graph}{$id}{$rel}{$a} = $d->{node_rel_target}{$id}{$rel}{$a};

				if (! $graph->get_term($id) || ! $graph->get_term($a) || ! $graph->get_relation($rel) )
				{	print STDERR "id : $id " . Dumper( $graph->get_node($id) ) .
					"relation: $rel " . Dumper( $graph->get_node($rel) ) .
					"target: $a " . Dumper( $graph->get_node($a) ) . "\n";
				}
				my $l = new GOBO::LinkStatement( node => $graph->get_term($id), relation => $graph->get_relation($rel), target => $graph->get_term($a) );
				push @links, $l;
				#	if we have a list2_by_rel, transfer it back to @list_by_rel
				push @list_by_rel, @list2_by_rel if @list2_by_rel;
			}
		}
	}

	$graph->add_links([ @links ]);
	if ($options && $options->{remove_unlinked_terms})
	{	my $unloved = $graph->get_orphan_terms;
		foreach (@$unloved)
		{	#print STDERR "removing $_\n";
			$graph->remove_node($_);
		}
#		$graph->remove_node($_) foreach @$unloved;
	}

	return $graph;
}


=head2 create_bucket_terms

input:  graph
        subset terms



output:

=cut

sub create_bucket_terms {
	my %args = (@_);
	my $graph = $args{graph};
	my $subset = $args{subset};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless $subset && defined $graph;

	my $p_seen;
	foreach (@$subset)
	{	my $t = $graph->get_term($_);
		if (! $t )
		{	print STDERR "Could not find term $t in graph!\n";
			next;
		}
		my $parent_nodes = $graph->get_outgoing_links(node=>$t);
		# this is a root node. Next!
		next if ! $parent_nodes;
		foreach my $p (@$parent_nodes)
		{	next if $p_seen->{$p->id};
			$p_seen->{$p->id}++;
			## get the children of the term
			my @kids;
			foreach my $id (map { $_->id } @{$graph->get_incoming_links($p->id)})
			{	push @kids, $_ if ! grep { $id eq $_ } @$subset;
			}
			if (@kids)
			{	print STDERR "Other " . $p->label . " term: " . join(", ", @kids) . "\n";
			}
		}

		## our boolean expression should be
		## parent($t) = X but annotation NOT to X
		##
	}
}



=head2 load_mapping

input:  mapping file in the format

 term ID [tab] closest ancestral subset terms for each relationship [tab] all other ancestral terms

 term [tab] r1 term, r1 term, r1 term; r2 term [tab] r1 term; r2 term, r2 term

output: data structure in the form
	$data->
		{termlist}{term}           ## all terms in the graph
		{obsolete}{term}           ## obsolete terms
		{all}{term}{rel}{term}     ## all links
		{graph}{term}{rel}{term}   ## closest links
		{subset_term}{term}        ## subset terms

=cut

sub load_mapping {
	my %args = (@_);
	my $file = $args{mapping_file};
	my $data;

	local $/ = "\n";
	open(IN, '<' . $file) or die "The file ".$file." could not be opened: $!\nDying";
	print "Loading $file...\n" if $args{verbose};

	while (<IN>)
	{	next if /^!/;
		if (/(.*?)\t(.+)/)
		{	my @rest = split("\t", $2);
			my $t = $1;
#			$t =~ s/(.*?), (.+)/$1/;
			if ($t =~ /(.+?), (.+)/)
			{	$data->{name}{$1} = $2;
				$t = $1;
			}
			$data->{termlist}{$t}++;
			if ($rest[0] =~ /obsolete/)
			{	$data->{obsolete}{$t}++;
			}
			else
			{	if (defined $rest[2] && $rest[2] eq 'subset_term')
				{	$data->{subset_term}{$t}++;
				}

				my $rest_h = {
					'graph' => $rest[0],
					'all' => $rest[1],
				};
				foreach my $k (keys %$rest_h)
				{	next unless $rest_h->{$k};
					my @arr = split(/[,;] /, $rest_h->{$k});
					foreach (@arr)
					{	if (/(.*?) (.+)/)
						{	my ($rel, $target) = ($1, $2);
							$data->{$k}{$t}{$rel}{$target} = 1;
							$data->{subset_term}{$target}++;
						}
						else
						{	print STDERR "Doesn't match pattern! $_\n";
						}
					}
				}
			}
		}
	}
	print "Finished loading term file.\n" if $args{verbose};
	close(IN);
	return $data;
}


=head2 load_mapping_as_graph

input:  mapping file in the format

 term ID [tab] closest ancestral subset terms for each relationship [tab] all other ancestral terms

 term [tab] r1 term, r1 term, r1 term; r2 term [tab] r1 term; r2 term, r2 term

output: a graph representing the mapping data

=cut

sub load_mapping_as_graph {
	my %args = (@_);
	my $file = $args{mapping_file};

	local $/ = "\n";
	open(IN, '<' . $file) or die "The file ".$file." could not be opened: $!\nDying";
	print "Loading $file...\n" if $args{verbose};

	my $graph = new GOBO::Graph;
	## add a subset to the subset index;
	my $ss_obj = $graph->subset_noderef('mapping_subset');
	$graph->subset_index->{'mapping_subset'} = $ss_obj;

	while (<IN>)
	{	next if /^!/;
		if (/(.*?)\t(.+)/)
		{	my @rest = split("\t", $2);
			my $t = $1;
			my $term;
#			$t =~ s/(.*?), (.+)/$1/;
			if ($t =~ /(.+?), (.+)/)
			{	$term = $graph->term_noderef($1);
				$term->name($2);
			}
			else
			{	$term = $graph->term_noderef($t);
			}
			$graph->add_term($term);

			if ($rest[0] =~ /obsolete/)
			{	$term->obsolete(1);
			}
			else
			{	if (defined $rest[2] && $rest[2] eq 'subset_term')
				{	$term->add_subsets($ss_obj);
				}

				my $rest_h = {
					'closest' => $rest[0],
					'all' => $rest[1],
				};
				foreach my $k (keys %$rest_h)
				{	next unless $rest_h->{$k};
					my @arr = split(/[,;] /, $rest_h->{$k});
					foreach (@arr)
					{	if (/(.*?) (.+)/)
						{	my ($rel, $target) = ($graph->relation_noderef($1), $graph->term_noderef($2));
							my $stt = new GOBO::LinkStatement( node => $term,
							relation => $rel, target => $target );
							$graph->add_statements_to_ix(ix=>$k, statements=>[$stt]);
						}
						else
						{	print STDERR "Doesn't match pattern! $_\n";
						}
					}
				}
			}
		}
	}
	print "Finished loading term file.\n" if $args{verbose};
	close(IN);

	$graph->update_graph;

	return $graph;
}


## quick sub to extract terms matching a certain criteria

sub select_terms {
	my %args = (@_);

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless defined $args{graph} && $args{crit} && ref($args{crit}) eq 'CODE';

	my $graph = $args{graph};
	my $crit = $args{crit};

	return [ grep { &$crit($_) } @{$graph->terms} ];

}




1;
