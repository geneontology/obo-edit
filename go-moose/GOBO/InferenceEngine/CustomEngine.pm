package GOBO::InferenceEngine::CustomEngine;
use Moose;
use Data::Dumper;
extends 'GOBO::InferenceEngine';

has test_sub => (is=>'rw', isa=>'CodeRef', predicate => 'has_sub');

has subrelation_closure_h => (is=>'rw', isa=>'HashRef[ArrayRef[GOBO::RelationNode]]', default=>sub{{}});
has subrelation_reflexive_closure_h => (is=>'rw', isa=>'HashRef[ArrayRef[GOBO::RelationNode]]', default=>sub{{}});

override 'get_inferred_target_links' => sub {
	my $self = shift;
	my $n = shift;
	my $g = $self->graph;
	my $ig = $self->inferred_graph;

	my $tlinks = $ig->get_outgoing_links($n);
	if (@$tlinks) {
		# cached
		return $tlinks;
	}

	# initialize link set based on input node;
	# we will iteratively extend upwards
	my @links = @{$g->get_outgoing_links($n)};
#	printf STDERR "looking at $n => @links\n";

	my %outlink_h = ();
	#my %link_closure_h = ();

	my $sub_r = sub {};
	if ($self->has_sub)
	{	$sub_r = $self->test_sub;
	}

	while (@links) {
		my $link = shift @links;
		next if $outlink_h{$link};
		$outlink_h{$link} = $link;
#		printf STDERR "looking at $n, $link\n";

		foreach my $srel (@{$self->get_subrelation_closure($link->relation)}) {
			my $newlink = new GOBO::LinkStatement(node=>$link->node,
												 relation=>$srel,
												 target=>$link->target);
			push(@links,$newlink);
		}

		## if the link passes the test, stop!
		if ( &$sub_r( $link ) )
		{	#print STDERR "$link passes the test! NEXTing!\n";
			next;
		}

		# otherwise, check out the parents of this mofo!
		my @x_link_arr;
		my $i;
		## see if we already have any inferred links...
		if (@{$ig->get_outgoing_links($link->target)})
		{	@x_link_arr = @{$ig->get_outgoing_links($link->target)};
			$i++;
		}
		else
		{	
			@x_link_arr = @{$g->get_outgoing_links($link->target)};
		}
		
		while (@x_link_arr)
		{	my $xlink = shift @x_link_arr;
#			print STDERR "  XLINK: $xlink\n";
			foreach my $rel_1 (@{$self->get_subrelation_reflexive_closure( $link->relation )})
			{	my $rel_2 = $xlink->relation;
				my @rels = $self->relation_composition($rel_1, $rel_2);
			
				# R1 subrelation_of R2, x R1 y => x R2 y
				@rels = map { @{$self->get_subrelation_reflexive_closure($_)} } @rels;
				foreach my $rel (@rels) {
					my $newlink = new GOBO::LinkStatement(node=>$link->node,
						relation=> $rel,
						target=> $xlink->target);
					# todo - provenance/evidence of link

					if (! $i )
					{	push(@links, $newlink);
					}
					else
					{	## otherwise, we've already done this node
						$outlink_h{$newlink} = $newlink;
					}
				}
			}
		}
	}
	$ig->add_links([values %outlink_h]);
#	if (values %outlink_h)
#	{	print STDERR "\nTerm: $n; adding links...\n" . join("\n", values %outlink_h) . "\n\n";
#	}
	return [values %outlink_h];
};
#*get_inferred_target_links = &get_inferred_outgoing_links;

around 'get_subrelation_closure' => sub {
	my $method = shift;
	my $self = shift;
	my $cl_h = $self->subrelation_closure_h;

	## see if the result exists already
	if ($cl_h && $cl_h->{ $_[0]->id })
	{	return $cl_h->{ $_[0]->id };
	}
	else
	{	my $results = $self->$method(@_);
		$self->subrelation_closure_h({ $_[0]->id => $results, %$cl_h });
		return $results;
	}
};


around 'get_subrelation_reflexive_closure' => sub {
	my $method = shift;
	my $self = shift;
	my $cl_h = $self->subrelation_reflexive_closure_h;

	## see if the result exists already
	if ($cl_h && $cl_h->{ $_[0]->id })
	{	return $cl_h->{ $_[0]->id };
	}
	else
	{	my $results = $self->$method(@_);
		$self->subrelation_reflexive_closure_h({ $_[0]->id => $results, %$cl_h });
		return $results;
	}
};


1;