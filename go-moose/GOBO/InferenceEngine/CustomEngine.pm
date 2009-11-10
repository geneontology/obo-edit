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
	
	my %outlink_h = ();
	my $sub_r = sub {};
	if ($self->has_sub)
	{	$sub_r = $self->test_sub;
	}

	while (@links) {
		my $link = shift @links;
		next if $outlink_h{$link};
		$outlink_h{$link} = $link;

		foreach my $srel (@{$self->get_subrelation_closure($link->relation)}) {
			my $newlink = new GOBO::LinkStatement(
				node=>$link->node, relation=>$srel, target=>$link->target);
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
			my $combined = $self->_combine_links($link, $xlink);
			if ($combined && @$combined)
			{	if (! $i )
				{	push(@links, @$combined);
				}
				else
				{	## otherwise, we've already done this node
					$outlink_h{$_} = $_ foreach @$combined;
				}
			}

=cut
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

					if ($rel_1->id =~ /(regulates|part_of)/ && $rel_2->id =~ /(regulates|part_of)/)
					{	#print STDERR "rel 1: $rel_1; rel_2 : $rel_2; result: " . join(", ", @rels) . "\n";
						print STDERR "making X link:\n$link + $xlink => \n$newlink\n";
					}

					if (! $i )
					{	push(@links, $newlink);
					}
					else
					{	## otherwise, we've already done this node
						$outlink_h{$newlink} = $newlink;
					}
				}
			}
=cut
		}
	}
	$ig->add_links([values %outlink_h]);
#	if (values %outlink_h)
#	{	print STDERR "\nTerm: $n; adding links...\n" . join("\n", values %outlink_h) . "\n\n";
#	}
	return [values %outlink_h];
};



#*get_inferred_outgoing_links = &get_inferred_target_links;

=head2 get_subrelation_closure

As that in the standard InferenceEngine, but saves the results

=cut

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

=head2 get_subrelation_reflexive_closure

As that in the standard InferenceEngine, but saves the results

=cut

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



sub get_inferred_incoming_links {
	my $self = shift;
	my $n = shift;
	my $g = $self->graph;
	my $ig = $self->inferred_graph;

	my $tlinks = $ig->get_incoming_links($n);
	if (@$tlinks) {
		# cached
		return $tlinks;
	}

	# initialize link set based on input node;
	# we will iteratively extend upwards
	my @links = @{$g->get_incoming_links($n)};
#	printf STDERR "looking at $n => @links\n";

	my %inlink_h = ();

	my $sub_r = sub {};
	if ($self->has_sub)
	{	$sub_r = $self->test_sub;
	}

	while (@links) {
		my $link = shift @links;
		next if $inlink_h{$link};
		$inlink_h{$link} = $link;

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

		# otherwise, check out the children of this mofo!
		my @x_link_arr;
		my $i;
#		## see if we already have any inferred links...
		if (@{$ig->get_incoming_links($link->node)})
		{	@x_link_arr = @{$ig->get_incoming_links($link->node)};
			$i++;
		}
		else
		{	@x_link_arr = @{$g->get_incoming_links($link->node)};
		}
		
		while (@x_link_arr)
		{	my $xlink = shift @x_link_arr;
			my $combined = $self->_combine_links($xlink, $link);
			if ($combined && @$combined)
			{	if (! $i )
				{	push(@links, @$combined);
				}
				else
				{	## otherwise, we've already done this node
					$inlink_h{$_} = $_ foreach @$combined;
				}
			}
		}
	}
	$ig->add_links([values %inlink_h]);
	return [values %inlink_h];
}


=cut

combining two links

link_a = X -- rel_a --> Y

link_b = Y -- rel_b --> Z

X -- rel_a --> Y -- rel_b --> Z

=>  X -- rel_x --> Z

=cut

sub _combine_links {
	my $self = shift;
	my ($link_a, $link_b) = (@_);

	my $links;
	foreach my $rel_a (@{$self->get_subrelation_reflexive_closure($link_a->relation)} )
	{	#printf STDERR "  XLINK: $xlink\n";
		my $rel_b = $link_b->relation;
		my @rels = $self->relation_composition($rel_a, $rel_b);
		#            print STDERR "result: " . join("\n", @rels) . "\n\n";

		# R1 subrelation_of R2, x R1 y => x R2 y
		@rels = map { @{$self->get_subrelation_reflexive_closure($_)} } @rels;
		foreach my $rel (@rels) {
			my $newlink = new GOBO::LinkStatement(node => $link_a->node, relation => $rel, target => $link_b->target);
			# todo - provenance/evidence of link
			push(@$links, $newlink);
		}
	}
	return $links;
}


sub get_inferred_graph {
	my $self = shift;
	my $g = $self->graph;
	my $ig = $self->inferred_graph;

	$g->update_graph;

	return unless $g->has_links;
	my $sub_r = sub {};
	if ($self->has_sub)
	{	$sub_r = $self->test_sub;
	}

	my $leaves;
	foreach (keys %{$g->link_ix->ixN})
	{	next if $g->link_ix->ixT->{$_};
		$leaves->{$_}++;
	}

	my $t_hash;
	my $matrix;
	my $acc = 1;
	foreach (@{$g->terms})
	{	$t_hash->{by_acc}{$acc} = $_;
		$t_hash->{by_id}{$_->id} = $acc;
#		$matrix->[$acc][$acc]{is_a} = 1;
		$acc++;
	}

	my $rel_h;
	foreach (@{$g->links})
	{	foreach my $srel (@{$self->get_subrelation_closure($_->relation)}) {
			$matrix->[ $t_hash->{by_id}{$_->node->id} ][ $t_hash->{by_id}{$_->target->id} ]{ $srel->id } = 1;
			$rel_h->{$srel->id} = $srel;
		}
		$matrix->[ $t_hash->{by_id}{$_->node->id} ][ $t_hash->{by_id}{$_->target->id} ]{ $_->relation->id } = 1;
		$rel_h->{$_->relation->id} = $_->relation;
	}

#	print STDERR "matrix: " . Dumper($matrix);

	for (my $k = 1; $k <= $acc; $k++)
	{	for (my $i = 1; $i <= $acc; $i++)
		{	for (my $j = 1; $j <= $acc; $j++)
			{	if (defined $matrix->[$i][$k] && defined $matrix->[$k][$j])
				{	## combine the links and save
					foreach my $rx (keys %{$matrix->[$i][$k]})
					{	##
						foreach my $r1 (@{$self->get_subrelation_reflexive_closure($rel_h->{$rx})})
						{	foreach my $r2 (keys %{$matrix->[$k][$j]})
							{	my @rels = $self->relation_composition($rel_h->{$r1}, $rel_h->{$r2});
								@rels = map { @{$self->get_subrelation_reflexive_closure($_)} } @rels;
								foreach my $rel (@rels) {
									$matrix->[$i][$j]{ $rel->id } = 1;
									$rel_h->{$rel->id} = $rel if ! $rel_h->{$rel->id};
								}
							}
						}
					}
				}
			}
		}
	}

	my @links;
	for (my $i = 1; $i <= $acc; $i++)
	{	for (my $j = 1; $j <= $acc; $j++)
		{	next unless $matrix->[$i][$j];
			foreach (keys %{$matrix->[$i][$j]})
			{	push @links, new GOBO::LinkStatement(node=> $t_hash->{by_acc}{$i},
												relation=> $rel_h->{$_},
												target=> $t_hash->{by_acc}{$j});
			}
		}
	}

	$ig->add_links( \@links );
	return $ig;

=cut
	foreach my $k (@$matrix)
	{	foreach my $i (@$matrix)
		{	foreach my $j (@$matrix)
			{	if ($matrix->[$i][$k] && $matrix->[$k][$j])
				{	## combine the links and save
					foreach my $rx (keys %{$matrix->[$i][$k]})
					{	##
						foreach my $r1 (@{$self->get_subrelation_reflexive_closure($rel_h->{$rx})})
						{	foreach my $r2 (keys %{$matrix->[$k][$j]});
							{	my @rels = $self->relation_composition($rel_h->{$r1}, $rel_h->{$r2});
								@rels = map { @{$self->get_subrelation_reflexive_closure($_)} } @rels;
								foreach my $rel (@rels) {
									$matrix->[$i][$j]{ $rel->id } = 1;
									$rel_h->{$rel->id} = $rel if ! $rel_h->{$rel->id};
								}
							}
						}
					}
				}
			}
		}
	}
=cut

}

1;