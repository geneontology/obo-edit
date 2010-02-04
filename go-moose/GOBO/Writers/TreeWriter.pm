package GOBO::Writers::TreeWriter;
use Moose;
use strict;
extends 'GOBO::Writers::Writer';
use Data::Dumper;

has 'show_annotation_counts' => (is=>'rw', isa=>'Bool');
has 'show_gp_names' => (is=>'rw', isa=>'Bool');
has 'relation_abbrev_h' => (is=>'rw', isa=>'HashRef',
	default=>sub{
		return {
		part_of => 'p',
		is_a => 'i',
		has_part => 'hP',
		regulates => 'r',
		negatively_regulates => 'r-',
		positively_regulates => 'r+',
		develops_from => 'd',
		};
	});

has 'header_data' => (is=>'rw', isa=>'ArrayRef[Str]', predicate=>'has_header_data');

sub draw_graph {
	my $self = shift;
	if (! $self->has_fh)
	{	$self->init_fh;
	}
	$self->write_header;
	$self->write_body;
}


sub write_header {
	my $self = shift;
	if ($self->has_header_data)
	{	my $fh = $self->fh;
		map { print $fh "! " . $_ . "\n" } @{$self->header_data};
		print $fh "!\n";
	}
	return;
}


sub write_body {
	my $self = shift;
	my $graph = $self->graph;
	my $fh = $self->fh;

	return if ! defined $graph;

	## get the relations present in the graph and create abbreviations for them
	my $rels = $graph->relations;
	foreach (@$rels)
	{	$self->create_r_abbrev($_->id);
	}

	my $write_term_sub;
	if ($self->show_gp_names)
	{	## set the writer to display association counts
		$write_term_sub = sub {
			my ($g, $t) = (@_);

			my $all_annots = $g->statements_in_ix_by_target_id($self->annotation_ix, $t->id);
			if (! $all_annots)
			{	return $t->id . ( " : " . $t->label || "" ) . " (0 / 0 total)";
			}
			else
			{	my $direct = 0;
				map { $direct++ if ! $_->inferred } @$all_annots;
				my $str = $t->id . ( " : " . $t->label || "" ) . " ($direct / " . (scalar @$all_annots)." total)\n";
				foreach (sort { $a->node->id cmp $b->node->id } @$all_annots)
				{	#$str .= "\t" . $_->node->id . "; ";
					$str .= "\t" . $_->node->data->[2] . ": ";
					if ($_->inferred)
					{	$str .= "INF\n";
					}
					else
					{	$str .= "DIR\n";
					}
				}
				return $str;
			}
		};
	}
	elsif ($self->show_annotation_counts)
	{	## set the writer to display association counts
		$write_term_sub = sub {
			my ($g, $t) = (@_);
			my $all_annots = $g->statements_in_ix_by_target_id($self->annotation_ix, $t->id);
			if (! $all_annots)
			{	return $t->id . ( " : " . $t->label || "" ) . " (0 / 0 total)";
			}
			else
			{	my $direct = 0;
				my $all;
#				map { $direct++ if ! $_->inferred } @$all_annots;


## NEW


		map {
			if ($_->node->isa('GOBO::AnnotDataArray'))
			{
				my $n_gp = scalar @{$_->node->data_arr};
				$all += $n_gp;
				$direct += $n_gp if ! $_->inferred;
			}
			else
			{	$direct++ if ! $_->inferred;
				$all++;
			}
		} @$all_annots;
				


### END NEW


				my $str = $t->id . ( " : " . $t->label || "" ) . " ($direct / $all total)";
				return $str;
			}
		};
	}
	else
	{	## just display the ID and label
		$write_term_sub = sub {
			my ($g, $t) = (@_);
			return $t->id . ( " : " . $t->label || "" );
		};
	}


	##
	foreach my $term (sort { $a->id cmp $b->id } @{$graph->get_roots})
	{	## print the term info
		my $indent = 0;
#		my $str = &$write_term_sub($graph, $term);
		print $fh "" . &$write_term_sub($graph, $term) . "\n";
		my $links = $graph->statements_in_ix_by_target_id($self->ontology_link_ix, $term->id);
		if ($links && @$links)
		{	$self->print_term_array(graph => $graph, links => $links, indent => $indent, write_term_sub => $write_term_sub, terms_on_path => [$term->id]);
		}
	}
	return;
}


sub print_term_array {
	my $self = shift;
	my %args = (@_);
	my $fh = $self->fh;

	my ($graph, $links, $indent, $write_term_sub, $terms_on_path) = ($args{graph}, $args{links}, $args{indent}, $args{write_term_sub}, $args{terms_on_path});
	## start the child printing
	## increase the indent level
	$indent += 3;
	foreach my $l ( sort { $a->node->id cmp $b->node->id || $a->relation->id cmp $b->relation->id } @$links)
	{	## print out the relationship and the term info
		my $rel = $self->r_abbrev( $l->relation->id );

		printf $fh "%${indent}s", " ";
		print $fh "[$rel] ". &$write_term_sub($graph, $l->node) . "\n";

		## make sure we're not going to be printing out a horrible cycle...
		if (grep { $l->node->id eq $_ } @$terms_on_path)
		{	warn "Not printing out further children of " . $l->node->id . " as it would create a cycle";
			next;
		}
		## see if this term has any incoming links
		my $inlinks = $graph->statements_in_ix_by_target_id($self->ontology_link_ix, $l->node->id);
		if ($inlinks && @$inlinks)
		{	$self->print_term_array(graph => $graph, links => $inlinks, indent => $indent, write_term_sub => $write_term_sub, terms_on_path => [ @$terms_on_path, $l->node->id ]);
		}
	}
	## decrease the indent now that we've finished printing them
	$indent -= 3;
};

sub r_abbrev {
	my $self = shift;
	my $r = shift;
	my $rel_h = $self->relation_abbrev_h;
	return $rel_h->{$r} || $self->create_r_abbrev($r);
}

sub create_r_abbrev {
	my $self = shift;
	my $r = shift;
	my $rel_h = $self->relation_abbrev_h;

	if (! $rel_h->{$r})
	{	#print STDERR "Looking at $r...\n" if $ENV{VERBOSE};
		my $orig_r = $r;
		$r =~ s/negative(ly)?(.*)/$2-/;
		$r =~ s/positive(ly)?(.*)/$2+/;
		
		## replace all the words with their first letters
		## remove the underscores
		$r =~ s/([a-z])[a-z]+(_|$)/$1$2/gi;
		$r =~ s/_//g;

		print STDERR "Trying $r\n" if $ENV{VERBOSE};

		## see if this exists...
		if (grep { $_ eq $r } values %$rel_h)
		{	## crap... let's try something else
			#print STDERR "Oh no, that didn't work! Using full name instead\n" if $ENV{VERBOSE};
			$r = $orig_r;
		}

		## we're ok! add this to the hash
		$rel_h->{$orig_r} = $r;
		## save the new version of the relation_abbrev_h
		$self->relation_abbrev_h($rel_h);
	}
	return $rel_h->{$r}
}

1;
