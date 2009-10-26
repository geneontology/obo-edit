package GOBO::Writers::TreeWriter;
use Moose;
use strict;
extends 'GOBO::Writers::Writer';
use Data::Dumper;

has 'show_annotation_counts' => (is=>'rw', isa=>'Bool');
has 'relation_h' => (is=>'rw', isa=>'HashRef',
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

sub write_tree {
	my $self = shift;
	my $graph = shift || $self->graph;

	if ($self->file)
	{	## open the file for some writing
		open(STDOUT, ">" . $self->file) or die "Could not open " . $self->file . ": $!";
	}

	## get the relations present in the graph and create abbreviations for them
	my $rels = $graph->relations;
	foreach (@$rels)
	{	$self->create_r_abbrev($_->id);
	}

	print STDERR "Finished creating abbreviations: rel_h now:\n".Dumper($self->relation_h);

	my $write_term_sub;
	if ($self->show_annotation_counts)
	{	## set the writer to display association counts
		print STDERR "setting show_annotation_counts!\n";
		$write_term_sub = sub {
			my ($g, $t) = (@_);
			my $annots = $g->get_annotations_by_target($t);
			if (! $annots)
			{	return $t->id . ( " : " . $t->label || "" ) . " (0 / 0 total)";
			}
			else
			{	my $direct = scalar ( grep { ! $_->inferred } @$annots ) || 0;
				return $t->id . ( " : " . $t->label || "" ) . " ($direct / " . (scalar @$annots)." total)";
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
	my $indent = 0;
	foreach my $term (sort { $a->id cmp $b->id } @{$graph->get_roots})
	{	## print the term info
		print &$write_term_sub($graph, $term) . "\n";
		my $links = $graph->get_incoming_links($term);
		if ($links && @$links)
		{	$self->print_term_array(graph => $graph, links => $links, indent => $indent, write_term_sub => $write_term_sub, terms_on_path => [$term->id]);
		}
	}
}


sub print_term_array {
	my $self = shift;
	my %args = (@_);
	
	my ($graph, $links, $indent, $write_term_sub, $terms_on_path) = ($args{graph}, $args{links}, $args{indent}, $args{write_term_sub}, $args{terms_on_path});
	## start the child printing
	## increase the indent level
	$indent += 3;
	foreach my $l ( sort { $a->target->id cmp $b->target->id } @$links)
	{	## print out the relationship and the term info
		my $rel = $self->r_abbrev( $l->relation->id );

		printf "%${indent}s", " ";
		print "[$rel] ". &$write_term_sub($graph, $l->node) . "\n";

		## make sure we're not going to be printing out a horrible cycle...
		if (grep { $l->node->id eq $_ } @$terms_on_path)
		{	warn "Not printing out further children of " . $l->node->id . " as it would create a cycle";
			next;
		}
		## see if this term has any incoming links
		my $inlinks = $graph->get_incoming_links($l->node);
		if ($inlinks && @$inlinks)
		{	$self->print_term_array(graph => $graph, links => $inlinks, indent => $indent, write_term_sub => $write_term_sub, terms_on_path => [ @$terms_on_path, $l->node->id ]);
		}
	}
	## decrease the indent now that we've finished printing them
	$indent -= 3;
}

sub r_abbrev {
	my $self = shift;
	my $r = shift;
	my $rel_h = $self->relation_h;
	return $rel_h->{$r} || $self->create_r_abbrev($r);
}

sub create_r_abbrev {
	my $self = shift;
	my $r = shift;
	my $rel_h = $self->relation_h;

	if (! $rel_h->{$r})
	{	print STDERR "Looking at $r...\n" if $ENV{VERBOSE};
		my $orig_r = $r;
		$r =~ s/negative(ly)?/-/;
		$r =~ s/positive(ly)?/+/;
		## replace all the words with their first letters
		## remove the underscores
		$r =~ s/([a-z])[a-z]+(_|$)/$1$2/gi;
		$r =~ s/_//g;

		print STDERR "Trying $r\n" if $ENV{VERBOSE};

		## see if this exists...
		if (grep { $_ eq $r } values %$rel_h)
		{	## crap... let's try something else
			print STDERR "Oh no, that didn't work! Using full name instead\n" if $ENV{VERBOSE};
			$r = $orig_r;
		}
		
		## we're ok! add this to the hash
		$rel_h->{$orig_r} = $r;
		## save the new version of the relation_h
		$self->relation_h($rel_h);
	}
	return $rel_h->{$r}
}

1;
