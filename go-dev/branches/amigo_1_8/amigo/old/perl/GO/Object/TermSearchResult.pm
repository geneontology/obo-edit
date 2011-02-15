package GO::CGI::TermSearchResult;

use GO::Model::Term;
@ISA = ("GO::Model::Term");
use GO::AppHandle;

use Data::Dumper;

use strict;

sub source {
	my $self = shift;
	if (@_) {
		$self->{source} = shift;
	}
	return $self->{source};
}

sub best_match {
	my $self = shift;
	if (@_) {
		$self->{best_match} = shift;
	}
	return $self->{best_match};
}

sub is_ontology_term {
	my $self = shift;
	if (@_) {
		$self->{is_ontology_term} = shift;
	}
	return $self->{is_ontology_term};
}

sub add_match {
	my $self = shift;
	#	data coming in should be field, original text,
	#	marked up text, relevance score
	my $field = shift;
	my $original = shift;
	my $markedup = shift;
	my $rel = shift || undef;

	if (ref($field) eq 'ARRAY')
	{	my $add = "add_".$field->[0]."_match";
		eval {
			$self->$add($field->[1], $markedup);
			$self->original_name_idx->{$markedup} = $original;
			$self->match_relevance_idx->{$markedup} = $rel if $rel;
		};
		print STDERR "Error with adding match: $@" if $@;
	}
	else
	{	push(@{$self->{$field."_match"}}, $markedup);
		$self->original_name_idx->{$markedup} = $original;
		$self->match_relevance_idx->{$markedup} = $rel if $rel;
	}
}

sub get_match_list {
	my $self = shift;
	my $field = shift;
	my $sortby = shift;

	if ($field eq 'term_synonym')
	{	return $self->get_synonym_match_list($sortby);
	}
	elsif ($field eq 'subset')
	{	return $self->get_subset_match_list($sortby);
	}

	my $matches = $self->{$field."_match"};
	return undef if (!$matches || $@);

	if (scalar @{$matches} == 1)
	{	return $matches;
	}

	my $match_l;
	if ($sortby eq 'rel')
	{	#	sort by relevance score, then original name
		foreach (@$matches)
		{#	print STDERR "list item: ".Dumper($_)."\n";
			my $rel = $self->match_relevance_idx->{ $_ } || $self->match_relevance_idx->{ $_->acc };

			push @$match_l, 
			join("\0", 
				sprintf("%05d", (1 - $self->match_relevance_idx->{ $_ })*10000),
				$self->original_name_idx->{$_},
				$_);
		}
	}
	else
	{	foreach (@$matches)
		{	push @$match_l, $self->original_name_idx->{$_}."\0".$_;
		}
	}

	my @sorted = map { $_ = (split("\0", $_))[-1] } sort @$match_l;
	return [ @sorted ];
}

sub xref_match {
	my $self = shift;
	if (@_) {
		$self->add_xref_match($_) foreach (@_);
	}
	return $self->{xref_match} || undef;
}

sub add_xref_match {
	my $self = shift;
	if (!$self->{xref_match}) {
		$self->{xref_match} = [];
	}
	push @{$self->{xref_match}}, (shift);
	return $self->{xref_match};
}

sub def_xref_match {
	my $self = shift;
	if (@_) {
		$self->add_def_xref_match($_) foreach (@_);
	}
	return $self->{def_xref_match} || undef;
}

sub add_def_xref_match {
	my $self = shift;
	if (!$self->{def_xref_match}) {
		$self->{def_xref_match} = [];
	}
	push @{$self->{def_xref_match}}, (shift);
	return $self->{def_xref_match};
}

sub add_synonym_match {
	my $self = shift;
	#	data coming in should be type, synonym
	my $type = shift || '';
	my $syn = shift;
	push(@{$self->synonyms_by_type_idx->{$type}}, $syn);
}

sub get_synonym_match_list {
	my $self = shift;
	my $sortby = shift;
	
	my $syns;

#	to sort by relevance score, make a big ol' list of syns, then sort by relevance
	if ($sortby eq 'rel')
	{	foreach my $t (@{$self->synonym_type_list})
		{	my $syn_list = $self->synonyms_by_type($t);
			if (@$syn_list)
			{	foreach (@$syn_list)
				{	push @$syns, 
					join("\0", 
						sprintf("%05d", (1 - $self->match_relevance_idx->{$_})*10000),
						$t,
						$self->original_name_idx->{$_},
						$_);
				}
			}
		}
	}
	else
	{	foreach my $t (@{$self->synonym_type_list})
		{	my $syn_list = $self->synonyms_by_type($t);
			if (@$syn_list)
			{	foreach (@$syn_list)
				{	push @$syns, $t."\0".$self->original_name_idx->{$_}."\0".$_;
				}
			}
		}
	}
	return undef if !$syns;
		
	my @sorted = map 
		{	my @arr = split("\0", $_);
#			$_ = { rel => $rel, type => $type, name => $name };
			$_ = { type => $arr[-3], name => $arr[-1] };
		}
		sort @$syns;
		
#	print STDERR "sorted: ".Dumper(\@sorted)."\n";
	return [ @sorted ];
}

sub subset_match {
	my $self = shift;
	if (@_) {
		$self->add_subset_match(@_);
	}
	return $self->{subset_match} || undef;
}

sub add_subset_match {
	my $self = shift;
	my $details = shift;
	my $match = shift;
	my $subset = $self->apph->create_term_obj( $details );

	$self->match_relevance_idx->{ $subset->acc } = $match if $match;

	push(@{$self->{subset_match}}, ($subset));
	return $self->{subset_match};
}

sub get_subset_match_list {
	my $self = shift;
	my $sortby = shift;

	my $subsets = $self->subset_match;
	my %s_ref;
	$s_ref{ $_->acc } = $_ foreach @$subsets;

	my $to_sort;
#	sort by relevance, subset name, subset acc
	if ($sortby eq 'rel')
	{	foreach (@{$subsets})
		{	#print STDERR "list item: ".Dumper($_)."\n";
			push @$to_sort, 
			join("\0", 
				sprintf("%05d", (1 - $self->match_relevance_idx->{$_->acc})*10000),
				$_->name,
				$_->acc);
		}
	}
	else
	{	foreach (@{$subsets})
		{	push @$to_sort, $_->name ."\0".$_->acc;
		}
	}
	return undef if !$to_sort;
		
	my @sorted = map { $s_ref{(split("\0", $_))[-1]} } sort @$to_sort;
	return [ @sorted ];
}

# private: lookup table indexed by string, value is relevance score
sub match_relevance_idx {
	my $self = shift;
	if (@_) {
		$self->{_match_relevance_idx} = shift;
	}
	else {
		$self->{_match_relevance_idx} = {} unless $self->{_match_relevance_idx};
	}
	return $self->{_match_relevance_idx};
}

# private: lookup table indexed by matched string, value is original string
sub original_name_idx {
	my $self = shift;
	if (@_) {
		$self->{_original_name_idx} = shift;
	}
	else {
		$self->{_original_name_idx} = {} unless $self->{_original_name_idx};
	}
	return $self->{_original_name_idx};
}

1;
