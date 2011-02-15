package GO::CGI::GeneProductSearchResult;

use GO::Model::GeneProduct;
@ISA = ("GO::Model::GeneProduct");
#use GO::AppHandle;

use strict;
use Data::Dumper;

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

sub n_terms {
	my $self = shift;
	if (@_) {
		$self->{n_terms} = shift;
	}
	return $self->{n_terms};
}

sub add_match {
	my $self = shift;
	#	data coming in should be field, original text,
	#	marked up text, relevance score
	my $field = shift;
	my $original = shift;
	my $markedup = shift;
	my $rel = shift || undef;

	push(@{$self->{$field."_match"}}, $markedup);
	$self->original_name_idx->{$markedup} = $original;
	$self->match_relevance_idx->{$markedup} = $rel if $rel;
}

sub get_match_list {
	my $self = shift;
	my $field = shift;
	my $sortby = shift;

	my $matches = $self->{$field."_match"};
	return undef if !$matches;

	if (scalar @{$matches} == 1)
	{	return $matches;
	}
	
	my $match_l;
	if ($sortby eq 'rel')
	{	#	sort by relevance score, then original name
		foreach (@$matches)
		{	#my $rel = $self->match_relevance_idx->{ $_ };
			push @$match_l, 
			join("\0", 
				sprintf("%05d", (1 - $self->match_relevance_idx->{ $_ })*10000),
				$self->original_name_idx->{$_},
				$_);
		}
	}
	else
	{	#	sort by original name
		foreach (@$matches)
		{	push @$match_l, $self->original_name_idx->{$_}."\0".$_;
		}
	}

	my @sorted = map { $_ = (split("\0", $_))[-1] } sort @$match_l;
	return [ @sorted ];
}

sub seq_xref_match {
	my $self = shift;
	return $self->{seq_xref_match};
}

sub product_synonym_match {
	my $self = shift;
	return $self->{product_synonym_match};
}

sub seq_name_match {
	my $self = shift;
	return $self->{seq_name_match};
}

sub gpxref {
	my $self = shift;
	return $self->xref->xref_dbname.":".$self->xref->xref_key;
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
