package GOBO::Indexes::StatementRefIndex;
use Moose;
use Carp;
use strict;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::RelationNode;
use GOBO::Util::Misc;

#use Sub::Identify;
use Data::Dumper;

#if ($ENV{VERBOSE})
#{
#	around qr/\S+/ => sub {
#		my $orig = shift;
#		my $self = shift;
#		my $m_name = Sub::Identify::sub_name($orig);
#		print STDERR "running StatementRefIndex->" . $m_name . "\n";
#		$self->$orig(@_);
#	};
#}

#has ixN => (is => 'rw', isa => 'HashRef[ArrayRef[GOBO::LinkStatement]]', default=>sub{{}});
#has ixT => (is => 'rw', isa => 'HashRef[ArrayRef[GOBO::LinkStatement]]', default=>sub{{}});

has ixN => (is => 'rw', isa => 'HashRef[ArrayRef[Ref]]', default=>sub{{}});
has ixT => (is => 'rw', isa => 'HashRef[ArrayRef[Ref]]', default=>sub{{}});

sub clear_all_statements {
	my $self = shift;
	$self->ixN({});
	$self->ixT({});
	return;
}

=cut
sub add_statement {
	my $self = shift;
	return $self->add_statements([@_]);
}

sub remove_statement {
	my $self = shift;
	return $self->remove_statements([@_]);
}
=cut

sub add_statements {
	my $self = shift;
	my $sl = shift;

#	print STDERR "sl for add_statements: " . Dumper($sl) . "\n";

	my @ok;
	ADD_LOOP:
	foreach my $sref (@$sl) {
		my $s = $$sref;
		confess("Statement lacks node: $s") && next unless $s->node;

		my $nid = $s->node->id;
		if ($self->ixN->{$nid} && grep { $_ == $sref } @{$self->ixN->{$nid}})
		{	## go on to the next statement if we already have this one
#			print STDERR "Found $sref was in the index already!\n";
			next ADD_LOOP;
		}
		else
		{	push @{$self->ixN->{$nid}}, $sref;
		}
		push @ok, $s;

		next unless $s->can('target') && $s->target;

		push(@{$self->ixT->{$s->target->id}}, $sref);
	}
	return [@ok];
}


sub remove_statements {
	my $self = shift;
	my $sl = shift;

	foreach my $sref (@$sl) {
#		my $s = $$sref;

#print STDERR "looks like this: " . Dumper($sref) . "\n";

		my $s = $sref->{statement};
#		$sref = $sref->{ref};
		confess("Statement lacks node: $s") && next unless $s->node;

		my $nid = $s->node->id;
		next unless $self->ixN->{$nid};

		# TODO - Set::Object?
		my $arr = $self->ixN->{$nid};
		@$arr = grep { $_ != $sref->{ref} } @$arr;
		if (scalar @$arr == 0)
		{	delete $self->ixN->{$nid};
		}

		next unless $s->can('target') && $s->target;

		$arr = $self->ixT->{ $s->target->id };
		@$arr = grep { $_ != $sref->{ref} } @$arr;
		if (scalar @$arr == 0)
		{	delete $self->ixT->{ $s->target->id };
		}
	}
	return;
}


=head2 remove_statements_by_id

Remove statements with matching node->ids or target->ids from the index

 input:  node_id => $n_id OR target_id => $t_id
 output: none

If both node_id and target_id are specified, both will be removed

=cut

sub remove_statements_by_id {
	my $self = shift;
	my %args = (@_);

	if ($args{node_id})
	{	return unless $self->ixN->{$args{node_id}};
		my $t_hash;
		foreach my $s (@{$self->ixN->{$args{node_id}}})
		{	## find the target and save the reference
			if ($$s->target)
			{	push @{$t_hash->{$$s->target->id}}, $s;
			}
			else
			{	push @{$t_hash->{""}}, $s;
			}
		}
		delete $self->ixN->{$args{node_id}};
		foreach my $t (keys %$t_hash)
		{	next unless $self->ixT->{$t};
			foreach my $ref (@{$self->ixT->{$t}})
			{	if (grep { $ref eq $_ } @{$t_hash->{$t}})
				{	undef $ref;
				}
			}
			$self->ixT->{$t} = [ grep { defined $_ } @{$self->ixT->{$t}} ];
			delete $self->ixT->{$t} if scalar @{$self->ixT->{$t}} == 0;
		}
	}

	if ($args{target_id})
	{	return unless $self->ixT->{$args{target_id}};
		my $n_hash;
		foreach my $s (@{$self->ixT->{$args{target_id}}})
		{	## find the node and save the reference
			if ($$s->node)
			{	push @{$n_hash->{$$s->node->id}}, $s;
			}
		}
		delete $self->ixT->{$args{target_id}};
		foreach my $n (keys %$n_hash)
		{	next unless $self->ixN->{$n};
			foreach my $ref (@{$self->ixN->{$n}})
			{	if (grep { $ref eq $_ } @{$n_hash->{$n}})
				{	undef $ref;
				}
			}
			$self->ixN->{$n} = [ grep { defined $_ } @{$self->ixN->{$n}} ];
			delete $self->ixN->{$n} if scalar @{$self->ixN->{$n}} == 0;
		}
	}
}



sub get_all_references {
	my $self = shift;
	return [ map { @$_ } values %{$self->ixN} ];
}

sub get_all_statements {
	my $self = shift;
	return [ map { map { $$_ } @$_ } values %{$self->ixN} ];
}

sub statements_by_node_id {
	my $self = shift;
	my $x = shift;
	confess("requires argument") unless $x;
	return [] unless $self->ixN->{$x} && @{$self->ixN->{$x}};
	return [ map { $$_ }  @{$self->ixN->{$x}} ] || [];
}

sub statements_by_target_id {
	my $self = shift;
	my $x = shift;
	confess("requires argument") unless $x;
	return [] unless $self->ixT->{$x} && @{$self->ixT->{$x}};
	return [ map { $$_ }  @{$self->ixT->{$x}} ] || [];
}

sub statement_node_index {
	my $self = shift;
	return [ keys %{$self->ixN} ] || [];
}

sub statement_target_index {
	my $self = shift;
	return [ keys %{$self->ixT} ] || [];
}

sub referenced_nodes {
	my $self = shift;
	# ixN maps node IDs to lists of statements;
	# take the distinct ixN IDs, then return the
	# node object from the first statement in each
	return [ map { ${$_->[0]}->node } values %{$self->ixN} ];
}


sub referenced_targets {
	my $self = shift;
	# ixT maps target IDs to lists of statements;
	# take the distinct ixT IDs, then return the
	# target object from the first statement in each
	return [ map { ${$_->[0]}->target } values %{$self->ixT} ];
}

## use get_matching_statements for a better fit

sub matching_statements {
	my $self = shift;
	my $s = shift;
	my $sl;
	if ($s->node) {
		$sl = $self->statements_by_node_id($s->node->id);
	}
	elsif ($s->target) {
		$sl = $self->statements_by_target_id($s->target->id);
	}
	else {
		$sl = $self->statements;
	}

	my $rel = $s->relation;
	if ($rel) {
		$sl = [grep {$_->relation && $_->relation->id eq $rel->id} @$sl];
	}
	return $sl;
}



1;


=head1 NAME

GOBO::Indexes::StatementRefIndex

=head1 SYNOPSIS

do not use this method directly

=head1 DESCRIPTION

Stores a collection of references to GOBO::Statement objects (stored in
a GOBO::Indexes::StatementObjectIndex, optimized for fast access. In
general you should not need to use this directly - use GOBO::Graph instead,
which includes different indexes for links, annotations etc

=head2 TODO

Currently there are 2 indexes, by node (subject) and by target. There
are a limited amount of query options.

Eventually it should support any combination of S-R-T indexing, and
any kind of S-R-T access

We also want a NodeIndex

=head2 Binding to a database

This index is in-memory. It can be extended to be bound to a database
(e.g. the GO Database) by overriding the methods

=cut
