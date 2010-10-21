package GOBO::Indexes::StatementObjectIndex;
use Moose;
use Carp;

use GOBO::Statement;
use GOBO::LinkStatement;
#use GOBO::Node;
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
#		print STDERR "running StatementObjectIndex->" . $m_name . "\n";
#		$self->$orig(@_);
#	};
#}

has ixN_R_T => (is => 'rw', isa => 'HashRef[HashRef[HashRef[ArrayRef[GOBO::Statement]]]]', default=>sub{{}});
#has ixT_R_N => (is => 'rw', isa => 'HashRef[HashRef[HashRef[ArrayRef[GOBO::Statement]]]]', default=>sub{{}});
#has statement_store => ( is=>'rw', isa=> 'ArrayRef[GOBO::Statement]', default=>sub{[]} );

sub get_all_references {
	my $self = shift;
	my @arr;
	foreach my $a (keys %{$self->ixN_R_T})
	{	foreach my $b (keys %{$self->ixN_R_T->{$a}})
		{	map { push @arr, map { \$_ } @$_ } values %{$self->ixN_R_T->{$a}{$b}};
		}
	}
	return [ @arr ];
}

sub get_all_statements {
	my $self = shift;

	my @arr;
	foreach my $a (keys %{$self->ixN_R_T})
	{	foreach my $b (keys %{$self->ixN_R_T->{$a}})
		{	map { push @arr, @$_ } values %{$self->ixN_R_T->{$a}{$b}};
		}
	}
	return [ @arr ];
}


sub clear_all_statements {
	my $self = shift;
	$self->ixN_R_T({});
#	$self->ixT_R_N({});
#	$self->statement_store([]);
	return;
}


=head2 get_statement_refs

 input:  hash of the form
         statements => [ Statement, Statement, ... ]
         add => 1     ## optional; if statements should be added
         remove => 1  ## optional; if statements should be removed

 output: a set of refs for the statements in the input
         if 'remove' is specified, the statements will be removed from the index

=cut

sub get_statement_refs {
	my $self = shift;
	my %args = (@_);

#	print STDERR "args: " . Dumper(\%args) . "\n";

	if (! $args{statements} || ( defined $args{add} && defined $args{remove} ) )
	{	warn "Check arguments for get_statement_refs" && return;
	}

	my @refs;
	my $errs;

	LOOP:
	foreach my $s (@{$args{statements}})
	{	#print STDERR "stt: " . $s . "\n";
		if (! $s->node)
		{	warn "Statement lacks node: $s\nSkipping";
			next;
		}

		if ($s->isa("GOBO::LinkStatement") && ( ! $s->relation || ! $s->target) )
		{	if ($s->isa("GOBO::Annotation") && ! $s->relation && $s->target)
			{	## this is OK...
			}
			else
			{	warn "Link statement lacks relation or target node: $s\nSkipping ";
				next;
			}
		}

		my $nid = $s->node->id;
		my $rid = (defined $s->relation) ? $s->relation->id : '';
		my $tid = ($s->can('target')) ? $s->target->id : '';

		my $ref;
		if ($self->ixN_R_T->{$nid} && $self->ixN_R_T->{$nid}{$rid} && $self->ixN_R_T->{$nid}{$rid}{$tid})
		{	## statement node/relation/target IDs match an existing statement...
			foreach ( @{ $self->ixN_R_T->{$nid}{$rid}{$tid} } )
			{	if ( GOBO::Util::Misc::compare_objects($s, $_) )
				{	## these are probably the same!
					$ref = \$_;
					push @refs, { statement => $s, ref => $ref, node => $nid, target => $tid };

					if ($args{remove})
					{
						undef $_;
						my @arr = grep { defined $_ } @{$self->ixN_R_T->{$nid}{$rid}{$tid}};
						if (@arr )
						{	$self->ixN_R_T->{$nid}{$rid}{$tid} = [ @arr ];
						}
						else
						{	delete $self->ixN_R_T->{$nid}{$rid}{$tid};
							## see if the hash structure needs updating...
							delete $self->ixN_R_T->{$nid}{$rid} if ! values %{$self->ixN_R_T->{$nid}{$rid}};
							delete $self->ixN_R_T->{$nid} if ! values %{$self->ixN_R_T->{$nid}};
						}
					}

#					warn "Link $s already exists" if $ENV{VERBOSE} && $args{add};
					last;
				}
			}
		}

		if (! $ref)
		{	if ($args{add})
			{	push @{$self->ixN_R_T->{$nid}{$rid}{$tid}}, $s;
				$ref = \$self->ixN_R_T->{$nid}{$rid}{$tid}[-1];
				push @refs, { statement => $s, ref => $ref } ;

			}
			else
			{	$errs->{not_found}{$s}++;
			}
		}
	}

	if ($errs && values %$errs && $ENV{VERBOSE})
	{	warn "The following errors were encountered:\nlinks not found\n" . join("; ", keys %{$errs->{not_found}}) . "\n\n";
	}
	return [ @refs ];
}

sub statement_node_index {
	my $self = shift;
	return [ keys %{$self->ixN_R_T} ] || [];
}

sub statement_target_index {
	my $self = shift;
	my $t_ix;
	foreach my $n (keys %{$self->ixN_R_T})
	{	foreach my $r (keys %{$self->ixN_R_T->{$n}})
		{	map { $t_ix->{$_}++ } keys %{$self->ixN_R_T->{$n}{$r}};
		}
	}
	return [ keys %$t_ix ] || [];
}

sub statements_by_node_id {
	my $self = shift;
	my $x = shift;
	confess("requires argument") unless $x;
	my @arr;
	foreach my $a (keys %{$self->ixN_R_T->{$x}})
	{	map { push @arr, @$_ } values %{$self->ixN_R_T->{$x}{$a}};
	}
	return [ @arr ] || [];
}

sub ixN {
	my $self = shift;
	my $ixN;
	foreach my $n (keys %{$self->ixN_R_T})
	{	foreach my $r (keys %{$self->ixN_R_T->{$n}})
		{	foreach my $t (keys %{$self->ixN_R_T->{$n}{$r}})
			{	push @{$ixN->{$n}}, map { \$_ } @{$self->ixN_R_T->{$n}{$r}{$t}};
			}
		}
	}
	return $ixN;
}

sub ixT {
	my $self = shift;
	my $ixT;
	foreach my $n (keys %{$self->ixN_R_T})
	{	foreach my $r (keys %{$self->ixN_R_T->{$n}})
		{	foreach my $t (keys %{$self->ixN_R_T->{$n}{$r}})
			{	push @{$ixT->{$t}}, map { \$_ } @{$self->ixN_R_T->{$n}{$r}{$t}};
			}
		}
	}
	return $ixT;
}

sub statements_by_target_id {
	my $self = shift;
	my $x = shift;
	confess("requires argument") unless $x;
	my @arr;
	foreach my $n (keys %{$self->ixN_R_T})
	{	foreach my $r (keys %{$self->ixN_R_T->{$n}})
		{	if ( $self->ixN_R_T->{$n}{$r}{$x} )
			{	push @arr, @{$self->ixN_R_T->{$n}{$r}{$x}};
			}
		}
	}
	return [@arr] || [];
}


sub remove_statements_by_id {
	my $self = shift;
	my %args = (@_);

	if ($args{node_id})
	{	delete $self->ixN_R_T->{$args{node_id}};
	}

	if ($args{target_id})
	{	## this is a bit more of a nuisance!
		foreach my $n (keys %{$self->ixN_R_T})
		{	foreach my $r (keys %{$self->ixN_R_T->{$n}})
			{	if ( $self->ixN_R_T->{$n}{$r}{ $args{target_id} } )
				{	delete $self->ixN_R_T->{$n}{$r}{ $args{target_id} };
					if (! keys %{$self->ixN_R_T->{$n}{$r}})
					{	delete $self->ixN_R_T->{$n}{$r};
						if (! keys %{$self->ixN_R_T->{$n}})
						{	delete $self->ixN_R_T->{$n};
						}
					}
				}
			}
		}
	}
}



1;


=head1 NAME

GOBO::Indexes::StatementObjectIndex

=head1 SYNOPSIS

do not use this method directly

=head1 DESCRIPTION

Stores a collection of GOBO::Statement objects, optimized for fast
access. In general you should not need to use this directly - use
GOBO::Graph instead, which includes different indexes for links,
annotations etc

=head2 TODO

Currently there is one index, which stores statements in a hash with the keys being
{node_id}{relation_id}{target_id}.

Eventually it should support any combination of N-R-T indexing, and
any kind of N-R-T access

We also want a NodeIndex

=head2 Binding to a database

This index is in-memory. It can be extended to be bound to a database
(e.g. the GO Database) by overriding the methods

=cut
