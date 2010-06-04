package AmiGO::Worker::Phylotree;
use warnings;
use strict;
use Data::Dumper;

=head1 NAME

AmiGO::Worker::Phylotree - Object to abstract phylotree access

=head1 DESCRIPTION

AmiGO::Worker::Phylotree is a blessed hash.  Several keys have values
that are meant to be accessed directly.  They are:

=over

=item dbname

This value analogous to the C<dbxref.xref_dbname> column.  If this is
not set all one can really do is get a list of
AmiGO::Worker::Phylotree objects where this is set.

=item key

This value analogous to the C<dbxref.xref_key> column.  This, in
combination I<dbname>, is how to identify a specific row in the
C<phylotree> table.

=item number_of_members

The number of members related to the C<phylotree> row.

=item number_of_refg_members

Number of members that are is a reference genome.

=back

=cut

use AmiGO::JavaScript;
use AmiGO::Aid::PantherDB qw/@species/;
use GOBO::DBIC::GODBModel::Query;

our $phylotree_gobo;
our $core;

sub BEGIN {
    #$ENV{DBIC_TRACE}=1;
    $core = AmiGO::JavaScript->new();
    $phylotree_gobo = GOBO::DBIC::GODBModel::Query->new({type=>'phylotree'});
}

=head2 Call From Object Name

=over

=item  AmiGO::Worker::Phylotree->new(I<key> => I<value>...);

Create a new AmiGO::Worker::Phylotree object.  If options are passed
it it will use that to create the hash, otherwise it will create an
empty one.

=cut

sub new{
    my $c = shift;
    my $s = scalar(@_) ? { @_ } : {};

    $s->{verbose} = '1';

    return bless $s, $c;
}

=back

=head2 Functions From Empty Objects

These functions need neither I<dbname> or I<key> to be set.

=over

=item $p->sets()

This fetches a distinct list of C<dbxref.xref_dbname> that have rows
in the C<phylotree> table.  Then returns a list of
AmiGO::Worker::Phylotree objects with I<dbname> set to each
C<dbxref.xref_dbname> value.

=cut
sub sets{
    my $s = shift;
    my $r = $phylotree_gobo->get_all_results
      ({},{ select => 'dbxref.xref_dbname', distinct => 1 });

    return map {
	my %new = %$s;
	$new{dbname} = $_->dbxref->xref_dbname;
	__PACKAGE__->new(%new);
    } @$r;
}

=item $p->url()

By default returns a relative URL that can be use to link to this
object from within AmiGO.  If you add arguments it will treat that as
a hash that it will use as query items for the returned URL.

=cut
sub url{
    my $s = shift;
    my %arg = @_;

    if ($s->{dbname}) {
	$arg{dbname} = $s->{dbname}   if (!$arg{dbname});
	if ($s->{key}) {
	    $arg{key}    = $s->{key}  if (!$arg{key});
	    $arg{mode}   = 'cluster'  if (!$arg{mode});
	} else {
	    $arg{mode}   = 'index'    if (!$arg{mode});
	}
    }

    return $core->get_interlink
      ({ mode => 'phylotree', arg => \%arg });
}


=back

=head2 Functions that need I<dbname> name to be set

=over

=item $p->set()

For every C<dbxref.xref_dbname> that equals I<dbname> and is joined to
the C<phylotree> table it will return a C<AmiGO::Worker::Phylotree>
object where I<key> is the C<dbxref.xref_key> value.

In the process it sets I<number_of_members>, and I<last_annotated> to there
values.

=cut
sub set{
    my $s = shift;
    my $rows = shift;
    my $page = shift;

    my $r = $phylotree_gobo->get_all_results
      ({ xref_dbname => $s->{dbname} },
       { select => 'me.id',
	 order_by => 'xref_key',
	 rows => $rows,
	 page => $page,
       });
    return $s->_index_display(map { $_->id } @$r);
}

sub show{
    my $s = shift;

    my $dbxref_gobo = GOBO::DBIC::GODBModel::Query->new({type=>'dbxref'});
    my $r = $dbxref_gobo->get_all_results({ xref_key => \@_ });
    my @dbxref = map {
	map {
	    $_->phylotree_id;
	} $_->gene_product->gene_product_phylotree->all()
    } @$r;

    $r = GOBO::DBIC::GODBModel::Query->new({type=>'seq'})->get_all_results
      ({ display_id => \@_ });
    my @seq = map {
    	map {
    	    $_->phylotree_id;
    	} $_->gene_product_seq->gene_product->gene_product_phylotree->all();
    } @$r;

    my %ids;
    for (@dbxref, @seq) {
	$ids{$_} = '1';
    }

    return $s->_index_display(keys %ids);
}

# Set this a list of phylotree ids and it will return a list of
# objects suitable for mod_cluster_index
sub _index_display{
    my $s = shift;

    return () if (0 == scalar(@_));

    my $r = GOBO::DBIC::GODBModel::Query->new({type=>'phylotree_lazy'})->get_all_results
      ({
	phylotree_id  => \@_,
	xref_dbname  => [ $s->{dbname} ],
       },
       {
	join =>
	 [
	  'dbxref',
	  { gene_product_phylotree => 'association' },
	 ],
	 select =>
	 [
	  'xref_key',
	  { count => 'DISTINCT gene_product_phylotree.gene_product_id' },
	  { max => 'association.assocdate' },
	 ],
	 as =>
	 [
	  'xref_key',
	  'members',
	  'last_anno',
	 ],
	group_by => 'xref_key', # Not needed in Lucid
	order_id => 'xref_key',
       });

    return map {
	my %new = %$s;
	$new{key}               = $_->get_column('xref_key');
	$new{number_of_members} = $_->get_column('members');
	$new{last_annotated}    = $_->get_column('last_anno');
	my $out = __PACKAGE__->new(%new);
	$out->{dist} = $out->url(mode => 'dist', ref => 1, diameter => 50);
	$out;
    } @$r;
}


=item last_annotated()

The C<MAX(association.assocdate)> value for all the members related to
the C<phylotree> row printed in a human readable format.

=cut
sub last_annotated{
    my $s = shift;

    if ($s->{last_annotated}) {
	if ($s->{last_annotated} =~ m/^(\d{4})(\d{2})(\d{2})$/) {
	    return "$1-$2-$3";
	}
	return 'Unknown Format';
    }
    return 'Never';
}



=back

=head2 When both C<dbname> and C<key> are set

=over

=item $p->title

Returns S<I<dbname>:I<key>>.

=cut

sub title{
    my $p = shift;
    return $p->{dbname} . ':' . $p->{key};
}

=item $p->gene_products()

Returns a list of hash references fore each C<gene_product> matched to
the C<phylotree> row.  Plus sets I<last_annotated>,
I<number_of_members>, and I<number_of_refg_members> is the active
object.

The hash references in the returned list will have the following key set:

=over

=item symbol

From C<gene_prodect.symbol> column.

=item species

The C<species.genus> and C<species.species> values put together with a
space between them.

=item color

Species that are reference genomes will also have a color set of the
C<#I<RRGGBB>> format.

=back

=cut
our @codes = qw/EXP IDA IMP IGI IEP IPI/;
sub gene_products{
    my $s = shift;

    my $r = $phylotree_gobo->get_all_results
      ({ xref_dbname => $s->{dbname}, xref_key => $s->{key} });
    if (length(@$r) != 1) {
	die 'I should of gotten one cluster';
    }
    $r = $r->[0];

    $s->{last_annotated} = $r->associations->get_column('assocdate')->max();
    $s->{exp} = $r->associations
      ({ code => \@codes },
       { prefetch => 'evidence' })->count();

    my @gp; # gene products
    for my $gp ($r->gene_products
		({},
		 { prefetch => [ 'species', 'dbxref' ],
		   order_by => [ 'genus', 'species' ] })->all) {
	my $species = $gp->species;
	## Get together to add a link too.
	my $dbname = $gp->dbxref->xref_dbname;
	my $dbkey = $gp->dbxref->xref_key;
	my $acc = $dbname . ':' . $dbkey;
	my %gp =
	  (
	   link => $core->get_interlink({ mode => 'gp-details',
					  arg => {gp=>$acc}}),
	   symbol  => $gp->symbol,
	   species => $species->genus . ' ' . $species->species,
	   dbxref  => $acc,
	  );
	my $aid = AmiGO::Aid::PantherDB->ncbi($species->ncbi_taxa_id);
	$gp{color} = $aid->color;
	$s->{number_of_refg_members}++ if ($gp{color});

	push @gp, \%gp;
    }
    $s->{number_of_members} = scalar(@gp);
    return @gp;
}


sub species_dist{
    my $s = shift;
    my $ref = shift;

    my $r = $phylotree_gobo->get_all_results
      ({ xref_dbname => $s->{dbname}, xref_key => $s->{key} });
    if (length(@$r) != 1) {
	die 'I should of gotten one cluster';
    }
    $r = $r->[0];
    my @gs = qw/genus species/;

    my @results = $r->gene_products
      ({},
       {
	join     => 'species',
	select   => [ @gs, 'ncbi_taxa_id', { count => 'gene_product.id' } ],
	as       => [ @gs, 'ncbi', 'count' ],
	group_by => \@gs,
	order_by => \@gs,
       });

    my @out = map {
	my $ncbi = $_->get_column('ncbi');
	my $aid = AmiGO::Aid::PantherDB->ncbi($ncbi);
	my $species = $_->get_column('species');
	{
	    ncbi => $ncbi,
	    genus => $_->get_column('genus'),
	    species => $species,
	    color => $aid->color,
	    count => $_->get_column('count'),
	};
    } @results;

    if ($s->{dbname} eq 'PantherDB') {

	# First, combine items AmiGO::Aid::Pantherdb says are the same.
	my %out;
	@out = map {
	    my $species = AmiGO::Aid::PantherDB->ncbi($_->{ncbi});
	    my $code = $species->{code};
	    if ($out{$code}) {
		$out{$code}->{count} += $_->{count};
		#$out{$code}->{color} = $_->{color} if ($_->{color});
		();
	    } else {
		$_->{code} = $code;
		$out{$code} = $_;
		($_);
	    }
	} @out;

	# now we add missing items.
	my @copy = qw/code common/;
	@out = map {
	    my $species = $_;
	    my $code = $species->{code};
	    my $aid = AmiGO::Aid::PantherDB->code($code);
	    if (exists $out{$code}) {
		($out{$code});
	    } else {
		my %o =
		  (
		   color => $aid->color,
		   count => 0,
		  );

		for my $copy (@copy) {
		    $o{$copy} = $species->{$copy} if ($species->{$copy});
		}
		\%o;
	    }
	} @species;
    }

    if ($ref) {
	@out = map {
	    $_->{color} ? ($_) : ();
	} @out;
    }


    return map {
	$_->{display} = [ $_->{common} || $_->{code} || ($_->{genus}, $_->{species}) ];
	$_;
    } @out;
    return @out;
}

=back

=cut

1;

