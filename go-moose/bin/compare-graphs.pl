#!/usr/bin/perl -w

=head1 NAME

compare-graphs.pl - compare two graphs

=head1 SYNOPSIS

 compare-graphs.pl --file_1 old_gene_ontology.obo --file_2 gene_ontology.obo
 -s goslim_generic -o results.txt

=head1 DESCRIPTION

Compares two OBO files and records the differences between them, including:

* new terms

* term merges

* term obsoletions

* changes to term content, such as addition, removal or editing of features like
synonyms, xrefs, comments, def, etc..

* term movements into or out of the subset designated by the subset option

At present, only term differences are recorded in detail, although this could
be extended to other stanza types in an ontology file. The comparison is based
on creating hashes of term stanza data, mainly because hashes are more tractable
than objects.

=head2 Input parameters

=head3 Required

=over

=item -f1 || --file_1 /path/to/file_name

"old" ontology file

=item -f2 || --file_2 /path/to/file_2_name

"new" ontology file

=item -s || --subset I<subset_name>

Subset to use for graph-based comparisons

=item -o || --output /path/to/file_name

output file for results

=back

=head3 Optional switches

=over

=item -v || --verbose

prints various messages

=item -f || --format I<html>

format the output as html (the default is a plain text file)

=back

=cut

use strict;
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use GOBO::Graph;
use GOBO::Parsers::OBOParserDispatchHash;
use GOBO::InferenceEngine;
use GOBO::Util::GraphFunctions;
#use Data::Compare;
use Template;
use Storable;

my @ordered_attribs = qw(id
is_anonymous
name
namespace
alt_id
def
comment
subset
synonym
xref
is_a
intersection_of
union_of
disjoint_from
relationship
is_obsolete
replaced_by
consider);

my @single_attribs = qw(name namespace is_obsolete def comment is_anonymous );

run_script(\@ARGV);

exit(0);

sub run_script {

my $options = parse_options(@_);

# check verbosity
if (! defined $options->{verbose})
{	$options->{verbose} = $ENV{GO_VERBOSE} || 0;
}

my $tt = Template->new({
    INCLUDE_PATH => 'web/templates',
}) || die "$Template::ERROR\n";

my $data;
my $output;
my $parser;
my $ss = $options->{subset};
my @tags_to_parse = qw(name is_a relationship subset);
my $regex = '^(' . join("|", @tags_to_parse) . '):\s*';
$regex = qr/$regex/;

=cut for now
if (-e 'stored_frozen_data.txt')
{	print STDERR "data was stored: retrieving! :D\n";
	my $temp = Storable::retrieve('stored_comp_data.txt');
	$data = Storable::thaw $temp;
	print STDERR "Got stored data.\n";
}
elsif (-e 'stored_comp_data.txt')
{	print STDERR "data was stored: retrieving! :D\n";
	$data = Storable::retrieve('stored_comp_data.txt');
	print STDERR "Got stored data.\n";
}
else {
}
=cut

#use Time::HiRes qw(gettimeofday);
#my $start_time = gettimeofday;

foreach my $f ('f1', 'f2')
{
	## let's quickly get the ontology data and do a big ass comparison that way
	local $/ = "\n[";
	$parser = new GOBO::Parsers::OBOParserDispatchHash;

#	print STDERR "Ready to read in $f!\n";
	open(FH, "<" . $options->{$f}) or die("Could not open " . $options->{$f} . "! $!");

	# remove and parse the header
	my @arr = split("\n", <FH> );
	$data->{$f}{header} = tag_val_arr_to_hash( \@arr );
	$data->{$f}{graph_header} = $parser->parse_header_from_array( array => [@arr] );

#	print STDERR "graph header: " . Dumper($data->{$f}{graph_header}) . "\n\n";

	print STDERR "Parsed $f header; starting body\n" if $options->{verbose};
	my @lines;
	while (<FH>)
	{	if (/^(\S+)\]\s*.*?^id:\s*(\S+)/sm)
		{	# store the data as a tag-value hash indexed by stanza type and id
			# data->{$file}{$stanza_type}{$stanza_id}
			$data->{$f . "_hash"}{$1}{$2} = block_to_hash( $_ );

			# save alt_ids
			if ($1 eq 'Term' && $data->{$f . "_hash"}{Term}{$2}{alt_id})
			{	# check for dodgy alt ids...

				map {
					if ($data->{$f . "_alt_ids"}{$_} )
					{	warn "$2: alt_id $_ is already assigned to " . $data->{$f . "_alt_ids"}{$_} if $options->{verbose};
					}
					else
					{	$data->{$f . "_alt_ids"}{$_} = $2;
					}
				} @{$data->{$f . "_hash"}{Term}{$2}{alt_id}};
			}

			# extract the interesting data
			# skip obsoletes
			if ($1 eq 'Term')
			{	## get stuff for stats
				$data->{$f . "_stats"}{total}++;
				
				if ($data->{$f."_hash"}{Term}{$2}{is_obsolete})
				{	$data->{$f."_stats"}{obs}++;
				}
				else
				{	## get the term's namespace...
					my $ns = 'unknown';
					if ($data->{$f."_hash"}{Term}{$2}{namespace})
					{	$ns = $data->{$f."_hash"}{Term}{$2}{namespace}[0];
					}
					else
					{	if ($parser->default_namespace)
						{	#print STDERR "default_namespace: " . $parser->default_namespace . "\n";
							$ns = $parser->default_namespace;
						}
					}
					$data->{$f . "_stats"}{by_ns}{$ns}{total}++;
					if ($data->{$f."_hash"}{Term}{$2}{def})
					{	$data->{$f."_stats"}{by_ns}{$ns}{def}++;
						$data->{$f."_stats"}{def_not_obs}++;
					}
				}
			}
			next if $data->{$f . "_hash"}{$1}{$2}{is_obsolete};

			if ($1 eq 'Term')
			{	push @lines, ( "[$1]", "id: $2");
				foreach my $tag (@tags_to_parse)
				{	if ($data->{$f . "_hash"}{$1}{$2}{$tag})
					{	map { push @lines, $tag . ": " . $_ } @{$data->{$f . "_hash"}{$1}{$2}{$tag}};
					}
				}
			}
			elsif ($1 eq 'Typedef')
			{	push @lines, ("[$1]", "id: $2");
				foreach my $tag (keys %{$data->{$f . "_hash"}{$1}{$2}})
				{	map { push @lines, $tag . ": " . $_; } @{$data->{$f . "_hash"}{$1}{$2}{$tag}};
				}
			}
		}
		else
		{	print STDERR "Couldn't understand data!\n";
		}
	}
	$data->{$f}{graph} = $parser->parse_body_from_array( array => [ @lines ] ) if $ss;
	print STDERR "Finished parsing $f body\n" if $options->{verbose};
	close FH;

	if ($ss)
	{	my $results = GOBO::Util::GraphFunctions::get_subset_nodes(graph=>$data->{$f}{graph}, options => { subset => $ss } );
		if ($results->{subset}{ $ss })
		{	$data->{$f}{subset_ids} = [ keys %{$results->{subset}{$ss} } ];
		}
		else
		{	die "No terms could be found in the specified subset";
		}
	}

	my $ie = new GOBO::InferenceEngine::CustomEngine( graph => $data->{$f}{graph} );
	$ie->slim_graph( subset_ids => $data->{$f}{subset_ids}, input_ids => [ map { $_->id } @{$data->{$f}{graph}->terms} ], from_ix => 'ontology_links', save_ix => 'trimmed', options => { return_as_graph => 1 } );
	$data->{$f}{trimmed} = $ie->graph;

}

#my $end_time = gettimeofday;
#print STDERR "took " . ($end_time - $start_time) . " secs to complete\n";

## ANALYSIS STAGE! ##

# ignore these tags when we're comparing hashes
my @tags_to_ignore = qw(id is_a relationship);
my $ignore_regex = '(' . join("|", @tags_to_ignore) . ')';
$ignore_regex = qr/$ignore_regex/;


## ok, check through the terms and compare 'em
## go through all the terms in f1 and add them to the stats

foreach my $t (keys %{$data->{f1_hash}{Term}})
{
	## check for term in f2
	## see if it is an alt ID (i.e. it has been merged)
	## if not, it may have been lost
	if (! $data->{f2_hash}{Term}{$t})
	{	# check it hasn't been merged
		if ($data->{f2_alt_ids}{$t})
		{
			# the term was merged. N'mind!
			$output->{f1_to_f2_merge}{$t} = $data->{f2_alt_ids}{$t};
			## make sure we have the data about the term in f1 and f2
			get_term_data( data => $data, output => $output, term => $data->{f2_alt_ids}{$t}, data_to_get => [ qw( name namespace) ], f_data => 'f2' );
			get_term_data( data => $data, output => $output, term => $t, data_to_get => [ qw( name namespace) ], f_data => 'f1' );

		}
		else
		{	warn "$t is only in file 1\n" if $options->{verbose};
			$output->{f1_only}{$t}++;
			get_term_data( data => $data, output => $output, term => $t, data_to_get => [ qw(name namespace anc) ], f_data => 'f1' );
#			$data->{diffs}{Term}{f1_only}{$t}++;
		}
	}

#	# map the subsets
#	if ($data->{f1_hash}{Term}{$t}{subset} && grep { $options->{subset} eq $_ }  @{$data->{f1_hash}{Term}{$t}{subset}} )
#	{	$data->{f1_hash}{subset}{ $options->{subset} }{$t} = 1;
#	}

}

#print STDERR "data->diffs->term->all_tags_used: " . Dumper($data->{diffs}{Term}{all_tags_used});

foreach my $t (sort keys %{$data->{f2_hash}{Term}})
{	# map the subsets
#	if ($data->{f2_hash}{Term}{$t}{subset} && grep { $options->{subset} eq $_ }  @{$data->{f2_hash}{Term}{$t}{subset}} )
#	{	$data->{f2_hash}{subset}{ $options->{subset} }{$t} = 1;
#	}

	if (! $data->{f1_hash}{Term}{$t})
	{	# check it hasn't been de-merged
		if ($data->{f1_alt_ids}{$t})
		{	# erk! it was an alt id... what's going on?!
			warn "$t was an alt id for " . $data->{f1_alt_ids}{$t} . " but it has been de-merged!";
			$output->{f2_to_f1_merge}{$t} = $data->{f1_alt_ids}{$t};

			get_term_data( data => $data, output => $output, term => $data->{f1_alt_ids}{$t}, data_to_get => [ qw(name namespace) ], f_data => 'f1' );
			get_term_data( data => $data, output => $output, term => $t, data_to_get => [ qw(name namespace) ], f_data => 'f2' );
		}
		else
		{	$output->{f2_only}{$t}++;	#= { id => $t, name => $data->{f2_hash}{Term}{$t}{name}[0], namespace => $ns };
			get_term_data( data => $data, output => $output, term => $t, data_to_get => [ qw(name namespace anc) ], f_data => 'f2' );

		}
	}
## the term is in f1 and f2. let's see if there are any differences
	else
	{	# quickly compare the arrays, see if they are the same

		my $f1_str = join("\0", map {
			join("\0", @{$data->{f1_hash}{Term}{$t}{$_}})
		} sort keys %{$data->{f1_hash}{Term}{$t}});
		my $f2_str = join("\0", map {
			join("\0", @{$data->{f2_hash}{Term}{$t}{$_}})
		} sort keys %{$data->{f2_hash}{Term}{$t}});
		next if $f1_str eq $f2_str;

#		next if join("\0", @{$data->{f1}{Term}{$t}}) eq join("\0", @{$data->{f2}{Term}{$t}});

		## the arrays are different. Let's see just how different they are...
		{	my $r = compare_hashes( f1 => $data->{f1_hash}{Term}{$t}, f2 => $data->{f2_hash}{Term}{$t}, to_ignore => $ignore_regex );
			if ($r)
			{	$data->{diffs}{Term}{both}{$t} = $r;
				get_term_data( data => $data, output => $output, term => $t, data_to_get => [ qw(name namespace) ], f_data => 'f2' );
				foreach (keys %$r)
#				foreach (keys %{$r->{summary}})
				{	$data->{diffs}{Term}{all_tags_used}{$_}{$t}++;
				}

			}
		}
	}
}

foreach my $a qw(name namespace)
{	if ($data->{diffs}{Term}{all_tags_used}{$a})
	{	map { $output->{$a . "_change" }{$_}++ } keys %{$data->{diffs}{Term}{all_tags_used}{$a}};
	}
}

if ($data->{diffs}{Term}{all_tags_used}{is_obsolete})
{	foreach my $t (keys %{$data->{diffs}{Term}{all_tags_used}{is_obsolete}})
	{	if ($data->{f2_hash}{Term}{$t}{is_obsolete})
		{	$output->{f2_obsoletes}{$t}++;
#			print STDERR "added $t to f2 obsoletes\n";
		}
		else
		{	$output->{f1_obsoletes}{$t}++;
#			print STDERR "added $t to f1 obsoletes\n";
		}
	}
}

#print STDERR "output - obsoletes: " . Dumper($output->{f2_obsoletes}) . "\n\nf1 obs: " . Dumper($output->{f1_obsoletes}) . "\n\n";

if ($ss)
{
	my $g1 = $data->{f1}{trimmed};
	my $g2 = $data->{f2}{trimmed};
	my $g1_stt_ix = $g1->get_statement_ix('trimmed')->ixT;
	my $g2_stt_ix = $g2->get_statement_ix('trimmed')->ixT;

	# go through the g2 subset terms and compare the data to that we got from g1
	foreach my $t (@{$data->{f2}{subset_ids}})
	{	my $count;
		## see if $t was a subset term in f1
		if (! grep { $t eq $_ } @{$data->{f1}{subset_ids}})
		{	# $t is a new subset term in f2
#			print STDERR "$t is a new subset term in g2\n";
#			next if $data->{f1_alt_ids}{$t};
#			push @{$data->{f2_subset_only}}, $t;
			$output->{f2_subset_only}{$t}++ unless $data->{f1_alt_ids}{$t};
			next;
		}
#		else
#		{	
		if (@{$g1->statements_in_ix_by_target_id('ontology_links', $t)})
		{	# links from target $t in the graph for f1
			foreach (@{$g1->statements_in_ix_by_target_id('ontology_links', $t)})
			{	$count->{$_->node->id}++;
			}
		}
		else
		{	warn "No links in f1 involving subset term $t" if $options->{verbose};
		}
#		}

		if (@{$g2->statements_in_ix_by_target_id('ontology_links', $t)})
		{	# links from target $t in the graph for f2
			foreach (@{$g2->statements_in_ix_by_target_id('ontology_links', $t)})
			{	$count->{$_->node->id}+= 10;
			}
		}
		else
		{	warn "No links in f2 involving subset term $t" if $options->{verbose};
		}

		foreach my $e (keys %$count) {
			next if $count->{$e} == 11;
			if ($count->{$e} == 1)
			{	# term has been removed from $ss
				$data->{subset_movements}{$t}{$e}{out} = 1 unless $data->{f2_alt_ids}{$e};
			}
			elsif ($count->{$e} == 10)
			{	# term has been added to $ss
				$data->{subset_movements}{$t}{$e}{in} = 1 unless $data->{f1_alt_ids}{$e};
			}
		}
	}


	# go through the g2 subset terms and compare the data to that we got from g1
	foreach my $t (@{ $data->{f1}{subset_ids}})
	{	if (! grep { $t eq $_ } @{$data->{f2}{subset_ids}})
		{	next if $data->{f2_alt_ids}{$t};
			# $t is no longer a subset term
			push @{$data->{f1_subset_only}}, $t;
			$output->{f1_subset_only}{$t}++;
		}
	}

	foreach my $t (keys %{$data->{subset_movements}}, keys %{$output->{f1_subset_only}}, keys %{$output->{f2_subset_only}})
	{	get_term_data( data => $data, output => $output, term => $t, data_to_get => [ qw(name namespace) ], f_data => 'f2' );
	}

	$output->{subset_movements} = $data->{subset_movements};
	print STDERR "Finished subset analysis\n" if $options->{verbose};
}



$output = store_header($output, $data, $options);
$output = store_term_changes($output, $data);
$output = generate_stats($output, $data);
$output = compare_other_stanzas($output, $data);

$output->{single_value_attribs} = [ @single_attribs ];
$output->{subset} = $ss;
$output->{show_term_changes} = 1;

#print STDERR "output keys: " . join("; ", sort keys %$output) . "\n";
print STDERR "Printing results!\n" if $options->{verbose};

$tt->process($options->{format} . '_report.tmpl', $output, $options->{output})
    || die $tt->error(), "\n";

#if (! -e 'stored_frozen_data.txt')
#{	my $frozen = Storable::freeze $data;
#	Storable::store $frozen, 'stored_frozen_data.txt';
#}
#elsif (! -e 'stored_comp_data.txt')
#{	Storable::store $data, 'stored_comp_data.txt';
#}

}





# parse the options from the command line
sub parse_options {
	my $args = shift;

	my $opt;

	while (@$args && $args->[0] =~ /^\-/) {
		my $o = shift @$args;
		if ($o eq '-f1' || $o eq '--file_1' || $o eq '--file_one') {
			if (@$args && $args->[0] !~ /^\-/)
			{	$opt->{f1} = shift @$args;
			}
		}
		elsif ($o eq '-f2' || $o eq '--file_2' || $o eq '--file_two') {
			if (@$args && $args->[0] !~ /^\-/)
			{	$opt->{f2} = shift @$args;
			}
		}
		elsif ($o eq '-s' || $o eq '--subset') {
			while (@$args && $args->[0] !~ /^\-/)
			{	my $s = shift @$args;
				$opt->{subset} = $s;
			}
		}
		elsif ($o eq '-o' || $o eq '--output') {
			$opt->{output} = shift @$args if @$args && $args->[0] !~ /^\-/;
		}
		elsif ($o eq '-f' || $o eq '--format') {
			if (@$args && $args->[0] !~ /^\-/)
			{	$opt->{format} = shift @$args;
			}
		}
		elsif ($o eq '-h' || $o eq '--help') {
			system("perldoc", $0);
			exit(0);
		}
		elsif ($o eq '-v' || $o eq '--verbose') {
			$opt->{verbose} = 1;
		}
		else {
			die "Error: no such option: $o\nThe help documentation can be accessed with the command 'go-slimdown.pl --help'\n";
		}
	}
	return check_options($opt);
}


# process the input params
sub check_options {
	my $opt = shift;
	my $errs;

	if (!$opt)
	{	die "Error: please ensure you have specified two input files, a subset, and an output file.\nThe help documentation can be accessed with the command 'compare-graphs.pl --help'\n";
	}

	foreach my $f qw(f1 f2)
	{	if (!$opt->{$f})
		{	push @$errs, "specify an input file using -$f /path/to/<file_name>";
		}
		elsif (! -e $opt->{$f})
		{	push @$errs, "the file " . $opt->{$f} . " could not be found.\n";
		}
		elsif (! -r $opt->{$f} || -z $opt->{$f})
		{	push @$errs, "the file " . $opt->{$f} . " could not be read.\n";
		}
	}
	## quick 'diff' check of whether the files are identical or not
	my $cmd = "diff -w -q " . $opt->{f1} . " " . $opt->{f2};

	my $status = `$cmd`;
	die "The two files specified appear to be identical!" if ! $status;

	if (!$opt->{format})
	{	$opt->{format} = 'text';
	}
	else
	{	if (! grep { $_ eq $opt->{format} } qw(text html) )
		{	push @$errs, "the output format " . $opt->{format} . "is invalid. Valid options are 'text' and 'html'";
		}
	}

	if (!$opt->{output})
	{	push @$errs, "specify an output file using -o /path/to/<file_name>";
	}

	if (!$opt->{subset})
	{	push @$errs, "specify a subset using -s <subset_name>";
	}



	if ($errs && @$errs)
	{	die "Error: please correct the following parameters to run the script:\n" . ( join("\n", map { " - " . $_ } @$errs ) ) . "\nThe help documentation can be accessed with the command\n\tgo-slimdown.pl --help\n";
	}

	if ($ENV{DEBUG})
	{	$opt->{verbose} = 1;
	}

	return $opt;
}


# store the data
sub store_header {
	my $vars = shift;
	my $d = shift;
	my $args = shift;
	my $f_text = {
		f1 => 'file 1 (old): ',
		f2 => 'file 2 (new): ',
	};

	foreach my $f ("f1", "f2")
	{	my @f_data;
		my $header = $d->{$f}{"header"};
		my $slash = rindex $args->{$f}, "/";
		if ($slash > -1)
		{	push @f_data, substr $args->{$f}, ++$slash;
		}
		else
		{	push @f_data, $args->{$f};
		}

		if ($header->{"data-version"})
		{	push @f_data, "data version: " . $header->{"data-version"}[0];
		}
		if ($header->{date})
		{	push @f_data, "date: " . $header->{date}[0];
		}
		if ($header->{remark})
		{	foreach (@{$header->{remark}})
			{	if (/cvs version: \$Revision:\s*(\S+)/)
				{	push @f_data, "CVS revision: " . $1;
					last;
				}
			}
		}

		if (@f_data)
		{	$vars->{$f . '_file_data'} =  join("; ", @f_data);
		}
		else
		{	$vars->{$f . '_file_data'} = "unknown";
		}
	}

	if ($args->{subset})
	{	$vars->{subset_used} = $args->{subset};
	}
	else
	{	$vars->{subset_used} = 'not applicable';
	}
	return $vars;
}


# find and store differences between the terms
sub store_term_changes {
	my $vars = shift;
	my $d = shift;

#	$data->{diffs}{Term}{both}{$t}

	return $vars unless $d->{diffs}{Term}{both};

	my $ignore = '^(' . join("|", qw(id) ) . ')$';
	$ignore = qr/$ignore/;

	my @attribs = grep { exists $d->{diffs}{Term}{all_tags_used}{$_} && $_ ne 'id' } @ordered_attribs;

	# nothing to report!
	return if ! @attribs;

	$vars->{term_change_attribs} = [ @attribs ];

	foreach my $t (sort keys %{$d->{diffs}{Term}{both}})
	{	my $h;
		## go through our list of attributes and see which ones differ
		foreach my $a (@attribs)
		{	next unless $d->{diffs}{Term}{both}{$t}{$a};

			## is this a single-valued attribute?
			if (grep { $a eq $_ } @single_attribs)
			{	if ($d->{diffs}{Term}{both}{$t}{$a}{f1} && $d->{diffs}{Term}{both}{$t}{$a}{f2})
				{	# changed
					$h->{$a} = 'C';
				}
				elsif ($d->{diffs}{Term}{both}{$t}{$a}{f2})
				{	$h->{$a} = 'A';
				}
				elsif ($d->{diffs}{Term}{both}{$t}{$a}{f1})
				{	$h->{$a} = 'D';
				}
			}
			else # multiple attributes
			{	#push @$line, "$a: ";
				if ($d->{diffs}{Term}{both}{$t}{$a}{f1} && $d->{diffs}{Term}{both}{$t}{$a}{f2})
				{	my $net = (scalar @{$d->{diffs}{Term}{both}{$t}{$a}{f2}}) - (scalar @{$d->{diffs}{Term}{both}{$t}{$a}{f1}});
					if ($net == 0)
					{	$h->{$a} = (scalar @{$d->{diffs}{Term}{both}{$t}{$a}{f2}}) . "C";

					}
					elsif ($net < 0)
					{	$net = 0 - $net;
						$h->{$a} = (scalar @{$d->{diffs}{Term}{both}{$t}{$a}{f2}}) . " C,  $net D";
					}
					elsif ($net > 1)
					{	$h->{$a} = "$net A, " . (scalar @{$d->{diffs}{Term}{both}{$t}{$a}{f1}}) . " C";

					}
				}
				elsif ($d->{diffs}{Term}{both}{$t}{$a}{f1})
				{	$h->{$a} = (scalar @{$d->{diffs}{Term}{both}{$t}{$a}{f1}}) . " D";
				}
				elsif ($d->{diffs}{Term}{both}{$t}{$a}{f2})
				{	$h->{$a} = (scalar @{$d->{diffs}{Term}{both}{$t}{$a}{f2}}) . " A";
				}
			}
		}
		#print $fh print_term_name($d, $t) . "\n" . join("; ", @$line) . "\n";
		$vars->{term_changes}{$t} = {
			d_hash => $h,
			raw => $d->{diffs}{Term}{both}{$t}
		};

		my $to_get = [ qw(name namespace) ];
		if ($h->{is_obsolete})
		{	push @$to_get, 'anc';
		}
		get_term_data( data => $d, output => $vars, term => $t, data_to_get => $to_get, f_data => 'f2' );
		if ($h->{is_obsolete} || $h->{name})
		{	get_term_data( data => $d, output => $vars, term => $t, data_to_get => $to_get, f_data => 'f1' );
		}
	}
	return $vars;
}


sub compare_other_stanzas {
	my $output = shift;
	my $d = shift;
	my $ignore = qw/id/;
	## compare the other types of stanza
	foreach my $type (keys %{$d->{f1_hash}})
	{	next if $type eq 'Term';
		foreach my $t (keys %{$d->{f1_hash}{$type}})
		{	if (! $d->{f2_hash}{$type}{$t})
			{	# check it hasn't been merged
				if ($d->{f2_alt_ids}{$t})
				{	# the term was merged. N'mind!
		#			print STDERR "$t was merged into " . $d->{f2_alt_ids}{$t} . "\n";

					$output->{$type}{f1_to_f2_merge}{$t} = $d->{f2_alt_ids}{$t};
				}
				else
				{	warn "$type $t is only in file 1\n";
					$output->{$type}{f1_only}{$t}++;
				}
			}
		}
		foreach my $t (keys %{$d->{f2_hash}{$type}})
		{	if (! $d->{f1_hash}{$type}{$t})
			{	# check it hasn't been de-merged
				if ($d->{f1_alt_ids}{$t})
				{	# erk! it was an alt id... what's going on?!
					warn "$t was an alt id for " . $d->{f1_alt_ids}{$t} . " but it has been de-merged!";
					$output->{$type}{f2_to_f21_merge}{$t} = $d->{f1_alt_ids}{$t};
				}
				else
				{	$output->{$type}{f2_only}{$t}++;
				}
			}

			else
			{	# quickly compare the arrays, see if they are the same
				my $f1_str = join("\0", map {
					join("\0", @{$d->{f1_hash}{$type}{$t}{$_}})
				} sort keys %{$d->{f1_hash}{$type}{$t}});

				my $f2_str = join("\0", map {
					join("\0", @{$d->{f2_hash}{$type}{$t}{$_}})
				} sort keys %{$d->{f2_hash}{$type}{$t}});
				next if $f1_str eq $f2_str;

#				next if join("\0", map { join("\0", @{$d->{f1_hash}{$type}{$t}{$_}}) } sort keys %{$d->{f1_hash}{$type}{$t}}) eq join("\0", map { join("\0", @{$d->{f2_hash}{$type}{$t}{$_}}) } sort keys %{$d->{f2_hash}{$type}{$t}});

				my $r = compare_hashes( f1 => $d->{f1_hash}{$type}{$t}, f2 => $d->{f2_hash}{$type}{$t}, to_ignore => $ignore );
				if ($r)
				{	$output->{diffs}{$type}{both}{$t} = $r;
					foreach (keys %$r)
					{	$output->{diffs}{$type}{all_tags_used}{$_}++;
					}
				}
			}
		}
	}
	return $output;
}


sub generate_stats {
	my $vars = shift;
	my $d = shift;

#	print STDERR "f1 stats: " . Dumper($d->{f1_stats}) . "\nf2 stats: " . Dumper($d->{f2_stats}) . "\n\n";

	$vars->{f2_stats} = $d->{f2_stats};
	$vars->{f1_stats} = $d->{f1_stats};

	foreach my $f qw( f1 f2 )
	{	foreach my $o (keys %{$vars->{$f . "_stats"}{by_ns}})
		{	## we have def => n terms defined
			## total => total number of terms
			if (! $vars->{$f. "_stats"}{by_ns}{$o}{def})
			{	$vars->{$f. "_stats"}{by_ns}{$o}{def} = 0;
				$vars->{$f. "_stats"}{by_ns}{$o}{def_percent} = 0;
			}
			else
			{	$vars->{$f . "_stats"}{by_ns}{$o}{def_percent} = sprintf("%.1f", $vars->{$f. "_stats"}{by_ns}{$o}{def} / $vars->{$f. "_stats"}{by_ns}{$o}{total} * 100);
			}
		}
		foreach my $x qw(obs def_not_obs)
		{	if (! $vars->{$f."_stats"}{$x})
			{	$vars->{$f."_stats"}{$x} = 0;
				$vars->{$f."_stats"}{$x . "_percent"} = 0;
			}
			else
			{	$vars->{$f."_stats"}{$x . "_percent"} = sprintf("%.1f", $vars->{$f. "_stats"}{$x} / $vars->{$f. "_stats"}{total} * 100);
			}
		}
	}

	foreach my $x qw(obs def_not_obs total)
	{	$vars->{delta}{$x} = $vars->{f2_stats}{$x} - $vars->{f1_stats}{$x};
		$vars->{delta}{$x . "_percent"} = sprintf("%.1f", $vars->{delta}{$x} / $vars->{f1_stats}{$x} * 100);
	}
	return $vars;
}


sub get_term_data {
	my %args = (@_);
	my $d = $args{data};
	my $output = $args{output};
	my $t = $args{term};
	my $to_get = $args{data_to_get};
	my $f = $args{f_data};

#	print STDERR "args: " . join("\n", map { $_ . ": " . Dumper($args{$_}) } qw(term data_to_get f_data)) ."\n";


#	print STDERR "data: " . Dumper($d->{$f . "_hash"}{Term}{$t}) . "\n\n";

	foreach (@$to_get)
	{	next if $output->{$f}{$t}{$_};
		if ($d->{$f . "_hash"}{Term}{$t}{$_})
		{	if (grep { /^$_$/ } @single_attribs)
			{	$output->{$f}{$t}{$_} = $d->{$f . "_hash"}{Term}{$t}{$_}[0];
			}
			else
			{	$output->{$f}{$t}{$_} = $d->{$f . "_hash"}{Term}{$t}{$_};
			}
			next;
		}
		if ($_ eq 'anc')
		{	if ($d->{$f . "_hash"}{Term}{$t}{is_obsolete})
			{	$output->{$f}{$t}{anc} = ['obsolete'];
			}
			else
			{	if ($d->{$f}{trimmed})
				{	my $stts = $d->{$f}{trimmed}->statements_in_ix_by_node_id('ontology_links', $t);
					if (@$stts)
					{	my %parent_h;
						map { $parent_h{$_->node->id} = 1 } @$stts;
						$output->{$f}{$t}{anc} = [ sort keys %parent_h ];
					}
				}
			}
		}
		if ($_ eq 'namespace' && $d->{$f . "_hash"}{Term}{$t}{is_obsolete})
		{	$output->{$f}{$t}{$_} = 'obsolete';
		}
	}
#	print STDERR "wanted " . join(", ", @$to_get) . " for $t; returning: " . Dumper($output->{$f}{$t}) . "\n";
}


=head2 Script methods

=head2 block_to_sorted_array

input:  a multi-line block of text (preferably an OBO format stanza!)
output: ref to an array with the following removed
        - empty lines
        - lines starting with "id: ", "[", and "...]"
        - trailing whitespace

        the array is sorted

=cut

sub block_to_sorted_array {
	my $block = shift;
	my $arr;
	foreach ( split( "\n", $block ) )
	{	next unless /\S/;
		next if /^(id: \S+|\[|\S+\])\s*$/;
		$_ =~ s/^(is_a:|relationship:)\s*(.+)\s*!\s.*$/$1 $2/;
		$_ =~ s/\s*$//;
		push @$arr, $_;
	}

	return [ sort @$arr ] || undef;
}


=head2 tag_val_arr_to_hash

input:  array ref containing ": " separated tag-value pairs
output: lines in the array split up by ": " and put into a hash
        of the form key-[array of values]

=cut

sub tag_val_arr_to_hash {
	my $arr = shift;
	if ($arr && ! ref $arr && $_[0])
	{	my @array = ( $arr, @_ );
		$arr = \@array;
	}

	return undef unless $arr && @$arr;
	my $h;
	foreach (@$arr)
	{	my ($k, $v) = split(": ", $_, 2);
		if (! $k || ! $v)
		{	#print STDERR "line: $_\n";
		}
		else
		{	push @{$h->{$k}}, $v;
		}
	}
	
#	map { $h->{$_} = [ sort @{$h->{$_}} ] } keys %$h;

	return $h;
}


=head2 block_to_hash

input:  a multi-line block of text (preferably an OBO format stanza!)
output: lines in the array split up by ": " and put into a hash
        of the form key-[array of values]

Directly does what could otherwise be accomplished by block_to_sorted_array
and tag_val_arr_to_hash

=cut

sub block_to_hash {
	my $block = shift;

	my $arr;
	foreach ( split( "\n", $block ) )
	{	next unless /\S/;
		next if /^(id: \S+|\[|\S+\])\s*$/;
		$_ =~ s/^(.+?:)\s*(.+)\s*!\s.*$/$1 $2/;
		$_ =~ s/\s*$//;
		push @$arr, $_;
	}
	return undef unless $arr && @$arr;
	my $h;
	foreach (@$arr)
	{	my ($k, $v) = split(": ", $_, 2);
		if (! $k || ! $v)
		{	#print STDERR "line: $_\n";
		}
		else
		{	push @{$h->{$k}}, $v;
		}
	}
	
	map { $h->{$_} = [ sort @{$h->{$_}} ] } keys %$h;

	return $h;
}


=head2 compare_hashes

input:  hash containing
        f1 => $f1_term_data
        f2 => $f2_term_data
        to_ignore => regexp for hash keys to ignore

output: hash of differences in the form
        {hash key}{ f1 => [ values unique to f1 ]
                    f2 => [ values unique to f2 ] }

=cut

sub compare_hashes {
	my %args = (@_);
	my $f1 = $args{f1};
	my $f2 = $args{f2};
	my $ignore = $args{to_ignore};

	my $results;
	my $all_values;
	foreach my $p (keys %$f1)
	{	# skip these guys
		next if $p =~ /^$ignore$/;
		if (! $f2->{$p})
		{	$results->{$p}{f1} += scalar @{$f1->{$p}};
			$all_values->{$p}{f1} = $f1->{$p};
		}
		else
		{	# find the same / different values
			my @v1 = values %$f1;
			my @v2 = values %$f2;

			my %count;
			foreach my $e (@{$f1->{$p}})
			{	$count{$e}++;
			}
			foreach my $e (@{$f2->{$p}})
			{	$count{$e} += 10;
			}

			foreach my $e (keys %count) {
				next if $count{$e} == 11;
				if ($count{$e} == 1)
				{	$results->{$p}{f1}++;
					push @{$all_values->{$p}{f1}}, $e;
				}
				elsif ($count{$e} == 10)
				{	$results->{$p}{f2}++;
					push @{$all_values->{$p}{f2}}, $e;
				}
			}
		}
	}
	foreach (keys %$f2)
	{	if (! $f1->{$_})
		{	$results->{$_}{f2} += scalar @{$f2->{$_}};
			$all_values->{$_}{f2} = $f2->{$_};
		}
	}

#	return { summary => $results, with_values => $all_values };
	return $all_values;
}

=head1 AUTHOR

Amelia Ireland

=head1 SEE ALSO

L<GOBO::Graph>, L<GOBO::InferenceEngine>, L<GOBO::Doc::FAQ>

=cut
