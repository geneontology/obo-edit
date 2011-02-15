=head1 SYNOPSIS

package GO::CGI::Utilities;

=cut

package GO::CGI::Utilities;

use strict;
#use GO::CGI::Session;
#use GO::Template::Template;
#use Carp;
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

use Exporter;
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);

@ISA = ('Exporter');
#@EXPORT = qw(get_environment_param get_valid_list add_value_to_list remove_value_from_list);
@EXPORT_OK = qw(get_subset get_n_pages set_message get_tmpl get_environment_param get_message get_valid_list add_value_to_list remove_value_from_list render_data_in_format); # process_page_template
%EXPORT_TAGS = (
	all => [ qw(get_subset get_n_pages set_message get_tmpl get_environment_param get_message get_valid_list add_value_to_list remove_value_from_list render_data_in_format) ],# process_page_template
	std => [ 
	qw(get_subset get_n_pages
	set_message
	get_valid_list
	get_environment_param
	get_tmpl
	render_data_in_format) ],#	process_page_template

);


=head2 get_subset

  Arguments - list, page number (optional), page size (optional)
  returns   - the subset of the input list that would appear on page X

=cut

sub get_subset {
	my $fullset = shift;
	my $page_n = shift || 1;
	my $page_size = shift || get_environment_param('page_size');
	print STDERR "get subset: page number: $page_n; list size = ".scalar(@$fullset)."\n";
	my @subset = @$fullset;
	my ($from, $to) = ($page_size * ($page_n - 1), $page_size * $page_n - 1);

	if ($to >= scalar(@$fullset)) {
		$to = scalar(@$fullset) - 1;
	}
	@subset = @$fullset[$from..$to];

	if (scalar @subset == 0)
	{	#	return the last page of results
		$page_n = get_n_pages(scalar @$fullset, $page_size);
		return get_subset($fullset, $page_n, $page_size);
	}
	
	return \@subset;
}


=head2 get_n_pages

  Arguments - no of results, page size (optional)
  returns   - number of pages required to show the results

=cut

sub get_n_pages {
	my $n_accs = shift;
	my $page_size = shift || get_environment_param('page_size');

	print STDERR "n_accs = $n_accs; page size: $page_size\n";

	my $n_pages = 1;
	if ($page_size ne 'all')
	{	$n_pages = int($n_accs/$page_size);
		$n_pages++ if ($n_accs % $page_size);
	}
	return $n_pages;
}


=head2 get_environment_param

	Get an AmiGO environment variable (from config.pl)

=cut

sub get_environment_param {
	my $var = shift;
	if (defined($ENV{uc("GO_$var")})) {
		return $ENV{uc("GO_$var")};
	}
	return;
}

=head2 process_page_template

	Prepare the page for lift-off

	Arguments - vars - hash containing data to go into the template
	            session (if created)
	            session_type (if not in session->ses_type)


sub process_page_template {
	my $vars = shift;
	my $session = shift;
	my $ses_type = shift;
	
	if (!$session)
	{	require GO::CGI::Session;
		$session = new GO::CGI::Session(-read_only=>1);
	}

	$ses_type = $session->ses_type if !$ses_type;

	my $tmpl_vars = $session->get_vars($vars);
	print "Content-type:text/html\n\n";
	GO::Template::Template->process_template($ses_type.".tmpl", $tmpl_vars);
}
=cut

=head2 set_message

	Set messages / errors

	Arguments - message hash,
	            message class: fatal, warning or info
	            message type: e.g. 'no_valid_query', 'no_results'
	            what it affects (optional)

	Returns   - new improved message hash
=cut

sub set_message {
	my $msg_h = shift;
	my $class = shift;
	my $type = shift;
	my $affects = shift || [];

	if (!ref($affects))
	{	$affects = [ $affects ];
	}

	push @{$msg_h->{$class}{$type}}, @$affects;

	return $msg_h;
}

#	not used (at the moment)
sub get_message {
	my $msg_h = shift;
	my $class = shift || undef;

	if ($class && $msg_h->{$class})
	{	return $msg_h->{$class};
	}
	return $msg_h;
}


=head2 get_tmpl

	Translates the format parameter into information about what data
	is required 

	Arguments - format (defaults to 'html')
	            what_to_get (are we getting terms, gene products, assocs, etc.?)

=cut

sub get_tmpl {
	my ($format, $what_to_get, $gp_count_ok) = @_;

	$format = 'html' if !$format;

	print STDERR "format: ".$format."\n";

#	valid values
#	gp => ['synonyms', 'gptype', 'spp', 'has_seq', 'seq']
#	term => ['synonym_list', 'definition', 'comment', 'def_dbxref_list', 'dbxref_list', 'subset_list', 'n_products', 'n_deep_products']
#	assoc => ['assocdate', 'assby', ]

	my $templates = {
		gp_assoc_cgi => {
			gp => ['gptype', 'spp', 'has_seq'],
			term => ['acc', 'n_deep_products'],
			assoc => ['assby'],
		},
		gp_details_cgi => {
			gp => ['synonyms', 'gptype', 'spp', 'seq'],
		},
		term_assoc_cgi => {
			term => ['acc', 'definition', 'n_deep_products'],
			gp => ['gptype', 'spp', 'has_seq'],
			assoc => ['assby'],
		},
		term_details_cgi => {
			term => ['synonym_list', 'definition', 'dbxref_list', 'subset_list', 'n_deep_products'],
			graph => ['acc', 'n_deep_products'],
		},
		html => {
			gp => ['synonyms', 'gptype', 'spp', 'has_seq'],
			term => ['acc'],
			assoc => [],
			graph => ['acc', 'n_deep_products'],
		},
		full => {
			gp => ['synonyms', 'gptype', 'spp', 'seq'],
			term => ['synonym_list', 'definition', 'dbxref_list', 'subset_list', 'n_deep_products'],
			assoc => [],
		},
		go_assoc => {
			gp => ['synonyms', 'gptype', 'spp'],
			assoc => ['assby', 'assocdate', 'return_graph'],
			term => ['acc'],
		},
		rdfxml => {
			term => ['definition', 'dbxref_list', '??? n_assocs' ],
			gp => ['name'],
			assoc => ['return_graph'],
		},
		fasta => {
			gp => ['gptype', 'spp', 'seq'],
		},
		gp_graph => {
			term => ['??? definition'],
			assoc => [ ],
		
		},
		png => {
			term => ['acc'],
		},
		dot => {
			term => ['acc'],
		},
		obo => {
			term => ['synonym_list', 'definition', 'dbxref_list', 'subset_list'],
		},
		go_ont => {
			term => ['acc'],
		},
		tree => {
			term => ['acc'],
		},
		
		html_full => {
		
		}
	};
	
	my $tmpl;
	if (defined $what_to_get && $templates->{$format}{$what_to_get})
	{	print STDERR "know what to get... let's get it!\n";
		$tmpl->{$_} = 1 foreach @{$templates->{$format}{$what_to_get}};
		if (!$gp_count_ok && ($tmpl->{n_deep_products} || $tmpl->{n_products}))
		{	#	if it's not ok to get the GP count, delete these fields
			delete $tmpl->{n_deep_products};
			delete $tmpl->{n_products};
		}
	}
	elsif ($templates->{$format})
	{	print STDERR "got the format... getting the data.\n";
		foreach my $t (keys %{$templates->{$format}})
		{	print STDERR "Working on $t...\n";
			$tmpl->{$t}{$_} = 1 foreach @{$templates->{$format}{$t}};
		}
		if (!$gp_count_ok)
		{	#	if it's not ok to get the GP count, delete these fields
			for ('graph', 'term')
			{	if ($tmpl->{$_} && ($tmpl->{$_}{n_deep_products} || $tmpl->{$_}{n_products}))
				{	delete $tmpl->{$_}{n_deep_products};
					delete $tmpl->{$_}{n_products};
				}
			}
		}
	}
	else
	{	print STDERR "Unknown format!\n";
	}
	
	print STDERR "tmpl: ".Dumper($tmpl)."\n";
	
	return $tmpl;
}


=head2 check_gp_count_ok

  Arguments - apph
  returns   - 1 or 0

  Looks at the apph filters to see if the gp count
  for a term will be correct

	should be done by Session


sub check_gp_count_ok {
	my $apph = shift;
	my $filters = $apph->filters;

	#	check whether it's going to be ok to get gp counts
	if (!keys %$filters || get_environment_param('calculate_gp_counts'))
	{	return 1;
	}
	else
	{	my @gp_count_correct_fields = GO::CGI::Session::_gp_count_correct_fields();
		
		print STDERR "gp count correct fields: ".Dumper(\@gp_count_correct_fields)."\n";
		
		#	check if there are filters which aren't in gp_count_correct_fields
		my $set;
		foreach my $f (keys %$filters)
		{	#print STDERR "f = $f\n";
			if (!grep { $f eq $_ } @gp_count_correct_fields)
			{	print STDERR "$f is not in gp_count_correct\n";
				return;
			}
		}
		return 1;
	}
}
=cut

=head2 get_valid_list

Takes a listref and checks for duplicates / undef entries, etc.
Returns a ref to the valid items in the list

=cut

sub get_valid_list {
	my $list = shift;

	return unless $list;

	return [$list] if !ref($list);

	#	check for / remove dups and blank entries
	my %hash;
	foreach (@$list)
	{	$hash{$_} = 1 if defined $_;
	}
	return [ keys %hash ] || undef;
}

=head2 add_value_to_list

Adds a value to a list, ensuring that there are no duplicates

=cut

sub add_value_to_list {
	my $list = shift;
	my $value = shift;

	return $list if !$value;
	return [ $value ] if (!$list || !@$list);
	
	my %hash;
	foreach (@$list)
	{	$hash{$_} = 1 if defined $_;
	}
	$hash{$value} = 1;

	return [ keys %hash ] || undef;
}

=head2 remove_value_from_list

Removes a value from a list

=cut

sub remove_value_from_list {
	my $list = shift;
	my $value = shift;

	return $list if !$value;
	return if (!$list || !@$list);

	my %hash;
	foreach (@$list)
	{	$hash{$_} = 1 if defined $_;
	}
	delete $hash{$value} if $hash{$value};

	return [ keys %hash ] || undef;
}

=head2 render_data_in_format {

Outputs the data in whatever format is required

=cut

sub render_data_in_format {
	my $data = shift;
	my $format = shift;
	return unless $data;

#	from GP assocs
	if ($format eq 'rdfxml')
	{	print "Content-type:text/plain\n\n";
		my $out = new FileHandle(">-");
		$data->to_xml($out);
	}
	elsif ($format eq 'go_assoc')
	{	my $out = new FileHandle(">-");
		my $ga_out = GO::IO::go_assoc->new($out);
		$ga_out->cgi_header;
		$ga_out->write_graph($data);
	}
	elsif ($format eq 'obo')
	{	print "Content-type:text/plain\n\n";
		$data->export({ format => 'obo' });
	}
	elsif ($format eq 'go_ont')
	{	print "Content-type:text/plain\n\n";
		$data->to_text_output(-fmt=>'gotext');
	}
	elsif ($format eq 'tree')
	{	print "Content-type:text/plain\n\n";
		$data->to_text_output(-fmt=>'tree');
	}
	elsif ($format eq 'dot')
	{	print "Content-type:text/plain\n\n";
		print $data->as_text;
	}
	else
	{	print "Content-type:text/plain\n\n";
		print $data->as_text;
	}
	exit;
}

1;
