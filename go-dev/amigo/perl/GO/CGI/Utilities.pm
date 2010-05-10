=head1 SYNOPSIS

package GO::CGI::Utilities;

Generally useful bits and pieces for AmiGO.

=cut

package GO::CGI::Utilities;

use strict;
use Exporter;
use Template;
#use Template 2.19;
#use Template::Constants qw( :all );
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);

## Just a little bit...
use AmiGO qw(:google_analytics_id);
my $core = AmiGO->new();
my $google_id = $core->google_analytics_id();
my $version = $core->amigo_env('version') || 'unknown';

@ISA = ('Exporter');
#@EXPORT = qw(get_environment_param get_valid_list add_value_to_list remove_value_from_list);
@EXPORT_OK = qw(get_results_chunk get_n_chunks set_message get_tmpl get_environment_param get_message get_valid_list add_value_to_list remove_value_from_list render_data_in_format create_apph get_environment_param get_external_environmental_param output_template); # process_page_template

%EXPORT_TAGS = (
	all => [ qw(get_results_chunk get_n_chunks set_message get_tmpl get_environment_param get_message get_valid_list add_value_to_list remove_value_from_list render_data_in_format create_apph get_environment_param get_external_environmental_param output_template) ],# process_page_template
	std => [ 
	qw(get_results_chunk get_n_chunks
	set_message
	get_valid_list
	get_environment_param
	get_external_environmental_param
	get_tmpl
	render_data_in_format
	create_apph
	output_template) ],#	process_page_template

);

use GO::AppHandle;
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

our $verbose = get_environment_param('verbose');

=head2 get_subset

  Arguments - list, page number (optional), page size (optional)
  returns   - the subset of the input list that would appear on page X

=cut

sub get_subset {
	my $fullset = shift;
	my $page_n = shift || 1;
	my $page_size = shift || get_environment_param('page_size');
	print STDERR "get subset: page number: $page_n; list size = ".scalar(@$fullset)."\n" if $verbose;
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


=head2 get_results_chunk

  Arguments - list, 
              chunk_n          - chunk number; defaults to 1
              chunk_size       - how many items in a chunk;
                                 defaults to $ENV{AMIGO_PAGE_SIZE}
              chunk_by         - what constitutes an 'item'; 
                                 defaults to a list item
  returns   - hash->{n_chunks} - total number of chunks,
              hash->{subset}   - chunk X of the input

=cut

sub get_results_chunk {
	my $fullset = shift;
	my $chunk_args = shift;

	my $chunk_by = $chunk_args->{chunk_by} || 'LIST_ITEM';
	my $chunk_n = $chunk_args->{chunk_n} || 1;
	my $chunk_size = $chunk_args->{chunk_size} || get_environment_param('page_size');

	print STDERR "getting a chunk: n = $chunk_n, size = $chunk_size, by = $chunk_by\nlist size: ".scalar(@$fullset)."\n" if $verbose;

	#	we want everything. No changes required.
	return { n_chunks => 1, subset => $fullset } if $chunk_size eq 'all';

	#	first let's see how many chunks we have
	#	we may need to transform the results slightly to be able to work this out
	my $total_items;      # the number of items in the list
	my $results_to_chunk; # another list to store results in for easy chunking
	my $subset;           # we'll store the results here

	my $transforms = {
		to => {
#			LIST_ITEM => sub {
#				my $list = shift;
#				return $list;
#			},
			DISTINCT_COL_1 => sub {
				my $list = shift;
				my $transformed_list;
				
				return $transformed_list;
			},
		},

		from => {
#			LIST_ITEM => sub {
#				my $list = shift;
#				return $list;
#			},
			DISTINCT_COL_1 => sub {
				my $list = shift;
				my $transformed_list;
				
				return $transformed_list;
			},
		},
	};

	if ($transforms->{to}{$chunk_by}) # we know how to transform the results
	{	$results_to_chunk = $transforms->{to}{$chunk_by}->($fullset);
	}
	else # we'll assume that the list item is what needs to be counted
	{	$results_to_chunk = $fullset;
	}
	$total_items = scalar @$results_to_chunk;

	#	do the calculation for the number of chunks
	my $n_chunks = get_n_chunks($total_items, $chunk_size);

	#	now let's do the chunking itself.
	my ($from, $to) = ($chunk_size * ($chunk_n - 1), $chunk_size * $chunk_n - 1);

	if ($from > $total_items) # our chunk number is beyond the range of viable chunks. Return the last page of results instead
	{	($from, $to) = ($chunk_size * ($n_chunks - 1), $total_items - 1);
	}

	if ($to >= $total_items) {
		$to = $total_items - 1;
	}

	@$subset = @$results_to_chunk[$from..$to];

	#	transform the results back, if necessary
	if ($transforms->{from}{$chunk_by}) # we know how to transform the results
	{	$subset = $transforms->{to}{$chunk_by}->(@$results_to_chunk[$from..$to]);
	}
	
	return { n_chunks => $n_chunks, subset => $subset };

}


=head2 get_n_chunks

  Arguments - no of results, chunk size (optional)
  returns   - number of chunks required to show the results

=cut

sub get_n_chunks {
	my $total_items = shift;
	my $chunk_size = shift || get_environment_param('page_size');
	my $n_chunks = 1;

	print STDERR "Utilities::get_n_chunks: n items = $total_items; chunk size: $chunk_size\n" if $verbose;

	return $n_chunks if $chunk_size eq "all";

#	do the calculation for the number of chunks
	$n_chunks = int($total_items / $chunk_size);
	$n_chunks++ if ($total_items % $chunk_size);

	return $n_chunks;
}

=head2 get_n_pages

  Arguments - no of results, page size (optional)
  returns   - number of pages required to show the results

=cut

sub get_n_pages {
	my $n_accs = shift;
	my $page_size = shift || get_environment_param('page_size');

	print STDERR "n_accs = $n_accs; page size: $page_size\n" if $verbose;

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
	if (defined($ENV{uc("AMIGO_$var")})) {
		return $ENV{uc("AMIGO_$var")};
	}
	return;
}


=head2 get_external_environmental_param

	Get an environment variable from the outside (like CGI)

=cut

sub get_external_environmental_param {

  my $var = shift;

  my $retval = "";
  if (defined($ENV{uc("$var")})) {
    $retval = $ENV{uc("$var")};
  }
  return $retval;
}



=head2 set_message

	Set messages / errors

	Arguments - message hash,
	            message class: fatal, warning or info
	            message type: e.g. 'no_valid_query', 'no_results'
	            what it affects (optional)

	Returns   - new improved message hash
=cut

sub set_message {
	my $error = shift;
	my $class = shift;
	my $type = shift;
	my $affects = shift || [];

	if (!ref($affects))
	{	$affects = [ $affects ];
	}

	push @{$error->{$class}{$type}}, @$affects;

	return $error;
}

#	not used (at the moment)
sub get_message {
	my $error = shift;
	my $class = shift || undef;

	if ($class && $error->{$class})
	{	return $error->{$class};
	}
	return $error;
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

	print STDERR "format: ".$format."\n" if $verbose;

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
			term => ['synonym_list', 'definition', 'dbxref_list', 'subset_list', 'n_deep_products', 'cons_rplc'],
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
#			term => ['definition', 'dbxref_list', '??? n_assocs' ],
			term => ['definition', 'dbxref_list' ],
			gp => ['name'],
			assoc => ['return_graph'],
		},
		fasta => {
			gp => ['gptype', 'spp', 'seq'],
		},
		gp_graph => {
#			term => ['??? definition'],
			term => ['acc'],
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
	{	$tmpl->{$_} = 1 foreach @{$templates->{$format}{$what_to_get}};
		if (!$gp_count_ok && ($tmpl->{n_deep_products} || $tmpl->{n_products}))
		{	#	if it's not ok to get the GP count, delete these fields
			delete $tmpl->{n_deep_products};
			delete $tmpl->{n_products};
		}
	}
	elsif ($templates->{$format})
	{	foreach my $t (keys %{$templates->{$format}})
		{	$tmpl->{$t}{$_} = 1 foreach @{$templates->{$format}{$t}};
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
	{	print STDERR "Utilities::get_tmpl: format $format is unknown!\n" if $verbose;
	}
	
	print STDERR "Utilities::get_tmpl: format: ".$format."\ntmpl: ".Dumper($tmpl)."\n" if $verbose;
	
	return $tmpl;
}

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

Args: $data
      $format
      

=cut

sub render_data_in_format {
	my $data = shift;
	my $format = shift;
	return unless $data;

#	from GP/term assocs
	if ($format eq 'rdfxml')
	{	my $show_assoc = shift;
		print "Content-type:text/plain\n\n";
		my $out = new FileHandle(">-");
#		$data->to_xml(-output=>$out, -show_associations=>$show_assoc);
		$data->to_xml($out, -show_associations=>$show_assoc);
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

sub create_apph {
#	my $dbname = get_environment_param("dbname") || 'go';
#	my $dbhost = get_environment_param("dbhost") || 'localhost';
#	my $dbuser = get_environment_param("dbuser");

	# DBPASS and DBAUTH both work.
#	my $dbpass = get_environment_param("dbpass");
#	my $dbauth = get_environment_param("dbauth");

#	my $dbsocket = get_environment_param("dbsocket");
#	my $dbport = get_environment_param("dbport");

	my $dbname = $ENV{GO_DBNAME} || 'go';
	my $dbhost = $ENV{GO_DBHOST} || 'localhost';
	my $dbuser = $ENV{GO_DBUSER};

	# DBPASS and DBAUTH both work.
	my $dbpass = $ENV{GO_DBPASS};
	my $dbauth = $ENV{GO_DBAUTH};

	my $dbsocket = $ENV{GO_DBSOCKET};
	my $dbport = $ENV{GO_DBPORT};

	my %params = (-dbname => $dbname,
		      -dbhost => $dbhost,
		     );

	if (defined ($dbuser) &&
	    $dbuser ne '' ) {
	  $params{-dbuser} = $dbuser;}
	# DBPASS and DBAUTH both work.
	if (defined ($dbpass) &&
	    $dbpass ne '' ) {
	  $params{-dbauth} = $dbpass; }
	if (defined ($dbauth) &&
	    $dbauth ne '' ) {
	  $params{-dbauth} = $dbauth; }

	if (defined ($dbsocket) &&
	    $dbsocket ne '' ) {
	  $params{-dbsocket} = $dbsocket; }
	if (defined ($dbport) &&
	    $dbport ne '' ) {
	  $params{-dbport} = $dbport; }

#	print STDERR "apph connection params: ".Dumper(\%params);

	my $apph;
	eval {
		$apph = GO::AppHandle->connect(%params);
	};
	if ($@)
	{	#print STDERR "Could not create apph: $@\n";
		my $error = set_message(undef, 'fatal', 'config_error', $@);
		output_template({error => $error});
		exit;
	}
	return $apph;
}

=head2 url_encoder {

Returns a string or a hash of strings to be used in URLs or inputs in templates

Args: $apph
      $format
      

=cut

sub url_encoder {
	my $apph = shift;
	my $format = shift || 'url';
	return if !$apph->filters;
	
	my %url_h;
	foreach (keys %{$apph->filters})
	{	if ($apph->filters->{$_} && @{$apph->filters->{$_}})
		{	$url_h{$_} = join(",", @{$apph->filters->{$_}});#map { $_ =~ s/,/\,/g } @{$apph->filters->{$_}});
		}
	}

	if ($format eq 'input')
	{	#	we need to return a set of strings
		return \%url_h;
	}
	else
	{	return join("&amp;", map { $_ ."=". $url_h{$_} } keys %url_h);
	}

}

=head2 url_decoder {

Decodes the filter settings in a URL (from a cgi) and sets the apph accordingly

Args:    $apph
         $cgi
      
Returns: $apph with the filters set
=cut

sub url_decoder {
	my $apph = shift;
	my $cgi = shift || return;

	my $valid_filters = get_valid_filters() || ['ont', 'assby', 'evcode', 'gptype', 'taxid', 'qual', 'speciesdb' ];
	my %params = $cgi->Vars;
	foreach (@$valid_filters)
	{	if ($params{$_})
		{	#	may be \0 or , separated
			my @list = split(/\0|,/, $params{$_});
		}
	}
}


=head2 get_bookmark

If possible, assembles a bookmark from the GET.

Returns: URL or ""

=cut
sub get_bookmark {

  my $bookmark = "";
  if ( 'GET' eq get_external_environmental_param('REQUEST_METHOD') ){

    ## Check that we're on the right port.
    my $port = "";
    my $incoming_port = get_external_environmental_param('SERVER_PORT');
    if( $incoming_port && $incoming_port ne '80' ){
      $port = ':' . $incoming_port;
    }

    $bookmark =
      'http://' . get_external_environmental_param('SERVER_NAME') . $port .
	get_external_environmental_param('SCRIPT_NAME') . '?' .
	  get_external_environmental_param('QUERY_STRING');
  }

  return $bookmark;
}


=head2 output_template {

Populates various useful variables from the environment,
then outputs an html page

Args:    $template_vars    # any data to go in the template
         $template_to_use  # template name, defaults to amigo_message
      
=cut

sub output_template {
	my $template_vars = shift;
	my $template_to_use = shift || 'amigo_message';

	if ($template_vars->{error} && keys %{$template_vars->{error}})
	{	$template_vars->{show_message_box} = 1;
	}

	my $tmpl_vars = 
	  {
	   %$template_vars,
	   page_name        => $template_to_use,
	   amigo_url        => get_environment_param('cgi_url'),    # environment
	   html_url         => get_environment_param('html_url'),   # environment
	   show_blast       => get_environment_param('show_blast'),         # env
	   show_graphviz    => get_environment_param('show_graphviz'),      # env
	   show_goose_links => get_environment_param('show_goose_links'),   # env
	   TROUBLE => get_environment_param('trouble_switch'),   # env
	   TROUBLE_MESSAGE => get_environment_param('trouble_message'),   # env
	   BETA => get_environment_param('beta'),   # env
	   image_dir => get_environment_param('html_url').'/images', # env
	   bookmark         => get_bookmark(),

	   ## Cram in some new AmiGO stuff...
	   GOOGLE_ANALYTICS_ID => $google_id,
	   version => $version,
	   VERSION => $version,
	};

	my $template_paths = get_environment_param('template_paths');
	my $tt = Template->new({
		INCLUDE_PATH=>$template_paths,
	#	PRE_CHOMP=>1,
	#	POST_CHOMP=>1,
	#	RECURSION=>1,
		TRIM=>1,
	#	DEBUG_ALL => 1,
	});

	print "Content-type:text/html\n\n";
	print STDERR "Utilities::output_template: Using template $template_to_use\n" if $verbose;
	$tt->process($template_to_use.".tmpl", $tmpl_vars);
}

1;
