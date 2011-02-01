package GO::CGI::Search;

use strict;
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);

@ISA = ('Exporter');
@EXPORT_OK = qw(new success get_msg results get_results_from_cache get_results_from_db);
%EXPORT_TAGS = (
	std => \@EXPORT_OK,
);

#use Carp;
use DBI;
use GO::AppHandle;
use GO::CGI::Utilities qw(:all);
use GO::SqlWrapper qw(sql_quote select_hashlist);
use HTML::Entities;

use Data::Dumper;
$Data::Dumper::Indent = 1;
#use Time::HiRes qw(gettimeofday);

use GO::Object::TermSearchResult;
use GO::Object::GeneProductSearchResult;

use GO::CGI::Query qw(get_gp_details get_term_in_graph get_seqs_for_gps get_gp_count_for_terms get_term_count_for_gps get_consider_and_replaced_by_terms);

## Some new stuff to pile on to help with debugging.
#use AmiGO;
#my $core = AmiGO->new();

our $verbose = get_environment_param('verbose');

=head2 new

	Arguments - arg_h with 1 / 0 for 'clever_mode', 'get_relevance',
	            and 'cache_me' (whether or not to set the cache)
	            defaults to having these on if nothing is specified
	Returns   - search object

=cut


sub new {
	my $class = shift;
	my $self = {};
	bless $self, $class;
	my $arg_h = shift;

	foreach ('clever_mode', 'get_relevance', 'cache_me')
	{	unless (exists $arg_h->{$_} && $arg_h->{$_} == 0)
		{	$self->{$_} = 1;
		}
	}
	return $self;
}

=head2 cache

	Gets or sets the result cache

=cut

sub cache {
	my $self = shift;
	if (@_)
	{	$self->{cache} = shift;
	}
	return $self->{cache};
}

=head2 apph

	Gets or sets the apph

=cut

sub apph {
	my $self = shift;
	if (@_)
	{	$self->{apph} = shift;
	}
	return $self->{apph};
}

=head2 get_param

	Gets a parameter of the Search object

=cut

sub get_param {
	my $self = shift;
	my $p = shift;
	return $self->{params}{$p} || get_environment_param($p) || undef;
}

=head2 set_param

	Sets a parameter of the Search object

=cut

sub set_param {
	my $self = shift;
	my $p = shift;
	my $x = shift;
	$self->{params}{$p} = $x;
}

=head2 get_query_param

	Method for external programs to get the query parameters

=cut

sub get_query_param {
	my $self = shift;
	my $q_type = shift || 'parsed';
	return $self->{query}{$q_type};
}

=head2 get_result_param

	Method for external programs to get the result parameters

=cut

sub get_result_param {
	my $self = shift;
	my $r_type = shift;
	return $self->{results}{$r_type} || undef;
}

=head2 set_msg

	Set messages / errors

	Arguments - self
	            message class: fatal, warning or info
	            message type: e.g. 'no_valid_query', 'no_search_results'
	            what it affects (optional)

	Updates self->{error} with the message

=cut

sub set_msg {
	my $self = shift;
	$self->{error} = set_message($self->{error}, @_);

#	print STDERR "\@_ = ".Dumper(\@_) if $verbose;
#	print STDERR "self->{error} = ".Dumper($self->{error})."\n" if $verbose;

}

=head2 get_msg

	Retrieve messages / errors

	Arguments - self
	            message class (optional)
	Returns the messages of that class (if there are any), or all messages

=cut

sub get_msg {
	my $self = shift;
	return get_message($self->{error}, @_);
}

=head2 success

	Get or set the success state of the search

	Arguments - search object, 1 or 0 (optional)
	Returns   - 1 or undefined;
	            1 means successful search,
	            undef means there was some sort of error

=cut

sub success {
	my $self = shift;
	if (@_)
	{	$self->{success} = shift;
	}
	return $self->{success};
}

=head2 n_results

	Get or set the number of results

	Arguments - search object, number of results (optional)
	Returns   - number of results or 0 if unset

=cut

sub n_results {
	my $self = shift;
	if (@_)
	{	$self->{n_results} = shift;
	}
	return $self->{n_results} || 0;
}

=head2 results

	Arguments - search object, results set (optional)
	Returns   - results set

=cut

sub results {
	my $self = shift;
	if (@_)
	{	$self->{results} = shift;
	}
	return $self->{results};
}

=head2 getResultList

	The basic function which is called by search.cgi to retrieve search results.

	Arguments:  search object, apph, the query, option_h containing:
	            - 
	Returns:    a list of search results
	            
	The search object will be updated with information about
	the query (retrievable through get_query_param) and about
	the results (get_result_param). Any messages about the
	search can be accessed via get_msg

=cut

sub getResultList {
	my $self = shift;
	my $args = shift;
	my ($apph, $query, $option_h) = ($args->{apph}, $args->{query}, $args->{option_h});
	$self->{error} = $args->{error};
	
#	my ($apph, $query, $option_h) = @_;
#	$self->{error} = $option_h->{error};

	if ($verbose)
	{	print STDERR "query: ". Dumper($query);
		foreach (keys %$option_h)
		{	print STDERR "$_: ".Dumper($option_h->{$_}) unless $_ eq 'cache';
		}
	}

	if (!$apph)
	{	$self->set_msg('fatal', 'missing_apph');
#		$self->success(0);
		return;
	}
	
	if (!$query)
	{	$self->set_msg('fatal', 'missing_query');
		print STDERR "No query found\n" if $verbose;
#		$self->success(0);
		return;
	}
	
#	set up the important stuff
	$self->set_param('query', $query);
	$self->apph($apph);
	my $sc = $option_h->{search_constraint};
	$self->set_param('search_constraint', $sc);
	
	#	if we've got a cache, check everything is in order
	#	and set the search params from it
	if ($option_h->{cache})
	{	my $cache = $option_h->{cache};
		## get the data from the cache
		$self->_set_search_fields($cache->{$sc.'fields'});

	#	check the query and turn it into a structure that we can use
		my $success = $self->_set_query($query);
		return if !$success;

		$self->{query}{input} = $self->get_param('query');
		$self->{query}{parsed} = $cache->{query}{parsed};
		$self->{query}{perl} = $cache->{query}{perl};

		my @perl_qlist = 
		map {
			[
			map { 
				if ($self->{query}{perl}{$_})
				{	qr/$self->{query}{perl}{$_}/i;
				}
				else
				{	qr/$_/i;
				}
			} @$_ ];
		} @{$self->{query}{parsed}};
	
		$self->{query}{perllist} = \@perl_qlist;

		if ($verbose)
		{	foreach my $qset qw(parsed perl perllist sql unmatched)
			{	print STDERR "CACHED $qset query: ".Dumper($self->{query}{$qset})."\n";
			}
		}

		$self->{from_cache} = 1;
		$self->cache($cache);
		print STDERR "Using results from cache...\n" if $verbose;
	}

	#	if there's no cache or something has gone wrong
	if (!$self->{from_cache})
	{	
	#	check the query and turn it into a structure that we can use
		my $success = $self->_set_query($query);
		return if !$success;
	
	#	find out what fields we're going to search
	#	if nothing is specified, use the default
		$self->_set_search_fields($option_h->{$sc."fields"});
	}

	foreach ($sc.'sort', 'exact_match', 'use_paging', 'page_size', 'page', 'show_gp_counts', 'show_term_counts') #, 'format')
	{	$self->set_param($_, $option_h->{$_}) if $option_h->{$_};
	}
	$self->set_param('gp_count_ok', $option_h->{gp_count_ok});
	$self->set_param('ont_list', $option_h->{ontology_list});

	#	set the filters
	$self->_set_filters;

	if ($option_h->{action})
	{	if ($option_h->{action} eq 'sort')
		{	$self->set_param('sort_me', 1);
			$self->set_param('page', 1);
		}
		elsif ($option_h->{action} eq 'search')
		{	$self->set_param('new_search', 1);
		}
	}

	my $results;
	if ($self->{from_cache})
	{	$results = $self->get_results_from_cache || $self->get_results_from_db;
	}
	else
	{	$results = $self->get_results_from_db;
	}
#	print STDERR "results: ". Dumper($results). "\n" if $verbose;
	
	#print STDERR "query params: ".Dumper($self->{query})."\n" if $verbose;
	
	return $results;
}

=head2 get_results_from_db

Search method. Dispatches the initial search and if that produces no results,
activates 'clever mode' searching.

=cut

sub get_results_from_db {
	my $self = shift;
	my $apph = $self->apph;
	my $dbh = $apph->dbh;

	print STDERR "\nStarting db search...\n" if $verbose;

	my $sc = $self->get_param('search_constraint');
	my $search_fields = $self->get_param('search_fields');

	#	do the search!
	my $results = $self->search;

#	print STDERR "Results data structure:\n".Dumper($results)."\n" if $verbose;
	
	if (!@$results && $self->{clever_mode} == 1)
	{	#	no results! uh-oh. see what fields we've searched and try
		#	searching some other fields
		print STDERR "No results found: entering clever mode!\n" if $verbose;

		#	look at the fields we haven't search and examine them for possible matches
		my @poss = grep { !$search_fields->{$_} } @{__search_field_list($sc, 'all')};

		if (@poss)
		{	print STDERR "fields to search: ".join(", ", @poss)."\n" if $verbose;
			my $temporary_search_fields;
			foreach (@poss)
			{	$search_fields->{$_} = 1;
				$temporary_search_fields->{$_} = 1;
			}
			$self->_set_search_fields([keys %$temporary_search_fields]);
			$results = $self->search;
		}
		#	other things we could do in a future smart search:
		#	turn off exact match if it's on
		#	try a name / acc / symbol search for the other search constraint

		$self->_set_search_fields([keys %$search_fields]);
	}

	if (!@$results)
	{	print STDERR "No results found\n" if $verbose;
		$self->n_results(0);
	#	$self->success(0);
		return;
	}
	
	#	we have results. set how many results we have in n_results
	$self->n_results( scalar @$results );
	print STDERR "n_results: ".$self->n_results."\n" if $verbose;

	if (scalar @$results == 1 && ($sc eq 'gp' || $sc eq 'term'))
	{	print STDERR "results: ".Dumper($results) if $verbose;
		#	if we only have one result, load up the details page for that result.
		#	get the ID that we want to do the redirect with
		my $id = $self->_get_id_for_url($results);
		$self->set_param('single_result', { id => $id, search_constraint => $sc } );
		print STDERR "id: $id\n" if $verbose;
		return;
	}

	#	see whether we found results for all our queries
	if (%{$self->{query}{unmatched}})
	{	#	We didn't find matches for everything.
		#	Add a warning message about queries that we didn't find a match for
		$self->set_msg('warning', 'no_search_results', [ map { join(" ", @$_) } values %{$self->{query}{unmatched}} ]);
	}

	#	if this looks like a new query, check if we've got too many results or not
	#	if we have, don't sort the results as the user might not want 'em
	if ($self->get_param('new_search'))
	{	#	get the maximums for downloads and number of pages that users are 
		#	allowed to see. Check the number of results against that figure.
		my $max_html = $self->get_param('max_results_html');
#		my $max_dl = $self->get_param('max_results_download'); # formatted DLs not yet implemented

		#	we'll use the max downloads figure as the maximum that people are
		#	allowed to see, full stop.
		my $max_allowed = $self->get_param('max_results_download');
#		print STDERR "Max_html: ".$max_html."\n";
#		print STDERR "Max_dl: ".$max_dl."\n";
		if ($self->n_results > $max_html) # too many results to display as HTML
		{	if ($self->n_results > $max_allowed)
			{	$self->set_param('large_result_set', 'too_large');
			}
			else
			{	$self->set_param('large_result_set', 1);
				$self->_set_cache_results($results);
			}
			return;
		}
	}

	#	sort the hash and get the subset we need
	my $sorted = $self->_sort_results($results);
	$self->_set_cache_results($sorted);
#	print STDERR "sorted results data structure:\n".Dumper($sorted)."\n" if $verbose;

	#	get the subset of terms and gps we want to look at
	if ($self->get_param('use_paging') && $self->get_param('use_paging') == 1)
	{	#print STDERR "Use paging is ON!\n";
		my $paged_results = get_results_chunk($sorted, 
		{	chunk_size => $self->get_param('page_size'),
			chunk_n => $self->get_param('page'),
			chunk_by => $self->get_param('chunk_by') });
		$sorted = $paged_results->{subset};
		$self->set_param('n_pages', $paged_results->{n_chunks});
	}

	print STDERR "Done apph method.\n" if $verbose;

#	print STDERR "sorted:\n".Dumper($sorted)."\n" if $verbose;
	return $self->get_result_details($sorted);
}

=head2 get_results_from_cache

Retrieves search results based on data from the cache

=cut

sub get_results_from_cache {
	my $self = shift;
	my $cache = $self->cache;
	my $sc = $self->get_param('search_constraint');
	my $result_list = $cache->{result_list};
	print STDERR "Getting the results from the cache!\n" if $verbose;

	if ($self->get_param('sort_me') && $self->get_param($sc.'sort') && $self->get_param($sc.'sort') eq 'rel')
	{	### TO DO : write new subroutine to sort by relevance
		#print STDERR "Ooops! Need to write this sub.\n";
		#	have a look at our cached results,
		#	see if we already have the relevance info
		
#		if (!$result_list->[0]{src}[1])  # position of the relevance score
#		{	#	Uh-oh! We need to find out the relevance scores.
#			$sorted = $self->get_relevance_of_cached_results;
#		}
		#	leave this like this for now.
		$self->{results} = $cache;
	}
	else
	{	$self->{results} = $cache;
	}

	$self->n_results( scalar @$result_list );
#	my $n_pages = get_n_pages($self->n_results, $self->get_param('page_size'));
#	$self->set_param('n_pages', $n_pages);
#	my $page_to_get = $self->get_param('page') || 1;
#	print STDERR "num page = $n_pages; result_list size: ".scalar @$result_list."\n" if $verbose;

	my $sorted;

	my $apph = $self->apph;
	my $dbh = $apph->dbh;

	my $tables = __search_sql_data($sc, 'results', 'tables');
	my $select = __search_sql_data($sc, 'results', 'select_str');
	my $where = "";


	#	if use_paging is ON and there's no sorting to be done,
	#	load up the subset for the page
	my $got_subset;
	if ($self->get_param('use_paging') && !$self->get_param('sort_me'))
	{	# get the subset for that page
		print STDERR "Getting subset for page ".$self->get_param('page')."\n" if $verbose;
		my $paged_results = get_results_chunk($result_list, 
		{	chunk_size => $self->get_param('page_size'),
			chunk_n => $self->get_param('page'),
			chunk_by => $self->get_param('chunk_by') });
		$result_list = $paged_results->{subset};
		$self->set_param('n_pages', $paged_results->{n_chunks});
		$got_subset = 1;
	}
	else
	{	#	sort queries or page_size == all require the whole data set
		print STDERR "Found parameter sort, format or all\n" if $verbose;

		#	add extra tables if we have to sort by species
		if ($sc eq 'gp' && $self->get_param('sort_me') && $self->get_param('gpsort') eq 'spp')
		{	$tables .= ", species";
			$where = "species.id = gp.species_id AND ";
			$select = $select.", species.genus, species.species";
#			$order = "ORDER BY species.genus, species.species LIMIT 50";
		}
	} 

	$where .= "$sc.id in (".join(",", map { $_->{id} } @$result_list).")";
	
	my $sql = "SELECT $select FROM $tables WHERE $where";
	print STDERR "sql: $sql\n" if $verbose;

	my $result_h = $dbh->selectall_hashref($sql, 'id');
#	print STDERR "result_h:\n".Dumper($result_h)."\n" if $verbose;

	#	result_list has the correct order
	#	put the data from result_h into sorted
	foreach (@$result_list)
	{	$result_h->{$_->{id}}{source} = $_->{src};
		push @$sorted, $result_h->{$_->{id}};
	}

#	print STDERR "sorted:\n".Dumper($sorted)."\n" if $verbose;
	return $self->get_result_details($sorted) if $got_subset;

	#	if the action was sort, we need to sort the results
	#	and either return the first page OR the whole set
	#	set the newly ordered results as the cache
	if ($self->get_param('sort_me'))
	{	print STDERR "Going into sort subroutine\n" if $verbose;
		$sorted = $self->_sort_results([@$sorted]);
		$self->_set_cache_results($sorted);
	}

	my $paged_results = get_results_chunk($sorted, 
	{	chunk_size => $self->get_param('page_size'),
		chunk_n => $self->get_param('page'),
		chunk_by => $self->get_param('chunk_by')
	});
	$self->set_param('n_pages', $paged_results->{n_chunks});

	return $self->get_result_details($paged_results->{subset});

}


=head2 _set_query

Internal method to set up the various query strings
using the values in the search parameter 'query'

=cut

sub _set_query {
	my $self = shift;

	my $query = $self->get_param('query');
	my $exact =  $self->get_param('exact_match');
	my $sc = $self->get_param('search_constraint');
	my $min_length = __min_q_length($sc, $exact);
	my $search_fields;

#	sort queries by length, descending?
#	ensure queries are unique
#	query length constraints

	my @qlist;     	#	original query
	my @perl_qlist;	#	perl versions of query
	my %sql;       	#	SQL version
	my %sql_regexp;
	my %perl = ();
	
	my @too_short;
	foreach (@$query)
	{	#print STDERR "\$_: $_\n" if $verbose;

		#	remove anything that isn't a digit or a letter
		#	and check the query length is long enough to be valid
		(my $temp = $_) =~ s/[^a-z0-9]//gi;
		if (length $temp < $min_length)
		{	#	this query is too short. Next!
			print STDERR "Rejected $_: too short\n" if $verbose;
			push @too_short, $_;
			next;
		}

		my @search_phrase;
		if ($exact)
		{	@search_phrase = ( $_ );
			$perl{$_} = '^'.$_.'$';
		}
		elsif ($sc eq 'spp')
		{#	do a little bit of checking to see
		#	what kind of query we might have
			if (/^\d+$/)
			{	#	ncbi taxon
				$search_fields->{ncbi_taxa_id} = 1;
			}
			elsif (/^[a-z]\.? .*/i)
			{	#	probably a spp name in binomial format
				$search_fields->{binomial} = 1;
				$_ =~ s/\.//;
				$self->{binomial_abbrev} = 1;
				($self->{query}{perl}{$_} = $_) =~ s/^([a-z]) /$1.*? /i;
			}
			else
			{	#	goodness only knows what this is!
				#	search the common names just in case
				$search_fields->{binomial} = 1 if ($_ =~ / /);
				$search_fields->{common_name} = 1;
			}
			@search_phrase = ( $_ );
		}
		else
		{	my @list = split /\s/, $_;
			print STDERR "words found in query line: ".join(", ", @list)."\n" if $verbose;
			my $c = 0;     # number indicating where the word appears in @list
			my $last = ''; # previous word in @list
			my %words;
			my $colon = 0;
			foreach (@list)
			{	#	add repeated words to the word before
				#	(assume the user actually wanted them)
				if ($last eq $_)
				{	push @{$words{$last." ".$_}}, $c;
					pop @{$words{$last}};
					next;
				}
	
				#	duplicate word
				if ($words{$_})
				{	push @{$words{$_}}, $c;
					$c++;
					next;
				}
	
				#	if there are any words shorter than our min length,
				#	append them to the previous word
				if (length $_ < $min_length)
				{	if ($last ne '')
					{	push @{$words{$last." ".$_}}, pop @{$words{$last}};
						$last .= " $_";
					}
					elsif ($c == 0 && $list[1])
					#	...or prepend it to the next list item
					{	$list[1] = "$_ ".$list[1];
					}
					else
					{	#	this shouldn't ever happen
						if (!$list[1])
						{	print STDERR "Program error: no subsequent list items found!\n" if $verbose;
							last;
						}
						print STDERR "last is undef'd but c != 0: what's going on?!\n" if $verbose;
					}
					next;
				}
				
				push @{$words{$_}}, $c;
				$last = $_;
				$c++;
			}
	
			if (!keys %words)
			{	next;
			}
	
			#	create three versions of the query:
			#	- the [parsed] original query
			#	- an sql version without duplicates and with SQL wildcards
			#	- a perl version 
	
			foreach (keys %words)
			{	if (!@{$words{$_}})
				{	print STDERR "Nothing found for $_\n" if $verbose;
					delete $words{$_};
					next;
				}
	
				#	check for metacharacters
				if (/[^a-z0-9 ]/i)
				{	print STDERR "Found some metacharacters in $_!\n" if $verbose;
	
					#	quote the SQL single char wildcard
					(my $sql = $_) =~ s/_/\_/g;
	
					#	convert any question marks to _
					$sql =~ s/(?<!\\)\?/_/g;
					$sql =~ s/\Q\?\E/?/g;
	
					#	convert any asterisks to _%
					$sql =~ s/(?<!\\)\*+|%+/_%/g;
					#	if the % is at the beginning or end of the word,
					#	change it to _ to force there to be some content
					#	(a % will automatically be added by $self->_set_sql_search_strings)
					$sql =~ s/^_%/_/;
					$sql =~ s/_%$/_/;
	
					$sql =~ s/\Q\*\E/*/g;
	
					$sql{$_} = $sql if $sql ne $_;
	
					if ($sql =~ /[^\w%\*\?\\ ]/i)
					{	# replace with [^[:alnum:] ^[:space:]]?
						(my $sql_r = $sql) =~ s/[^\w%\*\?\\ ]/[^[:alnum:] ^[:space:]]?/gi;
						$sql_regexp{$_} = $sql_r if ($sql_r ne $sql);
						print STDERR "sql_regexp: ".Dumper($sql_regexp{$_})."\n" if $verbose;
					}
=cut
				#	if we have any non-alphanumeric / space characters,
				#	fake a regexp (because sql regexps are SLOOOOOOW)
				if ($sql =~ /[^a-z0-9_%\*\? ]/i)
				{	my @arr = split(/[^a-z0-9_%\*\? ]/, $sql);
					foreach (@arr[0..-2])
					{	$_ .= '_';
					}
					foreach (@arr[1..-1])
					{	$_ = "_$_";
					}
					$sql_regexp{$_} = [@arr];
					print STDERR "sql_regexp: ".Dumper($sql_regexp{$_})."\n" if $verbose;
				}
=cut
					#	perl version
					#	replace non-alphanumeric chars with text
			#		my $perl = $_;
					(my $perl = $_) =~ s/([^\w%\*\?\\ ])/START_NONALPHA$1END_NONALPHA/g;
	
					#	convert wildcards, use non-greedy matching
					$perl =~ s/(?<!\\)\?/./g;
					$perl =~ s/(?<!\\)\*+|%+/\\w+/g;
	
					#	if the alphanumeric char appears at the beginning of the word,
					#	make sure it's not missed out
					$perl =~ s/^START_NONALPHA(.)END_NONALPHA/[\\$1\\W]/;
			#		$perl =~ s/^START_NONALPHA(.)END_NONALPHA/\\$1/;
	
					#	replace text with a regexp for non-alpha chars
					$perl =~ s/START_NONALPHA(.)END_NONALPHA/[\\$1\\W]?/g;
			#		$perl =~ s/START_NONALPHA(.)END_NONALPHA/\\$1/g;
	
					print STDERR "perl: $perl; sql: $sql\n" if $verbose;
					
					$perl{$_} = $perl if ($perl ne $_);
	
					#	if it's in the middle of some words, change the search to 
					#	word1*word2
					#	word1_ AND _word2
					#	perl version: word1[^\W%\*\?]*word2
	#	{	# replace with [^[:alnum:] ^[:space:]]?
	#		(my $sql_r = $sql) =~ s/[^a-z0-9%_\*\? ]/[^[:alnum:] ^[:space:]]?/gi;
	#		$perl =~ s/[^a-z0-9%_\*\? ]/[^a-z0-9%_\*\? ]?/gi;
	#		 = $sql_r if ($sql_r ne $sql);
	#	}
	
				}
			}
	
		#	print STDERR "words: ".Dumper(\%words) if $verbose;
			foreach my $w (keys %words)
			{	foreach (@{$words{$w}})
				{	if ($search_phrase[$_])
					{	print STDERR "Found an entry for $w, $search_phrase[$_]\n" if $verbose;
					}
					$search_phrase[$_] = $w;
				}
			}
		}

		#	check the search phrase doesn't already exist in our list
		if (@qlist)
		{	my $sf = join " ", @search_phrase;
			next if (grep { $sf eq join(" ", @$_) } @qlist);
		}
		
		push @qlist, [ @search_phrase ];
		push @perl_qlist,
		[ map { 
				if ($perl{$_})
				{	qr/$perl{$_}/i;
				}
				else
				{	qr/$_/i;
				}
			} @search_phrase ];
	}

	if (@too_short)
	{	if (!@qlist)
		{	$self->set_msg('fatal', 'query_too_short', $query);
			return;
		}
		$self->set_msg('warning', 'query_too_short', \@too_short);
	}

	if (!@qlist)
	{	$self->set_msg('fatal', 'no_valid_query');
		return;
	}
	elsif (scalar @qlist > 1)
	{	$self->{query}{multi} = 1;
	}

#	if ($sc eq 'spp')
#	{	$session->set_param('1', 'sppfields', [keys %$search_fields])
#	}

	#	set all the query params
	$self->{query}{parsed} = [ @qlist ];

	if (keys %sql || keys %sql_regexp)
	{	$self->{query}{sql} = { sql => \%sql, regexp => \%sql_regexp };
	}
	$self->{query}{perl} = \%perl;
	$self->{query}{perllist} = [ @perl_qlist ];

	my %match_h;
	@match_h{@perl_qlist} = @qlist;
	$self->{query}{unmatched} = \%match_h;

	foreach my $qset qw(parsed perl perllist sql unmatched)
	{	print STDERR "$qset query: ".Dumper($self->{query}{$qset})."\n" if $verbose;
	}

	return 1;
}

=head2 _set_filters

Internal method to set the filters for the search

=cut

sub _set_filters {
	my $self = shift;
	my $apph = $self->apph;
	my $sc = $self->get_param('search_constraint');
	my $dbh = $apph->dbh;

	my $filters = $apph->filters;
	if (!$filters)
	{	return [];
	}
	else
	{	print STDERR "filters:\n".Dumper($filters)."\n" if $verbose;
	}
	my @tables;
	my @where;

	if ($sc eq 'term')
	{	my @extra = ();
		my $onts = $filters->{ont};
		if ($onts) {
			print STDERR "Found ontology filter!\n" if $verbose;
			$onts = [$onts] unless (ref($onts) eq 'ARRAY');
			push @where, "term.term_type IN (".join(",", map { sql_quote($_) } @$onts).")";
		}
	}
	elsif ($sc eq 'gp') {
	#	ontology filter
		my $onts = $filters->{ont};
		if ($onts) {
			print STDERR "Found ontology filter!\n" if $verbose;
			$onts = [$onts] unless (ref($onts) eq 'ARRAY');
			push @tables, "association a", "term";
			push @where, "a.gene_product_id=gp.id", "term.id=a.term_id", "term.term_type IN (".join(",",map{sql_quote($_)}@$onts).")";
		}

		#	species DB
		my $spdbs = $filters->{speciesdb};
		if ($spdbs) {
			$spdbs = [$spdbs] unless (ref ($spdbs) eq 'ARRAY');
	
			push @tables, "dbxref gpx";
			push @where, "gp.dbxref_id=gpx.id", "gpx.xref_dbname IN (".join(",", map{sql_quote($_)} @$spdbs).")";
		}
	
		#	NCBI Taxon IDs
		my $taxids = $filters->{taxid};
		if ($taxids) {
			if (!ref($taxids)) {
				$taxids = [$taxids];
			}
			
			#	convert ncbi taxon ids into gp.species_ids so we don't have to query an additional table
			my $spp_ids = $dbh->selectall_arrayref("SELECT id FROM species WHERE species.ncbi_taxa_id IN (".join(",", @$taxids).")");
			push @where, "gp.species_id IN (".join(",", map { $_->[0] } @$spp_ids).")";

#			push @tables, "species";
#			push @where, "species.id = gp.species_id", "species.ncbi_taxa_id IN (".join(",", @$taxids).")";
		}
	
		#	evidence codes
		my $evcodes = $filters->{evcode};
		if ($evcodes) {
			if (!ref($evcodes)) {
				$evcodes = [$evcodes];
			}
			if (!$filters->{ont})
			{	push @tables, "association a";
				push @where, "a.gene_product_id=gp.id";
			}
			push @tables, "evidence e";
			push @where, "a.id=e.association_id";
			
			my @wanted = grep {$_ !~ /^\!/} @$evcodes;
			my @unwanted = grep {/^\!/} @$evcodes;
			if (@wanted) {
				push(@where, "e.code IN (".join(",", map{sql_quote($_)} @wanted).")");
			}
			if (@unwanted) {
				push(@where, "e.code NOT IN (".join(",",map{sql_quote($_)} @unwanted).")");
			}
		}

		#	gp type filter
		my $types = $filters->{gptype};
		if ($types) {
			if (!ref($types)) {
				$types = [$types];
			}

			#	convert types into gp.type_ids so we don't have to query an additional table
			my $type_ids = $dbh->selectall_arrayref("SELECT id FROM term WHERE acc IN (".join(",", map { sql_quote($_) }@$types).") AND term_type='sequence'");
			push @where, "gp.type_id IN (".join(", ", map { $_->[0] } @$type_ids).")";
			
		}
	}
	if (@tables || @where)
	{	$self->set_param('filters', { tables => \@tables, where => \@where });
	}
}

## not yet implemented!
sub get_relevance_of_cached_results {
	my $self = shift;

}

sub search {
	my $self = shift;
	my $subset = shift || undef; # list of IDs to search within
	my $apph = $self->apph;

	my $dbh = $apph->dbh;
	my $search_fields = $self->get_param('search_fields');
	my $sc = $self->get_param('search_constraint');
	my $filters = $self->get_param('filters') || undef;
	my $exact = $self->get_param('exact_match') || undef;

	my $to_check = {
		gp => [ 'full_name', 'symbol'],
		term => ['name', 'acc'],
		spp => [],
	};

	my @fields = @{$to_check->{$sc}};

	my $extra = '';
	if ($subset)
	{	$extra = " AND $sc.id IN (".join ",", @$subset.")";
	}

	#	hash containing the search phrases for the different search types
	my $sql_search_strings = $self->_set_sql_search_strings;
	
	print STDERR "SQL search strings: ".Dumper($sql_search_strings)."\n" if $verbose;
	
	#	if our search list contains anything *other* than full_name and symbol,
	#	we search for these first
	my $other_matches;
	my @names;
	my @others;
	my $combo_fields;

	if ($sc eq 'gp' || $sc eq 'term')
	{	foreach my $s (keys %$search_fields)
		{	if ( grep { $_ eq $s } @fields)
			{	push @names, $s;
			}
			else
			{	push @others, $s;
			}
		}
	}
	
	if ($sc eq 'term')
	{	if ($search_fields->{definition} && $search_fields->{comment})
		{	$combo_fields = 1;
			$sql_search_strings->{def_comm} = "(".$sql_search_strings->{definition}.") OR (". $sql_search_strings->{comment}.")";
			$search_fields->{def_comm} = 1;
			delete $search_fields->{definition};
			delete $search_fields->{comment};
		}
		if ($search_fields->{subset})
		{	$combo_fields = 1;
		}
	}

	if (@others)
	{	print STDERR "Checking fields other than name and symbol/acc...\n" if $verbose;
		my $base = __search_sql_data($sc, 'base');

		my $sql =
			join(" UNION ", 
				map {
					my $data = __search_sql_data($sc, $_);
					my $q = 
					"SELECT ".$base->{"select_str"}.", ".$data->{"select_str"}.
					" FROM ".$base->{tables}.", ".$data->{tables}.
					" WHERE ".$data->{table_join_sql}.
					" AND (".$sql_search_strings->{$_}.")".$extra;
					$_ = $q;
				} @others);

		print STDERR "sql: $sql\n" if $verbose;
		my $sth = $dbh->prepare($sql);
		$sth->execute();

		if (!$combo_fields)
		{	while (my $d = $sth->fetchrow_arrayref) {
				push @{$other_matches->{$d->[0]}{$d->[1]}}, $d->[2];
			}
		}
		else
		{	while (my $d = $sth->fetchrow_arrayref) {
			#	print STDERR "next: ".Dumper($d)."\n" if $verbose;
				if ($d->[1] eq 'term_definition_or_comment')
				{	push @{$other_matches->{$d->[0]}{definition}}, $d->[2] if $d->[2];
					push @{$other_matches->{$d->[0]}{comment}}, $d->[3] if $d->[3];
				}
			#	elsif ($d->[1] eq 'seq')
			#	{	push @{$other_matches->{$d->[0]}{seq_name}}, $d->[2] if $d->[2];
			#		push @{$other_matches->{$d->[0]}{seq_xref}}, $d->[3] if $d->[3];
			#	}
				elsif ($d->[1] eq 'subset')
				{	push @{$other_matches->{$d->[0]}{subset_acc}}, $d->[2] if $d->[2];
					push @{$other_matches->{$d->[0]}{subset_name}}, $d->[3] if $d->[3];
				}
				else
				{	push @{$other_matches->{$d->[0]}{$d->[1]}}, $d->[2];
				}
			}
		}
		
		if (!keys %$other_matches)
		{	print STDERR "No results found in other fields.\n" if $verbose;
		}
	}
	
	#	now get the information for what we found in the query above,
	#	and do the name / symbol / acc search
	#	apply filters to this search
	my @tables = ( __search_sql_data($sc, 'results', 'tables') );
	my @select = ( __search_sql_data($sc, 'results', 'select_str') );
	my @where;
	my @srch;
	
#	print STDERR "Filters: ".Dumper($filters) if $verbose;
	
	if ($filters)
	{	if ($filters->{tables})
		{	push @tables, @{$filters->{tables}};
		}
		if ($filters->{where})
		{	push @where, @{$filters->{where}};
		}
	}
	
	if ($sc eq 'gp')
	{	foreach (@fields)
		{	push @srch, $sql_search_strings->{$_} if $sql_search_strings->{$_};
		}
		#	retrieve species info if the sort parameter is species
		if ($self->get_param('gpsort') && $self->get_param('gpsort') eq 'spp')
		{	if (!grep { /species/ } @tables)
			{	push @tables, "species";
				push @where, "species.id = gp.species_id";
			}
			push @select, "species.genus, species.species";
		}
	}
	elsif ($sc eq 'term')
	{	foreach (@fields)
		{	push @srch, $sql_search_strings->{$_} if $sql_search_strings->{$_};
		}
	}
	# NEW
	elsif ($sc eq 'spp')
	{	@srch = map { $sql_search_strings->{$_} } keys %$search_fields;
		push @select, "CONCAT(genus,' ',species) AS binomial, COUNT(gp.id) AS gp_count";
		push @tables, 'gene_product gp';
		$extra .= ' AND gp.species_id=species.id GROUP BY species.id';
	}
	
	if (keys %$other_matches)
	{	push @srch, $sc.".id IN (".join(",", keys %$other_matches).")";
	}
	
	my $results;
	my %obs_term_ids;
	if (@srch)
	{	(scalar @srch > 1)
		? push @where, "((".join(") OR (", @srch)."))"
		: push @where, "(".$srch[0].")";
	
		my $sql =
				"SELECT DISTINCT ".join(", ", @select).
				" FROM ".join(", ", @tables).
				" WHERE ".join(" AND ", @where).$extra;
		print STDERR "sql: $sql\n" if $verbose;
		
		print STDERR "Main search: retrieving info and searching on full name/symbol\n" if $verbose;
		
#		my $results = $dbh->selectall_arrayref($sql);
		my $sth = $dbh->prepare($sql);
		$sth->execute();

		my $query = $self->{query}{perllist};
		my $field_list = $self->get_param('field_list');
		
#		print STDERR "fieldlist: ".Dumper($field_list)."\n" if $verbose;
		if (!$query || !$field_list)
		{	print STDERR "WARNING! No query or fieldlist found!\n" if $verbose;
		}

		my $sub_h = {
			std => sub {
				my $d = shift;
				$results->{$d->{id}} = $d;
				if ($other_matches->{$d->{id}})
				{	while ( my ($key, $val) = each %{$other_matches->{$d->{id}}})
					{	$results->{$d->{id}}{$key} = $val;
					}
				}
			},
			obs_ignore => sub {
				my $d = shift;
			#	print STDERR "Doing ignore on $d...\n" if $verbose;
				return 1 if $d->{is_obsolete} == 1;
			},
			obs_include_commented => sub {
				my $d = shift;
				#$core->kvetch('include_commented');

			#	print STDERR "Doing include_commented on ".$d->{id}."...\n" if $verbose;
				$obs_term_ids{$d->{id}} = 1 if $d->{is_obsolete} == 1;
			},
			get_relevance => sub {
				my $d = shift;
			#	print STDERR "Doing get_relevance on $d...\n" if $verbose;
				my $rel = $self->_get_relevance($results->{$d->{id}}, $sc, $query, $field_list, $exact);
			#	print STDERR $d->{id}." relevance score = ".Dumper($rel)."\n" if $verbose;
				
				if (!$rel)
				{	print STDERR "Error: no relevance scores returned for ".$d->{id}."!\n" if $verbose;
				#	print STDERR "data: ".Dumper($d)."\n" if $verbose;
					delete $results->{$d->{id}};
					return 1;
				}
				else
				{	$results->{$d->{id}}{source} = $rel;
				}
			},
			get_match => sub {
				my $d = shift;
				my $match_fields = $self->_get_match($results->{$d->{id}}, $sc, $query, $field_list);
				if (!$match_fields || !keys %$match_fields)
				{	print STDERR "Error: no matches returned for ".$d->{id}."!\n" if $verbose;
				#	print STDERR "data: ".Dumper($d)."\n" if $verbose;
					delete $results->{$d->{id}};
					return 1;
				}
				else
				{	$results->{$d->{id}}{source} = $match_fields;
				}
			}
		};

		#	put together the subs to perform on each result
		my @subs = ( $sub_h->{std} );

		if ($sc eq 'term' && get_environment_param('obsolete_behaviour') eq 'ignore')
		{	#print STDERR "Adding obs_ignore to the sub list\n" if $verbose;
			unshift @subs, $sub_h->{obs_ignore};
		}
		if ($self->{get_relevance})
		{	#print STDERR "Adding get_relevance to the sub list\n" if $verbose;
			push @subs, $sub_h->{get_relevance};
		}
		else
		{	#print STDERR "Adding get_match to the sub list\n" if $verbose;
			push @subs, $sub_h->{get_match};
		}
		if ($sc eq 'term' && get_environment_param('obsolete_behaviour') eq 'include_commented')
		{	#print STDERR "Adding include commented to the sub list\n" if $verbose;
			push @subs, $sub_h->{obs_include_commented};
		}

		while (my $d = $sth->fetchrow_hashref) {
			foreach (@subs)
			{	last if $_->($d) && $_->($d) == 1;
			}
		}
	}

	if (keys %$results)
	{	#	sort out the data
		print STDERR "Results found; returning data\n" if $verbose; 
		#.Dumper($results)."\n";
	}
	else
	{	print STDERR "No results found. Sob!\n" if $verbose;
	}

	if (keys %obs_term_ids)
	{	print STDERR "Obsolete terms found: ".Dumper([keys %obs_term_ids])."\n" if $verbose;
	
		#	if we have terms, we need to check for obsoletes
		return $self->_obsolete_check($results, [keys %obs_term_ids]);
	}
	return [values %$results];
}

## _set_sql_search_strings creates the bits of SQL used to query the DB
sub _set_sql_search_strings {
	my $self = shift;
	my $search_fields = shift || $self->get_param('search_fields');
	my $query = $self->{query}{parsed};
	my $query_sql = $self->{query}{sql} || {};
	my $sc = $self->get_param('search_constraint');
	my $exact = $self->get_param('exact_match');

	my %srch_h =
	(	# gp fields
		symbol => { table => 'gp', col => ['symbol'] },
		full_name => { table => 'gp', col => ['full_name'] },
		product_synonym => { table => 'synonym', col => ['product_synonym'] },
		seq_name => { table => 'seq', col => ['display_id'] },
#		gpxref => { table => 'dbxref', col => ['xref_dbname', 'xref_key'] },
		# term fields
		name => { table => 'term', col => ['name'] },
		term_synonym => { table => 'term_synonym', col => ['term_synonym'] },
		acc => { table => 'term', col => 'acc' },
		definition => { table => 'term_definition', col => ['term_definition'] },
		comment => { table => 'term_definition', col => ['term_comment'] },
		subset => { table => 'subset', col => ['acc', 'name'] },
		def_comm => { table => 'term_definition', col => ['term_definition', 'term_comment'] },
#		dbxref => { table => 'dbxref', col => ['xref_dbname', 'xref_key'] },
#		xref => { table => 'dbxref', col => ['xref_dbname', 'xref_key'] },
		common_name => { col => ['genus', 'species', 'common_name'] },
		ncbi_taxa_id => { col => ['ncbi_taxa_id'] },
#		binomial => { col => ['genus', 'species'] },
	);

	#	fields to apply the regexp search to
	my @regexp_fields = qw(symbol full_name product_synonym name term_synonym definition);

	my %search_strs;

	#	make a data struct of suitable queries?
	#	

=add

	my $map_hash = {

	binomial => sub {
		return
		map {
			my ($g, $s) = split(/\.?\s/, $_, 2);
			if (length $g == 1)
			{	$g .= "%";
			}
			return "genus $op ".sql_quote($g) .
			" AND species $op ".sql_quote($s.'%');
		} @$query;
	},

	ncbi_taxa_id => sub {
		my $sf = shift; # the search field name
		my @ids = grep { /^\d+$/ } @$query;
		return $hash->default_map(
			__search_sql_data($sc, $sf, 'search_table'),
			__search_sql_data($sc, $sf, 'search_field'),
			$op,
			@ids);
	},
	
	default_map => sub {
		my $table = shift;
		my $field = shift;
		my $op = shift;
		my $queries = shift;
		return unless @$queries;
		
		return
		'('.
		map {
			my $q = $_;
			join ") OR (",
			map {
				my $f = $_;
				if ($exact)
				{	"$table.$f $op 

				}
				else
				{	
					join " AND ",
					map {
						"$table.$f $op %$_%"
					} @$q;
				}
			} @$field;
		} @$queries
		.')';
	},

	default_map_regexp => sub {
		my $table = shift;
		my $field = shift;
		my $op = shift;
		my $queries = shift;
		return unless @$queries;
		
		return
		'('.
		map {
			my $q = $_;
			join ") OR (",
			map {
				my $f = $_;
				join " AND ",
				map {
					$_ = "%".$_."%" if !$exact;
					if ($query
					"$table.$f $op $_"
				} @$q;
			} @$field;
		} @$queries
		.')';
	},

	xref => sub {
		my $f = shift;
		return
		map {
			my @xrefs = split(/:/, $_, 2);
			map { '%'.$_.'%' } @xrefs if !$exact;
			if (!$xrefs[1])  # no colon found
			{	return
				$table->{$sc}{$f}{table}
				.".xref_key $op "
				.sql_quote($xrefs[0]);
			}
			else
			{	return 
				"("
				.$table->{$sc}{$f}{table}
				.".xref_dbname $op "
				.sql_quote($xrefs[0])
				." AND "
				.$table->{$sc}{$f}{table}
				.".xref_key $op "
				.sql_quote($xrefs[1])
				.")";
			}
		} @$query );
		

	acc => sub {
		my $acc_like = $self->_get_accs;
		print STDERR "acc_like: ".Dumper($acc_like)."\n" if $verbose;
		if (@$acc_like)
		{	$search_fields->{acc} = 1;
			$self->_set_search_fields([keys %$search_fields]);
			return 
			map {
				"$srch_h{acc}{table}
				."."
				.$srch_h{acc}{col}
				." $op "
				.sql_quote($_)
			} @$acc_like;
		}
	},

	default => sub {
		

	}
=cut

	if ($sc eq 'spp')
	{	foreach my $f (keys %$search_fields)
		{	my $valid = _query_field_format($query, $f);
			next unless @$valid;

			$search_strs{$f} = join(') OR (', 
				map
				{	my $q = $_->[0];
					if ($f eq 'binomial')
					{	#	split the query string into two parts
						my ($g, $s) = split(/\.?\s/, $q, 2);
						if (length $g == 1)
						{	$g .= "%";
						}
						"genus LIKE ".sql_quote($g) .
						" AND species LIKE ".sql_quote($s.'%');
					}
					elsif ($f eq 'ncbi_taxa_id')
					{	"$f = ".sql_quote($q);
					}
					else
					{	$q .= '%' if !$exact;
						join(' OR ',
							map {
								$q = '%'.$q if $_ eq 'common_name' && !$exact;
								"$_ LIKE " . sql_quote($q);
							} @{$srch_h{$f}{col}});
					}
				} @$valid);
			if (scalar @$valid > 1)
			{	$search_strs{$f} = "(".$search_strs{$f}.")";
			}
		}
		return \%search_strs;
	}

	foreach my $f (keys %$search_fields)
	{	print STDERR "f = $f\n" if $verbose;
		my $valid = $self->_get_valid_search_strings($sc, $f, $query);
		next unless @$valid;
		$search_strs{$f} = join(') OR (', 
			map
			{	my $q = $_;  # $q refers to a query in the list of queries
				#	print STDERR "field: $f; q: ".Dumper($q)."\n" if $verbose;
				if ($f =~ /xref/)
				{	join(' AND ',
						map { __xrefer($_, $sc, $f, $exact) } @$q);
				}
				else
				{	my $table = __search_sql_data($sc, $f, 'search_table')."." || '';
					join(' AND ', 
					map {
						my $qstr = (split ("\0", $_, 2))[1];
						my $op = 'LIKE';
						if ($exact || $f eq 'acc')
						{	$op = '=';
						}
						else
						{	if ($query_sql->{regexp}{$qstr} && grep { $f } @regexp_fields)
							{	$op = 'REGEXP';
								$qstr = $query_sql->{regexp}{$qstr};
							}
							elsif ($query_sql->{sql}{$qstr})
							{	$qstr = '%'.$query_sql->{sql}{$qstr}.'%';
							}
							else
							{	$qstr = '%'.$qstr.'%';
							}
							$qstr =~ s/%{2,}/%/g;
						}
					#	print STDERR "qstr = $qstr\n" if $verbose;

						my $search_fields = __search_sql_data($sc, $f, 'search_field');
					#	if (scalar @$search_fields > 1)
					#	{	
							join(' OR ',
								map {
									$table . "$_ $op " . sql_quote($qstr);
								} @$search_fields);
							
					#	}
					#	else
					#	{	$table . $search_fields->[0] . " $op ".
					#		sql_quote($qstr);
					#	}
					} 
					sort
					map {
						sprintf("%03d", 500 - length($_))."\0".$_;
					} @$q);
				}
			} @$valid);
		if (scalar @$valid > 1)
		{	$search_strs{$f} = "(".$search_strs{$f}.")";
		}
	}

#	if ($sc eq 'term')
#	{	my $acc_like = $self->_get_accs;
#		print STDERR "acc_like: ".Dumper($acc_like)."\n" if $verbose;
#		if (@$acc_like)
#		{	$search_fields->{acc} = 1;
#			$self->_set_search_fields([keys %$search_fields]);
#			$search_strs{acc} = "(".join(') OR (', 
#				map {
#						"$srch_h{acc}{table}.$srch_h{acc}{col} LIKE ".sql_quote($_)
#						} @$acc_like) .")";
#		}
#	}
	print STDERR "search_fields: ".Dumper($self->get_param('search_fields'))."\n" if $verbose;
	return \%search_strs;
}

#	some search params come in a certain format
#	Check and return the search strings that pass the test
sub _get_valid_search_strings {
	my $self = shift;
	my $sc = shift;
	my $field = shift;
	my $query = shift;
	#print STDERR "Starting get_valid_search_strings with field $field...\n";

	my $param_format = {
		spp => {
			ncbi_taxa_id => [
				[ "[0-9]+" ],
			],
		},
		term => {
			acc => [
				["(GO)?[: ]?0{0,6}([1-9][0-9%\?\*]{0,6})", "GO:%07s"],
			],
		},
	};

	if ($param_format->{$sc}{$field})
	{	#	go through the query list and check if any of the queries match
		#	return the list of matching queries

		my @list;
		foreach my $q (@$query)
		{	foreach (@{$param_format->{$sc}{$field}})
			{	my $regexp = $_->[0];
				my $print = $_->[1] || undef;
				my @qlist = @$q;
				my @temp = map { 
					if (/^$regexp$/io)
					{	print STDERR "\$2: $2\n" if $verbose;
						if ($print)
						{	$_ = sprintf("$print", $2);
					#		if ($self->{query}{perl}{$_})
					#		{	$self->{query}{perl}{$_} .= '|'.$_;
					#		}
					#		$self->{query}{formatted_field}{$field} = $_;
							push @{$self->{query}{perllist}}, [ qr/$_/i ];
						}
						$_;
					}
					else 
					{ () }
				} @qlist;
				push @list, [@temp] unless !@temp;
			}
		}
		return \@list || undef;
	}
	else
	{	return $query;
	}
}

## creates the SQL for queries involving dbxrefs
sub __xrefer {
	my ($q, $sc, $f, $exact) = @_;
	print STDERR "q = $q; sc = $sc; f = $f; exact = ".Dumper($exact)."\n" if $verbose;

	my $op = ' LIKE ';
	$op = ' = ' if $exact;
	if ($q =~ /.+:.+/)
	{	my ($db, $key) = split(":", $q, 2);
		$key = '%'.$key.'%' if !$exact;
		return "("
		. __search_sql_data($sc, $f, 'search_table')
		. ".xref_dbname" . $op . sql_quote($db)
		. " AND "
		. __search_sql_data($sc, $f, 'search_table')
		. ".xref_key" . $op . sql_quote($key).")";
	}
	else
	{	$q = '%'.$q.'%' if !$exact;
		return 
		__search_sql_data($sc, $f, 'search_table')
		. ".xref_key" . $op . sql_quote($q);
	}
}

## creates the SQL for queries involving dbxrefs
sub __xrefer_with_extra_data {
	my ($q, $sc, $f, $exact) = @_;
	print STDERR "q = $q; sc = $sc; f = $f; exact = ".Dumper($exact)."\n" if $verbose;

	my $table =
	{	gp =>
		{	gpxref => 
			{	to_check =>
				{	colon => [ 'mgi', 'rgd' ],
					rpt => [ 'ddb', 'fb' ],
					keystart => [ 'ddb', 'fb', 'wb', 'gr' ],
				},
			},
			seq_xref =>
			{	to_check =>
				{	colon => [ 'mgi', 'dip', 'hgnc', 'go' ],
					rpt => [ 'pirsf' ]
				},
			},
		},
	};

=sql queries:
select distinct seqx.xref_dbname from seq_dbxref, dbxref AS seqx where seqx.id = seq_dbxref.dbxref_id and LOCATE(concat(seqx.xref_dbname, ':'), seqx.xref_key) != 0

select distinct seqx.xref_dbname from seq_dbxref, dbxref AS seqx where seqx.id = seq_dbxref.dbxref_id and LOCATE(seqx.xref_dbname, seqx.xref_key) != 0

select distinct seqx.xref_dbname from seq_dbxref, dbxref AS seqx where seqx.id = seq_dbxref.dbxref_id and LOCATE(seqx.xref_dbname, seqx.xref_key) = 0 and seqx.xref_dbname IN ( [results of above query] )

select distinct dbxref.xref_dbname from gene_product, dbxref where gp.dbxref_id = dbxref.id and LOCATE(concat(dbxref.xref_dbname, ':'), dbxref.xref_key) != 0

select distinct dbxref.xref_dbname from gene_product, dbxref where gp.dbxref_id = dbxref.id and LOCATE(dbxref.xref_dbname, dbxref.xref_key) != 0

select distinct dbxref.xref_dbname from gene_product, dbxref where gp.dbxref_id = dbxref.id and LOCATE(dbxref.xref_dbname, dbxref.xref_key) = 0 and dbxref.xref_dbname IN ( [results of above query] )

=cut
	my $op = ' LIKE ';
	$op = ' = ' if $exact;
	if ($q =~ /.+:.+/)
	{	my ($db, $key) = split(":", $q, 2);
		print STDERR "db = $db; key = $key\n" if $verbose;
		if ($table->{$sc}{$f}{to_check})
		{	print STDERR "Found the 'to_check' table\n" if $verbose;
			foreach my $type (keys %{$table->{$sc}{$f}{to_check}})
			{	my @matches = grep { lc $db eq $_ } @{$table->{$sc}{$f}{to_check}{$type}};
				if (@matches)
				{	if ($type eq 'colon')
					{	$key =~ s/^($db)?:?/$db:/i;
					}
					else
					{	$key =~ s/^($db)?:?/$db/i;
					}
				}
			}
		}

		$key = '%'.$key.'%' if !$exact;
		return "("
		. __search_sql_data($sc, $f, 'search_table')
		. ".xref_dbname" . $op . sql_quote($db)
		. " AND "
		. __search_sql_data($sc, $f, 'search_table')
		. ".xref_key" . $op . sql_quote($key).")";
	}
	else
	{	
		if ($q =~ /^(ddb|fb|wb|gr)/i && !$exact && $f eq 'dbxref' && $sc eq 'gp')
		{	return "("
			. __search_sql_data($sc, $f, 'search_table')
			. ".xref_dbname=".sql_quote($1)." AND "
			. __search_sql_data($sc, $f, 'search_table')
			. ".xref_key LIKE ".sql_quote($q.'%').')';
		}

		$q = '%'.$q.'%' if !$exact;
		return 
		__search_sql_data($sc, $f, 'search_table')
		. ".xref_key" . $op . sql_quote($q);
	}
}

sub _get_match {
	my $self = shift;
	my ($data, $sc, $query, $field_list) = @_;

#	print STDERR "field_list: ".Dumper(\@field_list)."\n" if $verbose;
	my @match_fields;
	FIELD_LOOP:
	foreach my $f ( @$field_list )
	{	if ($data->{$f})
		{	my $matchset = $data->{$f};
			if (ref($matchset ) ne 'ARRAY')
			{	$matchset = [ $matchset ];
			}
			foreach my $m (@$matchset)
			{	if ($self->__relevance_algorithm($m, $query, undef, { match_only => 1 }) == 1)
				{	push @match_fields, __fieldname($f);
				#	next FIELD_LOOP;
					last;
				}
			}
		}
	}
#	return @match_fields;
	return { map { $_ => 'y' } @match_fields } || undef;
}

sub _get_relevance {
	my $self = shift;
	my ($data, $sc, $query, $field_list, $exact) = @_;

#	print STDERR "\nget relevance: sc = $sc\n" if $verbose;
#	print STDERR "data: ".Dumper($data)."\n" if $verbose;
#	print STDERR "qset: ".Dumper($query)."\n" if $verbose;

	my $best_match_rel = 0; #	relevance score for the best match
	my $best_match_field;   #	field with the best match
	my $best_match_text;    #	text of the best match
	my $all_rel_data;       #	all the relevance data

	foreach ( @$field_list )
	{	my ($f, $factor) = @$_; # search field, weighting factor
		#	if the factor is less than our current high rel,
		#	skip this field as things ain't gonna get any better
		next if ($factor < $best_match_rel);
		if ($data->{$f})
		{	my $matchset = $data->{$f};
			if (ref($matchset ) ne 'ARRAY')
			{	$matchset = [ $matchset ];
			}
			
			foreach my $m (@$matchset)
			{	my $score = $self->__relevance_algorithm($m, $query, $factor, {relevance => 1}, $exact);
				next if !$score;
			#	print STDERR "score: $score; matchset: $m; match field: ".($fields{$f} || $f)."\n" if $verbose;
				
				$data->{all_rel_data}->{$m} = $score;
				
				if ($score > $best_match_rel)
				{	$best_match_rel = $score;
					$best_match_field = __fieldname($f);
					$best_match_text = $m;
					last if $best_match_rel == 1;
				}
				
#				print STDERR "string: $matchstr\n" if $verbose;
#				QUERY_LIST:
#				foreach my $q (@$perl_list)
#				{#	print STDERR "q: ".Dumper($q)."\n" if $verbose;
#					next if ($score == 0);
#					print STDERR "score: $score\n" if $verbose;
#				}
			}
		}
		else
		{	#print STDERR "$f: No matchset found!\n" if $verbose;
		}
	}

	if ($best_match_rel == 0)
	{	print STDERR "qset: ".Dumper($query)."\n" if $verbose;
		print STDERR "Data: ".Dumper($data)."\n" if $verbose;
		return;
	}
#	elsif ($best_match_rel == 1)
#	{	return [$best_match_field, $best_match_rel, $best_match_text];
#	}
	else
	{	return { $best_match_field => $best_match_rel };
#	{	return [$best_match_field, $best_match_rel, $best_match_text];
	}
}

sub _get_match_score_and_hilite {
	my $self = shift;
	my $matchstr = shift;
	my $field = shift;
	my $query = shift;

	return $self->__relevance_algorithm($matchstr, $query, __search_field_weighting($field), { hilite => 1, relevance => 1 });
}

sub __relevance_algorithm {
	my $self = shift;
	my $original_matchstr = shift;
	my $query = shift;
	my $factor = shift || 1;  #	weighting factor for the field
	my $options = shift || {};
	my $exact = shift;

	my $best_rel_score = 0;   #	best relevance score
	my $matchtxt;             #	best matching text
	my $n;


	QUERY_LIST:
	foreach my $q (@$query)
	{	my $matchstr = $original_matchstr;
		$n = 1;
		foreach (@$q)
		{	next QUERY_LIST unless ($matchstr =~ s/($_)/START_MATCH $n: $1END_MATCH/gi);
			$n++;
		}

		#	check for overlapping matches
		my @matches = split('START_MATCH', $matchstr);
	#	print STDERR "matches: ".Dumper(\@matches)."\n" if $verbose;
		next if grep { /END_MATCH.*?END_MATCH/ } @matches;

		#	delete the query from {query}{unmatched} (if it still exists)
		#	as we have found a match for it
		delete $self->{query}{unmatched}{$q} if $self->{query}{unmatched}{$q};

		if ($options->{match_only})
		{	return 1;
		}

		if ($options->{relevance})
		{	#	if we have a match and the match type is exact, we don't need
			#	to check any of the other queries
			if ($exact)
			{	$best_rel_score = 1;
			#	if the match is exact, end here
				last QUERY_LIST;
			}

			my $score;
			my $match = $matchstr;
#			(my $match = $matchstr) =~ s/\s?(complex|activity)$//;
			#	remove common prefixes and suffixes and the preceding/following space
			$match =~ s/^$_\s// foreach &__common_prefixes;
			$match =~ s/\s$_$// foreach &__common_suffixes;
			
			$match =~ s/START_MATCH (\d+): .*?END_MATCH/START_MATCH$1END_MATCH/g;
			my $b = 0;
			while ( $match =~ /(?<!END_MATCH)\b(?!START_MATCH)/g)
			{	$b++;
			}
		
			my $consec = 0;
			if (scalar @$q > 1)
			{	while ($match =~ /START_MATCH(\d+)END_MATCH(\s+)START_MATCH(\d+)END_MATCH/g)
				{	if ($3 == $1 + 1)
					{	$consec += length($2);
					}
				}
			}
		
			(my $match2 = $matchstr) =~ s/START_MATCH.*?END_MATCH//g;
			my $length_m = length($match2);
			$match2 =~ s/\W//g;
			my $w_chars = length($match2);
		
			my $word_char_weighting = 1;
			my $boundary_weighting = 0.25;
			my $non_word_char_weighting = 0.25;
			
			#	the length is going to be
			#	no. of word chars + no. of boundaries + no. of non-word chars
			#	each of those properties is weighted
		
			my $l_m = $w_chars*$word_char_weighting + $b*$boundary_weighting + ($length_m-($w_chars + $consec))*$non_word_char_weighting;
		
			my $l_q = length (join "", @$q);
		
		#		print STDERR "l_q: $l_q, l_m = $l_m\n" if $verbose;
		#		my $score = (2x$l_q)/(2 x $l_q + $l_m);
			$score = $factor * (1 - ($l_m/($l_q + $l_m)));

			if ($score > $best_rel_score)
			{	$best_rel_score = $score;
				$matchtxt = _hiliter($matchstr) if $options->{hilite};
			#	if the match is exact, end here
				last QUERY_LIST if $score == 1;
			}
		}
		elsif ($options->{hilite})
		{	return _hiliter($matchstr);
		}
	}

	if ($options->{match_only})
	{	return 0;
	}

	if (!$best_rel_score)
	{	return;
	}

	if (!$options->{hilite})
	{	return sprintf("%.4f", $best_rel_score);
	}
	
	return [ $matchtxt, sprintf("%.4f", $best_rel_score) ];
}

sub hilite {
	my $self = shift;
	my $text = shift;
	my $return_orig = shift || 0;
	my $query = shift || $self->{query}{perllist};

	my $matchstr = $text;

	QUERY_LIST:
	foreach my $q (@$query)
	{	foreach (@$q)
		{	next QUERY_LIST unless $matchstr =~ s/($_)/START_MATCH$1END_MATCH/gi;
		}
		my @matches = split('START_MATCH', $matchstr);
		next if grep { /END_MATCH.*?END_MATCH/ } @matches;
		return _hiliter($matchstr);
	}

	return $text if $return_orig;

	return undef;
}

sub _hiliter {
	my $str = shift;
	my $encoded = encode_entities($str);
	$encoded =~ s/START_MATCH( \d+: )?(.*?)END_MATCH/<em class="hilite">$2<\/em>/g;
	return $encoded;
}

sub _obsolete_check {
	my $self = shift;
	my $results = shift;
	my $obs = shift;
	
	#	check what our obsoletes behaviour is
	
	print STDERR "Starting the obsolete check...\n" if $verbose;
	print STDERR "obs terms are ".Dumper($obs)."\n" if $verbose;

	my $apph = $self->apph;
	my $dbh = $apph->dbh;

	if (@$obs){
	  # get the comments for the term;
	  # return only comments with GO IDs in them
	  my @terms_in_comments = ();
	  my %not_in_results = ();
	  my $comments = {};
	  my $comment_id_to_term_acc = {};
	  my $comment_id_to_term_id = {};

	  if (get_environment_param("term2term_metadata_loaded")){
	    #print STDERR "term2term_metadata is loaded!\n" if $verbose;
	    my $rels = $dbh->selectall_arrayref("SELECT id FROM term WHERE acc IN (". join(", ", map { sql_quote($_) } qw(replaced_by consider) ).")");

	    my $sql = "SELECT term2_id, term1_id, relationship_type_id FROM term2term_metadata WHERE term2_id IN ("
	      .join(", ", @$obs)
		.") AND relationship_type_id IN ("
		  .join(",", map { $_->[0] } @$rels).")";

	    my $cons_repl = $dbh->selectall_arrayref($sql);
	    # group the consider/replaced by terms by the
	    # ID of the obsolete term
	    foreach (@$cons_repl){
	      push @{$comments->{$_->[0]}}, $_->[1];
	    }
	    %$comment_id_to_term_id = %$comments;
	  }else{
			my $sql =
					"SELECT term_id, term_comment FROM term_definition WHERE term_id IN ("
					.join(", ", @$obs).") AND term_comment REGEXP ".sql_quote(".*GO:[0-9]{7}.*");
			print STDERR "sql: $sql\n" if $verbose;
	
			$comments = $dbh->selectall_hashref($sql, 'term_id');
		#	transform into term id and acc (id?) of consider term

			print STDERR "Comments: ".Dumper($comments) if $verbose;

			foreach (keys %$comments)
			{	my @cterms = grep { s/.*?(GO:\d{7}).*/$1/g } split(/\s/, $comments->{$_}{term_comment});
				if (@cterms)
				{	$comment_id_to_term_acc->{$_} = \@cterms;
				}
			}
		#	print STDERR "comments: ".Dumper($comments)."\n" if $verbose;
		}

		foreach my $id (@$obs)
		{	delete $results->{$id} if !$comments->{$id};
		}
		

		#	REWRITE!

		#	Delete any terms which don't have a comment with a GOID in it
		COMMENT_LOOP:
		foreach my $id (keys %$comments)
		{	if ($comment_id_to_term_acc->{$id} || $comment_id_to_term_id->{$id})
			{	if ($comment_id_to_term_acc->{$id})
				{	foreach my $c (@{$comment_id_to_term_acc->{$id}})
					{	if ($not_in_results{$c} || !grep { /$c/ } map { $results->{$_}{acc} } keys %$results)
						{	$not_in_results{$c} = 1;
							next COMMENT_LOOP;
						}
					}
				}
				elsif ($comment_id_to_term_id->{$id})
				{	foreach my $c (@{$comment_id_to_term_id->{$id}})
					{	if ($not_in_results{$c} || !$results->{$c})
						{	$not_in_results{$c} = 1;
							next COMMENT_LOOP;
						}
					}
				}
			}
			print STDERR "deleting $id\n" if $verbose;
			delete $results->{$id};
		}
	

	}
	return [values %$results];
}

sub _sort_results {
	my $self = shift;
	my $results = shift;
	my $sc = $self->get_param('search_constraint');

#	print STDERR "results: ".Dumper($results)."\n" if $verbose;

#	if the action is to sort, leave the list in its initial order
#	and sort by the new criteria

#	if the sort criteria is relevance, secondary sort is by the
#	relevant field, i.e. the field in which the match with the
#	search string was found

	my $sortby;
	if ($self->get_param($sc.'sort'))
	{	my $sort_crit = $self->get_param($sc.'sort');
		if ($self->{from_cache})
		{	#	this is a cached search, so leave the list in its current order
			push @$sortby, $sort_crit;
		}
		else
		{	#	this is a new search, so we should sort by other criteria too
			my $default_sort = __sort_default($sc, $self->{get_relevance});
			if ($sort_crit eq $default_sort->[0])
			{	#	the default sorting order is OK here
				$sortby = $default_sort;
			}
			else
			{	#	put the current sort criteria at the top of the list
				#	leave the rest of the list in its current order
				push @$sortby, $sort_crit;
				foreach (@$default_sort)
				{	push @$sortby, $_ if $_ ne $sort_crit;
				}
			}
		}
	}
	else
	{	$sortby = __sort_default($sc, $self->{get_relevance});
	}
	
	unshift @$sortby, 'is_obsolete' if $sc eq 'term';
	
	print STDERR "sortby is ".Dumper($sortby)."\n" if $verbose;

	my %refs;
	@refs{ map { $_->{id} } @$results } = @$results; # cache references

#	print STDERR "keys of refs: ".Dumper([ keys %refs ]) if $verbose;

	my @sorted = map { $refs{(split("\0", $_))[-1]} }
					sort
					map { my $obj = $_;
						join("\0", 
							map
							{	if ($_ eq 'rel')
								{	my @rel_sort = %{$obj->{source}};
									(sprintf( "%05d", (10000 - $rel_sort[1] * 10000)), 
										__field_importance($rel_sort[0]));
								}
								elsif (($_ eq 'binomial' && $sc eq 'spp') || ($_ eq 'spp' && $sc eq 'gp'))
								{	$obj->{genus}." ".$obj->{species};
								}
								else
								{	if (exists $obj->{$_})
									{	lc $obj->{$_};
									}
									else
									{	print STDERR "Error: did not return a value for sorting! $sc, $_, $obj\n" if $verbose;
										print STDERR Dumper($obj)."\n" if $verbose;
										0;
									}
								}
							} @$sortby
						) . "\0".$obj->{id};
					}
					@$results;

	return \@sorted;
}

### Getting objects from the search results

sub get_result_details {
	my ($self, $sorted) = @_;

	if ($self->get_param('search_constraint') eq 'gp')
	{	return $self->_get_gp_details($sorted);
	}
	elsif ($self->get_param('search_constraint') eq 'term')
	{	return $self->_get_term_details($sorted);
	}
	elsif ($self->get_param('search_constraint') eq 'spp')
	{	return $self->_get_spp_details($sorted);
	}
}

sub _get_term_details {
	my $self = shift;
	my $term_ref = shift;

	my $apph = $self->apph;
	my $dbh = $apph->dbh;

	my $search_fields = $self->get_param('search_fields');
	my $query = $self->{query}{perllist};

	if (!@$term_ref)
	{	exit;
	}

#	create a term array since it's easier to update the data in it
	my $terms_by_id;
	my $term_l;
	my $ont_list = $self->get_param('ont_list');
	foreach (@$term_ref)
	{	#print STDERR "term ref looks like this:\n".Dumper($_)."\n" if $verbose;
		my $term = $self->_create_term_search_result_obj($apph, $_);
		$terms_by_id->[$term->id] = $term;
		$term->is_ontology_term(1) if (grep { $_ eq $term->namespace } @$ont_list);
		$term->source($_->{source}) if $_->{source};
#		$term->best_match( [$_->{source}[1], $_->{source}[2]]) if $_->{source};
		$term->{all_rel_data} = $_->{all_rel_data};
		$_ = $terms_by_id->[$term->id];
		push @$term_l, $_;
	}

	# now let's populate our GO::Model::Term objects with
	# other adornments such as synonyms, dbxrefs and definitions
	print STDERR "\nGetting term defs...\n" if $verbose;

	my $sql = "SELECT term_id, term_definition, term_comment FROM term_definition WHERE term_id in (".join(", ", map { $_->id } @$term_ref).")";
	print STDERR "sql: $sql\n" if $verbose;
	my $sth = $dbh->prepare($sql);
	$sth->execute();

	while (my $d = $sth->fetchrow_arrayref) {
		if ($terms_by_id->[$d->[0]]) {
			my $def = $d->[1];
			if (exists $terms_by_id->[$d->[0]]->source->{definition})
			{	$def = $self->hilite($d->[1], 1, $query);
			}
			$terms_by_id->[$d->[0]]->definition($def);

			if ($terms_by_id->[$d->[0]]->source->{comment})
			{	$terms_by_id->[$d->[0]]->comment($self->hilite($d->[2], 1, $query));
			}

			if ($terms_by_id->[$d->[0]]->is_obsolete && !$terms_by_id->[$d->[0]]->comment)
			{	$terms_by_id->[$d->[0]]->comment($d->[2]);
			}
		}
	}

	get_consider_and_replaced_by_terms($apph, $term_l);
	
#	if the search constraint includes syns, load 'em
	if ($search_fields->{term_synonym}) {
		print STDERR "Looking for synonyms...\n" if $verbose;
		my @termset = map { $_->id } grep { exists $_->source->{term_synonym} } @$term_ref;

		if (@termset)
		{	print STDERR "synonyms: termset: ".join(", ", @termset)."\n" if $verbose;
			my $sql =
			"SELECT term_id, term_synonym, term.name FROM term_synonym, term WHERE term.id=term_synonym.synonym_type_id AND term_id in (".join(", ", @termset).")";
			my $sth = $dbh->prepare($sql);
			$sth->execute();

			if ($self->{get_relevance})
			{	while (my $d = $sth->fetchrow_arrayref)
				{	my $match = $self->_get_match_score_and_hilite($d->[1], $d->[2]."_term_synonym", $query);
					$terms_by_id->[$d->[0]]->add_match(['synonym', $d->[2]], $d->[1], @{$match}) unless !$match;
				}
			}
			else
			{	while (my $d = $sth->fetchrow_arrayref)
				{	my $match = $self->hilite($d->[1], 0, $query);
					$terms_by_id->[$d->[0]]->add_match(['synonym', $d->[2]], $d->[1], $match) unless !$match;
				}
			}
		}
	}

	if ($search_fields->{dbxref} || $search_fields->{xref}) {
		print STDERR "looking at xrefs\n" if $verbose;
		my @termset = map { $_->id } grep { (exists $_->source->{dbxref} || exists $_->source->{xref}) } @$term_ref;
		if (@termset)
		{	print STDERR "dbxrefs: termset: ".join(", ", @termset)."\n" if $verbose;
			my $sql=
				"SELECT term_dbxref.term_id, CONCAT(dbxref.xref_dbname,':', dbxref.xref_key), IF(term_dbxref.is_for_definition=1,'def_xref','xref') FROM term_dbxref, dbxref WHERE term_dbxref.dbxref_id = dbxref.id AND term_id in (".join(", ", @termset).")";
			if ($search_fields->{dbxref})
			{	$sql .= " AND term_dbxref.is_for_definition = 0";
			}
		#	$sql .= " ORDER BY xref_dbname, xref_key";
			my $sth = $dbh->prepare($sql);
			$sth->execute();

			if ($self->{get_relevance})
			{	while (my $d = $sth->fetchrow_arrayref)
				{	my $match = $self->_get_match_score_and_hilite($d->[1], $d->[2], $query);
					$terms_by_id->[$d->[0]]->add_match($d->[2], $d->[1], @{$match}) if $match;
				}
			}
			else
			{	while (my $d = $sth->fetchrow_arrayref) {
					my $match = $self->hilite($d->[1], 0, $query);
					$terms_by_id->[$d->[0]]->add_match($d->[2], $d->[1], $match) if $match;
				}
			}
		}
	}


	if ($search_fields->{subset}) {
		print STDERR "looking at subsets\n" if $verbose;
		my @termset = map { $_->id } grep { exists $_->source->{subset} } @$term_ref;
		if (@termset)
		{	print STDERR "subset: termset: ".join(", ", @termset)."\n" if $verbose;
			my $sql = "SELECT term_id, subset.acc, subset.name FROM term_subset, term AS subset WHERE subset.id=term_subset.subset_id AND term_id IN (".join(", ", @termset).") ORDER BY subset.acc, subset.name";
			my $sth = $dbh->prepare($sql);
			$sth->execute();

			if ($self->{get_relevance})
			{	while (my $d = $sth->fetchrow_arrayref) {
					#print STDERR "d: ".Dumper($d)."\n" if $verbose;
					my $acc = $self->_get_match_score_and_hilite($d->[1], 'subset_acc', $query);
					my $name = $self->_get_match_score_and_hilite($d->[2], 'subset_name', $query);
					if ($acc || $name)
					{	#	match score is the greater of $acc->[1] and $name->[1]
						my $match = $acc->[1] || $name->[1];
						if ($name->[1] && $name->[1] > $match)
						{	$match = $name->[1];
						}
						$acc = $acc->[0] || $d->[1];
						$name = $name->[0] || $d->[2];
					#	my $subset = create_term_obj( { name => $name, acc => $acc } );
					#	$terms_by_id->[$d->[0]]->add_subset($subset);
						$terms_by_id->[$d->[0]]->add_subset_match( { name => $name, acc => $acc }, $match);
					}
				}
			}
			else
			{	while (my $d = $sth->fetchrow_arrayref) {
					my $acc = $self->hilite($d->[1], 0, $query);
					my $name = $self->hilite($d->[2], 0, $query);
					if ($acc || $name)
					{	$acc = $d->[1] unless $acc;
						$name = $d->[2] unless $name;
						$terms_by_id->[$d->[0]]->add_subset_match({ name => $name, acc => $acc });
					}
				}
			}
		}
	}

#	if there are no association filters, get the number of associations
	if ($self->get_param('show_term_counts') || $self->get_param('gp_count_ok') == 1)
	{	print STDERR "Getting the deep product count!\n" if $verbose;
		get_gp_count_for_terms($apph, $term_ref, { show_all_ass => 1, use_filters => 1, gp_count_ok => $self->get_param('gp_count_ok') });

#		my $c = {
#			per_term=>1,
#			terms=> $term_ref
#		};
#		my $countl = $apph->get_deep_product_count($c);
#		foreach (@$countl) 
#		{	$terms_by_id->[$_->{term_id}]->n_deep_products($_->{"c"}) if ($terms_by_id->[$_->{term_id}]);
#		}
	}
#	print STDERR "term_h:\n".Dumper($term_h)."\n" if $verbose;

#	print STDERR "Results to return:\n".Dumper($term_ref)."\n" if $verbose;

	return $term_ref;
}

sub _get_gp_details {
	my $self = shift;
	my $gp_ref = shift;

	my $apph = $self->apph;
	my $dbh = $apph->dbh;

	my $search_fields = $self->get_param('search_fields');
	my $query = $self->{query}{perllist};

	# create GP objects and add dbxrefs, species info, etc.
	my $hashref = $dbh->selectall_hashref("SELECT gp.id, dbxref.xref_key AS acc, dbxref.xref_dbname AS speciesdb FROM gene_product gp, dbxref WHERE gp.dbxref_id=dbxref.id AND gp.id IN (".join(",", map{$_->{id}}@$gp_ref).")", "id");

#	create the gp array since it's easier to update the data in it
	my $gps_by_id;

	foreach (@$gp_ref)
	{	if (!$hashref->{$_->{id}})
		{	print STDERR "Could not find info for $_->{id}: something wrong with script?\n" if $verbose;
			next;
		}
		my %hash = (%{$hashref->{$_->{id}}}, %$_);
		$hash{full_name} = $hash{symbol} if !$hash{full_name};
		my $gp = $self->_create_gp_search_result_obj($apph, \%hash);
		foreach ('species_id', 'type_id', 'product_synonym', 'seq_xref', 'seq_name')
		{	$gp->{$_} = $hash{$_} if $hash{$_};
		}
		$gp->source($hash{source}) if $hash{source};
#		$gp->best_match( [$hash{source}->[1], $hash{source}->[2]]) if $hash{source};
	#	print STDERR "gp: ".Dumper($gp)."hash: ".Dumper(\%hash)."\n" if $verbose;
		$gps_by_id->[$_->{id}] = $gp;
		$_ = $gp;
	}

	#	Add species, type, whether or not has sequence
	$apph->_get_product_species($gp_ref);
	$apph->_get_product_types($gp_ref);
	
	use GO::CGI::Query;
	if ((!$search_fields->{seq_name} && !$search_fields->{seq_xref}) || !$self->{from_cache})
	{	get_seqs_for_gps($apph, $gp_ref, 'has_seq');

		if ($search_fields->{seq_name})
		{	my @list = grep { exists $_->source->{seq_name} } @$gp_ref;
			if (@list)
			{	if ($self->{get_relevance})
				{	foreach my $gp (@list)
					{	foreach (@{$gp->{seq_name}})
						{	my $match = $self->_get_match_score_and_hilite($_, 'seq_name', $query);
							$gp->add_match('seq_name', $_, @$match) if $match;
						}
					}
				}
				else
				{	foreach my $gp (@list)
					{	foreach (@{$gp->{seq_name}})
						{	my $match = $self->hilite($_, 0, $query);
							$gp->add_match('seq_name', $_, $match) if $match;
						}
					}
				}
			}
		}
		if ($search_fields->{seq_xref})
		{	my @list = grep { exists $_->source->{seq_xref} } @$gp_ref;
			if (@list)
			{	if ($self->{get_relevance})
				{	foreach my $gp (@list)
					{	foreach (@{$gp->{seq_xref}})
						{	my $match = $self->_get_match_score_and_hilite($_, 'seq_xref', $query);
							$gp->add_match('seq_xref', $_, @$match) if ($match)
						}
					}
				}
				else
				{	foreach my $gp (@list)
					{	foreach (@{$gp->{seq_xref}})
						{	my $match = $self->hilite($_, 0, $query);
							$gp->add_match('seq_xref', $_, $match) if $match;
						}
					}
				}
			}
		}
	}
	else
	{	#	Add sequence!
		my %ph = map { $_->_seqs_obtained(1); $_->id => $_ } @$gp_ref;
#		print STDERR "ph: ".Dumper(\%ph)."\n" if $verbose;
		my @pids = keys %ph;

		if (@pids) {
			my $select = "gene_product_seq.*, seq.id";

			my %gp_seq_name;
			if ($search_fields->{seq_name}) {
				my @list = grep { exists $_->source->{seq_name} } @$gp_ref;				if (@list)
				{	@gp_seq_name{ map { $_->id } @list } = (1) x @list;
					$select .= ", seq.display_id";
				}
			}

			my %gp_seq_xref;
			if ($search_fields->{seq_xref}) {
				my @list = grep { exists $_->source->{seq_xref} } @$gp_ref;
				if (@list)
				{	@gp_seq_xref{ map { $_->id } @list} = (1) x @list;
				}
			}
			
			print STDERR "seq_name: ".(scalar keys %gp_seq_name)."; gp_seq_xref: ".(scalar keys %gp_seq_xref)."\n" if $verbose;

			my $hl = select_hashlist($dbh,
						["gene_product_seq", "seq"],
						["seq.id = seq_id", "gene_product_id in (".join(',', @pids).")"],
						[$select]
						);


			if (keys %gp_seq_xref || keys %gp_seq_name)
			{	my (@seqs, @byid);
				foreach my $h (@$hl)
				{	my $p = $ph{$h->{gene_product_id}};
					my $seq = $apph->create_seq_obj($h);
					$p->add_seq($seq);
					if ($gp_seq_name{ $h->{gene_product_id} })
					{	if ($self->{get_relevance})
						{	my $match = $self->_get_match_score_and_hilite($h->{display_id}, 'seq_name', $query);
							$gps_by_id->[$h->{gene_product_id}]->add_match('seq_name', $h->{display_id}, @{$match}) if $match;
						}
						else
						{	my $match = $self->hilite($h->{display_id}, 0, $query);
							$gps_by_id->[$h->{gene_product_id}]->add_match('seq_name', $h->{display_id}, $match) if $match;
						}
					}
						
					if ($gp_seq_xref{ $h->{gene_product_id} })
					{	print STDERR "Looking for ".$seq->id."\n" if $verbose;
						#	add this sequence to the list of seqs to check
						push(@seqs, $seq->id);
						$byid[$seq->id] = $h->{gene_product_id};
					}
				}

				if (@seqs && @byid)
				{	#	get the sequence IDs of the gps in gpset

					print STDERR "SQL = SELECT seq_id, CONCAT(xref_dbname,':', xref_key) AS seq_xref FROM dbxref, seq_dbxref WHERE dbxref.id=dbxref_id AND seq_id IN (".join(", ", @seqs).")\n" if $verbose;

					my $sth = $dbh->prepare("SELECT seq_id, CONCAT(xref_dbname,':', xref_key) AS seq_xref FROM dbxref, seq_dbxref WHERE dbxref.id=dbxref_id AND seq_id IN (".join(", ", @seqs).")");
					$sth->execute();
					if ($self->{get_relevance})
					{	while (my $d = $sth->fetchrow_arrayref)
						{	my $match = $self->_get_match_score_and_hilite($d->[1], 'seq_xref', $query);
							$gps_by_id->[$byid[$d->[0]]]->add_match('seq_xref', $d->[1], @{$match}) if $match;
						}
					}
					else
					{	while (my $d = $sth->fetchrow_arrayref)
						{#	print STDERR Dumper($_) if $verbose;
							my $match = $self->hilite($d->[1], 0, $query);
							$gps_by_id->[$byid[$d->[0]]]->add_match('seq_xref', $d->[1], $match) if $match;
						}
					}
				}
			} 
			else
			{	foreach my $h (@$hl)
				{	my $seq = $apph->create_seq_obj($h);
					my $p = $ph{$h->{gene_product_id}};
					$p->add_seq($seq);
				}
			}
		}
	}

	if ($search_fields->{product_synonym}) {
		print STDERR "Looking for synonyms...\n" if $verbose;
		my @gpset = grep { exists $_->source->{product_synonym} } @$gp_ref;
		if (@gpset)
		{	if ($self->{from_cache})
			{	#print STDERR "synonyms: gpset: ".join(", ", @gpset)."\n" if $verbose;
				my $sth = $dbh->prepare("SELECT * from gene_product_synonym WHERE gene_product_id in (".join(", ", map { $_->id } @gpset).")");
				$sth->execute();
				if ($self->{get_relevance})
				{	while (my $d = $sth->fetchrow_arrayref)
					{	my $match = $self->_get_match_score_and_hilite($d->[1], 'product_synonym', $query);
						$gps_by_id->[$d->[0]]->add_match('product_synonym', $d->[1], @$match) if $match;
					}
				}
				else
				{	while (my $d = $sth->fetchrow_arrayref)
					{	my $match = $self->hilite($d->[1], 0, $query);
						$gps_by_id->[$d->[0]]->add_match('product_synonym', $d->[1], $match) if $match;
					}
				}
			}
			else
			{	if ($self->{get_relevance})
				{	foreach my $gp (@gpset)
					{	foreach (@{$gp->{product_synonym}})
						{	my $match = $self->_get_match_score_and_hilite($_, 'product_synonym', $query);
							$gp->add_match('product_synonym', $_, @$match) if $match;
						}
					}
				}
				else
				{	foreach my $gp (@gpset)
					{	foreach (@{$gp->{product_synonym}})
						{	my $match = $self->hilite($_, 0, $query);
							$gp->add_match('product_synonym', $_, $match) if $match;
						}
					}
				}
			}
		}
	}
	
	if ($self->get_param('show_gp_counts'))
	{	my $counts = get_term_count_for_gps($apph, $gp_ref, 1);
		print STDERR "counts: ".Dumper($counts)."\n" if $verbose;
		my %count_h = @$counts;
		foreach (@$gp_ref)
		{	if ($count_h{$_->id})
			{	$_->n_terms( $count_h{$_->id} );
			}
			else
			{	$_->n_terms(0);
			}
		}
	}
	return $gp_ref;
}

sub _get_spp_details {
	my $self = shift;
	my $spp_ref = shift;
	my $apph = $self->apph;

	foreach (@$spp_ref) {
		$_->{gp_count} = $_->{spp}{gp_count};
		my $spp = $apph->create_species_obj($_->{spp});
		$_->{spp} = $spp;
	}
	return $spp_ref;
}

sub _create_term_search_result_obj {
	my $self = shift;
	my $apph = shift;
	my $term = GO::Object::TermSearchResult->new(@_);
	$term->apph( $apph->apph );
	return $term;
}

sub _create_gp_search_result_obj {
	my $self = shift;
	my $apph = shift;
	my $gp = GO::Object::GeneProductSearchResult->new(@_);
	$gp->apph( $apph->apph );
	return $gp;
}

=head2 _set_search_fields

Set up the fields to be searched

Arguments: self, 
           $fields                    # list of fields
           $option_h->{results_only}  # set to 1 if not calculating relevance

Sets the following parameters:
search_fields => { hash with search field names as keys }
ordered_search_fields => [ search fields in order ]
field_list => [ { Search.pm field name => relevance score },
                { Search.pm field name => relevance score } ] # for relevance calculations

=cut

sub _set_search_fields {
	my $self = shift;
	my $fields = shift;
	my $option_h = shift;
	my $sc = $self->get_param('search_constraint');
	my $all_fields = __search_field_list($sc, 'all');

	print STDERR "fields: " if $verbose;
	if ($fields && @$fields)
	{	print STDERR join(", ", @$fields)."\n" if $verbose;
	}
	else
	{	print STDERR "none set\n" if $verbose;
	}

	my $search_fields;
	if ($fields)
	{	if (grep { $_ eq 'all' } @$fields)
		{	$search_fields->{$_} = 1 foreach @$all_fields;
		}
		else
		{	#	ensure the fields are valid
			foreach (@$fields)
			{	$search_fields->{$_} = 1 unless (!grep { $_ } @$all_fields);
			}
		}
	}
	if (!$search_fields)
	{	map { $search_fields->{$_} = 1 } @{__search_field_list($sc, 'default')};
	}

	$self->set_param('search_fields', $search_fields);
#	print STDERR "search_fields: ".Dumper($search_fields)."\n" if $verbose;
	
	if ($option_h && $option_h->{results_only})
	{	return $search_fields;
	}
	
#	put the search_fields in the order in which they should be
#	checked when working out the relevance of the term results

	my @ordered_search_fields = grep { exists $search_fields->{$_} } @{__search_field_list($sc, 'ordered')};
	$self->set_param('ordered_search_fields', \@ordered_search_fields);
#	print STDERR "ordered_search_fields: ".Dumper(\@ordered_search_fields)."\n" if $verbose;

	my $field_list;
	if ($sc eq 'spp')
	{	if ($search_fields->{common_name})
		{	$search_fields->{binomial} = 1;
		}
	}

	if ($self->{get_relevance})
	{	foreach my $field (@ordered_search_fields)
		{	push @$field_list, [ $_, __search_field_weighting($_) ] foreach @{__fieldname_alias($field)};
		}
	}
	else
	{	foreach my $field (@ordered_search_fields)
		{	push @$field_list, $_ foreach @{__fieldname_alias($field)};
		}
	}

	$self->set_param('field_list', $field_list);

	return $search_fields;
}

=head2 _set_cache_results

Creates the data structure that will be stored in the cache

Arguments: self, 
           $result_list  # list of result entities
           $cache        # any bits of old cache

Sets the following
cache->{result_list} = [ { id => <id of result>, src => <where match occurred> },
                         { id => <id of result>, src => <where match occurred> },
                         ... ]
cache->{query}{input}    # original query, as entered by the user
cache->{query}{parsed}   # parsed version of the query
cache->{query}{sql}      # SQL version (if applicable)
cache->{query}{perl}     # perl version (if applicable)
cache->{<sc>fields}      # the fields where the search took place

Puts all this into $self->cache, which is then saved by the cgi that called the search

=cut

sub _set_cache_results {
	my $self = shift;
	return unless $self->{cache_me};
	my $result_list = shift || undef;
	my $cache = shift || undef;
	my $sc = $self->get_param('search_constraint');

	if ($self->{from_cache})
	{	#	the results came from the cache, so we don't need to change 'em...
		#	...unless, of course, we did a sort
		return unless $self->get_param('sort_me');
	}

	if ($result_list)
	{	#print STDERR "Source size: ".scalar @$result_list."\n" if $verbose;
		#print STDERR "Source: ".Dumper($result_list)."\n" if $verbose;
		$cache->{result_list} =
			[ map { 
						{	id => $_->{id},
							src => $_->{source}
						}
					} @$result_list ];
	}

	$self->{results} = $cache;
	
	$cache->{$sc.'fields'} = $self->get_param('ordered_search_fields');

	$cache->{query}{input} = $self->get_param('query');
	$cache->{query}{parsed} = $self->{query}{parsed};
	$cache->{query}{sql} = $self->{query}{sql} if $self->{query}{sql};
	$cache->{query}{perl} = $self->{query}{perl} if $self->{query}{perl};

	$self->cache($cache);
	print STDERR "cache query:\n".Dumper($self->cache->{query})."\n" if $verbose;
}

=head2 _get_id_for_url

Returns the appropriate value to be used by a <obj-type>-details cgi
i.e. for a term, returns the GO acc, and for a GP, returns the dbxref

Arguments: self, results from a search, search constraint
Returns:   appropriate values as a string

=cut

sub _get_id_for_url {
	my $self = shift;
	my $results = shift;
	my $sc = $self->get_param('search_constraint');

	print STDERR "results: ".Dumper($results)."\n" if $verbose;

	if ($sc eq 'term')
	{	return $results->[0]{acc} if scalar @$results == 1;
	}
	elsif ($sc eq 'gp')
	{	#	get the gp xref
		if (scalar @$results == 1)
		{	my $dbxref = $results->[0]{dbxref_id};
			if (!$dbxref)
			{	return;
			}
			my $dbh = $self->apph->dbh;
			my $sql = "SELECT xref_dbname, xref_key FROM dbxref WHERE id=".sql_quote($dbxref);
			my @dbxref = @{$dbh->selectall_arrayref($sql)};
			if (@dbxref && scalar @dbxref == 1)
			{	return join(":", @{$dbxref[0]});
			}
		}
	}
	return undef;
}

=head2 get_results_list

External method for using Search.pm
Commented out for the time being

	Arguments - search object, argument hash
	            argument hash should contain:
	            query => [ list of query terms ]
	            
	            it can also contain
	            exact_match => 1
	               if exact match should be on
	            search_constraint => 'term' / 'gp'
	               if there isn't already a search constraint session param
	               (default is 'term')
	            search_fields => [ list of fields to search ]
	               (default will be used otherwise)
	            use_filters => 1
	               if you want the search to be filtered

	Returns   - list of terms / GPs, depending on search constraint


sub get_results_list {
	my $self = shift;

	if (!$self->apph || !$self->query)
	{	$self->set_msg('fatal', 'missing_apph_or_query');
		$self->success(0);
		return $self;
	}

	my $arg_h = shift || {};
	my $apph = $self->apph;

	my $sc = $arg_h->{search_constraint};
	if ($sc && grep { $sc } qw(term gp spp))
	{	$self->set_param('search_constraint', $sc);
	}

	#	otherwise, set it to the default (terms)
	$sc = $self->get_param('search_constraint') unless $sc;

	print STDERR "\nStarting apph search...\n" if $verbose;

	if ($arg_h->{exact_match})
	{	$self->set_param('exact_match', 1);
	}
	
#	check the query and turn it into a structure that we can use
	my $success = $self->_set_query($arg_h->{query});
	return if (!$success);
	
#	find out what fields we're going to search
#	if nothing is specified, use the default
	$self->_set_search_fields($arg_h->{search_fields}, { results_only => 1 } );
	my $search_fields = $self->get_param('search_fields');

	print STDERR "search_fields:\n".Dumper($search_fields)."\n" if $verbose;

	if ($arg_h->{use_filters} && $arg_h->{use_filters} == 1)
	{	#	set the filters
		$self->_set_filters($apph, $sc);
	}
	
	#	do the search!
	my $results = $self->search;

	if (!$results)
	{	#	return some kind of error here
		$self->set_msg('fatal', 'no_search_results');
		return;
	}

	my $result_h;
	#	see whether we found results for all our queries
	if (%{$self->{query}{unmatched}})
	{	#	We didn't find matches for everything.
		#	Add a warning message about queries that we didn't find a match for
		@{$result_h->{lost}} = map { join(" ", @$_ ) } values %{$self->{query}{unmatched}};
	}

	#	convert the results into the appropriate object
	$result_h->{found} = $self->get_result_objects($results, $sc, $arg_h->{template});
	return $result_h;
}

sub get_result_objects {
	my ($self, $results, $sc, $tmpl) = @_;
	my $apph = $self->apph;
	my $dbh = $apph->dbh;

	if ($sc eq 'gp')
	{	# create GP objects and add dbxrefs, species info, etc.
		my $hashref = $dbh->selectall_hashref("SELECT gp.id, dbxref.xref_key AS acc, dbxref.xref_dbname AS speciesdb FROM gene_product gp, dbxref WHERE gp.dbxref_id=dbxref.id AND gp.id IN (".join(",", map{$_->{id}}@$results).")", "id");

#		print STDERR "hashref: ".Dumper($hashref)."\n" if $verbose;
#		print STDERR "results: ".Dumper($results)."\n" if $verbose;

		foreach (@$results)
		{	if (!$hashref->{$_->{id}})
			{	print STDERR "Could not find info for $_->{id}: something wrong with script?\n" if $verbose;
				next;
			}
			my %hash = (%{$hashref->{$_->{id}}}, %$_);
			$hash{full_name} = $hash{symbol} if !$hash{full_name};
			my $gp = $apph->create_gene_product_obj(\%hash);
			foreach ('species_id', 'type_id') #, 'product_synonym', 'seq_xref', 'seq_name')
			{	$gp->{$_} = $hash{$_} if ($hash{$_});
			}
			$_ = $gp;
		}
		#	Add species, type, synonyms
		$apph->_get_product_species($results) if $tmpl->{species};
		$apph->_get_product_types($results) if $tmpl->{type};
		$apph->_get_product_synonyms($results) if $tmpl->{synonym};
	}
	elsif ($sc eq 'term')
	{	foreach (@$results)
		{	$_ = $apph->_create_term_obj($_);
		}
	}
	elsif ($sc eq 'spp')
	{	foreach (@$results)
		{	$_->{spp} = $apph->create_species_obj($_->{spp});
		}
	}
	return $results;
}
=cut

### Data only ###

#	Names of the different sets of search fields
#	subsets are all fields, default fields, srch_options
#	(options to display on the adv search page) and
#	ordered (fields put into order)
sub __search_field_list {
	my $sc = shift;
	my $list = shift;

	my $hash = {
		gp => {
			all => [ 'symbol', 'full_name', 'product_synonym', 'gpxref', 'seq_name', 'seq_xref' ],
			default => [ 'symbol', 'full_name', 'product_synonym' ],
			srch_options => ['symbol', 'full_name', 'product_synonym', 'gpxref', 'seq_name', 'seq_xref'],
			ordered => ['symbol', 'full_name', 'product_synonym', 'gpxref', 'seq_name', 'seq_xref'],
		},
		
		term => {
			all => [ 'name', 'term_synonym', 'definition', 'comment', 'xref', 'subset', 'acc' ],
			default => [ 'acc', 'name', 'term_synonym' ],
			srch_options => ['name', 'term_synonym', 'definition', 'comment', 'dbxref', 'subset'],
			ordered => ['acc', 'name', 'term_synonym', 'definition', 'dbxref', 'subset', 'comment', 'xref'],
		},
		spp => {
			all => ['binomial', 'common_name'],
			default => ['binomial', 'common_name'],
			srch_options => ['ncbi_taxa_id', 'binomial', 'common_name'],
			ordered => ['ncbi_taxa_id', 'binomial', 'common_name'],
		},
	};
	
	return $hash->{$sc}{$list};
}

#	Relative weighting of search fields for the relevance calculation
sub __search_field_weighting {
	my $field = shift;
	my %weighting = (
	#	gp fields
		product_synonym => 0.9,
		seq_xref => 0.7,
	#	term fields
		narrow_term_synonym => 0.7,
		broad_term_synonym => 0.7,
		related_term_synonym => 0.5,
		dbxref => 0.7,
		def_xref => 0.5,
		comment => 0.6,
		subset_acc => 0.4,
		subset_name => 0.4,
	);
	
	return $weighting{$field} || 1;
}

#	fieldname from AmiGO search --> fieldname used by AmiGO cache
sub __fieldname {
	my $field = shift;
	my %fields = (
		exact_term_synonym => 'term_synonym',
		narrow_term_synonym => 'term_synonym',
		broad_term_synonym => 'term_synonym',
		related_term_synonym => 'term_synonym',
		alt_id_term_synonym => 'term_synonym',
		dbxref => 'xref',
		def_xref => 'xref',
		subset_name => 'subset',
		subset_acc => 'subset',
#		name => 'name_acc',
#		acc => 'name_acc',
	);
	return $fields{$field} || $field;
}

#	fieldname used by AmiGO cache --> fieldname from AmiGO search
sub __fieldname_alias {
	my $field = shift;
#	term_synonym is an alias for exact / narrow / broad / related syns
#	xref is an alias for dbxref and def_xref
#	seq is an alias for seq_name and seq_xref
#	name_acc is an alias for name and accession
	my %alias = (
		term_synonym => [ 'exact_term_synonym', 'narrow_term_synonym', 'broad_term_synonym', 'related_term_synonym', 'alt_id_term_synonym'],
		xref => ['dbxref', 'def_xref'],
		subset => ['subset_acc', 'subset_name'],
#		name_acc => ['acc', 'name'],
	);
	return $alias{$field} || [ $field ];
}

#	default sorting order for the various search types
sub __sort_default {
	my $sc = shift;
	my $relevance = shift;
	
#	gene products: possible sort values:
#	full_name symbol spp relevance
#	default: relevance, symbol, full_name (n.b. full name may be blank!)

#	terms:
#	name acc ontology relevance
#	default: relevance, name, acc
#	obsoletes get sent to the end

	my $sort_default = {
		gp => ['symbol', 'full_name'],
		term => ['name', 'acc', 'term_type'],
		spp => ['binomial', 'common_name'],
	};
	
	if ($relevance)
	{	return [ 'rel', @{$sort_default->{$sc}} ];
	}
	else
	{	return $sort_default->{$sc} || [];
	}
}

#	prepares the data required to sort a list of search results
#	inputs: obj (actually a hash from the db query)
#	        search constraint
#	        list of sort criteria
#	output: \0-separated string version of obj, ready for sorting

sub __data_for_sort {
	my $obj = shift;
	my $sc = shift;
	my $crit = shift;

	my @list = map
	{	if ($_ eq 'rel') # sort by relevance
		{	if (!$obj->{source})
			{	(10000, 10);
			}
			else
			{	my @rel_sort = %{$obj->{source}};
				(sprintf( "%05d", (10000 - $rel_sort[1] * 10000)), 
					__field_importance($rel_sort[0]));
			}
		}
		#	sort by species
		elsif (($_ eq 'binomial' && $sc eq 'spp') || ($_ eq 'spp' && $sc eq 'gp'))
		{	$obj->{genus}." ".$obj->{species};
		}
		else # any other criteria
		{	if (exists $obj->{$_})
			{	lc $obj->{$_};
			}
			else
			{	print STDERR "Error: did not return a value for sorting! $sc, $_, $obj\n" if $verbose;
				print STDERR Dumper($obj)."\n" if $verbose;
				0;
			}
		}
	} @$crit;
#	unshift @list, $obj;
#	print STDERR Dumper(\@list) if $verbose;
#	print STDERR Dumper($obj) if $verbose;
	my $ref = $obj->{id};
	return join("\0", (@list, $ref));
}



#	'importance' of search field (1 = most important, 10 = least)
sub __field_importance {
	my $field = shift;
	my %rel = (
	#	term search fields
		acc => 1,
		name => 1,
		term_synonym => 2,
		definition => 3,
		comment => 4,
		xref => 4,
		subset => 5,
#		dbxref => 

	#	gp search fields
		symbol => 1,
		full_name => 2,
		gpxref => 3,
		product_synonym => 4,
		seq_name => 5,
		seq_xref => 6,
		
	#	spp search fields
		binomial => 1,
		ncbi_taxa_id => 2,
		common_name => 3,
	);
	return $rel{$field} || 10;
}

=head2 __min_q_length

Internal method to get minimum query length

=cut

sub __min_q_length {
	my $sc = shift;
	my $exact = shift || undef;

	my $min_q = {
		gp_exact => 1,
		gp => 2,
		term_exact => 3,
		term => 3,
		spp => 3,
		spp_exact => 3,
	};

	return $exact ? $min_q->{$sc.'_exact'} : $min_q->{$sc};
}

=head2 __search_sql_data

Contains data used to create SQL queries
search_table   => the table in which the field appears
search_field   => the field name(s)
tables         => any tables in addition to the base table needed
                  in the SQL query
table_join_sql => how to join the tables together
select_str     => what will go in the "SELECT *** FROM " part of the query

=cut

sub __search_sql_data {
	my $sc = shift;      # search constraint
	my $type = shift;    # what we're looking for
	my $field = shift;   # whether we want table names, column names, etc..

	#	extra info, search-type specific
	my $search = {
		gp => {
			base => {
				tables => "gene_product gp",
				select_str => "gp.id",
			},
			results => {
				tables => "gene_product gp",
				select_str => "gp.id, gp.symbol, gp.dbxref_id, gp.species_id, gp.type_id, IF (gp.full_name = '', gp.symbol, gp.full_name) as full_name",
			},
			symbol => {
				search_table => "gp",
				search_field => [ 'symbol' ],
				select_str => "gp.symbol, IF (full_name = '', gp.symbol, gp.full_name) as full_name, gp.dbxref_id, gp.species_id, gp.type_id",
			},
			full_name => {
				search_table => "gp",
				search_field => [ 'full_name' ],
				select_str => "gp.symbol, IF (full_name = '', gp.symbol, gp.full_name) as full_name, gp.dbxref_id, gp.species_id, gp.type_id",
			},
			product_synonym => {
				search_table => "gene_product_synonym",
				search_field => [ 'product_synonym' ],
				select_str => "'product_synonym', gene_product_synonym.product_synonym, ''",
				tables => "gene_product_synonym",
				table_join_sql => "gp.id=gene_product_synonym.gene_product_id",
			},
			gpxref => {
				search_table => "gpx",
				select_str => "'gpxref', CONCAT(gpx.xref_dbname,':',gpx.xref_key), ''",
				tables => "dbxref AS gpx",
				table_join_sql => "gp.dbxref_id=gpx.id",
			},
			seq_name => {
				search_table => "seq",
				search_field => [ 'display_id' ],
				select_str => "'seq_name', seq.display_id, ''",
				tables => "gene_product_seq, seq",
				table_join_sql => "gp.id = gene_product_seq.gene_product_id AND gene_product_seq.seq_id  = seq.id",
			},
			seq_xref => {
				search_table => "seqx",
				select_str => "'seq_xref', CONCAT(seqx.xref_dbname,':',seqx.xref_key), ''",
				tables => "gene_product_seq, seq, seq_dbxref, dbxref AS seqx",
				table_join_sql => "gp.id = gene_product_seq.gene_product_id AND gene_product_seq.seq_id  = seq.id AND seq.id = seq_dbxref.seq_id AND seq_dbxref.dbxref_id = seqx.id",
			},
			seq => {
				select_str => "'seq', seq.display_id, CONCAT(seqx.xref_dbname,':',seqx.xref_key)",
				tables => "gene_product_seq, seq, seq_dbxref, dbxref AS seqx",
				table_join_sql => "gp.id = gene_product_seq.gene_product_id AND gene_product_seq.seq_id  = seq.id AND seq.id = seq_dbxref.seq_id AND seq_dbxref.dbxref_id = seqx.id",
			},
		},
		term => {
			base => {
				search_table => "term",
				tables => "term",
				select_str => "term.id",
			},
			results => {
				search_table => "term",
				tables => "term",
				select_str => "term.*",
			},
			name => {
				search_table => "term",
				search_field => [ 'name' ],
				select_str => "term.acc, term.name, term.term_type, term.is_obsolete, term.is_root"
			},
			acc => {
				search_table => "term",
				search_field => [ 'acc' ],
				select_str => "term.acc, term.name, term.term_type, term.is_obsolete, term.is_root"
			},
#			name_acc => {
#				search_table => "term",
#				search_field => [ 'name', 'acc' ],
#				select_str => "term.acc, term.name, term.term_type, term.is_obsolete, term.is_root"
#			},
			term_synonym => {
				search_table => "term_synonym",
				search_field => [ 'term_synonym' ],
				select_str => "CONCAT(syntype.name, '_term_synonym'), term_synonym.term_synonym, ''",
				tables => "term_synonym, term AS syntype",
				table_join_sql => "term.id=term_synonym.term_id AND term_synonym.synonym_type_id=syntype.id",
			},
			dbxref => { # database cross reference
				search_table => "dbxref",
				select_str => "'dbxref', CONCAT(dbxref.xref_dbname,':',dbxref.xref_key), ''",
				tables => "term_dbxref, dbxref",
				table_join_sql => "term.id=term_dbxref.term_id AND term_dbxref.is_for_definition = 0 AND term_dbxref.dbxref_id = dbxref.id",
			},
			xref => { # database cross reference or def xrefs
				search_table => "dbxref",
				select_str => "IF (term_dbxref.is_for_definition=0,'dbxref','def_xref'), CONCAT(dbxref.xref_dbname,':',dbxref.xref_key), ''",
				tables => "term_dbxref, dbxref",
				table_join_sql => "term.id=term_dbxref.term_id AND term_dbxref.dbxref_id = dbxref.id",
			},
			definition => {
				search_table => "term_definition",
				search_field => [ 'term_definition' ],
				select_str => "'definition', term_definition.term_definition, ''",
				tables => "term_definition",
				table_join_sql => "term.id=term_definition.term_id",
			},
			comment => {
				search_table => "term_definition",
				search_field => [ 'term_comment' ],
				select_str => "'comment', term_definition.term_comment, ''",
				tables => "term_definition",
				table_join_sql => "term.id=term_definition.term_id",
			},
			def_comm => {
				search_table => "term_definition",
				search_field => [ 'term_definition', 'term_comment' ],
				select_str => "'term_definition_or_comment', term_definition.term_definition, term_definition.term_comment",
				tables => "term_definition",
				table_join_sql => "term.id=term_definition.term_id",
			},
			subset => {
				search_table => "subset",
				search_field => [ 'acc', 'name' ],
				select_str => "'subset', subset.acc, subset.name",
				tables => "term_subset, term AS subset",
				table_join_sql => "term.id = term_subset.term_id AND term_subset.subset_id = subset.id",
			},
		},
		spp => {
			results => {
				search_table => "species",
				tables => "species",
				select_str => "species.*",
			},
		},
	};


=insert
			subset_name => {
				tables => ['term', 'term_subset', 'term AS subset'],
				search_table => "subset",
				field_name => 'name',
				table_join_sql => ['term.id = term_subset.term_id', 'term_subset.subset_id = subset.id'],
			},
			subset_acc => {
				tables => ['term', 'term_subset', 'term AS subset'],
#				search_table => "subset",
#				field_name => 'acc',
				search_field => 'subset.acc',
				table_join_sql => ['term.id = term_subset.term_id', 'term_subset.subset_id = subset.id'],
			},

			term_xref => {
				tables => ['term', 'term_dbxref', 'dbxref AS term_xref'],
				search_field => {
					fields => [ 'term_xref.xref_dbname', 'term_xref.xref_key' ],
					op => 'AND',
				},
				table_join_sql => ['term.id=term_dbxref.term_id', 'term_dbxref.dbxref_id=term_xref.id'],
			},
			term_xref_dbname => {
				tables => ['term', 'term_dbxref', 'dbxref AS term_xref'],
				search_field => 'term_xref.xref_dbname',
				table_join_sql => ['term.id=term_dbxref.term_id', 'term_dbxref.dbxref_id=term_xref.id'],
			},
			term_xref_key => {
				tables => ['term', 'term_dbxref', 'dbxref AS term_xref'],
				search_field => 'term_xref.xref_key',
				table_join_sql => ['term.id=term_dbxref.term_id', 'term_dbxref.dbxref_id=term_xref.id'],
			},
=cut


	if (defined $field)
	{	return $search->{$sc}{$type}{$field} || undef;
	}
	return $search->{$sc}{$type} || undef;
}

=head2 __common_suffixes

Array of suffixes which are common enough that they should not be included in the relevance calculations

=cut

sub __common_suffixes {
	return qw(activity complex);
}

=head2 __common_prefixes

Array of prefixes which are common enough that they should not be included in the relevance calculations

=cut

sub __common_prefixes {
	return qw();
}


=head2 __accession_forms

array containing regexps - the first is what will be accepted as an accession,
and the second is the format into which they should be formatted

=cut

sub __accession_forms {
	return [
		["(GO)?[: ]?0*([1-9][0-9%\?\*]{0,6})", "GO:%07s"],
	];

}

1;
