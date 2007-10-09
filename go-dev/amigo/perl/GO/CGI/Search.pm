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
#use GO::Utils qw(rearrange);
use GO::CGI::Utilities qw(:all);
use GO::SqlWrapper qw(sql_quote select_hashlist);
use HTML::Entities;
#use Digest::MD5;

use Data::Dumper;
$Data::Dumper::Indent = 1;
#use Time::HiRes qw(gettimeofday);

use GO::Object::TermSearchResult;
use GO::Object::GeneProductSearchResult;

use GO::CGI::Query qw(get_gp_details get_term_in_graph _get_products_seqs get_nit get_gp_count_for_terms get_term_count_for_gps);

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

	print STDERR "search object: ". Dumper($self) ."\n";
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
	my $q_type = shift || 'orig';
	return $self->{queryset}{$q_type};
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
	            message type: e.g. 'no_valid_query', 'no_results'
	            what it affects (optional)

	Updates self->{msg_h} with the message

=cut

sub set_msg {
	my $self = shift;

	print STDERR "\@_ = ".Dumper(\@_);

	$self->{msg_h} = set_message($self->{msg_h}, @_);

	print STDERR "self->{msg_h} = ".Dumper($self->{msg_h})."\n";

}

=head2 get_msg

	Retrieve messages / errors

	Arguments - self
	            message class (optional)
	Returns the messages of that class (if there are any), or all messages

=cut

sub get_msg {
	my $self = shift;
	return get_message($self->{msg_h}, @_);
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

=head2 __get_accs

Examines the queryset and puts anything resembling an accession in an array

=cut

sub __get_accs {
	my $self = shift;
	my $queryset = $self->{queryset}{orig};

	#	if it already vaguely resembles an ID,
	#	put it on a list of accs to search
	#	ignore any zeros as padding and allow wildcards

	my $acc_like = [
			["(GO)?[: ]?0*([1-9][0-9%\?]{0,6})", "GO:%07d"],
	];

	my @list;
	foreach my $q (@$queryset)
	{	foreach (@$acc_like)
		{	my $regexp = $_->[0];
			my $print = $_->[1];
			my @temp = map { 
				if (/^$regexp$/io)
				{	print STDERR "$2\n";
					$_ = sprintf("$print", $2)
				}
				else { () }
			} @$q;
			print STDERR "temp: ".join(", ", @temp) if @temp;
			push @list, @temp unless !@temp;
		}
	}

	print STDERR "list: ".Dumper(\@list);
	
	return \@list || undef;
}

=head2 getResultList

	The basic function which is called by search.cgi to retrieve search results.

	Arguments:  search object, apph, the query, option_h containing:
	            - 
	Returns:    a list of search results
	            
	The search object will be updated with information about
	the query (retrievable through get_query_param) and about
	the results (get_result_param). Any messages about the
	search can be accessed via get_message
	            

=cut

sub getResultList {
	my $self = shift;
#	my ($apph, $query, $option_h) = rearrange([qw(apph query option_h)], @_);
	my ($apph, $query, $option_h) = @_;

#	print STDERR Dumper($apph);
	print STDERR "query: ". Dumper($query);
	print STDERR "option_h: ". Dumper($option_h);

	if (!$apph)
	{	$self->set_msg('fatal', 'missing_apph');
		return;
#		$self->success(0);
	}
	
	if (!$query)
	{	$self->set_msg('fatal', 'missing_query');
		print STDERR "No query found\n";
#		$self->success(0);
		return;
	}

	$self->apph($apph);
	my $sc = $option_h->{search_constraint};
	$self->set_param('search_constraint', $sc);
	

	if ($option_h->{cache})
	{	my $cache = $option_h->{cache};
		## get the data from the cache
		$self->_set_selected($cache->{$sc.'fields'});
#		$self->set_param('select_list', $cache->{$sc.'fields'});
		$self->{queryset}{orig} = $cache->{queryset};
		$self->{queryset}{perl} = $cache->{queryset_perl};

		my @perl_qlist = 
		map {
			[
			map { 
				if ($self->{queryset}{perl}{$_})
				{	qr/$self->{queryset}{perl}{$_}/i;
				}
				else
				{	qr/$_/i;
				}
			} @$_ ];
		} @{$self->{queryset}{orig}};
	
		$self->{queryset}{perllist} = \@perl_qlist;

		print STDERR "Using results from cache...\n";
		$self->{from_cache} = 1;
		$self->cache($cache);
	}

	#	if there's no cache or something has gone wrong
	if (!$self->{from_cache})
	{	###	set the query
		$self->set_param('query', $query);
	
	#	check the queryset and turn it into a structure that we can use
		my $success = $self->_set_queryset($query);
		return if !$success;
	
	#	find out what fields we're going to search
	#	if nothing is specified, use the default
#		$self->_set_selected($option_h->{search_fields});
		$self->_set_selected($option_h->{$sc."fields"});
	}

	foreach ($sc.'sort', 'exact_match', 'page_size', 'page', 'show_gp_counts', 'show_term_counts') #, 'format')
	{	$self->set_param($_, $option_h->{$_}) if $option_h->{$_};
	}

	#	set the filters
	$self->_set_filters;

	$self->set_param('gp_count_ok', $option_h->{gp_count_ok});
	$self->set_param('ont_list', $option_h->{ontology_list});

	if ($option_h->{action} && $option_h->{action} eq 'sort')
	{	$self->set_param('sort_me', 1);
	}

	my $results;
	if ($self->{from_cache})
	{	$results = $self->get_results_from_cache || $self->get_results_from_db;
	}
	else
	{	$results = $self->get_results_from_db;
	}
	
=cut
	if ($self->{results}{single_result})
	{	my $sc = $self->get_param('search_constraint');
		#	if we only have one result, load up the details page for that result.
		my $id = $self->_get_url($results);
		print STDERR "id: $id\n";
		
#		if ($url)
#		{	print "Location: ".$session->get_param('cgi_url')."/".$sc."-details.cgi?$sc=".$url."&session_id=".$session->get_param('session_id')."\n\n";
#			return;
#		}
		if ($id)
		{	my $result;
			if ($sc eq 'gp')
#			{	my $result_h = get_gp_details($apph, $msg_h, {gpxref => $id});
			{	my $result_h = get_gp_details($apph, undef, {gpxref => $id});

#	$result->{msg_h} = $result_h->{msg_h};
	$result->{ses_type} = 'gp_details';
	$result->{gp} = $result_h->{results}[0] if $result_h->{results};
				$self->set_msg('info', 'AmiGO found only one result for your search');
				return $result;
			}
			elsif ($sc eq 'term')
			{	print "Location: ".$session->get_param('cgi_url')."/".$sc."-details.cgi?$sc=".$url."&session_id=".$session->get_param('session_id')."\n\n";
				return;
			}
		}
	}
			
#			if ($result)
#			{	#one result
#				$self->set_msg('info', 'AmiGO found only one result for your search');
#				if ($session->ses_type =~ /search/)
#				{	#	change the session type accordingly
#					$session->ses_type($sc.'_details');
#				}
#				return $result;
#			}


		}
		#	if we're still here, something has gone wrong
		#	so get the results in the standard way
		return $self->get_result_details($results);
	}

=cut

	return $results;
}

=head2 get_results_from_db

Search method

=cut

sub get_results_from_db {
	my $self = shift;
	my $apph = $self->apph;
	my $dbh = $apph->dbh;

	print STDERR "\nStarting db search...\n";

	my $sc = $self->get_param('search_constraint');
	my $selected = $self->get_param('selected');

	#	do the search!
	my $results = $self->search;

#	print STDERR "Results data structure:\n".Dumper($results)."\n";
	
	if (!@$results && $self->{clever_mode} == 1)
	{	#	no results! uh-oh. see what fields we've searched and try
		#	searching some other fields
		print STDERR "No results found: entering clever mode!\n";

		#	look at the fields we haven't search and examine them for possible matches
		my @poss = grep { !$selected->{$_} } @{__search_field_list($sc, 'all')};

		if (@poss)
		{	print STDERR "fields to search: ".join(", ", @poss)."\n";
			my $temporary_selected;
			foreach (@poss)
			{	$selected->{$_} = 1;
				$temporary_selected->{$_} = 1;
			}
			$self->set_param('selected', $temporary_selected);
			$results = $self->search;
		}
		#	turn off exact match if it's on
		#	try a name / acc / symbol search for the other search constraint

		$self->set_param('selected', $selected);
	}

	if (!@$results)
	{	print STDERR "No results found\n";
	#	$self->_set_cache_results($session, { n_results => 0 });
	#	$self->success(0);
		return;
	}

	#	we have results
	#	see whether we found results for all our queries
	if (%{$self->{queryset}{unmatched}})
	{	#	We didn't find matches for everything.
		#	Add a warning message about queries that we didn't find a match for
		$self->set_msg('warning', 'no_results', [ map { join(" ", @$_) } values %{$self->{queryset}{unmatched}} ]);
	}

	my $max = get_environment_param('page_size') * get_environment_param('max_results_pages');
	if (scalar @$results > $max )
	{	$self->_set_cache_results({ n_results => scalar @$results,
		n_pages => 1, large_result_set => 1 }, $results );
		return;
	}
	elsif (scalar @$results == 1 && ($sc eq 'gp' || $sc eq 'term'))
	{	$self->{results}{single_result} = 1;
		print STDERR "results: ".Dumper($results);
		#	get the ID that we want to do the redirect with

		#	if we only have one result, load up the details page for that result.
		my $id = $self->_get_url($results);
		print STDERR "id: $id\n";
		return { id => $id, search_constraint => $sc };
	}

	print STDERR "scalar \@\$results: ".scalar (@$results)."\n";

	my $n_pages = get_n_pages(scalar @$results);
	my $cache = {
		n_results => scalar @$results,
		n_pages => $n_pages,
	};

	#	sort the hash and get the subset we need
	my $sorted = $self->_sort_results($results);
#	print STDERR "sorted results data structure:\n".Dumper($sorted)."\n";

	$self->_set_cache_results($cache, $sorted);
	$sorted = get_subset($sorted, 1) unless ($n_pages == 1);
	print STDERR "Done apph method.\n";
#	print STDERR "sorted:\n".Dumper($sorted)."\n";
	return $self->get_result_details($sorted);
}

=head2 _set_queryset

Internal method to set the queryset using the values in the parameter 'query'

=cut

sub _set_queryset {
	my $self = shift;

	my $query = $self->get_param('query');
	my $exact =  $self->get_param('exact_match');
	my $sc = $self->get_param('search_constraint');
	my $min_length = __min_q_length($sc, $exact);
	my $selected;

#	sort queries by length, descending?
#	ensure queries are unique
#	query length constraints

	my @qlist;     	#	original query
	my @perl_qlist;	#	perl versions of query
	my %sql;
	my %sql_regexp;
	my %perl = ();
	
	my @too_short;
	foreach (@$query)
	{	#print STDERR "\$_: $_\n";

		#	remove anything that isn't a digit or a letter
		#	and check the query length is long enough to be valid
		(my $temp = $_) =~ s/[^a-z0-9]//gi;
		if (length $temp < $min_length)
		{	#	this query is too short. Next!
			print STDERR "Rejected $_: too short\n";
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
				$selected->{ncbi_taxa_id} = 1;
			}
			elsif (/^[a-z]\.? .*/i)
			{	#	probably a spp name in binomial format
				$selected->{binomial} = 1;
				$_ =~ s/\.//;
				$self->{binomial_abbrev} = 1;
				($self->{queryset}{perl}{$_} = $_) =~ s/^([a-z]) /$1.*? /i;
			}
			else
			{	#	goodness only knows what this is!
				#	search the common names just in case
				$selected->{binomial} = 1 if ($_ =~ / /);
				$selected->{common_name} = 1;
			}
			@search_phrase = ( $_ );
		}
		else
		{#	print STDERR "\$_ = $_\n";
			my @list = split /\s/, $_;
			my $c = 0;
			my $last = '';
			my %words;
			my $colon = 0;
			foreach (@list)
			{	print STDERR "\$_ = $_\n";
				#	add repeated words to the word before
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
					else
					{	if ($c == 0 && $list[1])
				#	...or prepend it to the next list item
						{	$list[1] = "$_ ".$list[1];
						}
						else
						{	if (!$list[1])
							{	print STDERR "Program error: no subsequent list items found!\n";
								last;
							}
							print STDERR "last is undef'd but c != 0: what's going on?!\n";
						}
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
	
			#	create three querysets:
			#	- the original query
			#	- an sql version without duplicates and with SQL wildcards
			#	- a perl version 
	
			foreach (keys %words)
			{	if (!@{$words{$_}})
				{	print STDERR "Nothing found for $_\n";
					delete $words{$_};
					next;
				}
	
				#	check for metacharacters
				if (/[^a-z0-9 ]/i)
				{	print STDERR "Found some metacharacters in $_!\n";
	
					#	quote the SQL single char wildcard
					(my $sql = $_) =~ s/_/\_/g;
	
					#	convert any question marks to _
					$sql =~ s/(?<!\\)\?/_/g;
					$sql =~ s/\Q\?\E/?/g;
	
					#	convert any asterisks to _%
					$sql =~ s/(?<!\\)\*+|%+/_%/g;
					#	if the % is at the beginning or end of the word,
					#	change it to _ to force there to be some content
					#	(a % will automatically be added by $self->_where)
					$sql =~ s/^_%/_/;
					$sql =~ s/_%$/_/;
	
					$sql =~ s/\Q\*\E/*/g;
	
					$sql{$_} = $sql if ($sql ne $_);
	
					if ($sql =~ /[^\w%\*\?\\ ]/i)
					{	# replace with [^[:alnum:] ^[:space:]]?
						(my $sql_r = $sql) =~ s/[^\w%\*\?\\ ]/[^[:alnum:] ^[:space:]]?/gi;
						$sql_regexp{$_} = $sql_r if ($sql_r ne $sql);
						print STDERR "sql_regexp: ".Dumper($sql_regexp{$_})."\n";
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
					print STDERR "sql_regexp: ".Dumper($sql_regexp{$_})."\n";
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
	
					print STDERR "perl: $perl; sql: $sql\n";
					
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
	
		#	print STDERR "words: ".Dumper(\%words);
	
			foreach my $w (keys %words)
			{	foreach (@{$words{$w}})
				{	if ($search_phrase[$_])
					{	print STDERR "Found an entry for $w, $search_phrase[$_]\n";
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
	{	#$self->_set_cache_results($session, { n_results => 0 }); # is this needed?
		$self->set_msg('fatal', 'no_valid_query');
		return;
	}
	elsif (scalar @qlist > 1)
	{	$self->{queryset}{multi} = 1;
	}

#	if ($sc eq 'spp')
#	{	$session->set_param('1', 'sppfields', [keys %$selected])
#	}

	$self->{queryset}{orig} = [ @qlist ];

	if (keys %sql || keys %sql_regexp)
	{	$self->{queryset}{sql} = { sql => \%sql, regexp => \%sql_regexp };
	}
	$self->{queryset}{perl} = \%perl;
	$self->{queryset}{perllist} = [ @perl_qlist ];

	my %match_h;
	@match_h{@perl_qlist} = @qlist;
	$self->{queryset}{unmatched} = \%match_h;

	foreach my $qset qw(orig perl perllist sql unmatched)
	{	print STDERR "$qset queryset: ".Dumper($self->{queryset}{$qset})."\n";
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
	{	print STDERR "filters:\n".Dumper($filters)."\n";
	}
	my @tables;
	my @where;

	if ($sc eq 'term')
	{	my @extra = ();
		my $onts = $filters->{ont};
		if ($onts) {
			print STDERR "Found ontology filter!\n";
			$onts = [$onts] unless (ref($onts) eq 'ARRAY');
			push @where, "term.term_type IN (".join(",", map { sql_quote($_) } @$onts).")";
		}
	}
	elsif ($sc eq 'gp') {
	#	ontology filter
		my $onts = $filters->{ont};
		if ($onts) {
			print STDERR "Found ontology filter!\n";
			$onts = [$onts] unless (ref($onts) eq 'ARRAY');
			push @tables, "association a", "term";
			push @where, "a.gene_product_id=gp.id", "term.id=a.term_id", "term.term_type IN (".join(",",map{sql_quote($_)}@$onts).")";
		}

		#	species DB
		my $spdbs = $filters->{speciesdb};
		if ($spdbs) {
			$spdbs = [$spdbs] unless (ref ($spdbs) eq 'ARRAY');
	
			push @tables, "dbxref";
			push @where, "gp.dbxref_id=dbxref.id", "dbxref.xref_dbname IN (".join(",", map{sql_quote($_)} @$spdbs).")";
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

sub get_results_from_cache {
	my $self = shift;
#	my $option_h = shift;
	my $cache = $self->cache;
	my $sc = $self->get_param('search_constraint');
	my $result_list = $cache->{result_list};
	my $sorted;
	my $n_pages = get_n_pages(scalar @$result_list, $self->get_param('page_size'));
	print STDERR "num page = $n_pages; result_list size: ".scalar @$result_list."\n";

	if ($self->get_param('sort_me') && $self->get_param($sc.'sort') && $self->get_param($sc.'sort') eq 'rel')
	{	### new subroutine to sort by relevance
		#	have a look at our cached results,
		#	see if we already have the relevance info
		
		if (!$result_list->[0]{src}[1])  # position of the relevance score
		{	#	Uh-oh! We need to find out the relevance scores.
			$sorted = $self->get_relevance_of_cached_results;
		}
	}
	else
	{	$self->{results} = $cache;
	}

	if (!$sorted)
	{	my $apph = $self->apph;
		my $dbh = $apph->dbh;

		my $tables = _search_data($sc, 'results', 'tables');
		my $cols = _search_data($sc, 'results', 'cols');


#		my $tables = $sc;
#		if ($sc eq 'gp')
#		{	$tables = 'gene_product';
#		}
#		elsif ($sc eq 'spp')
#		{	$tables = 'species spp';
#		}
#		my $cols = "*";
		
		my $where = "";
	
		#	these queries all require the whole data set to be loaded
		if ($self->get_param('sort_me') ||
		#	$self->get_param('format') || # not implemented
			$self->get_param('page_size') eq 'all') {
			print STDERR "Found parameter sort, format or all\n";

			if ($sc eq 'gp' && $self->get_param('sort_me') && $self->get_param('gpsort') eq 'spp')
			{	$tables .= ", species";
				$where = "species.id = gp.species_id AND ";
				$cols = "gp.".$cols.", species.genus, species.species";
	#			$order = "ORDER BY species.genus, species.species LIMIT 50";
			}
		} else {
		# get the subset for that page
			print STDERR "Getting subset for page ".$self->get_param('page')."\n";
			$result_list = get_subset($result_list, $self->get_param('page'));
		}
	
	#	print STDERR "Source:\n".Dumper($source)."\n";
		$where .= "$sc.id in (".join(",", map { $_->{id} } @$result_list).")";
		
		my $sql = "SELECT $cols FROM $tables WHERE $where";
		print STDERR "sql: $sql\n";
	
		my $result_h = $dbh->selectall_hashref($sql, 'id');
	
	#	print STDERR "result_h:\n".Dumper($result_h)."\n";
	
		#	result_list has the correct order
		#	put the data from result_h into sorted
		foreach (@$result_list)
		{	$result_h->{$_->{id}}{source} = $_->{src};
			push @$sorted, $result_h->{$_->{id}};
		}
	}
#	print STDERR "sorted:\n".Dumper($sorted)."\n";
	
	#	if the action was sort, we need to sort the results
	#	and either return the first page OR the whole set
	if ($self->get_param('sort_me'))
	{	print STDERR "Going into sort subroutine\n";
		$sorted = $self->_sort_results([@$sorted]);
	#	print STDERR "sorted arr : ".join(", ", map{ $_->{$sc}->{acc} }@$sorted)."\n";

		$self->_set_cache_results($cache, $sorted);

		return $self->get_result_details(get_subset($sorted, $self->get_param('page'))) unless ($n_pages == 1);
	}

	#	otherwise, our cache hasn't changed, so we don't need to save it
	return $self->get_result_details($sorted);
}

sub get_relevance_of_cached_results {
	my $self = shift;

}

sub search {
	my $self = shift;
	my $subset = shift || undef; # list of IDs to search within
	my $apph = $self->apph;

	my $dbh = $apph->dbh;
	my $selected = $self->get_param('selected');
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
	my $where_phrases = $self->_where;
	
	print STDERR "Where strings: ".Dumper($where_phrases)."\n";
	
	#	if our search list contains anything *other* than full_name and symbol,
	#	we search for these first
	my $other_matches;
	my @names;
	my @others;
	my $combo_fields;

	if ($sc eq 'gp' || $sc eq 'term')
	{	foreach my $s (keys %$selected)
		{	if ( grep { $_ eq $s } @fields)
			{	push @names, $s;
			}
			else
			{	push @others, $s;
			}
		}
	}
	
	if ($sc eq 'term')
	{	if ($selected->{definition} && $selected->{comment})
		{	$combo_fields = 1;
			$where_phrases->{def_comm} = "(".$where_phrases->{definition}.") OR (". $where_phrases->{comment}.")";
			$selected->{def_comm} = 1;
			delete $selected->{definition};
			delete $selected->{comment};
		}
		if ($selected->{subset})
		{	$combo_fields = 1;
		}
	}

	if (@others)
	{	print STDERR "Checking fields other than name and symbol/acc...\n";
		my $base = _search_data($sc, 'base');

		my $sql =
			join(" UNION ", 
				map {
					my $data = _search_data($sc, $_);
					my $q = 
					"SELECT ".$base->{cols}.", ".$data->{cols}.
					" FROM ".$base->{tables}.", ".$data->{tables}.
					" WHERE ".$data->{where}." AND (".$where_phrases->{$_}.")".$extra;
					$_ = $q;
				} @others);

		print STDERR "sql: $sql\n";
		my $sth = $dbh->prepare($sql);
		$sth->execute();

		if (!$combo_fields)
		{	while (my $d = $sth->fetchrow_arrayref) {
				push @{$other_matches->{$d->[0]}{$d->[1]}}, $d->[2];
			}
		}
		else
		{	while (my $d = $sth->fetchrow_arrayref) {
			#	print STDERR "next: ".Dumper($d)."\n";
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
		{	print STDERR "No results found in other fields.\n";
		}
	}
	
	#	now get the information for what we found in the query above,
	#	and do the name / symbol / acc search
	#	apply filters to this search
	my @tables = ( _search_data($sc, 'results', 'tables') );
	my @cols = ( _search_data($sc, 'results', 'cols') );
	my @where;
	my @srch;
	
#	print STDERR "Filters: ".Dumper($filters);
	
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
		{	push @srch, $where_phrases->{$_} if $where_phrases->{$_};
		}
		#	retrieve species info if the sort parameter is species
		if ($self->get_param('gpsort') && $self->get_param('gpsort') eq 'spp')
		{	if (!grep { /species/ } @tables)
			{	push @tables, "species";
				push @where, "species.id = gp.species_id";
			}
			push @cols, "species.genus, species.species";
		}
	}
	elsif ($sc eq 'term')
	{	foreach (@fields)
		{	push @srch, $where_phrases->{$_} if $where_phrases->{$_};
		}
	}
	# NEW
	elsif ($sc eq 'spp')
	{	@srch = map { $where_phrases->{$_} } keys %$selected;
		push @cols, "CONCAT(genus,' ',species) AS binomial, COUNT(gp.id) AS gp_count";
		push @tables, 'gene_product gp';
		$extra .= ' AND gp.species_id=species.id GROUP BY species.id';
	}
	
	if (keys %$other_matches)
	{	push @srch, $sc.".id IN (".join(",", keys %$other_matches).")";
	}
	
	my $results;
	my @obs;
	if (@srch)
	{	(scalar @srch > 1)
		? push @where, "((".join(") OR (", @srch)."))"
		: push @where, "(".$srch[0].")";
	
		my $sql =
				"SELECT DISTINCT ".join(", ", @cols).
				" FROM ".join(", ", @tables).
				" WHERE ".join(" AND ", @where).$extra;
		print STDERR "sql: $sql\n";
		
		print STDERR "Main search: retrieving info and searching on full name/symbol\n";
		
#		my $results = $dbh->selectall_arrayref($sql);
		my $sth = $dbh->prepare($sql);
		$sth->execute();

		my $queryset = $self->{queryset}{perllist};
		my $field_list = $self->get_param('field_list');
		
		print STDERR "fieldlist: ".Dumper($field_list)."\n";
		if (!$queryset || !$field_list)
		{	print STDERR "WARNING! No queryset or fieldlist found!\n";
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
			#	print STDERR "Doing ignore on $d...\n";
				return 1 if $d->{is_obsolete} == 1;
			},
			obs_include_commented => sub {
				my $d = shift;
			#	print STDERR "Doing include_commented on $d...\n";
				push @obs, $d->{id} if $d->{is_obsolete} == 1;
			},
			get_relevance => sub {
				my $d = shift;
			#	print STDERR "Doing get_relevance on $d...\n";
				my $rel = $self->_get_relevance($results->{$d->{id}}, $sc, $queryset, $field_list, $exact);
			#	print STDERR $d->{id}." relevance score = ".Dumper($rel)."\n";
				
				if (!$rel)
				{	print STDERR "Error: no relevance scores returned for ".$d->{id}."!\n";
				#	print STDERR "data: ".Dumper($d)."\n";
					delete $results->{$d->{id}};
					return 1;
				}
				else
				{	$results->{$d->{id}}{source} = $rel;
				}
			},
			get_match => sub {
				my $d = shift;
				my $match_fields = $self->_get_match($results->{$d->{id}}, $sc, $queryset, $field_list);
				if (!$match_fields || !keys %$match_fields)
				{	print STDERR "Error: no matches returned for ".$d->{id}."!\n";
				#	print STDERR "data: ".Dumper($d)."\n";
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
		{	#push @subs, $sub_h->{obs_ignore};
			unshift @subs, $sub_h->{obs_ignore};
		}
		if ($self->{get_relevance})
		{	push @subs, $sub_h->{get_relevance};
		}
		else
		{	push @subs, $sub_h->{get_match};
		}
		if ($sc eq 'term' && get_environment_param('obsolete_behaviour') eq 'include_commented')
		{	print STDERR "Adding include commented to the sub list\n";
			push @subs, $sub_h->{obs_include_commented};
		}

		print STDERR "subs: ".Dumper(\@subs)."\n";

		while (my $d = $sth->fetchrow_hashref) {
			foreach (@subs)
			{	last if $_->($d) && $_->($d) == 1;
			}
		}

=cut me!
		#		if ($self->{get_relevance} == 1)
		#		{	# calculate the relevance here
					my $rel = $self->_get_relevance($results->{$d->{id}});
				#	print STDERR $d->{id}." relevance score = ".Dumper($rel)."\n";
					
					if (!$rel)
					{	print STDERR "Error: no relevance scores returned!\n";
					#	print STDERR "data: ".Dumper($d)."\n";
						delete $results->{$d->{id}};
					}
					else
					{	$results->{$d->{id}}{source} = $rel;
					}
				#	check the obsolete status
				if ($d->{is_obsolete} == 1)
				{	push @obs, $d->{id};
				}
			}
		}



		print STDERR "Getting result relevance using queryset: ".Dumper($self->{queryset}{perllist});
		if ($sc eq 'term')
		{	while (my $d = $sth->fetchrow_hashref) {
			#	print STDERR "next: ".Dumper($d)."\n";
				$results->{$d->{id}} = $d;
				if ($other_matches->{$d->{id}})
				{	while ( my ($key, $val) = each %{$other_matches->{$d->{id}}})
					{	$results->{$d->{id}}{$key} = $val;
					}
				}
		#		if ($self->{get_relevance} == 1)
		#		{	# calculate the relevance here
					my $rel = $self->_get_relevance($results->{$d->{id}});
				#	print STDERR $d->{id}." relevance score = ".Dumper($rel)."\n";
					
					if (!$rel)
					{	print STDERR "Error: no relevance scores returned!\n";
					#	print STDERR "data: ".Dumper($d)."\n";
						delete $results->{$d->{id}};
					}
					else
					{	$results->{$d->{id}}{source} = $rel;
					}
		#		}
				else
				{	#	check if there is a match in the acc / name
					foreach my $f ('acc', 'name')
					{	if ($selected->{$f} && $d->{$f})
						{	if ($self->_get_match($d->{$f}) == 1)
							{	$results->{$d->{id}}{src} = { $f => undef };
								last;
							}
						}
					}
					if (!$results->{$d->{id}}{src})
					{	@{$results->{$d->{id}}{src}}{keys %{$other_matches->{$d->{id}}}} = ();
					}
				}
				#	check the obsolete status
				if ($d->{is_obsolete} == 1)
				{	push @obs, $d->{id};
				}
			#	print STDERR "next post-processing:\n".Dumper($results->{$d->{id}})."\n";
			}
		}
		elsif ($sc eq 'gp') # search constraint = gp
		{	while (my $d = $sth->fetchrow_hashref) {
			#	print STDERR "next: ".Dumper($d)."\n";
			#	$results->{$d->{id}}{$sc} = $d;
				$results->{$d->{id}} = $d;
				if ($other_matches->{$d->{id}})
				{	while ( my ($key, $val) = each %{$other_matches->{$d->{id}}})
					{	#$results->{$d->{id}}{$sc}{$key} = $val;
						$results->{$d->{id}}{$key} = $val;
					}
				}
#				$results->{$d->{id}}{extra} =  if $other_matches->{$d->{id}};
		#		if ($self->{get_relevance} == 1)
		#		{	# calculate the relevance here
					my $rel = $self->_get_relevance($results->{$d->{id}});
					if (!$rel)
					{	print STDERR "Error: no relevance scores returned!\n";
						print STDERR "data: ".Dumper($d)."\n";
						delete $results->{$d->{id}};
					}
					else
					{	$results->{$d->{id}}{source} = $rel;
					}
		#		}
				else
				{	#	check if there is a match in the symbol / name
					foreach my $f ('symbol', 'full_name')
					{	if ($selected->{$f} && $d->{$f})
						{	if ($self->_get_match($d->{$f}) == 1)
							{	$results->{$d->{id}}{src} = { $f => undef };
								last;
							}
						}
					}
					if (!$results->{$d->{id}}{src})
					{	@{$results->{$d->{id}}{src}}{keys %{$other_matches->{$d->{id}}}} = ();
					}
				}
			}
		}
		elsif ($sc eq 'spp') # species search
		{	while (my $d = $sth->fetchrow_hashref) {
				#print STDERR "next: ".Dumper($d)."\n";
				$results->{$d->{id}} = $d;
				if ($self->{get_relevance} == 1)
				{	# calculate the relevance here
					my $rel = $self->_get_relevance($results->{$d->{id}});
					if (!$rel)
					{	print STDERR "Error: no relevance scores returned!\n";
						print STDERR "data: ".Dumper($d)."\n";
						delete $results->{$d->{id}};
					}
					else
					{	$results->{$d->{id}}{source} = $rel;
					}
				}
			}
		}
=cut
	}

	if (keys %$results)
	{	#	sort out the data
		print STDERR "Results found; returning data\n"; #.Dumper($results)."\n";
	}
	else
	{	print STDERR "No results found. Sob!\n";
	}

	if (@obs)
	{	print STDERR "Obsolete terms found: ".Dumper(\@obs)."\n";
	
		#	if we have terms, we need to check for obsoletes
		return $self->_obsolete_check($results, \@obs);
	}
	return [values %$results];
}

sub _search_data {
	my $sc = shift;
	my $type = shift;
	my $field = shift || undef;

	#	extra info, search-type specific
	my $search = {
		gp => {
			base => {
				tables => "gene_product gp",
				cols => "gp.id",
			},
			results => {
				tables => "gene_product gp",
				cols => "gp.id, gp.symbol, gp.dbxref_id, gp.species_id, gp.type_id, IF (gp.full_name = '', gp.symbol, gp.full_name) as full_name",
			},
			symbol => {
				cols => "gp.symbol, IF (full_name = '', gp.symbol, gp.full_name) as full_name, gp.dbxref_id, gp.species_id, gp.type_id",
			},
			full_name => {
				cols => "gp.symbol, IF (full_name = '', gp.symbol, gp.full_name) as full_name, gp.dbxref_id, gp.species_id, gp.type_id",
			},
			product_synonym => {
				cols => "'product_synonym', synonym.product_synonym, ''",
				tables => "gene_product_synonym synonym",
				where => "gp.id=synonym.gene_product_id",
			},
			gpxref => {
				cols => "'gpxref', CONCAT(dbxref.xref_dbname,':',dbxref.xref_key) as gp_dbxref, ''",
				tables => "dbxref",
				where => "gp.dbxref_id=dbxref.id",
			},
			seq_name => {
				cols => "'seq_name', seq.display_id, ''",
				tables =>"gene_product_seq, seq",
				where => "gp.id = gene_product_seq.gene_product_id AND gene_product_seq.seq_id  = seq.id",
			},
			seq_xref => {
				cols => "'seq_xref', CONCAT(seqxref.xref_dbname,':',seqxref.xref_key) as seq_xref, ''",
				tables => "gene_product_seq, seq, seq_dbxref, dbxref AS seqxref",
				where => "gp.id = gene_product_seq.gene_product_id AND gene_product_seq.seq_id  = seq.id AND seq.id = seq_dbxref.seq_id AND seq_dbxref.dbxref_id = seqxref.id",
			},
			seq => {
				cols => "'seq', seq.display_id, CONCAT(seqxref.xref_dbname,':',seqxref.xref_key) as seq_xref",
				tables => "gene_product_seq, seq, seq_dbxref, dbxref AS seqxref",
				where => "gp.id = gene_product_seq.gene_product_id AND gene_product_seq.seq_id  = seq.id AND seq.id = seq_dbxref.seq_id AND seq_dbxref.dbxref_id = seqxref.id",
			},
		},
		term => {
			base => {
				tables => "term",
				cols => "term.id",
			},
			results => {
				tables => "term",
				cols => "term.*",
			},
			name => {
				cols => "term.acc, term.name, term.term_type, term.is_obsolete, term.is_root"
			},
			acc => {
				cols => "term.acc, term.name, term.term_type, term.is_obsolete, term.is_root"
			},
			term_synonym => {
				cols => "CONCAT(syntype.name, '_term_synonym'), term_synonym.term_synonym, ''",
				tables => "term_synonym, term syntype",
				where => "term.id=term_synonym.term_id AND term_synonym.synonym_type_id=syntype.id",
			},
			dbxref => { # database cross reference
				cols => "'dbxref', CONCAT(dbxref.xref_dbname,':',dbxref.xref_key) as term_dbxref, ''",
				tables => "term_dbxref, dbxref",
				where => "term.id=term_dbxref.term_id AND term_dbxref.is_for_definition = 0 AND term_dbxref.dbxref_id = dbxref.id",
			},
			xref => {
				cols => "IF (term_dbxref.is_for_definition=0,'dbxref','def_xref'), CONCAT(dbxref.xref_dbname,':',dbxref.xref_key) as term_xref, ''",
				tables => "term_dbxref, dbxref",
				where => "term.id=term_dbxref.term_id AND term_dbxref.dbxref_id = dbxref.id",
			},
			definition => {
				cols => "'definition', term_definition.term_definition, ''",
				tables => "term_definition",
				where => "term.id=term_definition.term_id",
			},
			comment => {
				cols => "'comment', term_definition.term_comment, ''",
				tables => "term_definition",
				where => "term.id=term_definition.term_id",
			},
			subset => {
				cols => "'subset', subset.acc, subset.name",
				tables => "term_subset, term AS subset",
				where => "term.id = term_subset.term_id AND term_subset.subset_id = subset.id",
			},
			def_comm => {
				cols => "'term_definition_or_comment', term_definition.term_definition, term_definition.term_comment",
				tables => "term_definition",
				where => "term.id=term_definition.term_id",
			},
		},
		spp => {
			results => {
				tables => "species",
				cols => "species.*",
			},
		},
	};

	if (defined $field)
	{	return $search->{$sc}{$type}{$field} || undef;
	}
	return $search->{$sc}{$type} || undef;
}

sub _where {
	my $self = shift;
	my $selected = shift || $self->get_param('selected');
	my $queryset = $self->{queryset}{orig};
	my $queryset_sql = $self->{queryset}{sql} || {};
	my $sc = $self->get_param('search_constraint');
	my $exact = $self->get_param('exact_match');

	if ($exact)
	{	print STDERR "exact = $exact\n";
	}
	else
	{	print STDERR "exact match is off\n";
	}
	
	my $string;

	my %srch_h =
	(	# gp fields
		symbol => { table => 'gp', col => ['symbol'] },
		full_name => { table => 'gp', col => ['full_name'] },
		product_synonym => { table => 'synonym', col => ['product_synonym'] },
		seq_name => { table => 'seq', col => ['display_id'] },
#			xref' => { table => 'dbxref', col => ['xref_dbname', 'xref_key'] },
		# term fields
		name => { table => 'term', col => ['name'] },
		term_synonym => { table => 'term_synonym', col => ['term_synonym'] },
		acc => { table => 'term', col => 'acc' },
		definition => { table => 'term_definition', col => ['term_definition'] },
		comment => { table => 'term_definition', col => ['term_comment'] },
		subset => { table => 'subset', col => ['acc', 'name'] },
		def_comm => { table => 'term_definition', col => ['term_definition', 'term_comment'] },
#		dbxref => { table => 'dbxref', col => 'xref_key' },
#		xref => { table => 'dbxref', col => 'xref_key' },
		common_name => { col => ['genus', 'species', 'common_name'] },
		ncbi_taxa_id => { col => ['ncbi_taxa_id'] },
#		binomial => { col => ['genus', 'species'] },

	);

	#	fields to apply the regexp search to
	my @regexp_fields = qw(symbol full_name product_synonym name term_synonym definition);

	my %where_strs;



	if ($sc eq 'spp')
	{	foreach my $f (keys %$selected)
		{	$where_strs{$f} = join(') OR (', 
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
					{	if (!$exact)
						{	$q .= "%";
						}
						
						join(' OR ',
								map {
									$q = '%'.$q if $_ eq 'common_name' && !$exact;

									"$_ LIKE " . sql_quote($q);

								} @{$srch_h{$f}{col}});
					}
				} @$queryset);
			if (scalar @$queryset > 1)
			{	$where_strs{$f} = "(".$where_strs{$f}.")";
			}
		}
		return \%where_strs;
	}


	foreach my $f (keys %$selected)
	{	print STDERR "f = $f\n";
		$where_strs{$f} = join(') OR (', 
			map
			{	my $q = $_;
			#	print STDERR "field: $f; q: ".Dumper($q)."\n";
				if ($f =~ /xref/)
				{	join(' AND ',
						map { __xrefer($_, $sc, $f, $exact) } @$q);
				}
				else
				{	my $table = "$srch_h{$f}{table}." || '';
					my $op = 'LIKE';
					join(' AND ', 
					map {
						my $qstr = (split ("\0", $_, 2))[1];
						if ($exact || $f eq 'acc')
						{	$op = '=';
						}
						else
						{	if ($queryset_sql->{regexp}{$qstr} && grep { $f } @regexp_fields)
							{	$op = 'REGEXP';
								
								
								
								$qstr = $queryset_sql->{regexp}{$qstr};
							}
							elsif ($queryset_sql->{sql}{$qstr})
							{	$qstr = '%'.$queryset_sql->{sql}{$qstr}.'%';
							}
							else
							{	$qstr = '%'.$qstr.'%';
							}
							$qstr =~ s/%{2,}/%/g;
						}
					#	print STDERR "qstr = $qstr\n";

						if (scalar @{$srch_h{$f}{col}} > 1)
						{	join(' OR ',
								map {
									$table . "$_ $op " . sql_quote($qstr);
								} @{$srch_h{$f}{col}});
							
						}
						else
						{	$table . $srch_h{$f}{col}[0] . " $op ".
							sql_quote($qstr);
						}
					} 
					sort
					map {
						sprintf("%03d", 500 - length($_))."\0".$_;
					} @$q);
				}
			} @$queryset);
		if (scalar @$queryset > 1)
		{	$where_strs{$f} = "(".$where_strs{$f}.")";
		}
	}

	if ($sc eq 'term')
	{	my $acc_like = $self->__get_accs;
		print STDERR "acc_like: ".Dumper($acc_like)."\n";
		if (@$acc_like)
		{	$selected->{acc} = 1;
			$self->set_param('selected', $selected);
			$where_strs{acc} = "(".join(') OR (', 
				map {
						"$srch_h{acc}{table}.$srch_h{acc}{col} LIKE ".sql_quote($_)
						} @$acc_like) .")";
		}
	}
	return \%where_strs;
}

sub __xrefer {
	my $q = shift;
	my $sc = shift;
	my $f = shift;
	my $exact = shift || 0;

	print STDERR "q = $q; sc = $sc; f = $f; exact = $exact\n";

	my $table =
	{	gp =>
		{	gpxref => 
			{	table => 'dbxref',
				to_check =>
				{	colon => [ 'mgi', 'rgd' ],
					rpt => [ 'ddb', 'fb' ]
				},
			},
			seq_xref =>
			{	table => 'seqxref',
				to_check =>
				{	colon => [ 'mgi', 'dip', 'hgnc', 'go' ],
					rpt => [ 'pirsf' ]
				},
			},
		},
		term =>
		{	xref =>
			{	table => 'dbxref',
			},
			dbxref =>
			{	table => 'dbxref',
			},
		},
	};

=sql queries:
select distinct seqxref.xref_dbname from seq_dbxref, dbxref AS seqxref where seqxref.id = seq_dbxref.dbxref_id and LOCATE(concat(seqxref.xref_dbname, ':'), seqxref.xref_key) != 0

select distinct seqxref.xref_dbname from seq_dbxref, dbxref AS seqxref where seqxref.id = seq_dbxref.dbxref_id and LOCATE(seqxref.xref_dbname, seqxref.xref_key) != 0

select distinct seqxref.xref_dbname from seq_dbxref, dbxref AS seqxref where seqxref.id = seq_dbxref.dbxref_id and LOCATE(seqxref.xref_dbname, seqxref.xref_key) = 0 and seqxref.xref_dbname IN ( [results of above query] )

select distinct dbxref.xref_dbname from gene_product, dbxref where gp.dbxref_id = dbxref.id and LOCATE(concat(dbxref.xref_dbname, ':'), dbxref.xref_key) != 0

select distinct dbxref.xref_dbname from gene_product, dbxref where gp.dbxref_id = dbxref.id and LOCATE(dbxref.xref_dbname, dbxref.xref_key) != 0

select distinct dbxref.xref_dbname from gene_product, dbxref where gp.dbxref_id = dbxref.id and LOCATE(dbxref.xref_dbname, dbxref.xref_key) = 0 and dbxref.xref_dbname IN ( [results of above query] )

=cut
	if ($q =~ /.*:.*/)
	{	my ($db, $key) = split(":", $q, 2);
		print STDERR "db = $db; key = $key\n";
		if ($table->{$sc}{$f}{to_check})
		{	print STDERR "Found the 'to_check' table\n";
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
		if ($exact)
		{	print STDERR "A Returning (".$table->{$sc}{$f}{table}.".xref_dbname=".sql_quote($db)." AND ".$table->{$sc}{$f}{table}.".xref_key=".sql_quote($key).")\n";

			return "(".$table->{$sc}{$f}{table}.".xref_dbname=".sql_quote($db)." AND ".$table->{$sc}{$f}{table}.".xref_key=".sql_quote($key).')';
		}
		else
		{	print STDERR "B Returning (".$table->{$sc}{$f}{table}.".xref_dbname=".sql_quote($db)." AND ".$table->{$sc}{$f}{table}.".xref_key LIKE ".sql_quote($key.'%').")\n";

			return "(".$table->{$sc}{$f}{table}.".xref_dbname=".sql_quote($db)." AND ".$table->{$sc}{$f}{table}.".xref_key LIKE ".sql_quote($key.'%').')';
		}
	}
	else
	{	if ($q =~ /^(ddb|fb|wb|gr)/i && $exact != 1 && $f eq 'dbxref' && $sc eq 'gp')
		{	print STDERR "C Returning (".$table->{$sc}{$f}{table}.".xref_dbname=".sql_quote($1)." AND ".$table->{$sc}{$f}{table}.".xref_key LIKE ".sql_quote($q.'%').")\n";

			return "(".$table->{$sc}{$f}{table}.".xref_dbname=".sql_quote($1)." AND ".$table->{$sc}{$f}{table}.".xref_key LIKE ".sql_quote($q.'%').')';
		}
		
		if ($exact)
		{	print STDERR "D Returning ".$table->{$sc}{$f}{table}.".xref_key=".sql_quote($q)."\n";

			return $table->{$sc}{$f}{table}.".xref_key=".sql_quote($q);
		}
		else
		{	print STDERR "E Returning ".$table->{$sc}{$f}{table}.".xref_key LIKE ".sql_quote('%'.$q.'%')."\n";

			return $table->{$sc}{$f}{table}.".xref_key LIKE ".sql_quote('%'.$q.'%');
		}
	}
}

sub _get_match {
	my $self = shift;
	my ($data, $sc, $queryset, $field_list) = @_;

#	print STDERR "field_list: ".Dumper(\@field_list)."\n";
	my @match_fields;
	FIELD_LOOP:
	foreach my $f ( @$field_list )
	{	if ($data->{$f})
		{	my $matchset = $data->{$f};
			if (ref($matchset ) ne 'ARRAY')
			{	$matchset = [ $matchset ];
			}
			foreach my $m (@$matchset)
			{	if ($self->__relevance_algorithm($m, $queryset, undef, { match_only => 1 }) == 1)
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
	my ($data, $sc, $queryset, $field_list, $exact) = @_;

#	print STDERR "\nget relevance: sc = $sc\n";
#	print STDERR "data: ".Dumper($data)."\n";
#	print STDERR "qset: ".Dumper($queryset)."\n";

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
			{	my $score = $self->__relevance_algorithm($m, $queryset, $factor, {relevance => 1}, $exact);
				next if !$score;
			#	print STDERR "score: $score; matchset: $m; match field: ".($fields{$f} || $f)."\n";
				
				$data->{all_rel_data}->{$m} = $score;
				
				if ($score > $best_match_rel)
				{	$best_match_rel = $score;
					$best_match_field = __fieldname($f);
					$best_match_text = $m;
					last if $best_match_rel == 1;
				}
				
#				print STDERR "string: $matchstr\n";
#				QUERY_LIST:
#				foreach my $q (@$perl_list)
#				{#	print STDERR "q: ".Dumper($q)."\n";
#					next if ($score == 0);
#					print STDERR "score: $score\n";
#				}
			}
		}
		else
		{	#print STDERR "$f: No matchset found!\n";
		}
	}

	if ($best_match_rel == 0)
	{	print STDERR "qset: ".Dumper($queryset)."\n";
		print STDERR "Data: ".Dumper($data)."\n";
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
	my $queryset = shift;

	return $self->__relevance_algorithm($matchstr, $queryset, __search_field_weighting($field), { hilite => 1, relevance => 1 });
}

sub __relevance_algorithm {
	my $self = shift;
	my $original_matchstr = shift;
	my $queryset = shift;
	my $factor = shift || 1;  # weighting factor for the field
	my $options = shift || {};
	my $exact = shift;

	my $best_rel_score = 0;  #	best relevance score
	my $matchtxt; #	best matching text
	my $n;


	QUERY_LIST:
	foreach my $q (@$queryset)
	{	my $matchstr = $original_matchstr;
		$n = 1;
		foreach (@$q)
		{	next QUERY_LIST unless ($matchstr =~ s/($_)/START_MATCH $n: $1END_MATCH/gi);
			$n++;
		}

		#	check for overlapping matches
		my @matches = split('START_MATCH', $matchstr);
	#	print STDERR "matches: ".Dumper(\@matches)."\n";
		next if grep { /END_MATCH.*?END_MATCH/ } @matches;

		#	delete the query from {queryset}{unmatched} (if it still exists)
		#	as we have found a match for it
		delete $self->{queryset}{unmatched}{$q} if $self->{queryset}{unmatched}{$q};

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
			
			#	my @matches = split('START_MATCH', $matchstr);
			#	print STDERR "matches: ".Dumper(\@matches)."\n";
			#	next QUERY_LIST if grep { /END_MATCH.*?END_MATCH/ } @matches;
	
			(my $match = $matchstr) =~ s/\s?(complex|activity)$//;
			
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
		
		#		print STDERR "l_q: $l_q, l_m = $l_m\n";
		#		my $score = (2x$l_q)/(2 x $l_q + $l_m);
			$score = $factor * (1 - ($l_m/($l_q + $l_m)));

			if ($score > $best_rel_score)
			{	$best_rel_score = $score;
				$matchtxt = __hiliter($matchstr) if $options->{hilite};
			#	if the match is exact, end here
				last QUERY_LIST if $score == 1;
			}
		}
		elsif ($options->{hilite})
		{	return __hiliter($matchstr);
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
	my $queryset = shift || $self->{queryset}{perllist};

	my $matchstr = $text;

	QUERY_LIST:
	foreach my $q (@$queryset)
	{	foreach (@$q)
		{	next QUERY_LIST unless $matchstr =~ s/($_)/START_MATCH$1END_MATCH/gi;
		}
		my @matches = split('START_MATCH', $matchstr);
		next if grep { /END_MATCH.*?END_MATCH/ } @matches;

#		encode_entities($matchstr);
#		$matchstr =~ s/START_MATCH(.*?)END_MATCH/<em class="hilite">$1<\/em>/g;
#		return $matchstr;
		return __hiliter($matchstr);
	}

	return $text if $return_orig;

	return undef;
}

sub __hiliter {
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
	
	print STDERR "Starting the obsolete check...\n";
	print STDERR "obs terms are ".Dumper($obs)."\n";

	my $apph = $self->apph;
	my $dbh = $apph->dbh;

	if (@$obs)
	{	#	get the comments for the term;
		#	return only comments with GO IDs in them
		my @terms_in_comments;
		my %not_in_results;
		my $comments;
		my $comment_id_to_term_acc;
		my $comment_id_to_term_id;

		if (get_environment_param("term2term_metadata_loaded"))
		{	my $rels = $dbh->selectall_arrayref("SELECT id WHERE name IN (". join(", ", map { sql_quote($_) } qw(replaced_by consider) ).")");
			my $sql = "SELECT term1_id, term2_id FROM term2term_metadata WHERE term_id IN ("
			.join(", ", @$obs)
			.") AND relationship_type_id IN ("
			.join(",", map { $_->[0] } @$rels).")";
			$comments = $dbh->selectall_hashref($sql, 'term_id');
		#	transform into term id and acc (id?) of consider term
			foreach (keys %$comments)
			{	push @{$comment_id_to_term_id->{$_}}, $comments->{$_}{term_comment};
			}
		}
		else
		{	
			my $sql =
					"SELECT term_id, term_comment FROM term_definition WHERE term_id IN ("
					.join(", ", @$obs).") AND term_comment REGEXP ".sql_quote(".*GO:[0-9]{7}.*");
			print STDERR "sql: $sql\n";
	
			$comments = $dbh->selectall_hashref($sql, 'term_id');
		#	transform into term id and acc (id?) of consider term

			print STDERR "Comments: ".Dumper($comments);

			foreach (keys %$comments)
			{	my @cterms = grep { s/.*?(GO:\d{7}).*/$1/g } split(/\s/, $comments->{$_}{term_comment});
				if (@cterms)
				{	$comment_id_to_term_acc->{$_} = \@cterms;
				}
			}
		#	print STDERR "comments: ".Dumper($comments)."\n";
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
					{	if ($not_in_results{$c} || !grep { /$c/ } keys %$results)
						{	$not_in_results{$c} = 1;
							next COMMENT_LOOP;
						}
					}
				}
			}
			print STDERR "deleting $id\n";
			delete $results->{$id};
		}
	

	}
	return [values %$results];
}

sub _sort_results {
	my $self = shift;
	my $results = shift;
	my $sc = $self->get_param('search_constraint');

#	print STDERR "results: ".Dumper($results)."\n";


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
#				{	push @$sortby, $_ if ($_ ne $sort_crit && $_ ne 'rel');
				}
			}
		}
	}
	else
	{	$sortby = __sort_default($sc, $self->{get_relevance});
	}
	
	unshift @$sortby, 'is_obsolete' if $sc eq 'term';
	
	print STDERR "sortby is ".Dumper($sortby)."\n";

	my %refs;
	@refs{ map { $_->{id} } @$results } = @$results; # cache references

#	print STDERR "keys of refs: ".Dumper([ keys %refs ]);

	my @sorted = map { $refs{(split("\0", $_))[-1]} }
					sort
#					map { __data_for_sort($_, $sc, $sortby) }
					map { my $obj = $_;
					#	my $str = 
						join("\0", 
							map
							{	if ($_ eq 'rel')
								{	my @rel_sort = %{$obj->{source}};
								#	my @pair = (sprintf( "%05d", (10000 - $rel_sort[1] * 10000)), 
								#		__field_importance($rel_sort[0]));
								#	print STDERR "pair: ".Dumper(\@pair)."\n";
								#	@pair;
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
									{	print STDERR "Error: did not return a value for sorting! $sc, $_, $obj\n";
										print STDERR Dumper($obj)."\n";
										0;
									}
								}
							} @$sortby) . "\0".$obj->{id};
#						$obj->{id});
				#		$str;
					}
					@$results;

	return \@sorted;
}

sub __data_for_sort {
	my $obj = shift;
	my $sc = shift;
	my $crit = shift;

	my @list = map
	{	if ($_ eq 'rel')
		{	#	print STDERR "keys: ".keys %{$obj->{src}}."\n";
#			if (!@{$obj->{source}})
			if (!$obj->{source})
			{	(10000, 10);
			}
			else
			{	#print STDERR "source: 0: ".$obj->{source}[0]."; 1: ".$obj->{source}[1]."\n";
				my @rel_sort = %{$obj->{source}};
#				(sprintf( "%05d", (10000 - $obj->{source}[1] * 10000)), 
#					$rel{$obj->{source}[0]});
				(sprintf( "%05d", (10000 - $rel_sort[1] * 10000)), 
					__field_importance($rel_sort[0]));
			}
		}
		elsif (($_ eq 'binomial' && $sc eq 'spp') || ($_ eq 'spp' && $sc eq 'gp'))
		{	$obj->{genus}." ".$obj->{species};
		}
		else
		{	if (exists $obj->{$_})
			{	lc $obj->{$_};
			}
			else
			{	print STDERR "Error: did not return a value for sorting! $sc, $_, $obj\n";
				print STDERR Dumper($obj)."\n";
				0;
			}
		}
	} @$crit;
#	unshift @list, $obj;
#	print STDERR Dumper(\@list);
#	print STDERR Dumper($obj);
	my $ref = $obj->{id};
	return join("\0", (@list, $ref));
}

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

sub _create_term_search_result_obj {
	my $self = shift;
	my $apph = shift;
	my $term = GO::CGI::TermSearchResult->new(@_);
	$term->apph( $apph->apph );
	return $term;
}

sub _create_gp_search_result_obj {
	my $self = shift;
	my $apph = shift;
	my $gp = GO::CGI::GeneProductSearchResult->new(@_);
	$gp->apph( $apph->apph );
	return $gp;
}

sub _get_term_details {
	my $self = shift;
	my $term_ref = shift;

	my $apph = $self->apph;
	my $dbh = $apph->dbh;

	my $selected = $self->get_param('selected');
	my $queryset = $self->{queryset}{perllist};

	if (!@$term_ref)
	{	exit;
	}

#	create a term array since it's easier to update the data in it
	my $terms_by_id;
	my $ont_list = $self->get_param('ont_list');
	foreach (@$term_ref)
	{	#print STDERR "term ref looks like this:\n".Dumper($_)."\n";
		my $term = $self->_create_term_search_result_obj($apph, $_);
		$terms_by_id->[$term->id] = $term;
		$term->is_ontology_term(1) if (grep { $_ eq $term->namespace } @$ont_list);
		$term->source($_->{source}) if $_->{source};
#		$term->source($_->{source}->[0]) if $_->{source};
#		$term->best_match( [$_->{source}[1], $_->{source}[2]]) if $_->{source};
		$term->{all_rel_data} = $_->{all_rel_data};
		$_ = $terms_by_id->[$term->id];
	}

	# now let's populate our GO::Model::Term objects with
	# other adornments such as synonyms, dbxrefs and definitions
	print STDERR "\nGetting term defs...\n";

	my $sql = "SELECT term_id, term_definition, term_comment FROM term_definition WHERE term_id in (".join(", ", map { $_->id } @$term_ref).")";
	print STDERR "sql: $sql\n";
	my $sth = $dbh->prepare($sql);
	$sth->execute();

	while (my $d = $sth->fetchrow_arrayref) {
		if ($terms_by_id->[$d->[0]]) {
			my $def = $d->[1];
#			if ($terms_by_id->[$d->[0]]->source eq 'definition')
			if (exists $terms_by_id->[$d->[0]]->source->{definition})
			{	$def = $self->hilite($d->[1], 1, $queryset);
			}
			$terms_by_id->[$d->[0]]->definition($def);

#			if ($terms_by_id->[$d->[0]]->source eq 'comment')
			if ($terms_by_id->[$d->[0]]->source->{comment})
			{	$terms_by_id->[$d->[0]]->comment($self->hilite($d->[2], 1, $queryset));
			}

			if ($terms_by_id->[$d->[0]]->is_obsolete && !$terms_by_id->[$d->[0]]->comment)
			{	$terms_by_id->[$d->[0]]->comment($d->[2]);
			}
		}
	}

#	if the search constraint includes syns, load 'em
	if ($selected->{term_synonym}) {
		print STDERR "Looking for synonyms...\n";
#		my @termset = map { $_->id } grep { $_->source eq 'term_synonym' } @$term_ref;
		my @termset = map { $_->id } grep { exists $_->source->{term_synonym} } @$term_ref;

		if (@termset)
		{	print STDERR "synonyms: termset: ".join(", ", @termset)."\n";
			my $sql =
			"SELECT term_id, term_synonym, term.name FROM term_synonym, term WHERE term.id=term_synonym.synonym_type_id AND term_id in (".join(", ", @termset).")";
			my $sth = $dbh->prepare($sql);
			$sth->execute();

			if ($self->{get_relevance})
			{	while (my $d = $sth->fetchrow_arrayref)
				{	my $match = $self->_get_match_score_and_hilite($d->[1], $d->[2]."_term_synonym", $queryset);
					$terms_by_id->[$d->[0]]->add_match(['synonym', $d->[2]], $d->[1], @{$match}) unless !$match;
				}
			}
			else
			{	while (my $d = $sth->fetchrow_arrayref)
				{	my $match = $self->hilite($d->[1], 0, $queryset);
					$terms_by_id->[$d->[0]]->add_match(['synonym', $d->[2]], $d->[1], $match) unless !$match;
				}
			}
		}
	}

	if ($selected->{dbxref} || $selected->{xref}) {
		print STDERR "looking at xrefs\n";
#			my @termset = grep { (exists $term_h->{$_}{src}{dbxref} || exists $term_h->{$_}{src}{xref}) } keys %$term_h;
#		my @termset = map { $_->id } grep { ($_->source eq 'dbxref' || $_->source eq 'xref') } @$term_ref;
		my @termset = map { $_->id } grep { (exists $_->source->{dbxref} || exists $_->source->{xref}) } @$term_ref;
		if (@termset)
		{	print STDERR "dbxrefs: termset: ".join(", ", @termset)."\n";
			my $sql=
				"SELECT term_dbxref.term_id, CONCAT(dbxref.xref_dbname,':', dbxref.xref_key), IF(term_dbxref.is_for_definition=1,'def_xref','xref') FROM term_dbxref, dbxref WHERE term_dbxref.dbxref_id = dbxref.id AND term_id in (".join(", ", @termset).")";
			if ($selected->{dbxref})
			{	$sql .= " AND term_dbxref.is_for_definition = 0";
			}
		#	$sql .= " ORDER BY xref_dbname, xref_key";
			my $sth = $dbh->prepare($sql);
			$sth->execute();

			if ($self->{get_relevance})
			{	while (my $d = $sth->fetchrow_arrayref)
				{	my $match = $self->_get_match_score_and_hilite($d->[1], $d->[2], $queryset);
					$terms_by_id->[$d->[0]]->add_match($d->[2], $d->[1], @{$match}) if $match;
				}
			}
			else
			{	while (my $d = $sth->fetchrow_arrayref) {
					my $match = $self->hilite($d->[1], 0, $queryset);
					$terms_by_id->[$d->[0]]->add_match($d->[2], $d->[1], $match) if $match;
				}
			}
		}
	}


	if ($selected->{subset}) {
		print STDERR "looking at subsets\n";
#			my @termset = grep { exists $term_h->{$_}{src}{subset} } keys %$term_h;
#		my @termset = map { $_->id } grep { $_->source eq 'subset' } @$term_ref;
		my @termset = map { $_->id } grep { exists $_->source->{subset} } @$term_ref;
		if (@termset)
		{	print STDERR "subset: termset: ".join(", ", @termset)."\n";
			my $sql = "SELECT term_id, subset.acc, subset.name FROM term_subset, term AS subset WHERE subset.id=term_subset.subset_id AND term_id IN (".join(", ", @termset).") ORDER BY subset.acc, subset.name";
			my $sth = $dbh->prepare($sql);
			$sth->execute();

			if ($self->{get_relevance})
			{	while (my $d = $sth->fetchrow_arrayref) {
					#print STDERR "d: ".Dumper($d)."\n";
					my $acc = $self->_get_match_score_and_hilite($d->[1], 'subset_acc', $queryset);
					my $name = $self->_get_match_score_and_hilite($d->[2], 'subset_name', $queryset);
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
					my $acc = $self->hilite($d->[1], 0, $queryset);
					my $name = $self->hilite($d->[2], 0, $queryset);
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
	{	print STDERR "Getting the deep product count!\n";
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
#	print STDERR "term_h:\n".Dumper($term_h)."\n";

#	print STDERR "Results to return:\n".Dumper($term_ref)."\n";

	return $term_ref;
}

sub _get_gp_details {
	my $self = shift;
	my $gp_ref = shift;

	my $apph = $self->apph;
	my $dbh = $apph->dbh;

	my $selected = $self->get_param('selected');
	my $queryset = $self->{queryset}{perllist};

	# create GP objects and add dbxrefs, species info, etc.
	my $hashref = $dbh->selectall_hashref("SELECT gp.id, dbxref.xref_key AS acc, dbxref.xref_dbname AS speciesdb FROM gene_product gp, dbxref WHERE gp.dbxref_id=dbxref.id AND gp.id IN (".join(",", map{$_->{id}}@$gp_ref).")", "id");

#	create the gp array since it's easier to update the data in it
	my $gps_by_id;

	foreach (@$gp_ref)
	{	if (!$hashref->{$_->{id}})
		{	print STDERR "Could not find info for $_->{id}: something wrong with script?\n";
			next;
		}
		my %hash = (%{$hashref->{$_->{id}}}, %$_);
		$hash{full_name} = $hash{symbol} if !$hash{full_name};
		my $gp = $self->_create_gp_search_result_obj($apph, \%hash);
		foreach ('species_id', 'type_id', 'product_synonym', 'seq_xref', 'seq_name')
		{	$gp->{$_} = $hash{$_} if $hash{$_};
		}
#		$gp->source($hash{source}->[0]) if $hash{source};
		$gp->source($hash{source}) if $hash{source};
#		$gp->best_match( [$hash{source}->[1], $hash{source}->[2]]) if $hash{source};
	#	print STDERR "gp: ".Dumper($gp)."hash: ".Dumper(\%hash)."\n";
		$gps_by_id->[$_->{id}] = $gp;
		$_ = $gp;
	}

	#	Add species, type, whether or not has sequence
	$apph->_get_product_species($gp_ref);
	$apph->_get_product_types($gp_ref);
	
	use GO::CGI::Query;
	if ((!$selected->{seq_name} && !$selected->{seq_xref}) || !$self->{from_cache})
	{	_get_products_seqs($apph, $gp_ref, 'has_seq');

		if ($selected->{seq_name})
#		{	my @list = grep { $_->source eq 'seq_name' } @$gp_ref;
		{	my @list = grep { exists $_->source->{seq_name} } @$gp_ref;
			if (@list)
			{	if ($self->{get_relevance})
				{	foreach my $gp (@list)
					{	foreach (@{$gp->{seq_name}})
						{	my $match = $self->_get_match_score_and_hilite($_, 'seq_name', $queryset);
							$gp->add_match('seq_name', $_, @$match) if $match;
						}
					}
				}
				else
				{	foreach my $gp (@list)
					{	foreach (@{$gp->{seq_name}})
						{	my $match = $self->hilite($_, 0, $queryset);
							$gp->add_match('seq_name', $_, $match) if $match;
						}
					}
				}
			}
		}
		if ($selected->{seq_xref})
#		{	my @list = grep { $_->source eq 'seq_xref' } @$gp_ref;
		{	my @list = grep { exists $_->source->{seq_xref} } @$gp_ref;
			if (@list)
			{	if ($self->{get_relevance})
				{	foreach my $gp (@list)
					{	foreach (@{$gp->{seq_xref}})
						{	my $match = $self->_get_match_score_and_hilite($_, 'seq_xref', $queryset);
							$gp->add_match('seq_xref', $_, @$match) if ($match)
						}
					}
				}
				else
				{	foreach my $gp (@list)
					{	foreach (@{$gp->{seq_xref}})
						{	my $match = $self->hilite($_, 0, $queryset);
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
#		print STDERR "ph: ".Dumper(\%ph)."\n";
		my @pids = keys %ph;

		if (@pids) {
			my $cols = "gene_product_seq.*, seq.id";

			my %gp_seq_name;
			if ($selected->{seq_name}) {
				my @list = grep { exists $_->source->{seq_name} } @$gp_ref;
#				my @list = grep { $_->source eq 'seq_name' } @$gp_ref;
				if (@list)
				{	@gp_seq_name{ map { $_->id } @list } = (1) x @list;
					$cols .= ", seq.display_id";
				}
			}

			my %gp_seq_xref;
			if ($selected->{seq_xref}) {
#				my @list = grep { $_->source eq 'seq_xref' } @$gp_ref;
				my @list = grep { exists $_->source->{seq_xref} } @$gp_ref;
				if (@list)
				{	@gp_seq_xref{ map { $_->id } @list} = (1) x @list;
				}
			}
			
			print STDERR "seq_name: ".(scalar keys %gp_seq_name)."; gp_seq_xref: ".(scalar keys %gp_seq_xref)."\n";

			my $hl = select_hashlist($dbh,
						["gene_product_seq", "seq"],
						["seq.id = seq_id", "gene_product_id in (".join(',', @pids).")"],
						[$cols]
						);


			if (keys %gp_seq_xref || keys %gp_seq_name)
			{	my (@seqs, @byid);
				foreach my $h (@$hl)
				{	my $p = $ph{$h->{gene_product_id}};
					my $seq = $apph->create_seq_obj($h);
					$p->add_seq($seq);
					if ($gp_seq_name{ $h->{gene_product_id} })
					{	if ($self->{get_relevance})
						{	my $match = $self->_get_match_score_and_hilite($h->{display_id}, 'seq_name', $queryset);
							$gps_by_id->[$h->{gene_product_id}]->add_match('seq_name', $h->{display_id}, @{$match}) if $match;
						}
						else
						{	my $match = $self->hilite($h->{display_id}, 0, $queryset);
							$gps_by_id->[$h->{gene_product_id}]->add_match('seq_name', $h->{display_id}, $match) if $match;
						}
					}
						
					if ($gp_seq_xref{ $h->{gene_product_id} })
					{	print STDERR "Looking for ".$seq->id."\n";
						#	add this sequence to the list of seqs to check
						push(@seqs, $seq->id);
						$byid[$seq->id] = $h->{gene_product_id};
					}
				}

				if (@seqs && @byid)
				{	#	get the sequence IDs of the gps in gpset

					print STDERR "SQL = SELECT seq_id, CONCAT(xref_dbname,':', xref_key) AS seq_xref FROM dbxref, seq_dbxref WHERE dbxref.id=dbxref_id AND seq_id IN (".join(", ", @seqs).")\n";

					my $sth = $dbh->prepare("SELECT seq_id, CONCAT(xref_dbname,':', xref_key) AS seq_xref FROM dbxref, seq_dbxref WHERE dbxref.id=dbxref_id AND seq_id IN (".join(", ", @seqs).")");
					$sth->execute();
					if ($self->{get_relevance})
					{	while (my $d = $sth->fetchrow_arrayref)
						{	my $match = $self->_get_match_score_and_hilite($d->[1], 'seq_xref', $queryset);
							$gps_by_id->[$byid[$d->[0]]]->add_match('seq_xref', $d->[1], @{$match}) if $match;
						}
					}
					else
					{	while (my $d = $sth->fetchrow_arrayref)
						{#	print STDERR Dumper($_);
							my $match = $self->hilite($d->[1], 0, $queryset);
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

	if ($selected->{product_synonym}) {
		print STDERR "Looking for synonyms...\n";
#		my @gpset = grep { $_->source eq 'product_synonym' } @$gp_ref;
		my @gpset = grep { exists $_->source->{product_synonym} } @$gp_ref;
		if (@gpset)
		{	if ($self->{from_cache})
			{	#print STDERR "synonyms: gpset: ".join(", ", @gpset)."\n";
				my $sth = $dbh->prepare("SELECT * from gene_product_synonym WHERE gene_product_id in (".join(", ", map { $_->id } @gpset).")");
				$sth->execute();
				if ($self->{get_relevance})
				{	while (my $d = $sth->fetchrow_arrayref)
					{	my $match = $self->_get_match_score_and_hilite($d->[1], 'product_synonym', $queryset);
						$gps_by_id->[$d->[0]]->add_match('product_synonym', $d->[1], @$match) if $match;
					}
				}
				else
				{	while (my $d = $sth->fetchrow_arrayref)
					{	my $match = $self->hilite($d->[1], 0, $queryset);
						$gps_by_id->[$d->[0]]->add_match('product_synonym', $d->[1], $match) if $match;
					}
				}
			}
			else
			{	if ($self->{get_relevance})
				{	foreach my $gp (@gpset)
					{	foreach (@{$gp->{product_synonym}})
						{	my $match = $self->_get_match_score_and_hilite($_, 'product_synonym', $queryset);
							$gp->add_match('product_synonym', $_, @$match) if $match;
						}
					}
				}
				else
				{	foreach my $gp (@gpset)
					{	foreach (@{$gp->{product_synonym}})
						{	my $match = $self->hilite($_, 0, $queryset);
							$gp->add_match('product_synonym', $_, $match) if $match;
						}
					}
				}
			}
		}
	}
	
	if ($self->get_param('show_gp_counts'))
	{	my $counts = get_term_count_for_gps($apph, $gp_ref, 1);
		print STDERR "counts: ".Dumper($counts)."\n";
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
#	} else {
#		return [];
#	}
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

sub _set_selected {
	my $self = shift;
	my $fields = shift;
	my $option_h = shift;
	my $sc = $self->get_param('search_constraint');
	my $all_fields = __search_field_list($sc, 'all');

	print STDERR "fields: ";
	if ($fields && @$fields)
	{	print STDERR join(", ", @$fields)."\n";
	}
	else
	{	print STDERR "none set\n";
	}

	my $selected;
	if ($fields)
	{	if (grep { $_ eq 'all' } @$fields)
		{	$selected->{$_} = 1 foreach @$all_fields;
		}
		else
		{	#	ensure the fields are valid
			foreach (@$fields)
			{	$selected->{$_} = 1 unless (!grep { $_ } @$all_fields);
			}
		}
	}
	if (!$selected)
	{	map { $selected->{$_} = 1 } @{__search_field_list($sc, 'default')};
	}

	$self->set_param('selected', $selected);
	print STDERR "selected: ".Dumper($selected)."\n";
	
	if ($option_h && $option_h->{results_only})
	{	return $selected;
	}
	
#	put the selected fields in the order in which they should be
#	checked when working out the relevance of the term results
#	my @ordered_selected = grep { exists $selected->{$_} } @{$order->{$sc}};
#	my $ordered = __search_field_list($sc, 'ordered');

	my @ordered_selected = grep { exists $selected->{$_} } @{__search_field_list($sc, 'ordered')};
	$self->set_param('select_list', \@ordered_selected);
	print STDERR "ordered_selected: ".Dumper(\@ordered_selected)."\n";

	my $field_list;
	if ($sc eq 'spp')
	{	if ($selected->{common_name})
		{	$selected->{binomial} = 1;
		}
	}

	if ($self->{get_relevance})
	{	foreach my $field (@ordered_selected)
		{	push @$field_list, [ $_, __search_field_weighting($_) ] foreach @{__fieldname_alias($field)};
		}
	}
	else
	{	foreach my $field (@ordered_selected)
		{	push @$field_list, $_ foreach @{__fieldname_alias($field)};
		}
	}

	$self->set_param('field_list', $field_list);

	return $selected;
}

sub _set_cache_results {
	my $self = shift;
	my $cache = shift || undef;
	my $result_list = shift || undef;
	my $sc = $self->get_param('search_constraint');
	return unless $self->{cache_me};

	if ($self->{from_cache})
	{	#	the results came from the cache, so we don't need to change 'em...
		#	...unless, of course, we did a sort
		return unless $self->get_param('sort_me');
	}

	if ($result_list)
	{	#print STDERR "Source size: ".scalar @$result_list."\n";
		#print STDERR "Source: ".Dumper($result_list)."\n";
		$cache->{result_list} =
			[ map { 
						{	id => $_->{id},
							src => $_->{source}
						}
					} @$result_list ];
	}
#	elsif (!$cache)
#	{	$cache = $session->get_all_caching_params;
	#	print STDERR "Source size: ".scalar @{$cache->{result_list}}."\n";
#	}

	if ($self->{from_cache} && $cache->{large_result_set})
	{	delete $cache->{large_result_set};
	}

	#print STDERR "Params to be set:\n".Dumper($cache)."\n";

	$self->{results} = $cache;
	
	$cache->{queryset} = $self->{queryset}{orig};
	$cache->{queryset_sql} = $self->{queryset}{sql} if $self->{queryset}{sql};
	$cache->{queryset_perl} = $self->{queryset}{perl} if $self->{queryset}{perl};
#	$cache->{queryset_perllist} = $self->{queryset}{perllist} if $self->{queryset}{perllist};

	
	print STDERR "queryset:\n".Dumper($self->{queryset}{orig})."\n";

#	$self->_set_select_list if !$self->get_param('select_list');
	$cache->{$sc.'fields'} = $self->get_param('select_list');

	$self->cache($cache);
}

sub _get_url {
	my $self = shift;
	my $results = shift;
	my $sc = $self->get_param('search_constraint');

	print STDERR "results: ".Dumper($results)."\n";

	if ($sc eq 'term')
	{	if (scalar @$results == 1)
		{	return $results->[0]{acc};
		}
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

=cut

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

	print STDERR "\nStarting apph search...\n";

	if ($arg_h->{exact_match})
	{	$self->set_param('exact_match', 1);
	}
	
#	check the queryset and turn it into a structure that we can use
	my $success = $self->_set_queryset($arg_h->{query});
	return if (!$success);
	
#	find out what fields we're going to search
#	if nothing is specified, use the default
	$self->_set_selected($arg_h->{search_fields}, { results_only => 1 } );
	my $selected = $self->get_param('selected');

	print STDERR "Selected:\n".Dumper($selected)."\n";

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
	if (%{$self->{queryset}{unmatched}})
	{	#	We didn't find matches for everything.
		#	Add a warning message about queries that we didn't find a match for
		@{$result_h->{lost}} = map { join(" ", @$_ ) } values %{$self->{queryset}{unmatched}};
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

#		print STDERR "hashref: ".Dumper($hashref)."\n";
#		print STDERR "results: ".Dumper($results)."\n";

		foreach (@$results)
		{	if (!$hashref->{$_->{id}})
			{	print STDERR "Could not find info for $_->{id}: something wrong with script?\n";
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

### Data only ###

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
			all => [ 'name', 'term_synonym', 'definition', 'comment', 'xref', 'subset' ],
			default => [ 'name', 'term_synonym' ],
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
	);
	return $fields{$field} || $field;
}

#	fieldname used by AmiGO cache --> fieldname from AmiGO search
sub __fieldname_alias {
	my $field = shift;
#	term_synonym is an alias for exact / narrow / broad / related syns
#	xref is an alias for dbxref and def_xref
#	seq is an alias for seq_name and seq_xref
	my %alias = (
		term_synonym => [ 'exact_term_synonym', 'narrow_term_synonym', 'broad_term_synonym', 'related_term_synonym', 'alt_id_term_synonym'],
		xref => ['dbxref', 'def_xref'],
		subset => ['subset_acc', 'subset_name'],
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



# not used
sub __search_constraint_specific_data {
	my $data_to_get = shift;
	my $sc = shift;

	my $data = {
		sort_default => {
			gp => ['rel', 'symbol', 'full_name'],
			term => ['rel', 'name', 'acc', 'term_type'],
			spp => ['rel', 'binomial', 'common_name'],
		},
	};

	return $data->{$data_to_get}{$sc} || undef;
}



1;
