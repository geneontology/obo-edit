=head1 SYNOPSIS

package GO::CGI::Query

=cut

package GO::CGI::Query;

use strict;
use Carp;
use DBI;
use Exporter;
use GO::AppHandle;
use GO::CGI::Utilities qw(:all);
use GO::CGI::Session;
use GO::SqlWrapper qw(sql_quote select_hashlist);
use GO::Utils qw(rearrange);
use HTML::Entities;
use Time::HiRes qw(gettimeofday); # just for testing purposes

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
@ISA = ('Exporter');
@EXPORT_OK = qw(get_gp_details get_gp_assocs get_graph_for_gp
	get_term_in_graph get_term_assocs get_current_graph get_data_for_chart
	get_nit get_permalink get_fasta get_seqs_for_gps
	get_gp_count_for_terms get_term_count_for_gps
	get_consider_and_replaced_by_terms);

our $verbose = get_environment_param('verbose');

=head2 get_gp_details

	Arguments - apph, error, listref of GP xrefs or IDs from the database,
	            option_h containing
	            - tmpl (opt; template for which bits of GP info to get),
	            - use_filters (opt; whether or not to use filters)
	            - ignore_errors (opt; whether to report errors in error)
	Returns   - hashref of the form
	            results => [listref of unsorted GP objects or undefined],
	            error => messages hash
	
	my $gp_list = get_gp_details($apph, $error, $gps, $option_h);

	where $gps is of the form
	{ gpxref => [ dbA:acc1, dbA:acc2, dbB:acc3 ] } or
	{ id => [ 2948271, 3985271, 29481 ] }
	
	and tmpl is a hashref which can include the following:
	{	synonyms => 1,     # load GP synonyms
		spp => 1,          # get species info
		gptype => 1,       # get gp type info
		seq => 1,          # get all the seq info OR
		has_seq => 1,      # just indicate whether the GP has a seq
	}
	
	the default is to get all info

=cut

sub get_gp_details {
	my ($apph, $error, $constr, $option_h) = rearrange([qw(apph error constr option_h)], @_);

	my $gps = $constr->{gpxref} || $constr->{id} || $constr->{seq_xref};
	if (!ref($gps))
	{	$gps = [$gps];
	}
	#	check for / remove dups and blank entries
	my %hash;
	foreach (@$gps)
	{	$hash{$_} = 1 if defined $_;
	}
	if (!keys %hash)
	{	print STDERR "GP list is empty\n" if $verbose;
		$error = set_message($error, 'fatal', 'no_gp');
		return { error => $error };
	}
	$gps = [ keys %hash ];

	
	my $tmpl = $option_h->{tmpl};
	if (!$tmpl)
	{	#	the default (as used by gp-details.cgi) is to have all the info
		$tmpl = {
			synonyms => 1,
			seq => 1,
			spp => 1,
			gptype => 1,
		};
	}
	
	print STDERR "tmpl: ".Dumper($tmpl) if $verbose;
	my $dbh = $apph->dbh;

	my $tables = ["gene_product", "dbxref"];
	my $where = ["gene_product.dbxref_id = dbxref.id"];
	
	if ($constr->{gpxref})
	{	push @$where, "(".join(" OR ", 
			map {
					if ($_ =~ /(.*?):(.*)/) {
						"(dbxref.xref_dbname = ".sql_quote($1).
						" AND dbxref.xref_key = ".sql_quote($2).")";
					}
					else {
						"dbxref.xref_key = ".sql_quote($_);
					}
				} @{$gps}). ")";
	}
	elsif ($constr->{id})
	{	push @$where, "gene_product.id IN (".join(",", map { sql_quote($_) }@{$gps} ). ")";
	}
	elsif ($constr->{seqxref})
	{	push @$tables, 'gene_product_seq', 'seq', 'seq_dbxref', 'dbxref seqxref';
		push @$where, 
		'gene_product.id=gene_product_seq.gene_product_id',
		'gene_product_seq.seq_id=seq.id',
		'seq.id=seq_dbxref.seq_id',
		'seq_dbxref.dbxref_id=seqxref.id';
		push @$where, "(".join(" OR ", 
			map {
					if ($_ =~ /(.*?):(.*)/) {
						"(seqxref.xref_dbname = ".sql_quote($1).
						" AND seqxref.xref_key = ".sql_quote($2).")";
					}
					else {
						"seqxref.xref_key = ".sql_quote($_);
					}
				} @{$gps}). ")";
	}

	if ($option_h->{use_filters} && keys %{$apph->filters})
	{	my $what_to_filter;
		if ($option_h->{use_filters} eq 'gp')
		{	#	ignoring association filters for the time being
			#	the filters that apply to GPs are speciesdb, type and taxid
			$what_to_filter = ['gp'];
		}
		_set_filters($apph->filters, $dbh, $tables, $where, $what_to_filter);
	}

	my $sql = "SELECT DISTINCT gene_product.*, dbxref.xref_key AS acc, dbxref.xref_dbname AS speciesdb from " . join(", ", @$tables) . " WHERE " . join(" AND ", @$where);

	if ($sql =~ /\(\)/)
	{	print STDERR "Problem with the SQL!\n$sql\n" if $verbose;
		$error = set_message($error, 'fatal', 'sql', $sql);
		return { error => $error } ;
	}

	print STDERR "SQL: $sql\n" if $verbose;
	
	my $sth = $dbh->prepare($sql);
	$sth->execute();

	my %gp_h;
	
	while (my $d = $sth->fetchrow_hashref) {
	#	print STDERR "$d:\n".Dumper($d)."\n" if $verbose;
		foreach my $n qw(full_name symbol)
		{	#$d->{$n} = spell_greek($d->{$n});
			encode_entities($d->{$n});
		}
		$d->{full_name} = $d->{symbol} if !$d->{full_name};
		$gp_h{$d->{id}} = $apph->create_gene_product_obj($d);
		$gp_h{$d->{id}}{species_id} = $d->{species_id};
		$gp_h{$d->{id}}{type_id} = $d->{type_id} if $d->{type_id};
	}

	if (keys %gp_h) {
		unless ($option_h->{ignore_errors})
		{	#	check we have all the GPs we were looking for
			if (scalar (keys %gp_h) != scalar @$gps)
			{	print STDERR "scalar gps = ".scalar @$gps."; scalar keys gp_h = ".scalar (keys %gp_h)."\n" if $verbose;
				my $missing;
				my %hash;
				if ($constr->{gpxref}) {
					map { $hash{ lc($_->xref->xref_dbname) . ":" . lc($_->xref->xref_key) } = 1 } values %gp_h;
				}
				elsif ($constr->{id}) {
					%hash = %gp_h;
				}

				foreach (@$gps)
				{	#if (!grep { $xref eq $_->speciesdb.":".$_->acc } values %gp_h)
					if (!$hash{ lc($_) })
					{	push @$missing, $_;
						print STDERR "Lost $_\n" if $verbose;
					}
				}
				$error = set_message($error, 'warning', 'gp_not_found', $missing);
			}
		}
		
		$apph->_get_product_species([values %gp_h]) if $tmpl->{spp};
		$apph->_get_product_types([values %gp_h]) if $tmpl->{gptype};

		if ($tmpl->{synonyms}) {
		# get synonyms (spelt greek)
			my $sl =
				select_hashlist($dbh,
					"gene_product_synonym",
					"gene_product_id in (".join(", ", keys %gp_h).")");
	
			foreach (@$sl) {
				$gp_h{$_->{gene_product_id}}->add_synonym( encode_entities($_->{product_synonym}));
			}
		}

		if (!$tmpl->{seq})
		{	if ($tmpl->{has_seq})
			{	#	just see if it has a sequence, don't retrieve the seq
				get_seqs_for_gps($apph, [values %gp_h], 'has_seq');
			}
		}
		else {
			get_seqs_for_gps($apph, [values %gp_h] );
		}
#		return [values %gp_h];
		return { error => $error, results => [values %gp_h ] };
	}

	$error = set_message($error, 'fatal', 'gp_not_found', $gps) if !$option_h->{ignore_errors};
	print STDERR "No gps found! error: ".Dumper($error) if $verbose;
#	$error = set_message($error, 'fatal', 'gp_not_found', $gps);
	return { error => $error };
}

=head2 get_gp_assocs

	Arguments - apph, listref of GP xrefs, error,
	            option_h containing
	            - tmpl (opt; templates for the format of the results)
	            - cache (results from prev query, if appropriate)
	            - die_nicely (opt; if present, produces page-specific error msg)
	            - termsort, gpsort (optional)
	            - gp_count_ok (whether we can get the assoc count for the terms)
	            - use_paging (whether or not we should page the results)
	            - page_size (opt; defaults to $ENV{AMIGO_PAGE_SIZE})
	            - page (opt; defaults to 1)
	Returns   - listref of GP objects or undefined, with an error message
	
	my $gp_list = get_gp_assocs({ apph => $apph, gp_list => $gps, error => $error, option_h => $option_h });

	where $gps is of the form
	{ gpxref => [ dbA:acc1, dbA:acc2, dbB:acc3 ] } or
	{ id => [ 2948271, 3985271, 29481 ] }
	
=cut

sub get_gp_assocs {
	my $args = shift;
	
	my ($apph, $gps, $error, $option_h) = ($args->{apph}, $args->{gp_list}, $args->{error}, $args->{option_h});

	if (!$apph || !$gps || !@$gps || !$option_h)
	{	$error = set_message($error, 'fatal', 'config_error', 'Missing vital arguments for get_gp_assocs: cannot continue.');
		return { error => $error };
	}

	my $pairs;       # pairs of term accs and GP ids
	my $term_h;      # term info for all terms
	my $product_h;   # GP info for all GPs
	my $to_cache;    # save info for caching here
	my $q_gp_list;   # valid GPs from the query list
	my $n_pages;     # number of pages of results we have

	my %terms_to_get;
	my %gps_to_get;

	my $gp_opts = { 
		tmpl => $option_h->{tmpl}{gp} || { has_seq => 1, gptype => 1, spp => 1 },
		use_filters => 'gp',
	};

	if ($option_h->{cache} && @{$option_h->{cache}})
	{	$pairs = $option_h->{cache};
		print STDERR "Using cached results.\n" if $verbose;
	#	print STDERR "pairs: ".Dumper($pairs)."\n" if $verbose;

		#	get the subset of terms and gps we want to look at
		if ($option_h->{use_paging} && $option_h->{use_paging} == 1) # paging turned on
		{	print STDERR "use paging is ON, mofo!\n" if $verbose;
			my $paged_results = get_results_chunk($pairs, 
			{	chunk_size => $option_h->{page_size},
				chunk_n => $option_h->{page},
				chunk_by => $option_h->{chunk_by} });
			$pairs = $paged_results->{subset};
			$n_pages = $paged_results->{n_chunks};
		}

		my @ordered_gps;
		my $last = '';
		foreach (@$pairs)
		{	$terms_to_get{$_->[0]} = 1;
			push @ordered_gps, $_->[1] unless $_->[1] eq $last;
			$last = $_->[1];
			$gps_to_get{$_->[1]} = 1;
		}

		#	get the GPs
		my $result_h = get_gp_details($apph, $error, { gpxref => $gps }, $gp_opts);
		$error = $result_h->{error};
		#	this shouldn't happen unless something's gone horribly wrong, but...
		return { error => $error } unless $result_h->{results};

		#	put the products into product_h, indexed by ID
		#	make a temporary hash of products indexed by gpxref to
		#	create the list of valid GP objects from
		my %temp;
		foreach (@{$result_h->{results}})
		{	$product_h->{$_->id} = $_;
			$temp{ $_->xref->xref_dbname .":". $_->xref->xref_key } = $product_h->{$_->id};
		}

		if ($option_h->{gpsort})
		{	if ($option_h->{gpsort} eq 'ordered_input')
			{	@$q_gp_list = map { $temp{$_} } @$gps;
			}
			else
			{	@$q_gp_list =
				map
				{	$product_h->{ (split("\0", $_))[-1] }
				}
				sort
				map
				{	my $gp = $_;
					join("\0", (map { $gp->$_ } @{$option_h->{gpsort}}), $gp->id)
				}
				values %$product_h;
			}
		}
		else
		{	print STDERR "No sort params found\n" if $verbose;
			@$q_gp_list = values %$product_h;
		}
	}

	if (!$pairs)
	{	#	get brief info about each gp - symbol, name, spp, dbxref, type
		my $result_h = get_gp_details($apph, $error, { gpxref => $gps }, $gp_opts);
		$error = $result_h->{error};
		return { error => $error } unless $result_h->{results};
		print STDERR "Found some gps\n" if $verbose;

		#	put the products into product_h, indexed by ID
		#	make a temporary hash of products indexed by gpxref to
		#	create the list of valid GP objects from

		my %temp;

		foreach (@{$result_h->{results}})
		{	$product_h->{$_->id} = $_;
			$temp{ $_->xref->xref_dbname .":". $_->xref->xref_key } = $product_h->{$_->id};
		}

		if ($option_h->{gpsort})
		{	if ($option_h->{gpsort} eq 'ordered_input')
			{	print STDERR "Sorting by ordered input\n" if $verbose;
				foreach (@$gps)
				{	push @$q_gp_list, $temp{$_} if $temp{$_};
				}
			}
			else
			{	print STDERR "Sorting by gpsort params\n" if $verbose;
				@$q_gp_list =
				map
				{	$product_h->{ (split("\0", $_))[-1] }
				}
				sort
				map
				{	my $gp = $_;
					join("\0", (map { $gp->$_ } @{$option_h->{gpsort}}), $gp->id)
				}
				values %$product_h;
			}
		}
		else
		{	print STDERR "No sort params found\n" if $verbose;
			@$q_gp_list = values %$product_h;
		}


### INSERT!


		#	if check_results is on, check this query isn't going to return too
		#	many results
		if ($option_h->{check_results})
		{	my $dbh = $apph->dbh;
			print STDERR "Checking the number of results...\n" if $verbose;
			#	find out the number of GP-term combos we have
			my $tables = ["association"];
			my $where = [];

			#	if the query is a show_all_ass one...:
			push @$where, "association.gene_product_id IN (".join(",", map { $_->id } @$q_gp_list).")";

			#	set the filters
			_set_filters($apph->filters, $dbh, $tables, $where, ['term', 'assoc']);

			my $sql = "SELECT association.gene_product_id, COUNT(DISTINCT association.term_id) FROM "
			.join(", ", @$tables) . " WHERE "
			.join(" AND ", @$where) . " GROUP BY association.gene_product_id";

			print STDERR "SQL: $sql\n" if $verbose;
			my $results = $dbh->selectall_arrayref($sql);
			my $pair_count = 0;
			foreach (@$results)
			{	$pair_count += $_->[1];
			}
			print STDERR "pair count: $pair_count\nResult of count query: ".Dumper($results)."\n" if $verbose;

			#	option_h contains the maximums for downloads and number of pages that
			#	users are allowed to see. Check the number of results against that figure.
			my $max_html = $option_h->{max_results_html};
			my $max_dl = $option_h->{max_results_download};

			#print STDERR "Max_html: ".$option_h->{max_results_html}."\n";
			#print STDERR "Max_dl: ".$option_h->{max_results_download}."\n";

			if ($pair_count > $max_dl ||  #	too many results to download or
			($pair_count > $max_html && !$option_h->{'format'}) ) # display as HTML
			{	#	too many results. Die!
				$error = set_message($error, 'fatal', 'too_many_assocs', [map { $_->symbol } @$q_gp_list ]);
				if ($option_h->{die_nicely})
				{	return gp_assoc_die($apph, $product_h, $error, [map { { gp => $_ } } @$q_gp_list],
					{	max_results_html => $option_h->{max_results_html},
						max_results_download => $option_h->{max_results_download},
						page_size => $option_h->{page_size},
						n_results => $pair_count,
						gp_term_count => $results,
					});
				}
				return { error => $error };
			}
			elsif ($pair_count == 0)
			{	print STDERR "No pairs found. Aborting...\n" if $verbose;
				$error = set_message($error, 'fatal', 'no_gp_assocs', [ map { $_->symbol.", ".$_->xref->xref_dbname.":".$_->xref->xref_key } values %$product_h]);
				if ($option_h->{die_nicely})
				{	return gp_assoc_die($apph, $product_h, $error, [map { { gp => $_ } } @$q_gp_list],
					{	max_results_html => $option_h->{max_results_html},
						max_results_download => $option_h->{max_results_download},
						page_size => $option_h->{page_size},
						n_results => 0,
					});
				}
				return { error => $error };
			}
		}




### END INSERT!

		#	get the pairs of gps and terms
		$pairs = get_assoc_pairs($apph, { products => [ map{ $_->id } values %$product_h ] }, { termsort => $option_h->{termsort}, gpsort => $option_h->{gpsort}, filters => ['assoc', 'term'] } );

	#	print STDERR "pairs:\n".Dumper($pairs)."\n" if $verbose;
	
		if (!$pairs || !@$pairs)
		{	print STDERR "No pairs found. Aborting...\n" if $verbose;
			$error = set_message($error, 'fatal', 'no_gp_assocs', [ map { $_->symbol.", ".$_->xref->xref_dbname.":".$_->xref->xref_key } values %$product_h]);
	
	#		if ($option_h->{cgi} && $option_h->{cgi} eq 'gp-assoc')
			if ($option_h->{die_nicely})
			{	return gp_assoc_die($apph, $product_h, $error, [map { { gp => $_ } } @$q_gp_list],
				{	max_results_html => $option_h->{max_results_html},
					max_results_download => $option_h->{max_results_download},
					page_size => $option_h->{page_size},
					n_results => 0,
				});
			}
			return { error => $error };
		}

	#	if the gpsort parameter was 'ordered_input',
	#	put these in the order they appear in in @$gps
	#	check for any GPs with no associations
		my @no_assocs;
		my @sorted_pairs;
		if ($option_h->{gpsort})
		{	#	group pairs by GP id
			my %gpid_h = ();
			foreach (@$pairs)
			{	#push @{$gpid_h{$_->[1]}}, $_->[0];
				push @{$gpid_h{$_->[1]}}, $_;
			}
		#	print STDERR "gpid_h: ".Dumper(\%gpid_h) if $verbose;

			foreach (@$q_gp_list)
			{	if ($gpid_h{$_->id})
				{	#push @sorted_pairs, [ $_, $id ] foreach @{$gpid_h{$id}};
					push @sorted_pairs, @{$gpid_h{$_->id}};
				}
				else
				{	push @no_assocs, $_;  # report any GPs with no assocs
				}
			}
		}
		else
		{	@sorted_pairs = @$pairs;
			if (!$option_h->{tmpl}{assoc}{return_graph})
			{	print STDERR "Checking for missing GPs in unsorted pairs\n" if $verbose;
				#	check for any gps with no associations
				#	(but only if we're returning html results)
				my %seen;
				map { $seen{$_->[1]}++ } @$pairs;
				@no_assocs = grep { !exists $seen{$_->id} } @$q_gp_list;
			}
		}

		if (@no_assocs)
		{	print STDERR "No assocs found for ".join(", ", @no_assocs)."\n" if $verbose;
			$error = set_message($error, 'warning', 'no_gp_assocs', [ map { $_->symbol.", ".$_->xref->xref_dbname.":".$_->xref->xref_key } @no_assocs ]);
		}

		#	set the cache
		$to_cache->{term_product_ids} = \@sorted_pairs;
		$to_cache->{query}{gp} = join(",", map { $_->xref->xref_dbname.":".$_->xref->xref_key } @$q_gp_list);

		$pairs = \@sorted_pairs;
		#	get the subset of terms and gps we want to look at
		if ($option_h->{use_paging} && $option_h->{use_paging} == 1) # paging turned on
		{	print STDERR "use paging is ON, mofo!\n" if $verbose;
			my $paged_results = get_results_chunk($pairs, 
			{	chunk_size => $option_h->{page_size},
				chunk_n => $option_h->{page},
				chunk_by => $option_h->{chunk_by} });
			$pairs = $paged_results->{subset};
			$n_pages = $paged_results->{n_chunks};
		}

		#	find out which terms to get and which GPs are involved
		foreach (@$pairs)
		{	$terms_to_get{$_->[0]} = 1;
			$gps_to_get{$_->[1]} = 1;
		}
	}

	### finished getting the pairs. Phew!

	#	retrieve the term info
	if (keys %terms_to_get)
	{	#	if we have filters on anything other than ont and speciesdb,
		#	don't get n deep associations
		my $tmpl = $option_h->{tmpl}{term};
		my $get_n_deep = 0;
		if ($tmpl->{n_deep_products})
		{	$get_n_deep = 1;
			delete $tmpl->{n_deep_products};
		}
		
		my $terms = $apph->get_terms({accs=>[keys %terms_to_get]}, $tmpl);
		
		get_gp_count_for_terms($apph, $terms, { use_filters => 1, gp_count_ok => $option_h->{gp_count_ok} } ) if $get_n_deep;
		
		foreach (@$terms)
		{	$term_h->{$_->acc} = $_;
		}
#		print STDERR "term_h:\n".Dumper(\%term_h)."\n" if $verbose;
	}
	else
	{	print STDERR "No keys to term_h found. Aborting...\n" if $verbose;
		$error = set_message($error, 'fatal', 'no_gp_assocs', [ map { $_->symbol.", ".$_->xref->xref_dbname.":".$_->xref->xref_key } values %$product_h]);

		if ($option_h->{die_nicely})
		{	return gp_assoc_die($apph, $product_h, $error, [map { { gp => $_ } } @$q_gp_list],
			{	max_results_html => $option_h->{max_results_html},
				max_results_download => $option_h->{max_results_download},
				page_size => $option_h->{page_size},
				n_results => 0,
			});
		}
		return { error => $error };
	}

	print STDERR "gps_to_get: ".Dumper(\%gps_to_get)."\n" if $verbose;

	#	get the association data
	#	gps_to_get contains the GPs that are actually used in this annotation set
	my $assocs = get_association_data($apph, [values %$term_h], [map { $product_h->{$_} } keys %gps_to_get], $option_h->{tmpl}{assoc});

	#	no associations matching our filters. Oh no!
	#	something probably went wrong involving caching. Grrrrr.
	if (!$assocs)
	{	$error = set_message($error, 'fatal', 'no_gp_assocs', [ map { $_->symbol.", ".$_->xref->xref_dbname.":".$_->xref->xref_key } values %$product_h]);

		if ($option_h->{die_nicely})
		{	return gp_assoc_die($apph, $product_h, $error, [map { { gp => $_ } } @$q_gp_list],
			{	max_results_html => $option_h->{max_results_html},
				max_results_download => $option_h->{max_results_download},
				page_size => $option_h->{page_size},
				n_results => 0,
			});
		}
		return { error => $error };
	}
	
	#	return the results if a format is specified
	if ($option_h->{tmpl}{assoc}{return_graph})
	{	return { results => $assocs, error => $error, to_cache => $to_cache };
	}

	my $id2acc;    # term ID to term acc mapping
	foreach (values %$term_h)
	{	$id2acc->{ $_->id } = $_->acc;
	}

	#	put the association data into an easy-to-access hash keyed by
	#	gp id, term acc and qualifier
	my $assoc_h;
	foreach my $assoc (@$assocs) {
	#	go through the association list and group together assocs by term and qualifier
		my $q = 'a';
		if ($assoc->qualifier_list)
		{	$q = join(",", map { $_->name } @{$assoc->qualifier_list});
		}
		
		push @{ $assoc_h->{ $assoc->gene_product->id }{ $id2acc->{$assoc->{term_id}} }{ $q } }, $assoc;
	}

	#	convert $pairs into an index list for the template to use
	my $acc_id_list;

	#	if we're showing all results on one page and we've got more than one GP,
	#	put the GPs with no assocs into the list too
#	print STDERR "q_gp_list: ".Dumper($q_gp_list)."\n\n";

	if (scalar @$q_gp_list > 1 && $n_pages == 1)
	{	my %terms_by_gpid;
		#	collect terms by GP ID
		foreach (@$pairs)
		{	push @{$terms_by_gpid{ $_->[1] }}, $term_h->{$_->[0]};
		}
		
		foreach (@$q_gp_list)
		{	if ($terms_by_gpid{$_->id})
			{	push @$acc_id_list, { gp => $_, terms => $terms_by_gpid{$_->id} };
			}
			else
			{	push @$acc_id_list, { gp => $_ };
			}
		}
	}
	else
	{	my $last = '';
		foreach (@$pairs)
		{	#print STDERR "pair: ".$_->[0]." AND ".$_->[1]."\n" if $verbose;
			if ($_->[1] ne $last)
			{	push @$acc_id_list, { gp => $product_h->{$_->[1]}, terms => [ $term_h->{$_->[0]} ] };
				$last = $_->[1];
			}
			else
			{	push @{$acc_id_list->[-1]{terms}}, $term_h->{$_->[0]};
			}
			#print STDERR Dumper($acc_id_list) if $verbose;
		}
	}

	return { 
		results => {
			order => $acc_id_list,
			assoc_h => $assoc_h,
			product_h => $product_h,
			term_h => $term_h,
			n_pages => $n_pages,
		},
		to_cache => $to_cache,
		error => $error };
}

=head2 gp_assoc_die

Die nicely when get_gp_assocs goes wrong

	Arguments - apph, hash of GPs, error, order that the GPs should be in,
	            option_h containing
	            - show_all_ass (whether to get deep associations or not)
	            - gp_count_ok (if it's OK to use n_deep_products)
	            - max_results_html (max # of results that can be shown as html)
	            - max_results_download (max # of results OK to download)
	            - page_size
	            - format (the format, if applicable)
	            result_h containing
	            - n_results (number of results returned)
	            - count_arr (the count array from getting the GP counts)

	Returns   - results and error
	            if we found too many annotations, results is 
	            term_h # hash with info about the (valid) query terms
	            direct_children # number of direct children of the query terms
	            
	            for queries where we're showing all assocs, n_products is
	            populated (number of GPs annotated directly to the query term)

	            if we didn't find any annotations, results contains
	            term_h # hash with info about the (valid) query terms
	            total  # number of results we'd get without filters

=cut

sub gp_assoc_die {
	my ($apph, $product_h, $error, $order, $option_h, $result_h) = @_;
	my $dbh = $apph->dbh;

	print STDERR "Doing gp_assoc_die\n" if $verbose;

	if (!$order || !@$order)
	{	$order = [ map { { gp => $_ } } values %$product_h ];
	}
#	print STDERR "product_h keys: ".Dumper( \(keys %$product_h ) ) if $verbose;
#	print STDERR "order: ".Dumper($order) if $verbose;
	print STDERR "error: ".Dumper($error) if $verbose;

	#	set flags for what is and isn't possible:
	#	dl_ok - can download assocs
	#	html_ok - can view assocs
	my $check_dl_html_ok = sub {
		my $n_results = shift;
		my $ok;
		if ($n_results >= 1)
		{	if ($n_results < $option_h->{max_results_html})
			#	we can download or show results as html
			{	$ok->{"html_ok"} = 1;
				$ok->{"dl_ok"} = 1;
			}
			elsif ($n_results < $option_h->{max_results_download})
			#	too many to show as html, but allow downloading
			{	$ok->{"dl_ok"} = 1;
			}
		}
		$ok->{"n_results"} = $n_results;
		$ok->{"n_pages"} = get_n_chunks($n_results, $option_h->{page_size});
		return $ok;
	};

	my $info = {};
	my $name = 'total_unfiltered';
	my $use_filters;
	if ($result_h->{n_results} != 0)
	{	$use_filters = 1;
		$name = 'total';
	}
	
	#	if there are no associations, get the term count for the GPs *without filters*
	#	if too many associations, get the filtered term count for each GP in the list

	#	store info about the number of term associations for each GP
	#	- number of terms (n_results) and number of pages (n_pages)
	#	- whether we can display them as html (html_ok) or download them (dl_ok)

	my $gpid_termc = get_term_count_for_gps($apph, [values %$product_h], $use_filters);
	my %hash = (@$gpid_termc);
	my $sum = 0;
	foreach (keys %hash)
	{	$info->{gp}{$_} = &$check_dl_html_ok($hash{$_});
		$sum += $hash{$_};
	}

	#	store the same info for the whole set
	$info->{$name} = &$check_dl_html_ok($sum);


	#print STDERR "info: ". Dumper ($info) . "\n";

	return { results => {
					product_h => $product_h, 
					order => $order,
					n_pages => 1,
					error_hash => { %$info },
				},
				error => $error };
}

=head2 get_term_assocs

	Arguments - apph, list of term accs, error
	            option_h containing
	            - tmpl (optional)
	            - check_results (whether or not to check how many results
	              we've got)
	            - cache (any cached results from a previous query)
	            - die_nicely (page-specific error message for term-assoc.cgi)
	            - show_all_ass (whether to get deep associations or not)
	            - cgi and session_id (for redirection, if appropriate)
	            - use_paging (whether or not we should page the results)
	            - page_size (opt; defaults to $ENV{AMIGO_PAGE_SIZE})
	            - page (opt; defaults to 1)
	            - format (present if a format is specified)
	Returns   - data structure containing terms, associations and
	            GPs, or a warning message

=cut

sub get_term_assocs {
	my $args = shift;
	my ($apph, $term_list, $error, $option_h) = ($args->{apph}, $args->{term_list}, $args->{error}, $args->{option_h});

	if (!$apph || !$term_list || !@$term_list || !$option_h)
	{	$error = set_message($error, 'fatal', 'config_error', 'Missing vital arguments for get_term_assocs: cannot continue.');
		return { error => $error };
	}
	my $dbh = $apph->dbh;
	print STDERR "Starting Query::get_term_assocs...\n\n" if $verbose;

#	print STDERR "option_h: ".Dumper($option_h)."\n" if $verbose;

	my $pairs;        # pairs of term accs and GP ids
	my $term_h;       # term info for all terms
	my $product_h;    # GP info for all GPs
	my $q_term_list;  # the original query terms
	my $to_cache;     # save info for caching here
	my $n_pages;

	my $tmpl = { acc => 1, definition => 1 };
	
	my %terms_to_get;
	my %gps_to_get;

	if ($option_h->{cache})
	{	print STDERR "Looking for cached results...\n" if $verbose;
	#	get the pairs from the cached results
		$pairs = $option_h->{term_product_ids};
		if ($pairs)
		{	print STDERR "Using cached results.\n" if $verbose;

			#	get the subset of terms and gps we want to look at
			if ($option_h->{use_paging} && $option_h->{use_paging} == 1) # paging turned on
			{	print STDERR "use paging is ON, mofo!\n" if $verbose;
				my $paged_results = get_results_chunk($pairs, 
				{	chunk_size => $option_h->{page_size},
					chunk_n => $option_h->{page},
					chunk_by => $option_h->{chunk_by} });
				$pairs = $paged_results->{subset};
				$n_pages = $paged_results->{n_chunks};
			}
		}
	}
	
	if (!$pairs)
	{	#	this is a new query. Let's start with a few
		#	checks to make sure our terms are OK
		my $result_h = _get_terms_with_checks({
			apph => $apph,
			accs => $term_list,
			error => $error,
			option_h =>
			{	tmpl => $tmpl,
				is_ont_term => 1,
				is_obsolete => 1,
				is_root_term => 1,
				cgi => $option_h->{cgi},
				session_id => $option_h->{session_id},
			},
		});

		$error = $result_h->{error};
		return { error => $error } if !$result_h->{results};
		$q_term_list = $result_h->{results};
		#	set term_h to be the (valid) terms in q_term_list
		$term_h->{$_->acc} = $_ foreach @$q_term_list;

		#	if check_results is on, check this query isn't going to return too
		#	many results
		if ($option_h->{check_results})
		{	print STDERR "Checking the number of results...\n" if $verbose;
			#	find out the number of term-GP combos we have
			my $tables = ["association"];
			my $where = [];
			my $term_str = 'association.term_id';
			my $group_by = '';

			#	if the query is a show_all_ass one...:
			if ($option_h->{show_all_ass})
			{	push @$tables, "graph_path";
				push @$where, "association.term_id=graph_path.term2_id", "graph_path.term1_id IN (".join(",", map { $_->id } @$q_term_list).")";
				$term_str = 'term1_id';
				$group_by = 'term1_id';
			}
			else # direct associations only
			{	push @$where, "association.term_id IN (".join(",", map { $_->id } @$q_term_list).")";
				$group_by = 'association.term_id'
			}

			#	set the filters
			_set_filters($apph->filters, $dbh, $tables, $where, ['gp', 'assoc']);

			my $sql = "SELECT $term_str, COUNT(DISTINCT association.term_id, association.gene_product_id), COUNT(DISTINCT association.gene_product_id) FROM "
			.join(", ", @$tables) . " WHERE "
			.join(" AND ", @$where)
			." GROUP BY ". $term_str;

			print STDERR "SQL: $sql\n" if $verbose;
			my $results = $dbh->selectall_arrayref($sql);
			print STDERR "Result of count query: ".Dumper($results)."\n" if $verbose;

			my $pair_count = 0;
			foreach (@$results)
			{	#	add up col 2 [aka list item 1], which is the # of distinct GP-term pairs
				$pair_count += $_->[1];
			}
			
			#	option_h contains the maximums for downloads and number of pages that
			#	users are allowed to see. Check the number of results against that figure.
			my $max_html = $option_h->{max_results_html};
			my $max_dl = $option_h->{max_results_download};

#			print STDERR "Max_html: ".$option_h->{max_results_html}."\n";
#			print STDERR "Max_dl: ".$option_h->{max_results_download}."\n";

			if ($pair_count > $max_dl ||  #	too many results to download or
				($pair_count > $max_html && !$option_h->{'format'}) ) # display as HTML
			{	#	too many results. Die!
				$error = set_message($error, 'fatal', 'too_many_assocs', [map { $_->name . " ; " . $_->acc } @$q_term_list ]);
				return term_assoc_die($apph, $q_term_list, $error, $option_h, {
					n_results => $pair_count,
					count_arr => $results,
				});
			}
			elsif ($pair_count == 0)
			{	#	no assocs found. Die!
				$error = set_message($error, 'fatal', 'no_assocs', [map { $_->name . " ; " . $_->acc } @$q_term_list]);
				return term_assoc_die($apph, $q_term_list, $error, $option_h, {
					n_results => 0,
				});
			}
		}

		#	we've got the all clear. Let's get them associations!
		my $all_pairs = get_assoc_pairs($apph, { terms => { id => [ map { $_->id } @$q_term_list], show_all_ass => $option_h->{show_all_ass} } }, { termsort => $option_h->{termsort}, gpsort => $option_h->{gpsort}, filters => ['gp', 'assoc'] });

		print STDERR "got pairs:\n".Dumper(scalar @$all_pairs)."\n" if $verbose;
		if (!$all_pairs || !@$all_pairs)
		{	print STDERR "No pairs found.\n" if $verbose;
			$error = set_message($error, 'fatal', 'no_assocs', [map { $_->name . " ; " . $_->acc } @$q_term_list]);
			if ($option_h->{die_nicely})
			{	return term_assoc_die($apph, $q_term_list, $error, $option_h, {
					n_results => 0,
				});
			}
			return { error => $error };
		}

	#	sort 'n' save the results
	#	we want to save our terms with the query terms first
	#	pairs already has the terms and gps sorted correctly,
	#	so we just need to pull out the query terms
		my @sorted_pairs;

	#	no termsort option - don't bother sorting
		if (!$option_h->{termsort})
		{	@sorted_pairs = @$all_pairs;
		}
		else
		{	my $idx = 0;
			@sorted_pairs =
				map { [ (split("\0", $_))[2..3] ] }
				sort
				map {
					my $acc = $_->[0];
					join("\0", 
						(grep { $acc eq $_->acc } @$q_term_list) ? 0 : 1,
						sprintf("%08d",$idx++),
						$_->[0],
						$_->[1] );
				} @$all_pairs;

		#	print STDERR "list:\n".Dumper(\@list)."\n" if $verbose;
		}
#		print STDERR "sorted pairs:\n".Dumper(\@sorted_pairs)."\n" if $verbose;

		
		#	set the cache if no format is specified
		if (!$option_h->{'format'})
		{	$to_cache->{term_product_ids} = \@sorted_pairs;
			$to_cache->{query} = { term => join(",", sort map { $_->acc } @$q_term_list), show_all_ass => $option_h->{show_all_ass} };
		}

		$pairs = \@sorted_pairs;
		#	get the subset of terms and gps we want to look at
		if ($option_h->{use_paging} && $option_h->{use_paging} == 1) # paging turned on
		{	print STDERR "use paging is ON, mofo!\n" if $verbose;
			my $paged_results = get_results_chunk($pairs, 
			{	chunk_size => $option_h->{page_size},
				chunk_n => $option_h->{page},
				chunk_by => $option_h->{chunk_by} });
			$pairs = $paged_results->{subset};
			$n_pages = $paged_results->{n_chunks};
		}
	}

	### End of the pairs generating code. Phew!

#	get the term accs and GP IDs from $pairs
	foreach (@$pairs)
	{	$terms_to_get{ $_->[0] } = 1;
		$gps_to_get{ $_->[1] } = 1;
	}
	my $assoc_terms = [keys %terms_to_get];


	#	if we don't have any terms in term_h already (i.e. we got $pairs from
	#	cache), put the query terms in our set of terms to get
	$terms_to_get{$_} = 1 foreach @$term_list;


	if (keys %terms_to_get)
	{	my $get_n_deep = 0;
		if ($tmpl->{n_deep_products})
		{	$get_n_deep = 1;
			delete $tmpl->{n_deep_products};
		}
		
		my $terms = $apph->get_terms({accs=>[keys %terms_to_get]}, $tmpl);
		#	get the gp count if appropriate
		get_gp_count_for_terms($apph, $terms, { use_filters => 1, gp_count_ok => $option_h->{gp_count_ok} } ) if $get_n_deep;

		#	put the info into term_h
		$term_h->{$_->acc} = $_ foreach @$terms;
		#	put the query terms in q_term_list
		@$q_term_list = map { $term_h->{$_} } @$term_list;
	}

	print STDERR "gp template: ".Dumper($option_h->{tmpl}{gp})."\n" if $verbose;

	my $gp_opts = { tmpl => $option_h->{tmpl}{gp} || { has_seq => 1, gptype => 1, spp => 1 }};

#	get the GP data and put it into product_h
	my $res_h = get_gp_details($apph, $error, { id => [keys %gps_to_get]}, $gp_opts);
	$error = $res_h->{error};
	map { $product_h->{$_->id} = $_ } @{$res_h->{results}};
	
	# if appropriate, create a hash with the number of terms annotated per GP
	my $counts;
	if ($option_h->{show_term_counts} )
	{	my $list = get_term_count_for_gps($apph, [values %$product_h], 1);
		print STDERR "results of get_term_count_for_gps: ".Dumper($list) if $verbose;
		%$counts = (@$list);
	}
	
#	print STDERR "product_h:\n".Dumper($product_h)."\n" if $verbose;

	#	get the association data
	my $assocs = get_association_data($apph, [map { $term_h->{$_} } @$assoc_terms], [ values %$product_h ], $option_h->{tmpl}{assoc});

	#	no associations matching our filters
	if (!$assocs)
	{	$error = set_message($error, 'fatal', 'no_assocs', [map { $_->name . " ; " . $_->acc } @$q_term_list]);
		if ($option_h->{die_nicely})
		{	return term_assoc_die($apph, $q_term_list, $error, $option_h, {
				n_results => 0,
			});
		}
		else
		{	return { error => $error };
		}
	}

	#	return the results if a format is specified
#	if ($option_h->{tmpl}{assoc}{return_graph})
	if ($option_h->{'format'})
	{	return { results => $assocs, error => $error }; #, to_cache => $to_cache };
	}

#	print STDERR "associations:\n".Dumper($assocs)."\n" if $verbose;

	my $id2acc;    # term ID to term acc mapping
	foreach (values %$term_h)
	{	$id2acc->{ $_->id } = $_->acc;
	}

	#	put the association data into an easy-to-access hash keyed by
	#	term acc, gp id and qualifier
	my $assoc_h;
	foreach my $assoc (@$assocs) {
	#	go through the association list and group together assocs by term and qualifier
		my $q = 'a';
		if ($assoc->qualifier_list)
		{	$q = join(",", map { $_->name } @{$assoc->qualifier_list});
		}
		
		push @{ $assoc_h->{ $id2acc->{$assoc->{term_id}} }{ $assoc->gene_product->id }{ $q } }, $assoc;
	}


#	convert $pairs into an index list for the template to use
	my $acc_id_list;
	my $last = '';
	foreach (@$pairs) {
		if ($_->[0] ne $last)
		{	if (!$term_h->{$_->[0]}) {
				printf STDERR "something is wrong, no term for acc: %s\n", $_->[0] if $verbose;
				$term_h->{$_->[0]}++;
			}

			push @$acc_id_list, { term => $term_h->{$_->[0]}, gps => [ $product_h->{$_->[1]} ] };
			$last = $_->[0];
		}
		else
		{	
			push @{$acc_id_list->[-1]{gps}}, $product_h->{$_->[1]};
		}
	}


	#	if we're showing all associations, find the parents of the terms in term_h
	#	(but only if there was more than one term in the original term list)
	my $parent;    # the parent(s) of each term
	if ($option_h->{show_all_ass} && scalar @$q_term_list > 1)
	{	
		my $sql = "SELECT DISTINCT term2_id, term1_id FROM graph_path WHERE DISTANCE <> 0 AND term1_id IN ("
		.join(",", map { $_->id } @$q_term_list)
		.") AND term2_id IN ("
		.join(",", map { $_->id } values %$term_h)
		.")";
		print STDERR "SQL: $sql\n" if $verbose;

		my $results = $dbh->selectall_arrayref($sql);

		print STDERR "results of parentage query: ".Dumper($results)."\n" if $verbose;
		if (@$results)
		{	#	get the accs from this list of IDs
			#	parent: key - child term acc, value(s) - parent acc
			foreach (@$results)
			{	#push @{$temp_parents{ $id2acc->{$_->[0]} }}, $id2acc->{$_->[1]};
				push @{$parent->{$id2acc->{$_->[0]}}}, $id2acc->{$_->[1]};
			}
		}
	}
	
	return { 
		results => {
			order => $acc_id_list,
			assoc_h => $assoc_h,
			product_h => $product_h,
			term_h => $term_h,
			parent_h => $parent,
			n_pages => $n_pages,
			n_terms => $counts,
		},
		to_cache => $to_cache,
		error => $error };
}

=head2 term_assoc_die

Die nicely when get_term_assocs goes wrong

	Arguments - apph, list of terms, error
	            option_h containing
	            - show_all_ass (whether to get deep associations or not)
	            - gp_count_ok (if it's OK to use n_deep_products)
	            - max_results_html (max # of results that can be shown as html)
	            - max_results_download (max # of results OK to download)
	            - page_size
	            - format (the format, if applicable)
	            result_h containing
	            - n_results (number of results returned)
	            - count_arr (the count array from getting the GP counts)


	Returns   - results and error
	            if we found too many annotations, results is 
	            term_h # hash with info about the (valid) query terms
	            direct_children # number of direct children of the query terms
	            
	            for queries where we're showing all assocs, n_products is
	            populated (number of GPs annotated directly to the query term)

	            if we didn't find any annotations, results contains
	            term_h # hash with info about the (valid) query terms
	            total  # number of results we'd get without filters

=cut

sub term_assoc_die {
	my ($apph, $term_l, $error, $option_h, $result_h) = rearrange([qw(apph term_list error option_h)], @_);
	my $dbh = $apph->dbh;
	my $term_h;
	map { $term_h->{$_->acc} = $_ } @$term_l;
	my $info = {};
	my $children = 0; # number of children (if any) of the term(s)
	my $filters;      # boolean: whether or not the query is filtered
	if (keys %{$apph->filters})
	{	$filters = 1;
	}
	
	#print STDERR "max html: " . $option_h->{max_results_html}
	#. "\nmax dl: " . $option_h->{max_results_download} . "\n";

	print STDERR "Doing the die nicely routine: " if $verbose;
	my $get_f_string = sub {
		my $bool = shift;
		return 'filtered' if $bool;
		return 'unfiltered';
	};
	my $get_ass_string = sub {
		my $bool = shift;
		return 'all' if $bool;
		return 'direct';
	};

	my $check_dl_html_ok = sub 
	{	#	set flags for what is and isn't possible:
		#	dl_ok - can download assocs
		#	html_ok - can view assocs
		my $n_results = shift;
		my $ok;
		if ($n_results >= 1)
		{	if ($n_results < $option_h->{max_results_html})
			#	we can download or show results as html
			{	$ok->{"html_ok"} = 1;
				$ok->{"dl_ok"} = 1;
			}
			elsif ($n_results < $option_h->{max_results_download})
			#	too many to show as html, but allow downloading
			{	$ok->{"dl_ok"} = 1;
			}
		}
		$ok->{"n_results"} = $n_results;
		$ok->{"n_pages"} = get_n_chunks($n_results, $option_h->{page_size});
		return $ok;
	};

	my $get_children = sub
	{	#	get counts for the number of [direct] children of each term
		my $direct_children;
		my $count_1 = $dbh->selectall_arrayref("SELECT term1_id, COUNT(DISTINCT term2_id) FROM term2term WHERE term1_id IN (".join(", ", map { $_->id } @$term_l).") GROUP BY term1_id");
		my %count1_h;
		foreach (@$count_1)
		{	$count1_h{ $_->[0] } = $_->[1];
			$children += $_->[1];
		}
		map { $info->{term}{$_->acc}{direct_children} = $count1_h{ $_->id } || 0 } @$term_l;
	};
	
	my $get_counts = sub
	{	#	get the counts for the number of GP-term pairs
		my ($ass, $gp_count_ok, $use_filters) = @_;
		get_gp_count_for_terms($apph, $term_l, { show_all_ass => $ass, gp_count_ok => $gp_count_ok, use_filters => $use_filters });
		my $f_string = &$get_f_string($use_filters);
		my $ass_str = &$get_ass_string($ass);
		my $sum;
		foreach (@$term_l)
		{	if ($ass_str eq 'all')
			{	$sum += $_->n_deep_products foreach @$term_l;
				$info->{term}{$_->acc}{$ass_str}{$f_string} = &$check_dl_html_ok($_->n_deep_products);
			}
			else
			{	$sum += $_->n_products foreach @$term_l;
				$info->{term}{$_->acc}{$ass_str}{$f_string} = &$check_dl_html_ok($_->n_products);
			}
		}
		$info->{total}{$ass_str}{$f_string} = &$check_dl_html_ok($sum);
	};


	#	if there are no associations, get the GP count for the terms *without filters*
	#	if too many associations, get the filtered GP count for each term in the list
	#	if we are getting all associations, get the figures for direct assocs
	#	get the number of [direct] children each term has

	#	store info about the number of GP associations for each term
	#	- number of GPs (n_results) and number of pages (n_pages)
	#	- whether we can display them as html (html_ok) or download them (dl_ok)

	#	if there are no associations, see if it is because
	#	we have filters by getting the product count *without filters*
	if ($result_h->{n_results} == 0)
	{	#print STDERR "No results found!\n";
		#	query is deep assocs, no filters. There is nothing else we can do here.
		if (!$filters && $option_h->{show_all_ass})
		{	$info->{orig_query} = $info->{total}{all}{unfiltered} = { n_results => 0 };
		}
		else
		{	my $a_str = &$get_ass_string($option_h->{show_all_ass});
			my $f_str = &$get_f_string($filters);
			$info->{orig_query} = $info->{total}{$a_str}{$f_str} = { n_results => 0 };
			
			#	get the count for deep assocs without filters (this will be the max
			#	no. of assocs we could get)

			&$get_counts(1, 1, undef); #	show_all_ass, gp_count_ok, use_filters

			#	if we have 0 results from this, there's no point in proceeding further
			#	otherwise...
			if ($info->{total}{all}{unfiltered}{n_results} > 0)
			{	&$get_children();
				#	check what the original query was
				#	if it was direct assocs with no filters,
				#	there are no more options for us to explore. However,
				#	if there *were* filters...
				if ($filters)
				{	#print STDERR "filters ON\n";
					#	original query was deep assocs with filters
					if ($option_h->{show_all_ass})
					{	#print STDERR "show all ass ON\n";
						# if we got a viable result from the unfiltered counts...
						if ($info->{total}{all}{unfiltered}{dl_ok} || $info->{total}{all}{unfiltered}{html_ok})
						{	#print STDERR "found a valid result. Quitting!\n";
							#	let's leave it at that
						}
						#	if there are children, get the direct counts
						elsif ($children > 0)
						{	#print STDERR "More checks.\n";
							#	direct, no filters
							&$get_counts(undef, 1, undef);
							#	direct, with filters
							&$get_counts(undef, $option_h->{gp_count_ok}, 1);
						}
					}
					else
					{	#print STDERR "show all ass OFF\n";
						if ($children > 0)
						{	#	original query was direct assocs with filters
							#	try getting the direct assocs without filters
							&$get_counts(undef, 1, undef);
							#	try all assocs with filters
							&$get_counts(1, $option_h->{gp_count_ok}, 1);
						}
					}
				}
			}
		}
	}

	else
	{	#print STDERR "Too many results!\n";
		#print STDERR "stuff we got from our last query: ".Dumper($option_h->{count_arr})."\n";

		my $a_str = &$get_ass_string($option_h->{show_all_ass});
		my $f_str = &$get_f_string($filters);

		#	save the original results
		$info->{orig_query} = $info->{total}{$a_str}{$f_str} = &$check_dl_html_ok($result_h->{n_results});

		#	convert the info in the count_arr hash
		my $id_to_acc;
		$id_to_acc->{$_->id} = $_->acc foreach @$term_l;
		if ($result_h->{count_arr})
		{	foreach (@{$result_h->{count_arr}})
			{	#	the term is $id_to_acc->{$_->[0]}
				$info->{term}{$id_to_acc->{$_->[0]}}{$a_str}{$f_str} = &$check_dl_html_ok($_->[1])
			}
			foreach (@$term_l)
			{	$info->{term}{$_->acc}{$a_str}{$f_str} = { n_results => 0 } if !$info->{term}{$_->acc}{$a_str}{$f_str};
			}
		}

		&$get_children();
		if ($option_h->{show_all_ass} && $children > 0)
		{	#	let's try the direct associations
			&$get_counts(undef, $option_h->{gp_count_ok}, $filters);
		}
		else
		{	#	too many associations, we are looking at the direct assocs
			#	nothing else to do. Sob!
		}
	}

	return { 
		results => {
			error_hash => { %$info },
			term_h => $term_h, 
		},
		error => $error,
	};
}

=head2 get_assoc_pairs

Gets term acc - GP id pairs

	Arguments - apph,
	            constraints: 
	            - terms =>
	            { id => [list of term ids], show_all_ass => { undef | 1 } }
	            - products => [list of GP ids]
	            option_h containing
	            - termsort
	            - gpsort

	Returns   - a list of pairs of term accs and GP ids or undef

=cut

sub get_assoc_pairs {
	my ($apph, $constr, $option_h) = rearrange([qw(apph constraints option_h)], @_);
	my $dbh = $apph->dbh;
	
	my $tables = ["association", "term"];
	my $where = ["association.term_id = term.id"];
	my @order;
	my @t_order;
	my @gp_order;

	if (defined $option_h->{termsort})
	{	@t_order = map { "LCASE(term.$_)" } @{$option_h->{termsort}};
	}

	if ($constr->{terms} && defined $option_h->{gpsort})
	{	@gp_order = map { "LCASE(gene_product.$_)" } @{$option_h->{gpsort}};
		push @$tables, 'gene_product';
		push @$where, "association.gene_product_id = gene_product.id";
	}

	if ($constr->{products})
	{	my $gps = $constr->{products};
		$gps = [$gps] unless (ref($gps) eq 'ARRAY');
		push @$where, "association.gene_product_id IN (".join(",", map{ sql_quote($_) } @$gps).")";
		if (defined $option_h->{termsort})
		{	@order = @t_order;
		}
	}

	if ($constr->{terms})
	{	#	we're looking at a hash in the form
		#	{	id => [...], show_all_ass => { undef | 1 } }
		my $ids = $constr->{terms}{id};
		$ids = [$ids] unless (ref($ids) eq 'ARRAY');
		if ($constr->{terms}{show_all_ass})
		{	push @$tables, "graph_path";
			push @$where,
				"association.term_id=graph_path.term2_id",
				"graph_path.term1_id IN (".join(",", @$ids).")";
		}
		else
		{	push @$where, 
			"association.term_id IN (".join(",", @$ids).")";
		}
		
		if (scalar @$ids > 1 || $constr->{terms}{show_all_ass})
		{	@order = (@t_order, @gp_order);
		}
		else
		{	@order = @gp_order;
		}
	}

	if (keys %{$apph->filters})
	{	#	set the filters
		_set_filters($apph->filters, $dbh, $tables, $where, $option_h->{filters});
	}

	my $sql = "SELECT DISTINCT term.acc, association.gene_product_id FROM "
				.join(", ", @$tables) . " WHERE "
				.join(" AND ", @$where);
	if (@order)
	{	$sql .= " ORDER BY ".join(", ", @order);
	}

	print STDERR "SQL: $sql\n" if $verbose;
	
	return $dbh->selectall_arrayref($sql);
}

=head2 get_association_data

Gets the associations for a list of terms and GPs

	Arguments - apph,
	            terms => [list of term objs]
	            products => [list of GP objs]
	            tmpl, # template for the assoc data

	Returns   - either a graph or an array of association objects

=cut

sub get_association_data {
#	my $self = shift;
#	my $dbh = $self->dbh;
	my ($apph, $terms, $products, $tmpl) = @_;
	my $dbh = $apph->dbh;

#print STDERR "products: ".Dumper($products) if $verbose;

	if (!$terms || !$products)
	{	return;
	}

	my $tables =
	[	"association",
		"evidence",
		"dbxref evdbxref"];
	my $where =
	[	"evidence.association_id = association.id",
		"evidence.dbxref_id = evdbxref.id"];
	my $cols =
	[	"association.id",
		"association.term_id",
		"association.is_not",
		"evidence.id AS ev_id",
		"evidence.code",
		"evidence.seq_acc",
		"evdbxref.xref_key AS evdbxref_acc",
		"evdbxref.xref_dbname AS evdbxref_dbname"];

	push @$cols, "association.source_db_id" if $tmpl->{assby};
	push @$cols, "association.assocdate" if $tmpl->{assocdate};

	if ($tmpl->{return_graph})
	{	push @$cols, "association.gene_product_id AS gp_id",
	}
	else
	{	push @$cols, "association.gene_product_id",
	}

	#set the term constraint
	$terms = [$terms] unless (ref($terms));
	unshift @$where,
		"association.term_id in (".join(", ", map { $_->id }@$terms).")";

	#set the product constraint
	$products = [$products] unless (ref($products));
	unshift @$where,
		"association.gene_product_id IN (".join(",", map { $_->id }@$products).")";

	#	set the filters
	_set_filters($apph->filters, $apph->dbh, $tables, $where, ['assoc']) if (keys %{$apph->filters});
	my $hl = select_hashlist($dbh, $tables, $where, $cols);

	if (!@$hl) {
		print STDERR "No associations found in get_association_data: error?\n" if $verbose;
		return;
	}

	my @assocs = ();
	my @assoc_lookup = ();
	my @assoc_ids = ();

	my ($gp_l, $term_l);
	if ($tmpl->{return_graph})
	{	map { $gp_l->[$_->id] = $_ } @$products;
		map { $term_l->[$_->id] = $_ } @$terms;
	}
	
	foreach my $h (@$hl) {
		my $assoc = $assoc_lookup[$h->{id}];
		if (!$assoc) {
			$assoc = $apph->create_association_obj($h);
			$assoc_lookup[$assoc->id] = $assoc;
			if ($tmpl->{return_graph})
			{	#print STDERR Dumper($assoc) if $verbose;
				if (!$gp_l->[$h->{gp_id}]) {
#				if (!$gp_l->[$h->{gene_product_id}]) {

					print STDERR "Could not find gene product for assoc ".$assoc->id."!\n".Dumper($assoc)."\n"  if $verbose;
				}
				else
#				{	$assoc->gene_product($gp_l->[$h->{gene_product_id}]);
				{	$assoc->gene_product($gp_l->[$h->{gp_id}]);
				}
				
				if (!$term_l->[$h->{term_id}]) {
				print STDERR "Could not find term for assoc ".$assoc->id."!\n".Dumper($assoc)."\n"  if $verbose;
				}
				else
				{	$term_l->[$h->{term_id}]->add_association($assoc);
				}
			}
			else
			{	$assoc->{term_id} = $h->{term_id};
			}

			push(@assoc_ids, $assoc->id);
			push(@assocs, $assoc);
		} 
		#print STDERR "assoc after: ".Dumper($assoc) if $verbose;
				
		my $ev = GO::Model::Evidence->new({
										code=>$h->{code},
										seq_acc=>$h->{seq_acc},
										});
		$ev->id($h->{ev_id});
		$ev->xref(GO::Model::Xref->new({xref_key=>$h->{evdbxref_acc},
										xref_dbname=>$h->{evdbxref_dbname}}));
		$assoc->add_evidence($ev);
	}

	if (!@assoc_ids) {
		print STDERR "No associations found in get_association_data: ERROR!!!\n" if $verbose;
		return;
	}
#	get extra association info
	print STDERR "getting evidence seq xrefs...\n" if $verbose;
	$apph->_get_evidence_seq_xrefs(\@assocs);
	print STDERR "getting qualifiers...\n" if $verbose;
	$apph->get_qualifiers(\@assocs);
	print STDERR "getting assigned by...\n" if $verbose;
	$apph->get_assigned_by(\@assocs) if $tmpl->{assby};

#	print STDERR "assocs: ".Dumper(\@assocs)."\n" if $verbose;

	if ($tmpl->{return_graph})
	{	my $graph = $apph->create_graph_obj;
		foreach (@$terms)
		{	$graph->add_term($_);
		}
		return $graph;
	}

	return \@assocs;
}

=head2 get_data_for_chart

	Arguments - apph, acc, error,
	            option_h containing
	            - session_id
	            - gp_count_ok
	Returns   - results and error
	            results is a hash in the form
	            parent => { acc => GOID, name => term_name, count => n_GPs },
	            graph => [ # sorted by count then term name
	            {	acc => GOID,
	            	name => term_name,
	            	count => n_GPs,
	            	percent => nn # count divided by parent->{count}
	            },
	            (etc.) ]

	Uses the term parameter and gets the data about the number of gene products
	annotated to or below that term. Used by the bar chart.

=cut

sub get_data_for_chart {
	my $args = shift;
	my ($apph, $acc, $error, $option_h) = ($args->{apph}, $args->{acc}, $args->{error}, $args->{option_h});
	
	if (!$apph || !$acc || !$option_h)
	{	$error = set_message($error, 'fatal', 'config_error', 'Missing vital arguments for get_data_for_chart: cannot continue.');
		return { error => $error };
	}

	my $dbh = $apph->dbh;
	my $tmpl = { acc => 1, n_deep_products => 1 };
	
	my $result_h = _get_terms_with_checks({
		apph => $apph,
		accs => [ $acc ],
		error => $error,
		option_h =>
		{	tmpl => { acc => 1 },
			is_ont_term => 1,
			is_obsolete => 1,
			cgi => 'term-chart',
			session_id => $option_h->{session_id},
			get_gp_count => 1,
			show_all_ass => 1,
			gp_count_ok => $option_h->{gp_count_ok},
		},
	});

	$error = $result_h->{error};
	return { error => $error } if !$result_h->{results};

	my $term_l = $result_h->{results};
	my $term = $term_l->[0];

	my $count = $term->n_deep_products;
	if ($count == 0)
	{	print STDERR "No GP assocs found\n" if $verbose;
		$error = set_message($error, 'fatal', 'no_term_assocs', $term->name." ; ".$term->acc." or its children");
		return { error => $error };
	}

	#	get the children of the term
	my $child_accs = $dbh->selectall_arrayref("SELECT term.acc FROM term2term, term WHERE term.id = term2term.term2_id AND term2term.term1_id = ".$term->id);
	if (!$child_accs || !@$child_accs)
	{	print STDERR "No children found\n" if $verbose;
		$error = set_message($error, 'fatal', 'no_children', $term->name." ; ".$term->acc);
		return { error => $error };
	}

	map { $_ = $_->[0] } @$child_accs;
	my $children = $apph->get_terms({ accs => $child_accs }, { acc => 1 });

	get_gp_count_for_terms($apph, $children, { show_all_ass => 1, use_filters => 1, gp_count_ok => $option_h->{gp_count_ok} });

	my $data;
	$data->{parent} = { name => $term->name, acc => $term->acc, count => $count};
#	print STDERR "term_data: ".Dumper($term)."\n" if $verbose;

	my $accumulator = 0;
	foreach my $child (@$children) {
		my $percent = sprintf("%.0f", ($child->n_deep_products/$count*100) );
		$accumulator += $percent;
		$data->{children}{$child->acc} = { acc => $child->acc, name => $child->name, count => $child->n_deep_products, percent => $percent };
	}

	if ($accumulator == 0)
	{	$error = set_message($error, 'warning', 'no_data_for_chart');
	}

	my $l = length($count);
	$data->{graph} = [
		map { $data->{children}{ (split("\0", $_))[-1] } }
		sort
		map { join("\0",
					sprintf("%0".$l."d", ($count - $data->{children}{$_}{count})),
					$data->{children}{$_}{name},
					$_ ) }
					keys %{$data->{children}} ];

	delete $data->{children};

	print STDERR Dumper($data) if $verbose;

	return { results => $data, error => $error };
}

=head2 get_term_in_graph

	Arguments - apph, GO term acc, error,
	            option_h containing
	            tree, # the current tree, with open_1, open_0 and closed nodes
	            term_context, # opt; set to 'sibling' to display sibling nodes
	            tmpl->{term},  # template for the term itself
	            tmpl->{graph}, # template for terms in the graph
	            gp_count_ok, # whether or not it's OK to get the GP counts
	            cgi, session_id # for redirecting if appropriate
	Returns   - hashref of the form
	            results => # graph with the term in it
	            error => # messages hash
	            ses_type => # session type, if not term details

=cut

sub get_term_in_graph {
	my $args = shift;
	my ($apph, $acc, $error, $option_h) = ($args->{apph}, $args->{acc}, $args->{error}, $args->{option_h});

	if (!$apph || !$acc || !$option_h)
	{	$error = set_message($error, 'fatal', 'config_error', 'Missing vital arguments for get_term_in_graph: cannot continue.');
		return { error => $error };
	}

	my $tree = $option_h->{tree};

	my $result_h = _get_terms_with_checks({
		apph => $apph,
		accs => [ $acc ],
		error => $error,
		option_h =>
		{	tmpl => $option_h->{tmpl}{term} || $option_h->{tmpl}{graph},
			cgi => $option_h->{cgi},
			session_id => $option_h->{session_id},
			gp_count_ok => $option_h->{gp_count_ok},
		},
	});

	$error = $result_h->{error};
	return { error => $error } if !$result_h->{results};

	my $term = $result_h->{results}[0];

#	print STDERR "term: ".Dumper($term)."\n" if $verbose;

	my $graph;

	#my $root = $apph->get_root_term(-template=>{acc=>1});
	my $roots = $apph->get_ontology_root_terms(-template=>{acc=>1});
#	push @$roots, $root;
	my %ont_h;
	foreach (@$roots){
	  $ont_h{$_->namespace} = 1;
	}
	$ont_h{'universal'} = 1;
	print STDERR "ont_h: ".Dumper(\%ont_h)."\n" if $verbose;

	print STDERR "option_h: ".Dumper($option_h)."\n" if $verbose;
	my $ses_type;
	if (grep { $_ eq $term->namespace } keys %ont_h)
	{	#	we're OK
		my $graph_type;
		$graph_type = 'DPSC' if ($option_h->{term_context} && $option_h->{term_context} eq 'sibling');

		$graph = _get_graph(-apph => $apph, -tree => $option_h->{tree}, -terms => [$term], -option_h => { graph_type => $graph_type, root => $roots, tmpl => $option_h->{tmpl}{graph}, close_type => 'close_iff_no_parent' });
		
	#	print STDERR "graph: ".Dumper($graph) if $verbose;
		
	}
	#	if it's a goslim, we can make a pretty graph for it
	elsif ($term->namespace eq 'subset' && $term->acc =~ /goslim/)
	{	$ses_type = 'subset_details';
		my $goslim = $apph->get_terms({subset=>$term->acc}, $option_h->{tmpl}{graph});

		$graph = $apph->get_graph_by_terms(-terms=>$goslim, -depth=>0, -template=>{terms=>$option_h->{tmpl}{graph}});

		my $tops = $graph->get_top_nodes;
		my @rel_list;
		foreach my $top (@$tops)
		{	push @rel_list, @{$graph->get_child_relationships($top)};
			$graph->delete_node($top->acc);
		}
		$graph->add_term($term);
		foreach (@rel_list)
		{	$graph->add_relationship($term->acc, $_->acc2, $_->type);
		}
		$graph->seed_nodes($term);
	}
	else
	#	this isn't an ontology. Maybe we shouldn't bother doing a graph?
	{	$ses_type = 'vocab_details';
#			$graph = $apph->create_graph_obj;
#			$graph->add_term($term);
	}
	return { results => { graph => $graph, term => $term }, error => $error, ses_type => $ses_type };
}

=head2 get_current_graph

Gets the current graph

	Arguments - apph,
	            error, # message hash
	            tree, # current tree, with lists of open_0, open_1 and closed
	            tmpl # template for terms in the graph

	Returns   - hashref containing
	            results => # the graph
	            error => # message hash

=cut

sub get_current_graph {
	my ($apph, $error, $tree, $option_h) = rearrange([qw(apph error tree option_h)], @_);

	my $tmpl = $option_h->{tmpl};
	print STDERR "tmpl (from Query):\n".Dumper($tmpl)."\n" if $verbose;

#	remove the get_n_deep_products bit as we will add that later
	my $get_n_deep_products;
	if ($tmpl->{n_deep_products})
	{	print STDERR "We need to get all products.\n" if $verbose;
		$get_n_deep_products = 1;
		delete $tmpl->{n_deep_products};
	}

	#my $root = $apph->get_root_term(-template => $tmpl);
	my $roots = $apph->get_root_terms(-template => $tmpl);
        my %rooth = map { ($_->{acc}=>1) } @$roots;
	print STDERR "Got root.\n" if $verbose;
#	print STDERR "tree: ".Dumper($tree)."\n" if $verbose;
	my $graph_type;
	my $terms;

	if ($tree->{open_0})
	{	my $result_h = _get_terms_with_checks({
			apph => $apph,
			accs => $tree->{open_0},
			error => $error,
			option_h =>
			{	tmpl => $tmpl,
				is_ont_term => 1,
				gp_count_ok => $option_h->{gp_count_ok},
			},
		});
		$error = $result_h->{error};
		$terms = $result_h->{results} if $result_h->{results};
	}
	elsif ($tree->{open_1})
	{	my $result_h = _get_terms_with_checks({
			apph => $apph,
			accs => $tree->{open_1},
			error => $error,
			option_h =>
			{	tmpl => $tmpl,
				is_ont_term => 1,
				gp_count_ok => $option_h->{gp_count_ok},
			},
		});
		$error = $result_h->{error};
		$terms = $result_h->{results} if $result_h->{results};
		#	add the root node to the tree
                foreach my $root (@$roots) {
                    unless (grep { $_ eq $root->acc } @{$tree->{open_1}})
                    {	
			push @{$tree->{open_1}}, $root->acc;
			push @$terms, $root;
                    }  # _get_terms_with_checks will get rid of the duplicate
		}
		$graph_type = 'by_terms_on_path';
	}

	if (!$terms)
	{	#	we've got no tree. Make the default tree, which is the root node
		print STDERR "No terms: putting root into open_1\n" if $verbose;
		$tree->{open_1} = [map {$_->acc} @$roots];
		$terms = [ @$roots ];
		$graph_type = 'by_terms_on_path';
	}

	if (!$tree->{open_0} && !$tree->{open_1})
	{	print STDERR "No open_0 or open_1: putting root into open_1\n" if $verbose;
		$tree->{open_1} = [map {$_->acc} @$roots];
		$graph_type = 'by_terms_on_path';
	}
#	print STDERR "template: ".Dumper($tmpl)."\n" if $verbose;

	my $graph = _get_graph(-apph => $apph, -tree => $tree, -terms => $terms, -option_h => { graph_type => $graph_type, root => $roots, tmpl => $tmpl});

#	print STDERR "seeding nodes...\n" if $verbose;
#	$graph->seed_nodes($terms);


#	print STDERR "Graph: ".Dumper($graph)."\n" if $verbose;

	my $ont = $apph->filters->{ont};

	#	remove the obsolete nodes from the graph in the summary and graphical view
	#	trim off the filtered nodes
        foreach my $root (@$roots) {
            foreach my $c (@{$graph->get_child_terms($root->acc) || []})
            {	if ($c->is_obsolete || ($ont && !grep { $_ eq $c->type } @$ont))
		{	$graph->close_below($c->acc);
			$graph->delete_node($c->acc);
		}
            }
        }
	print STDERR "Finished.\n" if $verbose;

	if ($get_n_deep_products)
	{	my $terms = $graph->get_all_terms;
		get_gp_count_for_terms($apph, $graph->get_all_terms, { use_filters => 1, gp_count_ok => $option_h->{gp_count_ok} });
	}
#	print STDERR "Doing get children in the get_current_graph query\n" if $verbose;
#	$apph->_get_n_children_h($graph);
	return { results => $graph, error => $error };
}

=head2 _get_graph

	Arguments - apph, tree, list of seed term(s), option_h containing
	            - graph type (optional; defaults to get_graph_by_terms)
	            - root node (if using the default graph type)
	            - close type (for the term graph view)
	            - tmpl (term template)
	Returns   - graph of the terms

=cut

sub _get_graph {
	my ($apph, $tree, $terms, $option_h) = rearrange([qw(apph tree terms option_h)], @_);
	my $dbh = $apph->dbh;
	my $graph_type = $option_h->{graph_type} || 'by_terms';
	my $tmpl = $option_h->{tmpl} || { acc => 1 };
	my $close_type = $option_h->{close_type} || undef;
	my $graph;

	if ($graph_type eq 'by_terms_on_path')
	{	print STDERR "Using get_graph_by_terms_on_path\n" if $verbose;
		print STDERR "open_1: ".join(", ", map { $_->acc } @$terms)."\n" if $verbose;

		$graph = $apph->get_graph_by_terms_on_path(-terms=>$terms, -root=>$option_h->{root}, -template=>{terms => $tmpl});
	#	print STDERR "graph: ".Dumper($graph)."\n" if $verbose;
	}
	elsif ($graph_type eq 'DPSC')
	{	#my $open_terms = $apph->get_terms({acc=>$tree->{open_1}}, $tmpl) if ($tree->{open_1} && @{$tree->{open_1}});
		my $open_terms;
		if ($tree->{open_1} && @{$tree->{open_1}})
		{	my $result_h = _get_terms_with_checks({
				apph => $apph,
				accs => $tree->{open_1},
				error => undef,
				option_h =>
				{	tmpl => $tmpl,
					is_ont_term => 1,
				},
			});
			$open_terms = $result_h->{results} if $result_h->{results};
		}

		$graph = $apph->get_graph_DPSC(-term=>$terms, -termh=>{open_terms=>$open_terms}, -template=>{terms=>$tmpl});
	}
	else
	{	print STDERR "Using get_graph_by_terms\n" if $verbose;
		print STDERR "open_0: ".join(", ", map { $_->acc } @$terms)."\n" if $verbose;

		$graph = $apph->get_graph_by_terms(-terms=>$terms, -depth=>0, -template=>{terms => $tmpl});

#		my $open_terms;
#		if ($tree->{open_1} && @{$tree->{open_1}})
#		{	my $result_h = _get_terms_with_checks({
#				apph => $apph,
#				accs => $tree->{open_1},
#				error => undef,
#				option_h =>
#				{	tmpl => $tmpl,
#					is_ont_term => 1,
#				},
#			});
#			$open_terms = $result_h->{results} if $result_h->{results};
#		}

		if ($tree->{open_1} && @{$tree->{open_1}})
		{	$apph->extend_graph(-graph=>$graph, -acc=>$_, -depth=>1, -template=>{terms => $tmpl}) foreach @{$tree->{open_1}};
		}
		print STDERR "Doing get children in the open_0 subroutine\n" if $verbose;
		$apph->_get_n_children_h($graph);
	}

	if ($tree->{closed} && @{$tree->{closed}}) {
		foreach my $close (@{$tree->{closed}}) {
			eval {
				$graph->close_below($close, $close_type);
			};
		}
	}
	
	print STDERR "seeding nodes...\n" if $verbose;
	$graph->seed_nodes($terms);
	return $graph;
}

=cut

		my $open = $tree->{open_1} if $tree->{open_1};
		if ($option_h->{term_context} && $option_h->{term_context} eq 'sibling') {
			print STDERR "accs: ".Dumper($open)."\n" if $verbose;
			my $open_terms = $apph->get_terms({acc=>$open}, $tmpl) if ($open && @$open);
			$graph = $apph->get_graph_DPSC(-term=>$term, -termh=>{open_terms=>$open_terms}, -template=>{terms=>$tmpl});

		} else {
			print STDERR "Getting the graph... " if $verbose;
			$graph = $apph->get_graph_by_terms(-terms=>[$term], -depth=>0, -template=>{terms=>$tmpl});
			if ($open && @$open)
			{	foreach my $open_term (@{$open || []}) {
					$apph->extend_graph(-graph=>$graph, -acc=>$open_term, -depth=>1, -template=>{terms=>$tmpl});
				}
			}
			print STDERR "Got graph. Wikkid!\n" if $verbose;
		}
	
		if ($tree->{closed})
		{	foreach my $close_below (@{$tree->{closed}}) {
				eval {
					$graph->close_below($close_below, 'close_iff_no_parent');
				};
			}
		}

=cut



=head2 get_graph_for_gp

	Arguments - apph, gps, error,
	            option_h containing
	            - session_id
	            - gp_count_ok
	Returns   - results and error
	            results a graph with the GPs attached to the appropriate terms

=cut

sub get_graph_for_gp {
	my $args = shift;
	my ($apph, $gps, $error, $option_h) = ($args->{apph}, $args->{gps}, $args->{error}, $args->{option_h});

	if (!$apph || !$gps || !@$gps)
	{	$error = set_message($error, 'fatal', 'config_error', 'Missing vital arguments for get_graph_for_gp: cannot continue.');
		return { error => $error };
	}

	my $result_h = get_gp_assocs({ apph => $apph, error => $error, gp_list => $gps, option_h => $option_h });
	return $result_h if !$result_h->{results};
	$error = $result_h->{error};
	my $data = $result_h->{results};

	# data consists of { order => $acc_id_list, assoc_h => $assoc_h, product_h => $product_h, term_h => \%term_h };

	#	hook the associations up to the terms
	foreach my $gp (keys %{$data->{assoc_h}})
	{	foreach my $acc (keys %{$data->{assoc_h}{$gp}})
		{	next unless $data->{term_h}{$acc};
			foreach my $qual (keys %{$data->{assoc_h}{$gp}{$acc}})
			{	foreach (@{$data->{assoc_h}{$gp}{$acc}{$qual}})
				{	$_->gene_product($data->{product_h}{$gp});
					$data->{term_h}{$acc}->add_association($_);
				}
			}
		}
	}

	print STDERR "terms:\n".Dumper($data->{term_h})."\n" if $verbose;

	my $terms = [ values %{$data->{term_h}} ];
#	push @$terms, $apph->get_root_term(-template=>{acc=>1});
	print STDERR "terms: ".Dumper($terms)."\n" if $verbose;
	
	# build a graph all the way to the leaf nodes
	# from the above terms
	my $graph;
	$graph = $apph->get_graph_by_terms(-terms=>$terms, -depth=>0, -template=>{terms=>{acc=>1}}) if @$terms;
	$graph->seed_nodes($terms);
	
	print STDERR "Finished messing with the graph.\n" if $verbose;
	
	return
	{	results =>
		{	graph => $graph,
			product_h => $data->{product_h},
		},
		error => $error,
	};
}

sub get_nit {
	my $graph = shift;
	my $closed = shift;
	my $compact_tree_view = shift || 0;

	my $open_1 = [];
	if ($closed) {
		foreach my $n (@{$graph->get_all_nodes || []}) {
			my $n_child = $graph->n_children($n->acc);
			if ($n_child && $n_child == @{$graph->get_child_relationships($n) || []}) {
				print STDERR "n_child: $n_child; n->acc: ".$n->acc."\n" if $verbose;
				push @$open_1, [ $n->acc ] if (grep {$n->acc ne $_} @$closed);
			}
		}
	}
	else
	{	$closed = [];
	}
	require "GO/Model/TreeIterator.pm";

	my $nit = GO::Model::TreeIterator->new($graph, $open_1, $open_1, [ map { [ $_ ] } @$closed], $compact_tree_view);
	$nit->set_bootstrap_mode;
	$nit->close_below;
	return $nit;
}

sub get_permalink {
	my $graph = shift;
	my $tree = shift;
	my $link = '';      # link which info about terms hidden in current tree
	my $permalink = ''; # WYSIWYG link

	print STDERR "tree: ".Dumper($tree) if $verbose;

	if ($tree->{term})
	{	foreach my $o qw(term open_1 closed)
		{	if ($tree->{$o} && @{$tree->{$o}})
			{	$link .= "&amp;$o=" . join(",", @{$tree->{$o}} );
			}
		}
		$permalink = $link;
		print STDERR "link: $link, permalink: $permalink\n" if $verbose;
	}
	elsif ($tree->{open_0})
	{	foreach my $o qw(open_0 open_1 closed)
		{	if ($tree->{$o} && @{$tree->{$o}})
			{	$link .= "&amp;$o=" . join(",", @{$tree->{$o}} );
			}
		}
		$permalink = $link;
	}
	else
	{	foreach my $o qw(open_1 closed)
		{	if ($tree->{$o} && @{$tree->{$o}})
			{	my @temp;
				my @all;
				foreach my $acc (@{$tree->{$o}})
				{	#print STDERR "looking for $acc...\n" if $verbose;
					my $t = $graph->get_term({acc => $acc});
					push @temp, $acc if $t;
					push @all, $acc;
					#print STDERR "found... ".Dumper($t)."\n" if $verbose;
				}
				$permalink .= "&amp;$o=" . join(",", @temp) if @temp;
				$link .= "&amp;$o=" . join(",", @all) if @all;
			}
		}
	}
	return { 'link' => $link, permalink => $permalink };
}

=head2 get_fasta

  Arguments - apph, error, list of GP xrefs
  returns   - error, list of FASTA seqs

=cut

sub get_fasta {
	my $apph = shift;
	my $error = shift;
	my $gps = shift;

	my $result_h = get_gp_details($apph, $error, { gpxref => $gps}, { tmpl => {spp => 1, gptype => 1, seq => 1 } });
	$error = $result_h->{error};

	my $gp_h;
	if ($result_h->{results})
	{	my @no_seqs;
		foreach (@{$result_h->{results}})
		{	if ($_->seq_list)
			{	$gp_h->{$_->xref->xref_dbname .":". $_->xref->xref_key} = $_;
			}
			else
			{	push @no_seqs, $_->xref->xref_dbname .":". $_->xref->xref_key;
			}
		}
		
		if (@no_seqs)
		{	my $m_type = 'fatal';
			$m_type = 'warning' if (keys %$gp_h);
			$error = set_message($error, $m_type, 'no_seq', [@no_seqs]);
		}
	}
	else
	{	$error = set_message($error, 'fatal', 'gp_not_found', $gps);
	}

	if (keys %$gp_h)
	{	return {
		error => $error,
		results =>
		[	map { $gp_h->{ (split("\0", $_))[-1] } }
			sort# { $a->[1] cmp $b->[1] } 
			map { join("\0",
					$gp_h->{$_}->xref->xref_dbname.":".$gp_h->{$_}->xref->xref_key,
					$_) } keys %$gp_h ]};
	}
	return { error => $error };
}


=head2 _get_terms_with_checks

  Arguments - apph, acc or list of accs, error,
              option_h containing any of the following (all are optional)
              - tmpl (specifies how much term info to get)
              - booleans is_root_term, is_obsolete, is_ont_term
                (if present, these checks are performed)
              - cgi and session_id (if present, will perform a redirect
                if necessary)
              - get_gp_count (get the gp count for the terms, if not
                specified in tmpl. checks for option_h->{gp_count_ok} and
                option->{show_all_ass}; show_all_ass defaults to 1 [showing all
                associations, not just direct])
              - ignore_errors (if set, ignores any missing terms; otherwise,
                missing terms are reported in the message hash)
              - get_cons_rplc (if set, the consider and replaced_by lists are
                added to the term)
                
  returns   - result_h->{results} - a list of GO terms
              result_h->{error} - messages / errors
              redirect if a secondary ID is used and the cgi and session_id
              params are supplied

  Takes a term or list of terms and checks that they
  are present in the ontology. If they aren't, it checks
  alt_ids and returns any matches there.

=cut

sub _get_terms_with_checks {
	my $args = shift;
	
	my ($apph, $accs, $error, $option_h) = ($args->{apph}, $args->{accs}, $args->{error}, $args->{option_h});

	if (!$apph || !$accs)
	{	$error = set_message($error, 'fatal', 'config_error', 'Missing vital arguments for _get_terms_with_checks: cannot continue.');
		return { error => $error };
	}


	my $tmpl = $option_h->{tmpl};
	my $template_to_use;

	if ($tmpl->{n_products} || $tmpl->{n_deep_products})
	{	#	remove and wait 'til we've got all the terms we need
		unless ($option_h->{get_gp_count})
		{	$option_h->{get_gp_count} = 1;
			if ($tmpl->{n_products})
			{	$option_h->{show_all_ass} = 0;
			}
			else
			{	$option_h->{show_all_ass} = 1;
			}
		}
		$template_to_use->{$_} = 1 foreach (keys %$tmpl);
		delete $template_to_use->{n_products};
		delete $template_to_use->{n_deep_products};
	}
	else
	{	$template_to_use = $tmpl;
	}

	if ($template_to_use->{cons_rplc})
	{	$option_h->{get_cons_rplc} = 1;
	}

	#	create a hash with the accs as keys that we can use for various checks
	#	check for / remove dups and blank entries
	my %acc_hash;
	if (!ref($accs))
	{	$accs = [$accs];
	}

	foreach (@$accs)
	{	$acc_hash{$_} = 1 if defined $_;
	}

	if (!keys %acc_hash)
	{	print STDERR "term list is empty\n" if $verbose;
		$error = set_message($error, 'fatal', 'no_terms');
		return { error => $error };
	}


	#	a list of terms
	my $term_l = $apph->get_terms({accs => [keys %acc_hash] }, $template_to_use);

	if (scalar @$term_l != scalar keys %acc_hash)
	{	#	we're missing some terms
		if (@$term_l)
		{	#	delete the accs we've found from %acc_hash
			#	check which IDs are missing, and do a synonym search
			foreach (@$term_l)
			{	delete $acc_hash{ $_->acc };
			}
		}

		#	search alt_ids for the accs in acc_hash
		my $dbh = $apph->dbh;
		my $sql = "SELECT term.acc, term_synonym.acc_synonym FROM term, term_synonym, term type WHERE term_synonym.term_id=term.id AND term_synonym.synonym_type_id=type.id AND type.acc='alt_id' AND term_synonym.acc_synonym IN (". join(", ", map { sql_quote($_) } keys %acc_hash).")";
		print STDERR "sql: $sql\n" if $verbose;
		my $acc_alt_id = $dbh->selectall_arrayref($sql);

		my %valid_term_accs;
		map { $valid_term_accs{$_->acc} = 1 } @$term_l;

		#	if we're redirecting, get the valid term accs and create the redirect
		if ($option_h->{cgi} && $option_h->{session_id})
		{	if (@$acc_alt_id)
			{	foreach (@$acc_alt_id)
				{	#	put the acc into valid_term_accs
					$valid_term_accs{$_->[0]} = 1;
				}
			}

			if (!keys %valid_term_accs)
			{	#	we didn't find anything.
				$error = set_message($error, 'fatal', 'term_not_found', [keys %acc_hash]) unless $option_h->{ignore_errors};
				return { error => $error };
			}

			#	create the redirect URL using valid_term_accs
			print "Location: ".get_environment_param('cgi_url')
				."/".$option_h->{cgi}.".cgi?term="
				.join("&term=", keys %valid_term_accs)
				."&session_id=".$option_h->{session_id}."\n\n";
			exit;
		}

		#	otherwise, get the valid accs and gather the term info
		if (@$acc_alt_id)
		{	###	should we save the info about which term maps to which somewhere?
			#	my %term_acc_to_alt_ids;
			#	push @{$term_acc_to_alt_ids{$_->[0]}}, $_->[1] foreach @$acc_alt_id;

			my %syn_accs;
			foreach (@$acc_alt_id)
			{	#	check if we already have the terms or not
				$syn_accs{$_->[0]} = 1 if !$valid_term_accs{$_->[0]};
				#	remove the terms we've found from $acc_hash
				delete $acc_hash{$_->[1]};
			}

			#	get the terms 
			my $syn_l = $apph->get_terms({accs => [ keys %syn_accs ]}, $template_to_use) unless !keys %syn_accs;
			push @$term_l, @$syn_l;
		}

		if (@$term_l && !$option_h->{ignore_errors} && keys %acc_hash)
		{	#	we found some terms but not all of them
			$error = set_message($error, 'warning', 'term_not_found', [keys %acc_hash]);
		}
	}

	if (!$term_l || !@$term_l)
	{	#	we didn't find anything.
		$error = set_message($error, 'fatal', 'term_not_found', [keys %acc_hash]) unless $option_h->{ignore_errors};
		return { error => $error };
	}

	#	now perform any checks that might have been specified
	if ($option_h->{is_obsolete} || $option_h->{is_ont_term} || $option_h->{is_root_term})
	{	my $err;
		my $roots;
		my %ont_h;
		if ($option_h->{is_ont_term} || $option_h->{is_root_term})
		{	
                        #$roots = $apph->get_ontology_root_terms(-template=>{acc=>1});
                        $roots = $apph->get_ontology_root_terms(-template=>{acc=>1});
			#push @$roots, $apph->get_root_term(-template=>{acc=>1});
			foreach (@$roots){
			  $ont_h{$_->namespace} = 1;
			}
			$ont_h{'universal'} = 1;
			print STDERR "ont_h: ".Dumper(\%ont_h)."\n"
			  if $verbose;

			print STDERR "roots: ". join("\n", map { $_->acc } @$roots)."\n\n" if $verbose;
			print STDERR "onts: ". join(", ", keys %ont_h)."\n" if $verbose;
		}

		my %hash = (
			is_ont_term => sub {
				#	check the term is in the ontologies
			  print STDERR "Checking term is in the ontologies\n" if $verbose;
				my $term = shift;
				if (!grep { $_ eq $term->namespace } keys %ont_h)
				{	print STDERR "Non ontology term: ".$term->name."\n" if $verbose;
					push @{$err->{not_ont_term}}, $term->name." : ".$term->acc;
					return 1;
				}
			},
			is_obsolete => sub {
				#	check that the term is not obsolete
				print STDERR "Checking term is not obsolete\n" if $verbose;
				my $term = shift;
				if ($term->is_obsolete == 1)
				{	print STDERR "Obsolete term: ".$term->name."\n" if $verbose;
					push @{$err->{obs_term}}, $term->name." : ".$term->acc;
					return 1;
				}
			},
			is_root_term => sub {
				#	check that the term is not one of the root nodes
				print STDERR "Checking term is not one of the root nodes\n" if $verbose;
				my $term = shift;
				if (grep { $term->acc eq $_->acc } @$roots)
				{	print STDERR "Root term: ".$term->name."\n" if $verbose;
					push @{$err->{root_term_assocs}}, $term->name." : ".$term->acc;
					return 1;
				}
			},
		);
		
		my @checklist;
		foreach my $t qw(is_ont_term is_obsolete is_root_term)
		{	if ($option_h->{$t})
			{	push @checklist, $t;
			}
		}

		my @ok;
		TERM_CHECKS:
		foreach (@$term_l)
		{	foreach my $c (@checklist)
			{	my $r = $hash{$c}->($_);
				next TERM_CHECKS if $r;
			}
			#	phew! It's OK. Add it to term_h
			push @ok, $_;
		}

		if (keys %$err && !$option_h->{ignore_errors})
		{	#	some errors were found
			if (@ok)
			{	#	we still have some OK terms, tho
				foreach (keys %$err)
				{	$error = set_message($error, 'warning', $_, $err->{$_});
				}
				$term_l = [@ok];
			}
			else
			{	#	no decent terms left
				foreach (keys %$err)
				{	$error = set_message($error, 'fatal', $_, $err->{$_});
					return { error => $error };
				}
			}
		}
	}
	#	finished the checks

	#	if option_h->{get_gp_count} was present,
	#	retrieve the gp counts for the terms
	if ($option_h->{get_gp_count})
	{	my %hash;
		$hash{use_filters} = 1;
		foreach ('show_all_ass', 'gp_count_ok')
		{	$hash{$_} = $option_h->{$_} if defined $option_h->{$_};
		}
		get_gp_count_for_terms($apph, $term_l, \%hash);
	}

	if ($option_h->{get_cons_rplc})
	{	get_consider_and_replaced_by_terms($apph, $term_l);
	}

#	print STDERR "term_l: ".Dumper($term_l)."\n" if $verbose;

	return { error => $error, results => $term_l };
}

=head2 get_consider_and_replaced_by_terms

  Arguments - apph, term object list
  Returns   - terms with the consider and replaced_by lists filled

=cut

sub get_consider_and_replaced_by_terms {
	my $apph = shift;
	my $term_l = shift || return;
	
	print STDERR "starting get_consider_and_replaced_by_terms...\n" if $verbose;
	
	my $dbh = $apph->dbh;
	# get the consider/replaced_by info
	my %obs_terms;
	my %terms_by_id;
	foreach (@$term_l)
	{	$obs_terms{$_->id}++ if $_->is_obsolete == 1;
		$terms_by_id{$_->id} = $_;
	}

	if (keys %obs_terms)
	{	# the SQL command we need is this:
		my $cons_rplc_list = $dbh->selectall_arrayref("SELECT term2_id, relationship.acc, term1_id FROM term2term_metadata, term AS relationship WHERE relationship.id=relationship_type_id AND term2_id IN (" . join(", ", keys %obs_terms ) .")");

		my %ids_to_get;
		my @add_rlnship_list;
		#	gather a list of replacement terms
		foreach (@$cons_rplc_list)
		{	if ($terms_by_id{$_->[2]})
			{	my $rel = "add_".$_->[1];
				$terms_by_id{$_->[0]}->$rel($terms_by_id{$_->[2]});
			}
			else
			{	$ids_to_get{$_->[2]}++;
				push @add_rlnship_list, $_;
			}
		}

		if (keys %ids_to_get)
		{	my $temp_l = $dbh->selectall_arrayref("SELECT id, acc, name FROM term WHERE id IN (". join(", ", keys %ids_to_get) . ")");
			foreach (@$temp_l)
			{	my $proto_term = $apph->create_term_obj({ id => $_->[0], acc => $_->[1], name => $_->[2] });
				$terms_by_id{$proto_term->id} = $proto_term unless !$proto_term;
			}
			foreach (@add_rlnship_list)
			{	my $rel = "add_".$_->[1];
				$terms_by_id{$_->[0]}->$rel($terms_by_id{$_->[2]});
			}
		}
	}
}

=head2 get_term_count_for_gps

  Arguments - apph, GP object list, boolean for whether or not to use filters
  Returns   - list of GP ID and term count

=cut

sub get_term_count_for_gps {
	my $apph = shift;
	my $gps = shift || return;
	my $use_filters = shift;

	return unless @$gps;

	print STDERR "use_filters: ".Dumper($use_filters)."\n" if $verbose;

	my $dbh = $apph->dbh;

	my $tables = ["association"];
	my $where = ["association.gene_product_id IN (".join(",", map{ $_->id } @$gps).")"];

	#	set the filters
	_set_filters($apph->filters, $dbh, $tables, $where, ['assoc', 'term']) if $use_filters;



	my $sql = "SELECT association.gene_product_id, COUNT(DISTINCT association.term_id) FROM "
				.join(", ", @$tables) . " WHERE "
				.join(" AND ", @$where)
				. " GROUP BY association.gene_product_id";

	print STDERR "SQL: $sql\n" if $verbose;
	
	my $results = $dbh->selectall_arrayref($sql);
	
	return if !$results;

	#	pop the results into a list and return 'em
	return [ map { ($_->[0], $_->[1]) } @$results ];
}

=head2 get_gp_count_for_terms

  Arguments - apph, term list, option hash containing booleans:
              show_all_ass - whether to get all assocs (1) or just direct (0),
              use_filters - whether or not to use filters,
              gp_count_ok - whether we can use apph->get_deep_product_count
  Adds the gps counts to the terms

=cut

sub get_gp_count_for_terms {
	my ($apph, $terms, $option_h) = (@_);

print STDERR "starting get_gp_count_for_terms\noption_h: ".Dumper($option_h)."\n" if $verbose;
my $t0 = gettimeofday();

	my $show_all_ass;
	if (defined $option_h->{show_all_ass})
	{	$show_all_ass = $option_h->{show_all_ass};
	}
	else
	{	$show_all_ass = 1;
	}
	my $use_filters = $option_h->{use_filters};

	my $gp_count_ok = $option_h->{gp_count_ok};

	if (!defined $option_h->{gp_count_ok} && (!keys %{$apph->filters} || !$option_h->{use_filters}))
	{	$gp_count_ok = '1';
	}

	my $dbh = $apph->dbh;
	
	#	get deep associations
	if ($show_all_ass)
	{	my @result_l;
		if ($gp_count_ok || !$use_filters)
		{	#	if gp_count_ok is on or we're not using filters,
			#	we can use apph->get_deep_product_count

			print STDERR "Getting the deep product count using the apph method\n" if $verbose;
			### HACK ALERT! ###
			#	ugly hack - remove the filters from the apph if use_filters isn't on
			my $filters;
			if (keys %{$apph->filters})
			{	if (!$use_filters) #	use filters is OFF
				{	$filters = $apph->filters;
					$apph->filters({});
				}
				elsif ($gp_count_ok && $gp_count_ok eq "ignore_db_filter")
				### HACK ###
				#	this is due to the species / db partition in the gpcount table of the db
				{	$filters = $apph->filters;
					delete $apph->filters->{speciesdb} if $apph->filters->{speciesdb};
				}
			}

			my $results = $apph->get_deep_product_count({ per_term=>1, terms => $terms });

			#	replace the filters if necessary
			$apph->filters($filters) if $filters;

			#	pop the results into a list and add 'em to the terms
			if (@$results)
			{	$result_l[$_->{term_id}] = $_->{c} foreach @$results;
			}
		#	print STDERR "Results: ".Dumper($results)."\n" if $verbose;
		}
		else
		{	print STDERR "Getting the deep product count using the new method\n" if $verbose;

			my $tables = ["association", "graph_path"];
			my $where = ["association.term_id=graph_path.term2_id", "graph_path.term1_id IN (".join(",", map { $_->id } @$terms).")"];

			_set_filters($apph->filters, $dbh, $tables, $where, ['assoc', 'gp']) if $use_filters;

			push @$where, "association.is_not=0";

			#	now count the number of distinct GPs annot'd to each term set
			my $sql = "SELECT graph_path.term1_id, COUNT(DISTINCT association.gene_product_id) FROM "
			.join(", ", @$tables) . " WHERE "
			.join(" AND ", @$where)
			." GROUP BY graph_path.term1_id";

			print STDERR "SQL: $sql\n" if $verbose;
			my $results = $dbh->selectall_arrayref($sql);
			#	pop the results into a list and add 'em to the terms
			if (@$results)
			{	$result_l[$_->[0]] = $_->[1] foreach @$results;
			}
		#	print STDERR "Results: ".Dumper($results)."\n" if $verbose;
		}
		foreach (@$terms)
		{	if ($result_l[$_->id])
			{	$_->n_deep_products($result_l[$_->id]);
			}
			else
			{	$_->n_deep_products(0);
			}
		#	print STDERR "n_deep_products: ".$_->n_deep_products."\n" if $verbose;
		}
	#	print STDERR "terms: ".Dumper($terms)."\n" if $verbose;
	}
	else
	#	just get associations to the term itself
	{	print STDERR "Getting GP counts to term itself\n" if $verbose;
		my $tables = ["association"];
		my $where = ["association.term_id IN (".join(",", map{ $_->id } @$terms).")"];

		push @$where, "association.is_not=0";

		_set_filters($apph->filters, $dbh, $tables, $where, ['assoc', 'gp']) if $use_filters;
		my $sql = "SELECT association.term_id, COUNT(DISTINCT association.gene_product_id) FROM "
				.join(", ", @$tables) . " WHERE "
				.join(" AND ", @$where)
				. " GROUP BY association.term_id";

		print STDERR "SQL: $sql\n" if $verbose;
	
		my $results = $dbh->selectall_arrayref($sql);
		print STDERR "Results: ".Dumper($results)."\n" if $verbose;
	
		#	pop the results into a list and add 'em to the terms
		if (@$results)
		{	my @result_l;
			$result_l[$_->[0]] = $_->[1] foreach (@$results);
			foreach (@$terms)
			{	if ($result_l[$_->id])
				{	$_->n_products($result_l[$_->id]);
				}
				else
				{	$_->n_products(0);
				}
			}
		}
		else
		{	$_->n_products(0) foreach (@$terms);
		}
	}
	print STDERR (gettimeofday() - $t0).": gp counting done\n" if $verbose;
}


=head2 get_seqs_for_gps

  Arguments - apph, list of gps, template (optional)
  returns   - the GPs with sequences added
  
  if the template is 'full', it will add all seq info
  and all dbxrefs to the GPs in the list
  
  if the template is 'has_seq', just adds the seq ID

=cut

sub get_seqs_for_gps {
	my $apph = shift;
	my $gps = shift || return;
	my $tmpl = shift || 'full';
	
	print STDERR "tmpl: ".$tmpl."\n" if $verbose;

	$gps = [$gps] unless (ref($gps) eq 'ARRAY');

	return unless $gps;

	my %gp_h = map {$_->_seqs_obtained(1); $_->id=>$_} @$gps;
	my @gp_ids = keys %gp_h;
	return unless @gp_ids;
	
	my $cols = 'gene_product_seq.gene_product_id, seq.*';
	if ($tmpl eq 'has_seq')
	{	$cols = "gene_product_seq.gene_product_id, seq.id";
	}

	my $hl = select_hashlist
		($apph->dbh,
		 ["gene_product_seq", "seq"],
		 ["seq.id = seq_id", "gene_product_id in (".join(',', @gp_ids).")"],
		 [$cols]
		);

	my (@seqs, @byid);
	foreach my $h (@$hl) {
		my $seq = $apph->create_seq_obj($h);
		my $gp = $gp_h{$h->{gene_product_id}};
		$gp->add_seq($seq);
		push(@seqs, $seq);
		$byid[$seq->id] = $seq;
	}

	if (@byid && $tmpl eq 'full') {
		$hl = select_hashlist($apph->dbh,
						["dbxref", "seq_dbxref"],
						["dbxref.id = dbxref_id",
						 "seq_id in (".join(", ", map {$_->id} @seqs).")"],
						 "dbxref.*, seq_id");
		map {
			$byid[$_->{seq_id}]->add_xref(GO::Model::Xref->new($_));
			} @$hl;
	}
	return;
}

=head2 _set_filters

  Arguments - filters (from the apph or manually set), dbh,
              tables and where arrays for an SQL query,
              what to filter (optional; aspects of the GP, term or assoc info
                can be filtered)
  returns   - amended tables and where arrays

  Adds the filter settings to an SQL query

=cut

sub _set_filters {
	my ($filters, $dbh, $tables, $where, $what_to_filter) = @_;
	
	print STDERR "Setting filters...\n" if $verbose;
#	foreach ($filters, $tables, $where)
#	{	print STDERR Dumper($_) if $verbose;
#	}

	if (!keys %$filters)
	{	print STDERR "No filters found!\n" if $verbose;
		return;
	}

	if (!$what_to_filter || grep { $_ eq 'term' } @$what_to_filter)
	{	print STDERR "checking term filters...\n" if $verbose;
		# ontology
		my $onts =  $filters->{ont};
		if ($onts) {
			if (!ref($onts)) {
				$onts = [$onts];
			}
	
			if (@$onts) {
				if (! grep { $_ eq 'term' } @$tables)
				{	push @$tables, "term";
					push @$where, "association.term_id=term.id";
					__table_check_assoc($tables, $where);
				}
	#			if (! grep { $_ eq 'association' } @$tables)
	#			{	push @$tables, "association", "term";
	#				push @$where, "gene_product.id=association.gene_product_id", "association.term_id=term.id";
	#			}
	
				push @$where, "term.term_type IN ("
					.join(",", map { sql_quote($_) } @$onts).")";
			}
		}
	}
	
	if (!$what_to_filter || grep { $_ eq 'gp' } @$what_to_filter)
	{	print STDERR "checking gene product filters...\n" if $verbose;
		#	species DB
		my $spdbs = $filters->{speciesdb};
		if ($spdbs) {
			if (!ref($spdbs)) {
				$spdbs = [$spdbs];
			}
			
			if (@$spdbs)
			{	__table_check_gp($tables, $where);
				if (! grep { $_ eq 'dbxref' } @$tables)
				{	push @$tables, "dbxref";
					push @$where, "gene_product.dbxref_id=dbxref.id";
				}
				push @$where, "dbxref.xref_dbname IN ("
					.join(",", map { sql_quote($_) } @$spdbs).")";
			}
		}
	
		# NCBI Taxa IDs
		my $taxids =  $filters->{taxid};
		if ($taxids) {
			if (!ref($taxids)) {
				$taxids = [$taxids];
			}
	
			if (@$taxids) {
				__table_check_gp($tables, $where);
	
				my $results = $dbh->selectall_arrayref("SELECT id FROM species WHERE species.ncbi_taxa_id IN (".join(",", @$taxids).")");
				
				push @$where, "gene_product.species_id IN (".join(",", map { $_->[0] } @$results).")";
	
	#			push @$tables, "species";
	#			push @$where, "species.id = gene_product.species_id";
	#			push @$where, "species.ncbi_taxa_id IN (".
	#				join(",", @$taxids).")";
			}
		}
	
		#	gp types
		my $types = $filters->{gptype};
		if ($types) {
			if (!ref($types)) {
				$types = [$types];
			}
	
			if (@$types) {
				__table_check_gp($tables, $where);
	
				my $sql = "SELECT id FROM term WHERE name IN (".join(",", map { sql_quote($_) } @$types).") AND term_type='sequence'";
				print STDERR "sql: $sql\n" if $verbose;
				my $results = $dbh->selectall_arrayref($sql);
				
				push @$where, "gene_product.type_id IN (".join(",", map { $_->[0] } @$results).")";
	
	#			push @$tables, "term as gptype";
	#			push @$where, "gene_product.type_id=gptype.id";
	#			push @$where, "gptype.name IN (".
	#				join(",", map { sql_quote($_) } @$types).")";
			}
		}
	}

	if (!$what_to_filter || grep { $_ eq 'assoc' } @$what_to_filter)
	{	print STDERR "checking association filters...\n" if $verbose;
		#	evidence codes
		my $evcodes = $filters->{evcode};
		if ($evcodes) {
			if (!ref($evcodes)) {
				$evcodes = [$evcodes];
			}
	
			if (@$evcodes) {
				__table_check_assoc($tables, $where);
				if (! grep { $_ eq 'evidence' } @$tables)
				{	push @$tables, "evidence";
					push @$where, "evidence.association_id = association.id";
				}
	
				my @wanted = grep {$_ !~ /^\!/} @$evcodes;
				my @unwanted = grep {/^\!/} @$evcodes;
				if (@wanted) {
					push @$where, "evidence.code IN (".
						join(",", map { sql_quote($_) } @wanted).")";
				}
				if (@unwanted) {
					push @$where, "evidence.code NOT IN (".
						join(",", map { sql_quote(substr($_,1)) } @unwanted).")";
				}
			}
		}
	
		#	assigned by
		my $assby = $filters->{assby};
		if ($assby) {
			if (!ref($assby)) {
				$assby = [$assby];
			}
	
			if (@$assby) {
				__table_check_assoc($tables, $where);
	
				my $sql = "SELECT id FROM db WHERE name IN (".join(",", map { sql_quote($_) } @$assby).")";
				print STDERR "sql: $sql\n" if $verbose;
				my $results = $dbh->selectall_arrayref($sql);
				
				push @$where, "association.source_db_id IN (".join(",", map { $_->[0] } @$results).")";
	
	
	#			push @$tables, "db";
	#			push @$where, "association.source_db_id=db.id";
	#			push @$where, "db.name IN ("
	#				.join(",", map { sql_quote($_) } @$assby).")";
			}
		}

=comment
# not yet implemented in GUI

		#	association negation
		my $negation = $filters->{is_not};
		if ($negation)
		{	__table_check_assoc($tables, $where);
			push @$where, "association.is_not = 1";
		}
		
		#	assocdate
		my $dates = $filters->{assocdate};
		if ($dates) {
			if (!ref($dates)) {
				$dates = [$dates];
			}
			if (@$dates) {
				__table_check_assoc($tables, $where);
				my @before = grep {$_ !~ /^\!/} @$dates;
				my @between = grep {/^\d{8}-\d{8}$/} @$dates;
				my @after = grep {/^\!/} @$dates;
				my @sql;
				if (@before) {
					#	choose the latest date
					my $date = (sort @before)[-1];
					push @sql, "association.assocdate < $date";
				}
				if (@between)
				{	foreach (@between)
					{	if (/(\d{8})-(\d{8})/)
						{	push @sql, "association.assocdate BETWEEN $1 AND $2";
						}
					}
				}
				if (@after) {
					#	choose the latest date
					my $date = (sort @after)[0];
					push @sql, "association.assocdate < $date";
				}
				push @$where, join(' OR ', @sql);
			}
		}
=cut

		#	association qualifiers
		my $quals = $filters->{qual};
		if ($quals) {
			if (!ref($quals)) {
				$quals = [$quals];
			}
	
			if (@$quals) {
				__table_check_assoc($tables, $where);
				push @$tables, "association_qualifier, term AS qualifier";
				push @$where, "association.id=association_qualifier.association_id", "association_qualifier.term_id=qualifier.id";
	
				push @$where, "qualifier.name IN ("
					.join(",", map { sql_quote($_) } @$quals).")";
			}
		}
	}
}

sub __table_check_gp {
	my $tables = shift;
	my $where = shift;
	return if (grep { $_ eq 'gene_product' } @$tables);
	push @$tables, 'gene_product';
	push @$where, 'gene_product.id=association.gene_product_id';
	if (! grep { $_ eq 'association' } @$tables)
	{	if (grep { $_ eq 'term' } @$tables)
		{	push @$tables, 'association';
			push @$where, 'association.term_id=term.id';
		}
		else
		{	print STDERR "ERROR! Could not find a table to attach association to!\ntables: ".Dumper($tables)."where: ".Dumper($where)."\n" if $verbose;
		}
	}
}

sub __table_check_assoc {
	my $tables = shift;
	my $where = shift;
	return if (grep { $_ eq 'association' } @$tables);
	push @$tables, 'association';
	if (grep { $_ eq 'gene_product' } @$tables)
	{	push @$where, 'association.gene_product_id=term.id';
	}
	elsif (grep { $_ eq 'term' } @$tables)
	{	push @$where, 'association.term_id=term.id';
	}
	else
	{	print STDERR "ERROR!! Could not find a table to attach associations to!\ntables: ".Dumper($tables)."where: ".Dumper($where)."\n" if $verbose;
	}
}


1;



