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
	get_nit get_permalink get_fasta _get_products_seqs
	get_gp_count_for_terms get_term_count_for_gps);

=head2 get_gp_details

	Arguments - apph, msg_h, listref of GP xrefs or IDs from the database,
	            option_h containing
	            - tmpl (opt; template for which bits of GP info to get),
	            - use_filters (opt; whether or not to use filters)
	            - ignore_errors (opt; whether to report errors in msg_h)
	Returns   - hashref of the form
	            results => [listref of unsorted GP objects or undefined],
	            msg_h => messages hash
	
	my $gp_list = get_gp_details($apph, $msg_h, $gps, $option_h);

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
	my ($apph, $msg_h, $constr, $option_h) = rearrange([qw(apph msg_h constr option_h)], @_);

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
	{	print STDERR "GP list is empty\n";
		$msg_h = set_message($msg_h, 'fatal', 'no_gps');
		return { msg_h => $msg_h };
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
	
	print STDERR "tmpl: ".Dumper($tmpl);
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
	{	print STDERR "Problem with the SQL!\n$sql\n";
		$msg_h = set_message($msg_h, 'fatal', 'sql', $sql);
		return { msg_h => $msg_h } ;
	}

	print STDERR "SQL: $sql\n";
	
	my $sth = $dbh->prepare($sql);
	$sth->execute();

	my %gp_h;
	
	while (my $d = $sth->fetchrow_hashref) {
	#	print STDERR "$d:\n".Dumper($d)."\n";
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
			{	print STDERR "scalar gps = ".scalar @$gps."; scalar keys gp_h = ".scalar (keys %gp_h)."\n";
				my $missing;
				my %hash;
				if ($constr->{gpxref}) {
					map { $hash{$_->xref->xref_dbname . ":" . $_->xref->xref_key} = 1 } values %gp_h;
				}
				elsif ($constr->{id}) {
					%hash = %gp_h;
				}

				foreach (@$gps)
				{	#if (!grep { $xref eq $_->speciesdb.":".$_->acc } values %gp_h)
					if (!$hash{$_})
					{	push @$missing, $_;
						print STDERR "Lost $_\n";
					}
				}
				$msg_h = set_message($msg_h, 'warning', 'gp_not_found', $missing);
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
				_get_products_seqs($apph, [values %gp_h], 'has_seq');
			}
		}
		else {
			_get_products_seqs($apph, [values %gp_h] );
		}
#		return [values %gp_h];
		return { msg_h => $msg_h, results => [values %gp_h ] };
	}

	$msg_h = set_message($msg_h, 'fatal', 'gp_not_found', $gps) if !$option_h->{ignore_errors};
	print STDERR "No gps found! msg_h: ".Dumper($msg_h);
#	$msg_h = set_message($msg_h, 'fatal', 'gp_not_found', $gps);
	return { msg_h => $msg_h };
}

=head2 get_gp_assocs

	Arguments - apph, listref of GP xrefs, msg_h,
	            option_h containing
	            - tmpl (opt; templates for the format of the results)
	            - cache (results from prev query, if appropriate)
	            - die_nicely (opt; if present, produces page-specific error msg)
	            - termsort, gpsort (optional)
	            - page_size (opt; defaults to 50)
	            - page (opt; defaults to 1)
	            - gp_count_ok (whether we can get the assoc count for the terms)
	Returns   - listref of GP objects or undefined, with an error message
	
	my $gp_list = get_gp_assocs($apph, $gps, $option_h);

	where $gps is of the form
	{ gpxref => [ dbA:acc1, dbA:acc2, dbB:acc3 ] } or
	{ id => [ 2948271, 3985271, 29481 ] }
	
=cut

sub get_gp_assocs {
	my ($apph, $gps, $msg_h, $option_h) = rearrange([qw(apph gp_list msg_h option_h)], @_);

	my $pairs;     # pairs of term accs and GP ids
	my $term_h;    # term info for all terms
	my $product_h; # GP info for all GPs
	my $to_cache;  # save info for caching here
	my $q_gp_list; # valid GPs from the query list
	my $n_pages;

	my %terms_to_get;
	my %gps_to_get;

	my $gp_opts = { 
		tmpl => $option_h->{tmpl}{gp} || { has_seq => 1, gptype => 1, spp => 1 },
		use_filters => 'gp',
	};

	if ($option_h->{cache} && $option_h->{cache}{term_product_ids})
	{	$pairs = $option_h->{cache}{term_product_ids} if @{$option_h->{cache}{term_product_ids}};
		print STDERR "pairs: ".Dumper($pairs)."\n";
		if ($pairs)
		{	print STDERR "Using cached results.\n";
			#	get the subset of terms and gps we want to look at
			#	print STDERR "subset before:\n".Dumper(\@subset)."\n";
			$n_pages = get_n_pages(scalar @$pairs, $option_h->{page_size});
			if ($n_pages != 1)
			{	$pairs = get_subset($pairs, $option_h->{page});
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
			my $result_h = get_gp_details($apph, $msg_h, { gpxref => $gps }, $gp_opts);
			$msg_h = $result_h->{msg_h};
			#	this shouldn't happen unless something's gone horribly wrong, but...
			return { msg_h => $msg_h } unless $result_h->{results};

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
						join("\0", map { $gp->$_ } @{$option_h->{gpsort}}, $gp->id)
					}
					values %$product_h;
				}
			}
			
		}
	}

	if (!$pairs)
	{	#	get brief info about each gp - symbol, name, spp, dbxref, type
		my $result_h = get_gp_details($apph, $msg_h, { gpxref => $gps }, $gp_opts);
		$msg_h = $result_h->{msg_h};
		return { msg_h => $msg_h } unless $result_h->{results};
		print STDERR "Found some gps\n";

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
			{	print STDERR "Sorting by ordered input\n";
				foreach (@$gps)
				{	push @$q_gp_list, $temp{$_} if $temp{$_};
				}
			}
			else
			{	print STDERR "Sorting by gpsort params\n";
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
		{	print STDERR "No sort params found\n";
			@$q_gp_list = values %$product_h;
		}
	
		#	get the pairs of gps and terms
		$pairs = get_assoc_pairs($apph, { products => [ map{ $_->id } values %$product_h ] }, { termsort => $option_h->{termsort}, gpsort => $option_h->{gpsort}, filters => ['assoc', 'term'] } );

	#	print STDERR "pairs:\n".Dumper($pairs)."\n";
	
		if (!$pairs || !@$pairs)
		{	print STDERR "No pairs found. Aborting...\n";
			$msg_h = set_message($msg_h, 'fatal', 'no_gp_assocs', [ map { $_->symbol.", ".$_->xref->xref_dbname.":".$_->xref->xref_key } values %$product_h]);
	
	#		if ($option_h->{cgi} && $option_h->{cgi} eq 'gp-assoc')
			if ($option_h->{die_nicely})
			{	print STDERR "q_gp_list: ".Dumper($q_gp_list)."\n";
				return gp_assoc_die($apph, $msg_h, $product_h, [map { { gp => $_ } } @$q_gp_list]);
			}
			return { msg_h => $msg_h };
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
			print STDERR "gpid_h: ".Dumper(\%gpid_h);

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
			{	print STDERR "Checking for missing GPs in unsorted pairs\n";
				#	check for any gps with no associations
				#	(but only if we're returning html results)
				my %seen;
				map { $seen{$_->[1]}++ } @$pairs;
				@no_assocs = grep { !exists $seen{$_->id} } @$q_gp_list;
			}
		}

		if (@no_assocs)
		{	print STDERR "No assocs found for ".join(", ", @no_assocs)."\n";
			$msg_h = set_message($msg_h, 'warning', 'no_gp_assocs', [ map { $_->symbol.", ".$_->xref->xref_dbname.":".$_->xref->xref_key } @no_assocs ]);
		}

		#	set the cache
		$to_cache->{term_product_ids} = \@sorted_pairs;
		$to_cache->{query}{gp} = join(",", map { $_->xref->xref_dbname.":".$_->xref->xref_key } @$q_gp_list);

		$n_pages = get_n_pages(scalar @sorted_pairs, $option_h->{page_size});

		#	get the subset of terms and gps we want to look at
	#	print STDERR "subset before:\n".Dumper(\@subset)."\n";
		$pairs = \@sorted_pairs;
		if ($n_pages != 1)
		{	$pairs = get_subset($pairs, $option_h->{page});
		}
	#	print STDERR "subset after:\n".Dumper(\@subset)."\n";

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
#		print STDERR "term_h:\n".Dumper(\%term_h)."\n";
	}
	else
	{	print STDERR "No keys to term_h found. Aborting...\n";
		$msg_h = set_message($msg_h, 'fatal', 'no_gp_assocs', [ map { $_->symbol.", ".$_->xref->xref_dbname.":".$_->xref->xref_key } values %$product_h]);

		if ($option_h->{die_nicely})
		{	return gp_assoc_die($apph, $msg_h, $product_h, [map { { gp => $_ } } @$q_gp_list]);
		}
		return { msg_h => $msg_h };
	}

	print STDERR "gps_to_get: ".Dumper(\%gps_to_get)."\n";

	#	get the association data
	#	gps_to_get contains the GPs that are actually used in this annotation set
	my $assocs = get_association_data($apph, [values %$term_h], [map { $product_h->{$_} } keys %gps_to_get], $option_h->{tmpl}{assoc});

	#	return the results if a format is specified
	if ($option_h->{tmpl}{assoc}{return_graph})
	{	return { results => $assocs, msg_h => $msg_h, to_cache => $to_cache };
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
		{	#print STDERR "pair: ".$_->[0]." AND ".$_->[1]."\n";
			if ($_->[1] ne $last)
			{	push @$acc_id_list, { gp => $product_h->{$_->[1]}, terms => [ $term_h->{$_->[0]} ] };
				$last = $_->[1];
			}
			else
			{	push @{$acc_id_list->[-1]{terms}}, $term_h->{$_->[0]};
			}
			#print STDERR Dumper($acc_id_list);
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
		msg_h => $msg_h };
}

sub gp_assoc_die {
	my ($apph, $msg_h, $product_h, $order) = @_;
	my $dbh = $apph->dbh;

	print STDERR "order: ".Dumper($order);

	#	if there are no associations, see if it is because
	#	we have filters by getting the term count for the GPs
	#	BUT WITHOUT FILTERS!!
	my $gpid_termc = get_term_count_for_gps($apph, [values %$product_h]);
	my %hash = (@$gpid_termc);
	return { results => {
					product_h => $product_h, 
					order => $order,
					term_count => \%hash,
					n_pages => 1,
				},
				msg_h => $msg_h };
}

=head2 get_term_assocs

	Arguments - apph, list of term accs, msg_h
	            option_h containing
	            - tmpl (optional)
	            - check_results (whether or not to check how many results
	              we've got)
	            - cache (any cached results from a previous query)
	            - page (the page number for html results)
	            - die_nicely (page-specific error message for term-assoc.cgi)
	            - show_all_ass (whether to get deep associations or not)
	            - cgi and session_id (for redirection, if appropriate)
	Returns   - data structure containing terms, associations and
	            GPs, or a warning message

=cut

sub get_term_assocs {
	my ($apph, $term_list, $msg_h, $option_h) = rearrange([qw(apph term_list msg_h option_h)], @_);
	my $dbh = $apph->dbh;

#	print STDERR "option_h: ".Dumper($option_h)."\n";

	print STDERR "Starting get_term_assocs...\n\n";

	my $pairs;     # pairs of term accs and GP ids
	my $term_h;    # term info for all terms
	my $product_h; # GP info for all GPs
	my $q_term_l;  # the original query terms
	my $to_cache;  # save info for caching here
	my $n_pages;

	my $tmpl = { acc => 1, definition => 1 };
	
	my %terms_to_get;
	my %gps_to_get;

	if ($option_h->{cache})
	{	print STDERR "Looking for cached results...\n";
	#	get the pairs from the cached results
		$pairs = $option_h->{cache}{term_product_ids};
		if ($pairs)
		{	print STDERR "Using cached results.\n";

			#	get the subset of terms and gps we want to look at
			#	print STDERR "subset before:\n".Dumper(\@subset)."\n";
			$n_pages = get_n_pages(scalar @$pairs, $option_h->{page_size});
			if ($n_pages != 1)
			{	$pairs = get_subset($pairs, $option_h->{page});
			}
		}
	}
	
	if (!$pairs)
	{	#	this is a new query. Let's start with a few
		#	checks to make sure our terms are OK
		my $result_h = _get_terms_with_checks(
			$apph, $term_list, $msg_h, 
			{	tmpl => $tmpl,
				is_ont_term => 1,
				is_obsolete => 1,
				is_root_term => 1,
				cgi => $option_h->{cgi},
				session_id => $option_h->{session_id},
			#	get_gp_count => 1,
			#	gp_count_ok => $option_h->{gp_count_ok},
			#	show_all_ass => $option_h->{show_all_ass},
			}
		);

		$msg_h = $result_h->{msg_h};
		return { msg_h => $msg_h } if !$result_h->{results};
		$q_term_l = $result_h->{results};
		#	set term_h to be the (valid) terms in q_term_l
		$term_h->{$_->acc} = $_ foreach @$q_term_l;

		#	if check_results is on, check this query isn't going to return too
		#	many results
		if ($option_h->{check_results})
		{	print STDERR "Checking the number of results...\n";
			#	find out the number of term-GP combos we have
			my $max = get_environment_param('page_size') * get_environment_param('max_results_pages');
			my $tables = ["association"];
			my $where = [];

			#	if the query is a show_all_ass one...:
			if ($option_h->{show_all_ass})
			{	push @$tables, "graph_path";
				push @$where, "association.term_id=graph_path.term2_id", "graph_path.term1_id IN (".join(",", map { $_->id } @$q_term_l).")";
			}
			else # direct associations only
			{	push @$where, "association.term_id IN (".join(",", map { $_->id } @$q_term_l).")";
			}

			#	set the filters
			_set_filters($apph->filters, $dbh, $tables, $where, ['gp', 'assoc']);

			my $sql = "SELECT COUNT(DISTINCT association.term_id, association.gene_product_id) FROM "
			.join(", ", @$tables) . " WHERE "
			.join(" AND ", @$where);
	
			print STDERR "SQL: $sql\n";
			my $results = $dbh->selectall_arrayref($sql);
			print STDERR "Result of count query: ".Dumper($results)."\n";

			if ($results->[0][0] > $max)
			{	#	too many results. Die!
				$msg_h = set_message($msg_h, 'fatal', 'too_many_assocs', [map { $_->name . " ; " . $_->acc } @$q_term_l ]);
				return term_assoc_die($apph, $q_term_l, $msg_h, { show_all_ass => $option_h->{show_all_ass}, n_results => $results->[0][0], gp_count_ok => $option_h->{gp_count_ok}});
			}
			elsif ($results->[0][0] == 0)
			{	#	no assocs found. Die!
				$msg_h = set_message($msg_h, 'fatal', 'no_assocs', [map { $_->name . " ; " . $_->acc } @$q_term_l]);
				return term_assoc_die($apph, $q_term_l, $msg_h, { show_all_ass => $option_h->{show_all_ass}, n_results => 0, gp_count_ok => $option_h->{gp_count_ok}});
			}
		}

		#	we've got the all clear. Let's get them associations!
		my $all_pairs = get_assoc_pairs($apph, { terms => { id => [ map { $_->id } @$q_term_l], show_all_ass => $option_h->{show_all_ass} } }, { termsort => $option_h->{termsort}, gpsort => $option_h->{gpsort}, filters => ['gp', 'assoc'] });

		print STDERR "got pairs:\n".Dumper(scalar @$all_pairs)."\n";
		if (!$all_pairs || !@$all_pairs)
		{	print STDERR "No pairs found.\n";
			$msg_h = set_message($msg_h, 'fatal', 'no_assocs', [map { $_->name . " ; " . $_->acc } @$q_term_l]);
			if ($option_h->{die_nicely})
			{	return term_assoc_die($apph, $q_term_l, $msg_h, { show_all_ass => $option_h->{show_all_ass}, n_results => 0, gp_count_ok => $option_h->{gp_count_ok}});
			}
			return { msg_h => $msg_h };
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
						(grep { $acc eq $_->acc } @$q_term_l) ? 0 : 1,
						sprintf("%08d",$idx++),
						$_->[0],
						$_->[1] );
				} @$all_pairs;

		#	print STDERR "list:\n".Dumper(\@list)."\n";
		}
#		print STDERR "sorted pairs:\n".Dumper(\@sorted_pairs)."\n";

		
		#	set the cache
		$to_cache->{term_product_ids} = \@sorted_pairs;
		$to_cache->{query} = { term => join(",", sort map { $_->acc } @$q_term_l), show_all_ass => $option_h->{show_all_ass} };

		$n_pages = get_n_pages(scalar @sorted_pairs, $option_h->{page_size});
		print STDERR "n_pages = ".$n_pages."\n";

		#	get the subset of terms and gps we want to look at
		$pairs = \@sorted_pairs;
		if ($n_pages != 1)
		{	$pairs = get_subset($pairs, $option_h->{page});
		}
	#	print STDERR "pairs:\n".Dumper($pairs)."\n";
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
	{	my $terms = $apph->get_terms({accs => [keys %terms_to_get]}, $tmpl);
		#	put the info into term_h
		$term_h->{$_->acc} = $_ foreach @$terms;
		#	put the query terms in q_term_l
		@$q_term_l = map { $term_h->{$_} } @$term_list;
	}

	print STDERR "gp template: ".Dumper($option_h->{tmpl}{gp})."\n";

	my $gp_opts = { tmpl => $option_h->{tmpl}{gp} || { has_seq => 1, gptype => 1, spp => 1 }};

#	get the GP data and put it into product_h
	my $res_h = get_gp_details($apph, $msg_h, { id => [keys %gps_to_get]}, $gp_opts);
	$msg_h = $res_h->{msg_h};
	map { $product_h->{$_->id} = $_ } @{$res_h->{results}};
	
#	print STDERR "product_h:\n".Dumper($product_h)."\n";

	#	get the association data
	my $assocs = get_association_data($apph, [map { $term_h->{$_} } @$assoc_terms], [ values %$product_h ], $option_h->{tmpl}{assoc});

	#	return the results if a format is specified
	if ($option_h->{tmpl}{assoc}{return_graph})
	{	return { results => $assocs, msg_h => $msg_h, to_cache => $to_cache };
	}

#	print STDERR "associations:\n".Dumper($assocs)."\n";

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
				printf STDERR "something is wrong, no term for acc: %s\n",$_->[0];
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
	if ($option_h->{show_all_ass} && scalar @$q_term_l > 1)
	{	
		my $sql = "SELECT DISTINCT term2_id, term1_id FROM graph_path WHERE DISTANCE <> 0 AND term1_id IN ("
		.join(",", map { $_->id } @$q_term_l)
		.") AND term2_id IN ("
		.join(",", map { $_->id } values %$term_h)
		.")";
		print STDERR "SQL: $sql\n";

		my $results = $dbh->selectall_arrayref($sql);

		print STDERR "results of parentage query: ".Dumper($results)."\n";
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
		},
		to_cache => $to_cache,
		msg_h => $msg_h };
}

=head2 term_assoc_die

Die nicely when get_term_assocs goes wrong

	Arguments - apph, list of term accs, msg_h
	            option_h containing
	            - show_all_ass (whether to get deep associations or not)
	            - gp_count_ok (if it's OK to use n_deep_products)
	            - n_results (number of results returned)
	Returns   - results and msg_h
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
	my ($apph, $term_l, $msg_h, $option_h) = rearrange([qw(apph term_list msg_h option_h)], @_);
	my $dbh = $apph->dbh;
	my $term_h;
	map { $term_h->{$_->acc} = $_ } @$term_l;

	print STDERR "Doing the die nicely routine\n";

	if ($option_h->{n_results} != 0)
	{	#	we found too many associations. Get some useful info
		my $n_direct;
		if ($option_h->{show_all_ass}) # deep associations
		{	print STDERR "Getting the direct GP count for the terms...\n";
			#	get the direct GP count for the terms (WITH filters)
			get_gp_count_for_terms($apph, $term_l, { show_all_ass => 0, use_filters => 1, gp_count_ok => $option_h->{gp_count_ok} } );
		}
			
		foreach (@$term_l)
		{	$n_direct += $_->n_products;
		}
			
		#	get the no. of children of term
		my $direct_children;
		my $count_1 = $dbh->selectall_arrayref("SELECT term1_id, COUNT(DISTINCT term2_id) FROM term2term WHERE term1_id IN (".join(", ", map { $_->id } @$term_l).") GROUP BY term1_id");
		my %count1_h;
		map { $count1_h{ $_->[0] } = $_->[1] } @$count_1 if $count_1;
		map {
			$direct_children->{$_->acc} = $count1_h{ $_->id } || 0;
			} @$term_l;

		return { results => { term_h => $term_h, direct_children => $direct_children, n_pages_all => get_n_pages($option_h->{n_results}), n_pages_direct => get_n_pages($n_direct) }, msg_h => $msg_h };
	}
	else
	{	#	if there are no associations, see if it is because
		#	we have filters by getting the product count

		my $total;

		if (keys %{$apph->filters})
		{	print STDERR "We have filters on!\n";
			
			get_gp_count_for_terms($apph, $term_l, { show_all_ass => $option_h->{show_all_ass} });
			
			if ($option_h->{show_all_ass})
			{	$total += $_->n_deep_products foreach @$term_l;
			}
			else
			{	$total += $_->n_products foreach @$term_l;
			}
			print STDERR "total n_products: $total\n";
	
			#	horrible hack for the time being
			my $filter_h = $apph->filters;
			$apph->filters({});
		#	print STDERR "filters: ".Dumper($apph->filters)."\n";
			if ($option_h->{show_all_ass})
			{	print STDERR "Getting deep product count for terms...\n";
				$total = $apph->get_deep_product_count({terms => $term_l});
				print STDERR "results: ".Dumper($total)."\n";
	
			}
			else
			{	print STDERR "Getting product count for terms...\n";
				$total = $apph->get_product_count({terms => $term_l});
				print STDERR "results: ".Dumper($total)."\n";
			}
			
			$apph->filters($filter_h);
		#	print STDERR "filters: ".Dumper($apph->filters)."\n";
		}
		
		return { results => { term_h => $term_h, total => $total }, msg_h => $msg_h };
	}
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

	print STDERR "SQL: $sql\n";
	
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

#print STDERR "products: ".Dumper($products);

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
		print STDERR "No associations found in get_association_data: error?\n";
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
			{	#print STDERR Dumper($assoc);
				if (!$gp_l->[$h->{gp_id}]) {
#				if (!$gp_l->[$h->{gene_product_id}]) {

					print STDERR "Could not find gene product for assoc ".$assoc->id."!\n"
					.Dumper($assoc)."\n";
				}
				else
#				{	$assoc->gene_product($gp_l->[$h->{gene_product_id}]);
				{	$assoc->gene_product($gp_l->[$h->{gp_id}]);
				}
				
				if (!$term_l->[$h->{term_id}]) {
				print STDERR "Could not find term for assoc ".$assoc->id."!\n"
				.Dumper($assoc)."\n";
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
		#print STDERR "assoc after: ".Dumper($assoc);
				
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
		print STDERR "No associations found in get_association_data: ERROR!!!\n";
		return;
	}
#	get extra association info
	print STDERR "getting evidence seq xrefs...\n";
	$apph->_get_evidence_seq_xrefs(\@assocs);
	print STDERR "getting qualifiers...\n";
	$apph->get_qualifiers(\@assocs);
	print STDERR "getting assigned by...\n";
	$apph->get_assigned_by(\@assocs) if $tmpl->{assby};

#	print STDERR "assocs: ".Dumper(\@assocs)."\n";

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

	Arguments - apph, acc, msg_h,
	            option_h containing
	            - session_id
	            - gp_count_ok
	Returns   - results and msg_h
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
	my ($apph, $acc, $msg_h, $option_h) = rearrange([qw(apph acc msg_h option_h)], @_);
	
	my $dbh = $apph->dbh;
	my $tmpl = { acc => 1, n_deep_products => 1 };
	
	my $result_h = _get_terms_with_checks(
		$apph, $acc, $msg_h, 
		{	tmpl => { acc => 1 },
			is_ont_term => 1,
			is_obsolete => 1,
			cgi => 'term-chart',
			session_id => $option_h->{session_id},
			get_gp_count => 1,
			show_all_ass => 1,
			gp_count_ok => $option_h->{gp_count_ok},
		}
	);

	$msg_h = $result_h->{msg_h};
	return { msg_h => $msg_h } if !$result_h->{results};

	my $term_l = $result_h->{results};
	my $term = $term_l->[0];

	my $count = $term->n_deep_products;
	if ($count == 0)
	{	print STDERR "No GP assocs found\n";
		$msg_h = set_message($msg_h, 'fatal', 'no_gp_assocs', $term->name." ; ".$term->acc." or its children");
		return { msg_h => $msg_h };
	}

	#	get the children of the term
	my $child_accs = $dbh->selectall_arrayref("SELECT term.acc FROM term2term, term WHERE term.id = term2term.term2_id AND term2term.term1_id = ".$term->id);
	if (!$child_accs || !@$child_accs)
	{	print STDERR "No children found\n";
		$msg_h = set_message($msg_h, 'fatal', 'no_children', $term->name." ; ".$term->acc);
		return { msg_h => $msg_h };
	}

	map { $_ = $_->[0] } @$child_accs;
	my $children = $apph->get_terms({ accs => $child_accs }, { acc => 1 });

	get_gp_count_for_terms($apph, $children, { show_all_ass => 1, use_filters => 1, gp_count_ok => $option_h->{gp_count_ok} });

	my $data;
	$data->{parent} = { name => $term->name, acc => $term->acc, count => $count};
#	print STDERR "term_data: ".Dumper($term)."\n";

	foreach my $child (@$children) {
		my $percent = sprintf("%.0f", ($child->n_deep_products/$count*100) );
		$data->{children}{$child->acc} = { acc => $child->acc, name => $child->name, count => $child->n_deep_products, percent => $percent };
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

	print STDERR Dumper($data);

	return { results => $data, msg_h => $msg_h };
}

=head2 get_term_in_graph

	Arguments - apph, GO term acc, msg_h,
	            option_h containing
	            tree, # the current tree, with open_1, open_0 and closed nodes
	            term_context, # opt; set to 'sibling' to display sibling nodes
	            tmpl->{term},  # template for the term itself
	            tmpl->{graph}, # template for terms in the graph
	            gp_count_ok, # whether or not it's OK to get the GP counts
	            cgi, session_id # for redirecting if appropriate
	Returns   - hashref of the form
	            results => # graph with the term in it
	            msg_h => # messages hash
	            ses_type => # session type, if not term details

=cut

sub get_term_in_graph {
	my ($apph, $acc, $msg_h, $option_h) = rearrange([qw(apph acc msg_h option_h)], @_);

	my $tree = $option_h->{tree};

	my $result_h = _get_terms_with_checks(
		$apph, [$acc], $msg_h, 
		{	tmpl => $option_h->{tmpl}{term} || $option_h->{tmpl}{graph},
			cgi => $option_h->{cgi},
			session_id => $option_h->{session_id},
			gp_count_ok => $option_h->{gp_count_ok},
		}
	);

	$msg_h = $result_h->{msg_h};
	return { msg_h => $msg_h } if !$result_h->{results};

	my $term = $result_h->{results}[0];

#	print STDERR "term: ".Dumper($term)."\n";

	my $graph;

	my $root = $apph->get_root_term(-template=>{acc=>1});
	my $roots = $apph->get_ontology_root_terms(-template=>{acc=>1});
#	push @$roots, $root;
	my %ont_h;
	foreach (@$roots)
	{	$ont_h{$_->namespace} = 1;
	}

	print STDERR "option_h: ".Dumper($option_h)."\n";
	my $ses_type;
	if (grep { $_ eq $term->namespace } keys %ont_h)
	{	#	we're OK
		my $graph_type;
		$graph_type = 'DPSC' if ($option_h->{term_context} && $option_h->{term_context} eq 'sibling');

		$graph = _get_graph(-apph => $apph, -tree => $option_h->{tree}, -terms => [$term], -option_h => { graph_type => $graph_type, root => $root, tmpl => $option_h->{tmpl}{graph}, close_type => 'close_iff_no_parent' });
		
	#	print STDERR "graph: ".Dumper($graph);
		
=cut



		my $open_1 = $tree->{open_1} if $tree->{open_1};
		$g_option_h->{tmpl} = 

		if ($option_h->{term_context} && $option_h->{term_context} eq 'sibling')
		{	print STDERR "Doing the DPSC routine. Accs: ".Dumper($open_1)."\n";

			my $open_terms = $apph->get_terms({acc=>$open_1}, $option_h->{tmpl}{graph}) if ($open_1 && @$open_1);

			$graph = $apph->get_graph_DPSC(-term=>$term, -termh=>{open_terms=>$open_terms}, -template=>{terms=>$option_h->{tmpl}{graph}});

		} else {
			print STDERR "Getting the graph... ";
			$graph = $apph->get_graph_by_terms(-terms=>[$term], -depth=>0, -template=>{terms=>$option_h->{tmpl}{graph}});

			if ($open_1 && @$open_1)
			{	foreach my $open_term (@{$open_1 || []}) {
					$apph->extend_graph(-graph=>$graph, -acc=>$open_term, -depth=>1, -template=>{terms=>$option_h->{tmpl}{graph}});
				}
			}
			print STDERR "Got graph. Wikkid!\n";
		}
	
		if ($tree->{closed})
		{	foreach my $close_below (@{$tree->{closed}}) {
				eval {
					$graph->close_below($close_below, 'close_iff_no_parent');
				};
			}
		}
=cut
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
	return { results => { graph => $graph, term => $term }, msg_h => $msg_h, ses_type => $ses_type };
}

=head2 get_current_graph

Gets the current graph

	Arguments - apph,
	            msg_h, # message hash
	            tree, # current tree, with lists of open_0, open_1 and closed
	            tmpl # template for terms in the graph

	Returns   - hashref containing
	            results => # the graph
	            msg_h => # message hash

=cut

sub get_current_graph {
	my ($apph, $msg_h, $tree, $option_h) = rearrange([qw(apph msg_h tree option_h)], @_);

	my $tmpl = $option_h->{tmpl};
	print STDERR "tmpl (from Query):\n".Dumper($tmpl)."\n";
	my $root = $apph->get_root_term(-template => $tmpl);
	print STDERR "Got root.\n";
	print STDERR "tree: ".Dumper($tree)."\n";
	my $graph_type;
	my $terms;

	if ($tree->{open_0})
	{	my $result_h = _get_terms_with_checks(
			-apph => $apph,
			-accs => $tree->{open_0},
			-msg_h => $msg_h,
			-option_h => { tmpl => $tmpl, is_ont_term => 1, gp_count_ok => $option_h->{gp_count_ok} });
		$msg_h = $result_h->{msg_h};
		$terms = $result_h->{results} if $result_h->{results};
	}
	elsif ($tree->{open_1})
	{	my $result_h = _get_terms_with_checks(
			-apph => $apph,
			-accs => $tree->{open_1},
			-msg_h => $msg_h,
			-option_h => { tmpl => $tmpl, is_ont_term => 1, gp_count_ok => $option_h->{gp_count_ok} });
		$msg_h = $result_h->{msg_h};
		$terms = $result_h->{results} if $result_h->{results};
		#	add the root node to the tree
		unless (grep { $_ eq $root->acc } @{$tree->{open_1}})
		{	
			push @{$tree->{open_1}}, $root->acc;
			push @$terms, $root;
		}  # _get_terms_with_checks will get rid of the duplicate
		
		$graph_type = 'by_terms_on_path';
	}

	if (!$terms)
	{	#	we've got no tree. Make the default tree, which is the root node
		print STDERR "No terms: putting root into open_1\n";
		$tree->{open_1} = [$root->acc];
		$terms = [ $root ];
		$graph_type = 'by_terms_on_path';
	}

	if (!$tree->{open_0} && !$tree->{open_1})
	{	print STDERR "No open_0 or open_1: putting root into open_1\n";
		$tree->{open_1} = [$root->acc];
		$graph_type = 'by_terms_on_path';
	}
#	print STDERR "template: ".Dumper($tmpl)."\n";

	my $graph = _get_graph(-apph => $apph, -tree => $tree, -terms => $terms, -option_h => { graph_type => $graph_type, root => $root, tmpl => $tmpl});

	print STDERR "seeding nodes...\n";
	$graph->seed_nodes($terms);

#	print STDERR "Graph: ".Dumper($graph)."\n";

	my $ont = $apph->filters->{ont};

	#	remove the obsolete nodes from the graph in the summary and graphical view
	#	trim off the filtered nodes
	foreach my $c (@{$graph->get_child_terms($root->acc) || []})
	{	if ($c->is_obsolete || ($ont && !grep { $_ eq $c->type } @$ont))
		{	$graph->close_below($c->acc);
			$graph->delete_node($c->acc);
		}
	}
	print STDERR "Finished.\n";

#	print STDERR "Doing get children in the get_current_graph query\n";
#	$apph->_get_n_children_h($graph);
	return { results => $graph, msg_h => $msg_h };
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
	{	print STDERR "Using get_graph_by_terms_on_path\n";
		print STDERR "open_1: ".join(", ", map { $_->acc } @$terms)."\n";

		$graph = $apph->get_graph_by_terms_on_path(-terms=>$terms, -root=>$option_h->{root}, -template=>{terms => $tmpl});
	#	print STDERR "graph: ".Dumper($graph)."\n";
	}
	elsif ($graph_type eq 'DPSC')
	{	my $open_terms = $apph->get_terms({acc=>$tree->{open_1}}, $tmpl) if ($tree->{open_1} && @{$tree->{open_1}});
		$graph = $apph->get_graph_DPSC(-term=>$terms, -termh=>{open_terms=>$open_terms}, -template=>{terms=>$tmpl});
	}
	else
	{	print STDERR "Using get_graph_by_terms\n";
		print STDERR "open_0: ".join(", ", map { $_->acc } @$terms)."\n";

		$graph = $apph->get_graph_by_terms(-terms=>$terms, -depth=>0, -template=>{terms => $tmpl});
	
		if ($tree->{open_1} && @{$tree->{open_1}})
		{	$apph->extend_graph(-graph=>$graph, -acc=>$_, -depth=>1, -template=>{terms => $tmpl}) foreach (@{$tree->{open_1}});
		}
		print STDERR "Doing get children in the open_0 subroutine\n";
		$apph->_get_n_children_h($graph);
	}

	if ($tree->{closed} && @{$tree->{closed}}) {
		foreach my $close (@{$tree->{closed}}) {
			eval {
				$graph->close_below($close, $close_type);
			};
		}
	}
	$graph->seed_nodes($terms);
	return $graph;
}

=cut

		my $open = $tree->{open_1} if $tree->{open_1};
		if ($option_h->{term_context} && $option_h->{term_context} eq 'sibling') {
			print STDERR "accs: ".Dumper($open)."\n";
			my $open_terms = $apph->get_terms({acc=>$open}, $tmpl) if ($open && @$open);
			$graph = $apph->get_graph_DPSC(-term=>$term, -termh=>{open_terms=>$open_terms}, -template=>{terms=>$tmpl});

		} else {
			print STDERR "Getting the graph... ";
			$graph = $apph->get_graph_by_terms(-terms=>[$term], -depth=>0, -template=>{terms=>$tmpl});
			if ($open && @$open)
			{	foreach my $open_term (@{$open || []}) {
					$apph->extend_graph(-graph=>$graph, -acc=>$open_term, -depth=>1, -template=>{terms=>$tmpl});
				}
			}
			print STDERR "Got graph. Wikkid!\n";
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

	Arguments - apph, gps, msg_h,
	            option_h containing
	            - session_id
	            - gp_count_ok
	Returns   - results and msg_h
	            results a graph with the GPs attached to the appropriate terms

=cut

sub get_graph_for_gp {
	my $apph = shift;
	my $gps = shift;
	my $msg_h = shift;
	my $option_h = shift;

	my $result_h = get_gp_assocs($apph, $msg_h, $gps, $option_h);
	return $result_h if !$result_h->{results};
	$msg_h = $result_h->{msg_h};
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

	print STDERR "terms:\n".Dumper($data->{term_h})."\n";

#	my ($db, $key) = split(":", $gp_list[0], 2);
#	my $terms = $apph->get_terms({product=>{ xref_dbname => $db, xref_key => $key } }, {acc=>1});

	my $terms = [ values %{$data->{term_h}} ];
#	push @$terms, $apph->get_root_term(-template=>{acc=>1});
	print STDERR "terms: ".Dumper($terms)."\n";
	
	# build a graph all the way to the leaf nodes
	# from the above terms
	my $graph;
	$graph = $apph->get_graph_by_terms(-terms=>$terms, -depth=>0, -template=>{terms=>{acc=>1}}) if @$terms;
	$graph->seed_nodes($terms);
	
	print STDERR "Finished messing with the graph.\n";
	
	return { results => { graph => $graph, product_h => $data->{product_h} }, msg_h => $msg_h };
}

sub get_nit {
	my $graph = shift;
	my $closed = shift;
	my $compact_tree_view = shift || 0;

	my $open_1 = [];
#	my $closed = $session->get_param_values(-field=>'closed') || [];
	if ($closed) {
		foreach my $n (@{$graph->get_all_nodes || []}) {
			my $n_child = $graph->n_children($n->acc);
			if ($n_child && $n_child == @{$graph->get_child_relationships($n) || []}) {
				print STDERR "n_child: $n_child; n->acc: ".$n->acc."\n";
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

	print STDERR "tree: ".Dumper($tree);

	if ($tree->{term})
	{	foreach my $o qw(term open_1 closed)
		{	if ($tree->{$o} && @{$tree->{$o}})
			{	$link .= "&amp;$o=" . join(",", @{$tree->{$o}} );
			}
		}
		$permalink = $link;
		print STDERR "link: $link, permalink: $permalink\n";
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
				{	print STDERR "looking for $acc...\n";
					my $t = $graph->get_term({acc => $acc});
					push @temp, $acc if $t;
					push @all, $acc;
					print STDERR "found... ".Dumper($t)."\n";
				}
				$permalink .= "&amp;$o=" . join(",", @temp) if @temp;
				$link .= "&amp;$o=" . join(",", @all) if @all;
			}
		}
	}
	return { 'link' => $link, permalink => $permalink };
}

sub get_nit_old {
	my $session = shift;
	my $graph = shift;
	my $compact_tree_view = shift || 0;

	my $open_0 = [];
	my $open_1 = [];
	my $closed = $session->get_param_values(-field=>'closed') || [];
	if ($session->get_param('closed')) {
		my @no_trim_tops;
		my @closed;
		my @all_closed;
		foreach my $t (@$closed) {
			push @all_closed, $t;
			push @closed, [$t];
		}

		foreach my $n (@{$graph->get_all_nodes || []}) {
			my $n_child = $graph->n_children($n->acc);
			if ($n_child && $n_child == @{$graph->get_child_relationships($n) || []}) {
				push @no_trim_tops, $n->acc if (grep {$n->acc ne $_} @all_closed);
			}
		}
		$open_1 = [@no_trim_tops];
	} else {
		$closed = [];
	}
	my @closed;
	foreach my $t (@$closed) {
		push @closed, [$t];
	}
	my @terms;
	foreach my $t (@$open_0) {
		push @terms, [$t];
	}
	my @op_1;
	foreach my $t (@$open_1) {
		push @op_1, [$t];
		push @terms, [$t];
	}

	require "GO/Model/TreeIterator.pm";

	my $nit = GO::Model::TreeIterator->new($graph, \@terms, \@op_1, \@closed, $compact_tree_view);
	
#	my $relt_filters = $session->get_param_list('reltype');
#	if (scalar(@{$relt_filters || []}) && grep{lc($_) ne 'all'}@{$relt_filters || []}) {
#		printf STDERR "GET only: %s\n",join("\t",@$relt_filters);
#		$nit->reltype_filter($relt_filters);
#	}	
	$nit->set_bootstrap_mode;
	$nit->close_below;
	return $nit;
}

=head2 get_fasta

  Arguments - apph, msg_h, list of GP xrefs
  returns   - msg_h, list of FASTA seqs

=cut

sub get_fasta {
	my $apph = shift;
	my $msg_h = shift;
	my $gps = shift;

	my $result_h = get_gp_details($apph, $msg_h, { gpxref => $gps}, { tmpl => {spp => 1, gptype => 1, seq => 1 } });
	$msg_h = $result_h->{msg_h};

	if ($result_h->{results})
	{	my $gp_h;
		my @no_seqs;
		foreach (@{$result_h->{results}})
		{	if ($_->seq_list)
			{	$gp_h->{$_->xref->xref_dbname .":". $_->xref->xref_key} = $_;
			}
			else
			{	push @no_seqs,$_->xref->xref_dbname .":". $_->xref->xref_key;
			}
		}
		
		if (@no_seqs)
		{	$msg_h = set_message($msg_h, 'warning', 'no_seq', [@no_seqs]);
		}

		return {
		msg_h => $msg_h,
		results =>
		[	map { $gp_h->{ (split("\0", $_))[-1] } }
			sort# { $a->[1] cmp $b->[1] } 
			map { join("\0",
					$gp_h->{$_}->xref->xref_dbname.":".$gp_h->{$_}->xref->xref_key,
					$_) } keys %$gp_h ]};
	}
	else
	{	$msg_h = set_message($msg_h, 'fatal', 'gp_not_found', $gps);
		return { msg_h => $msg_h };
	}
}


=head2 _get_terms_with_checks

  Arguments - apph, acc or list of accs, msg_h,
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
                
  returns   - result_h->{results} - a list of GO terms
              result_h->{msg_h} - messages / errors
              redirect if a secondary ID is used and the cgi and session_id
              params are supplied

  Takes a term or list of terms and checks that they
  are present in the ontology. If they aren't, it checks
  alt_ids and returns any matches there.

=cut

sub _get_terms_with_checks {
	my ($apph, $accs, $msg_h, $option_h) =
		rearrange([qw(apph accs msg_h option_h)], @_);
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

	#	create a hash with the accs as keys that we can use for various checks
	#	check for / remove dups and blank entries
	my %acc_hash;
	foreach (@$accs)
	{	$acc_hash{$_} = 1 if defined $_;
	}

	if (!keys %acc_hash)
	{	print STDERR "term list is empty\n";
		$msg_h = set_message($msg_h, 'fatal', 'no_terms');
		return { msg_h => $msg_h };
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
		print STDERR "sql: $sql\n";
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
				$msg_h = set_message($msg_h, 'fatal', 'term_not_found', [keys %acc_hash]) unless $option_h->{ignore_errors};
				return { msg_h => $msg_h };
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
			$msg_h = set_message($msg_h, 'warning', 'term_not_found', [keys %acc_hash]);
		}
	}

	if (!$term_l || !@$term_l)
	{	#	we didn't find anything.
		$msg_h = set_message($msg_h, 'fatal', 'term_not_found', [keys %acc_hash]) unless $option_h->{ignore_errors};
		return { msg_h => $msg_h };
	}

	#	now perform any checks that might have been specified
	if ($option_h->{is_obsolete} || $option_h->{is_ont_term} || $option_h->{is_root_term})
	{	my $err;
		my $roots;
		my %ont_h;
		if ($option_h->{is_ont_term} || $option_h->{is_root_term})
		{	$roots = $apph->get_ontology_root_terms(-template=>{acc=>1});
			push @$roots, $apph->get_root_term(-template=>{acc=>1});
			foreach (@$roots)
			{	$ont_h{$_->namespace} = 1;
			}

			print STDERR "roots: ". join("\n", map { $_->acc } @$roots)."\n\n";
			print STDERR "onts: ". join(", ", keys %ont_h)."\n";
		}

		my %hash = (
			is_ont_term => sub {
				#	check the term is in the ontologies
				print STDERR "Checking term is in the ontologies\n";
				my $term = shift;
				if (!grep { $_ eq $term->namespace } keys %ont_h)
				{	print STDERR "Non ontology term: ".$term->name."\n";
					push @{$err->{not_ont_term}}, $term->name." : ".$term->acc;
					return 1;
				}
			},
			is_obsolete => sub {
				#	check that the term is not obsolete
				print STDERR "Checking term is not obsolete\n";
				my $term = shift;
				if ($term->is_obsolete == 1)
				{	print STDERR "Obsolete term: ".$term->name."\n";
					push @{$err->{obs_term}}, $term->name." : ".$term->acc;
					return 1;
				}
			},
			is_root_term => sub {
				#	check that the term is not one of the root nodes
				print STDERR "Checking term is not one of the root nodes\n";
				my $term = shift;
				if (grep { $term->acc eq $_->acc } @$roots)
				{	print STDERR "Root term: ".$term->name."\n";
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
				{	$msg_h = set_message($msg_h, 'warning', $_, $err->{$_});
				}
				$term_l = [@ok];
			}
			else
			{	#	no decent terms left
				foreach (keys %$err)
				{	$msg_h = set_message($msg_h, 'fatal', $_, $err->{$_});
					return { msg_h => $msg_h };
				}
			}
		}
	}
	#	finished the checks

=cut
	if ($tmpl->{synonyms} || $tmpl->{synonym_list})
	{	
	
	}

	if ($tmpl->{definition} && $tmpl->{comment})
	{	
	
	}
	elsif ($tmpl->{definition})
	{
	
	}
	elsif ($tmpl->{comment})
	{
	
	}
	
	if ($tmpl->{subset_list})
	{
	}

	if ($tmpl->{dbxref_list})
	{
	}
	
=cut
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

#	print STDERR "term_l: ".Dumper($term_l)."\n";

	return { msg_h => $msg_h, results => $term_l };
}

=head2 get_term_count_for_gps

  Arguments - apph, GP object list, boolean for whether or not to use filters
  Returns   - list of GP ID and term count

=cut

sub get_term_count_for_gps {
	my $apph = shift;
	my $gps = shift;
	my $use_filters = shift;

	print STDERR "use_filters: ".Dumper($use_filters)."\n";

	my $dbh = $apph->dbh;

	my $tables = ["association"];
	my $where = ["association.gene_product_id IN (".join(",", map{ $_->id } @$gps).")"];

#	if ($use_filters && $use_filters == 1)
#	{	my $filters = $apph->filters || {};
#		if ($filters && keys %{$filters})
#		{	#	set the filters
#			_set_filters($filters, $dbh, $tables, $where, ['assoc', 'term']);
#		}
#	}

	#	set the filters
	_set_filters($apph->filters, $dbh, $tables, $where, ['assoc', 'term']) if $use_filters;


	my $sql = "SELECT association.gene_product_id, COUNT(DISTINCT association.term_id) FROM "
				.join(", ", @$tables) . " WHERE "
				.join(" AND ", @$where)
				. " GROUP BY association.gene_product_id";

	print STDERR "SQL: $sql\n";
	
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

print STDERR "starting get_gp_count_for_terms\noption_h: ".Dumper($option_h)."\n";
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

	my $dbh = $apph->dbh;
	
	#	get deep associations
	if ($show_all_ass)
	{	if ($gp_count_ok || !$use_filters)
		{	#	if gp_count_ok is on or we're not using filters,
			#	we can use apph->get_deep_product_count
			print STDERR "Getting the deep product count using the apph method\n";
		#	print STDERR "terms: ".Dumper($terms)."\n";
			my $terms_by_id;
			$terms_by_id->[$_->id] = $_ foreach (@$terms);
			my $count_l = $apph->get_deep_product_count({ per_term=>1, terms => $terms });
			
			print STDERR "Results: ".Dumper($count_l)."\n";
			foreach (@$count_l) 
			{	$terms_by_id->[$_->{term_id}]->n_deep_products($_->{"c"}) if ($terms_by_id->[$_->{term_id}]);
			}
			print STDERR "terms: ".Dumper($terms)."\n";
		}
		else
		{	print STDERR "Getting the deep product count using the new method\n";

			my $tables = ["association", "graph_path"];
			my $where = ["association.term_id=graph_path.term2_id", "graph_path.term1_id IN (".join(",", map { $_->id } @$terms).")"];

			_set_filters($apph->filters, $dbh, $tables, $where, ['assoc', 'gp']) if $use_filters;
			
			#	now count the number of distinct GPs annot'd to each term set
			my $sql = "SELECT graph_path.term1_id, COUNT(DISTINCT association.gene_product_id) FROM "
			.join(", ", @$tables) . " WHERE "
			.join(" AND ", @$where)
			." GROUP BY graph_path.term1_id";

			print STDERR "SQL: $sql\n";
			my $results = $dbh->selectall_arrayref($sql);
			print STDERR "Results: ".Dumper($results)."\n";
			my @result_l;
			#	pop the results into a list and add 'em to the terms
			if (@$results)
			{	$result_l[$_->[0]] = $_->[1] foreach (@$results);
			}
			foreach (@$terms)
			{	if ($result_l[$_->id])
				{	$_->n_deep_products($result_l[$_->id]);
				}
				else
				{	$_->n_deep_products(0);
				}
				print STDERR "n_deep_products: ".$_->n_deep_products."\n";
			}
		}
	}
	else
	#	just get associations to the term itself
	{	print STDERR "Getting GP counts to term itself\n";
		my $tables = ["association"];
		my $where = ["association.term_id IN (".join(",", map{ $_->id } @$terms).")"];

		
		_set_filters($apph->filters, $dbh, $tables, $where, ['assoc', 'gp']) if $use_filters;
		my $sql = "SELECT association.term_id, COUNT(DISTINCT association.gene_product_id) FROM "
				.join(", ", @$tables) . " WHERE "
				.join(" AND ", @$where)
				. " GROUP BY association.term_id";

		print STDERR "SQL: $sql\n";
	
		my $results = $dbh->selectall_arrayref($sql);
		print STDERR "Results: ".Dumper($results)."\n";
	
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
	print STDERR (gettimeofday() - $t0).": gp counting done\n";
}


=head2 _get_products_seqs

  Arguments - apph, list of gps, template (optional)
  returns   - the GPs with sequences added
  
  if the template is 'full', it will add all seq info
  and all dbxrefs to the GPs in the list
  
  if the template is 'has_seq', just adds the seq ID

=cut

sub _get_products_seqs {
	my $apph = shift;
	my $gps = shift || return;
	my $tmpl = shift || 'full';
	
	print STDERR "tmpl: ".$tmpl."\n";

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
	
	print STDERR "Setting filters...\n";
#	foreach ($filters, $tables, $where)
#	{	print STDERR Dumper($_);
#	}

	if (!keys %$filters)
	{	print STDERR "No filters found!\n";
		return;
	}

	if (!$what_to_filter || grep { $_ eq 'term' } @$what_to_filter)
	{	print STDERR "checking term filters...\n";
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
	{	print STDERR "checking gene product filters...\n";
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
	
				my $sql = "SELECT id FROM term WHERE name IN (".join(",", map { sql_quote($_) } @$types).")";
				print STDERR "sql: $sql\n";
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
	{	print STDERR "checking association filters...\n";
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
				print STDERR "sql: $sql\n";
				my $results = $dbh->selectall_arrayref($sql);
				
				push @$where, "association.source_db_id IN (".join(",", map { $_->[0] } @$results).")";
	
	
	#			push @$tables, "db";
	#			push @$where, "association.source_db_id=db.id";
	#			push @$where, "db.name IN ("
	#				.join(",", map { sql_quote($_) } @$assby).")";
			}
		}
	
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
		{	print STDERR "ERROR! Could not find a table to attach association to!\ntables: ".Dumper($tables)."where: ".Dumper($where)."\n";
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
	{	print STDERR "ERROR!! Could not find a table to attach associations to!\ntables: ".Dumper($tables)."where: ".Dumper($where)."\n";
	}
}


1;



