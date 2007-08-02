=head1 SYNOPSIS

package GO::CGI::Query

=head2 Usage

use CGI 'param';
my $q = new CGI;
my $params = $q->Vars;

my $data = GO::CGI::Query->do_query(-params=>$params);


=head2 do_query

  Arguments - $CGI->Vars
  returns	- GO::Model::Graph

  Takes a hash of parameters from CGI.pm and 
  returns:
	if view=query - list of GO::Model::Term
	Tree view  - GO::Model::Graph

=cut

package GO::CGI::Query;

use Exporter;
@ISA = ('Exporter');
@EXPORT_OK = qw(get_gp_details get_term_details get_gp_assocs get_term_assocs get_nit get_current_state_node_graph get_data_for_chart get_graph_for_gp);

use strict;

use DBI;
use Carp;
use GO::AppHandle;
#use GO::Utils qw(rearrange spell_greek);
use GO::Utils qw(rearrange);
use GO::SqlWrapper qw(sql_quote select_hashlist);
use HTML::Entities;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;
#use Time::HiRes qw(gettimeofday);

=head2 get_gp_details

	Arguments - session, listref of GP xrefs or ID from the database
	Returns   - listref of GP objects or undefined, with a session message
	
	my $gp_list = get_gp_details($session, $gps, $tmpl);

	where $gps is of the form
	{ gpxref => [ dbA:acc1, dbA:acc2, dbB:acc3 ] } or
	{ id => [ 2948271, 3985271, 29481 ] }
	
	and $tmpl is a hashref which can include the following:
	{	synonyms => 1,     # load GP synonyms
		has_seq => 1,      # just indicate whether the GP has a seq
		seq => 1,          # get all the seq info
		use_filters => 1,  # whether or not to use filters
	}

=cut

sub get_gp_details {
	my $session = shift;
	my $constr = shift;
	my $template = shift;
	my $apph = $session->apph;
	my $dbh = $apph->dbh;
	
	if (!$template)
	{	#	the default (as used by gp-details.cgi) is to have all the info
		$template = {
			synonyms => 1,
			seq => 1,
		};
	}

	my $gps;
	
	my $tables = ["gene_product", "dbxref"];
	my $where = ["gene_product.dbxref_id = dbxref.id"];
	
	if ($constr->{gpxref})
	{	$gps = $constr->{gpxref};
		if (!ref($gps))
		{	$gps = [$gps];
		}
		else
		{	#	check for / remove dups
			my %hash;
			@hash{@$gps} = {};
			$gps = [ keys %hash ];
		}
		
		push @$where, "(".join(" OR ", 
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
	{	$gps = $constr->{id};
		if (!ref($gps))
		{	$gps = [$gps];
		}
		else
		{	#	check for / remove dups
			my %hash;
			@hash{@$gps} = {};
			$gps = [ keys %hash ];
		}
		push @$where, "gene_product.id IN (".join(",", map { sql_quote($_) }@{$gps} ). ")";
	}

	if ($template->{use_filters} && keys %{$apph->filters})
	{	#	set the filters
		set_filters($apph->filters, $tables, $where);
	}

	my $sql = "SELECT DISTINCT gene_product.*, dbxref.xref_key AS acc, dbxref.xref_dbname AS speciesdb from " . join(", ", @$tables) . " WHERE " . join(" AND ", @$where);

	if ($sql =~ /\(\)/)
	{	print STDERR "Problem with the SQL!\n$sql\n";
		return undef;
	}

	print STDERR "SQL: $sql\n";
	
	my $sth = $dbh->prepare($sql);
	$sth->execute();

	my %gp_h = ();
	
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

		#	check we have all the GPs we were looking for
		if (scalar (keys %gp_h) != scalar @$gps)
		{	print STDERR "scalar gps = ".scalar @$gps."; scalar keys gp_h = ".scalar (keys %gp_h)."\n";
			my @missing;
			if ($constr->{gpxref}) {
				my %hash;
				map { $hash{$_->xref->xref_dbname . ":" . $_->xref->xref_key} = 1 } values %gp_h;
				
				foreach (@$gps)
				{	#if (!grep { $xref eq $_->speciesdb.":".$_->acc } values %gp_h)
					if (!$hash{$_})
					{	push @missing, $_;
						print STDERR "Lost $_\n";
					}
				}
				$session->add_message('warning', ['gp_not_found', @missing]);
			}
		}

		$apph->_get_product_species([values %gp_h]);
		$apph->_get_product_types([values %gp_h]);
		if ($template && $template->{synonyms}) {
		# get synonyms (spelt greek)
			my $sl =
				select_hashlist($dbh,
					"gene_product_synonym",
					"gene_product_id in (".join(", ", keys %gp_h).")");
	
			foreach (@$sl) {
				$gp_h{$_->{gene_product_id}}->add_synonym( encode_entities($_->{product_synonym}));
			}
		}

		if (!$template->{seq})
		{	if ($template->{has_seq})
			{	#	just see if it has a sequence, don't retrieve the seq
				_get_products_seqs($apph, [values %gp_h], 'has_seq');
			}
		}
		else {
			_get_products_seqs($apph, [values %gp_h] );
		}
		return [values %gp_h];
	}

	$session->add_message('fatal', ['gp_not_found', @$gps]);
	return 0;
}

=head2 get_gp_assocs

	Arguments - session, listref of GP xrefs, format (optional)
	Returns   - listref of GP objects or undefined, with a session message
	
	my $gp_list = get_gp_assocs($session, $gps, $options);

	where $gps is of the form
	{ gpxref => [ dbA:acc1, dbA:acc2, dbB:acc3 ] } or
	{ id => [ 2948271, 3985271, 29481 ] }
	
	and $options is a hashref which can include the following:
	{	format => xxx,     # format that the results are to be output in
	}

=cut

sub get_gp_assocs {
	my $session = shift;
	my $gps = shift;  # list of GPs
	my $format = shift;
	my $page = shift;
	my $apph = $session->apph;
	my $dbh = $apph->dbh;
	my $gp_tmpl = { no_seq => 1 };
	if ($format)
	{	#	need synonyms
		$gp_tmpl->{synonyms} = 1;
	}
	
#	print STDERR "gps: ".Dumper($gps)."\n";

	#	check for / remove dups
	my %hash;
	@hash{@$gps} = {};
	$gps = [ keys %hash ];

	my $results;
	my %asso_h;

	#	get brief info about each gp - symbol, name, spp, dbxref, type
	my $product_h;
	my $gp_l = get_gp_details($session, { gpxref => $gps }, $gp_tmpl);
	return 0 unless $gp_l;
	
	map { $product_h->{$_->id} = $_ } @$gp_l;

	if (scalar keys %$product_h > 1 && $session->ses_type eq 'gp_assoc')
	{	$session->ses_type('gp_assoc_multi');
	}

	#	create a handy little mapping of gpxref to gpid
	my %xref_to_gpid;
	map { $xref_to_gpid{ $_->xref->xref_dbname .":". $_->xref->xref_key } = $_->id } values %$product_h;

#	print STDERR "product_h: ".Dumper($product_h);

	my $page_size = $session->get_param('page_size');
	
	my $pairs;
	my $cache;
	#	get the pairs of gps and terms
	#	these params appear if we have already got the results in some form
	if ($page_size eq 'all' || $page || $format)
	{	$pairs = $session->get_caching_param('gpid_2_acc');
		$cache = 1 unless !$pairs;
	}
	
	if (!$pairs)
	{	$pairs = get_assoc_pairs($apph, { products => [ map{ $_->id } values %$product_h ] } );
	}

#	print STDERR "pairs:\n".Dumper($pairs)."\n";

	if (!$pairs || !@$pairs)
	{	print STDERR "No pairs found. Aborting...\n";
		$session->add_message('fatal', 'no_gp_assocs');
		if ($session->ses_type eq 'gp_assocs' && !$format)
		{	return { product_h => $product_h, 
				order => [ map { { gp => $product_h->{$xref_to_gpid{$_}} } } @$gps ] };
		}
		return 0;
	}
	else
	{	#	put these in the order they appear in in @$gps
		my %gpid_h = ();
		foreach (@$pairs)
		{	push @{$gpid_h{$_->[1]}}, $_->[0];
		}

		my @ordered;
		map
		{	my $id = $xref_to_gpid{$_};
			if ($gpid_h{$id})
			{	map
				{	push @ordered, [ $id, $_ ]
				} @{$gpid_h{$id}};
			}
		} @$gps;
		#	replace pairs with @ordered
		$pairs = [@ordered];
	}


	#	get the subset of terms and gps we want to look at
	my @subset = @$pairs;

	#	if there are several gps, show all the info on the same page
	if (scalar @$gps > 1)
	{	$page_size = 'all';
	}

#	print STDERR "subset before:\n".Dumper(\@subset)."\n";
	my $n_pages = $session->get_n_pages(scalar @$pairs, $page_size);
	if ($n_pages != 1)
	{	@subset = @{$session->get_subset(\@subset)};
	}
#	print STDERR "subset after:\n".Dumper(\@subset)."\n";
	
	$session->set_caching_param('gpid_2_acc', $pairs) unless $cache;
#	$session->set_param('current_query', 'n_pages', [$n_pages]);
#	$session->set_param('current_query', 'n_results', [scalar(@$pairs)]);

	#	get the unique term accs and term info
	my %term_h;
	@term_h{ map { $_->[1] } @subset } = ();
	if (keys %term_h)
	{	#	if we have filters on anything other than ont and speciesdb,
		#	don't get n deep associations
		my $tmpl = { acc=>1 };
		if ($session->check_gp_count_ok == 1)
		{	$tmpl->{n_deep_products} = 1;
		}
		if ($format && $format eq 'rdfxml')
		{	#	need synonyms
			$tmpl->{definition} = 1;
		}
		my $terms = $apph->get_terms({accs=>[keys %term_h]}, $tmpl);
		foreach (@$terms)
		{	$term_h{$_->acc} = $_;
		}
#		print STDERR "term_h:\n".Dumper(\%term_h)."\n";
	}
	else
	{	print STDERR "No keys to term_h found. Aborting...\n";
		$session->add_message('fatal', 'no_gp_assocs');
		if ($session->ses_type eq 'gp_assocs' && !$format)
		{	return { product_h => $product_h, 
				order => [ map { { gp => $product_h->{$xref_to_gpid{$_}} } } @$gps ] };
		}
		return 0;
	}

	my $assocs = get_association_data(-apph=>$apph, -terms=>[values %term_h], -products=>[ values %$product_h ], '-format'=>$format);

	#	return the results if a format is specified
	if ($format)
	{	return $assocs;
	}

	#	sort according to the order in gps and subset
	my $acc_id_list;
	my %gp_id_h;
	foreach (@subset) {
		push @{$gp_id_h{ $_->[0] }}, $term_h{$_->[1]};
	}
	
	foreach (@$gps)
	{	my $id = $xref_to_gpid{$_};
		if ($gp_id_h{ $id })
		{	push @$acc_id_list, { gp => $product_h->{$id}, terms => $gp_id_h{$id} };
		}
		else
		{	push @$acc_id_list, { gp => $product_h->{$id} };
		}
	}


#	print STDERR "assocs from get_association_data: ".Dumper($assocs)."\n";

	my %id_2_acc;
	foreach (values %term_h)
	{	$id_2_acc{ $_->id } = $_->acc;
	}

	my $assoc_h;
	foreach my $assoc (@$assocs) {
		#	go through the association list and group together assocs by term and qualifier
		my $q = 'a';
		if ($assoc->qualifier_list)
		{	$q = join(",", map { $_->name } @{$assoc->qualifier_list});
		}
		
		push @{ $assoc_h->{ $assoc->gene_product->id }{ $id_2_acc{$assoc->{term_id}} }{ $q } }, $assoc;
	}

	return { order => $acc_id_list, assoc_h => $assoc_h, product_h => $product_h, term_h => \%term_h, n_pages => $n_pages };
}

=head2 get_term_details

	Arguments - session, a GO term acc, current tree,
	            term context (for the tree), format
	Returns   - the GO term in an appropriate graph

=cut

sub get_term_details {
	my ($session, $term_id, $tree, $term_context, $format) = rearrange([qw(session term_id tree term_context format)], @_);
	my $apph = $session->apph;

	my $tmpl;
	if ($format eq 'png' || $format eq 'dot')
	{	$tmpl = { acc => 1 };
	}

	my $term_l = _term_check($session, $term_id, $tmpl);
	return if !$term_l;

	my $term = $term_l->[0];

#	print STDERR "term: ".Dumper($term)."\n";

	if ($format || $session->check_gp_count_ok == 0)
	{	$tmpl = { acc => 1 };
	}
	else
	{	$tmpl = { acc => 1, n_deep_products => 1 };
	}

	my $graph;
	my $ontologies = $session->get_ontology_list;

	if (grep { $_ eq $term->namespace } @$ontologies)
	{	#	we're OK
	#	$session->ses_type('term_details') unless $format;
		my $open = $tree->{open_1};
		if ($term_context eq 'sibling') {
#			my $accs = $session->get_param_values('open_1');
			print STDERR "accs: ".Dumper($open)."\n";
			
			my $open_terms = $apph->get_terms({acc=>$open}, $tmpl) if ($open && @$open);
			$graph = $apph->get_graph_DPSC(-term=>$term, -termh=>{open_terms=>$open_terms}, -template=>{terms=>$tmpl});

		} else {
			print STDERR "Getting the graph... ";
	
			$graph = $apph->get_graph_by_terms(-terms=>[$term], -depth=>0, -template=>{terms=>$tmpl});
	
#			my $accs = $session->get_param_values('open_1');
			if ($open && @$open)
			{	foreach my $open_term (@{$open || []}) {
					$apph->extend_graph(-graph=>$graph, -acc=>$open_term, -depth=>1, -template=>{terms=>$tmpl});
				}
			}
			print STDERR "Got graph. Wikkid!\n";
		}
	
#		foreach my $close_below (@{$session->get_param_values('closed') || []}) {
		if ($tree->{closed})
		{	foreach my $close_below (@{$tree->{closed}}) {
				eval {
					$graph->close_below($close_below, 'close_iff_no_parent');
				};
			}
		}

	}
	else
	{	#	if it's a goslim, we can make a pretty graph for it
		if ($term->namespace eq 'subset' && $term->acc =~ /goslim/)
		{	$session->ses_type('subset_details');
		  my $goslim = $apph->get_terms({subset=>$term->acc}, $tmpl);

			$graph = $apph->get_graph_by_terms(-terms=>$goslim, -depth=>0, -template=>{terms=>$tmpl});

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
		}
		else
		#	this isn't an ontology. Maybe we shouldn't bother doing a graph?
		{	$session->ses_type('vocab_details');
			$graph = $apph->create_graph_obj;
			$graph->add_term($term);
		}
	}
	$graph->seed_nodes($term);
	return $graph;
}

=head2 get_term_assocs

	Arguments - session, list of term accs, format (optional)
	Returns   - data structure containing terms, associations and
	            GPs, or a warning message

=cut

sub get_term_assocs {
	my ($session, $term_list, $action, $format, $all_ass, $page) = rearrange([qw(session term_list action format all_ass page)], @_);
	#	all_ass means get all associated GPs; if it is not set or set to 0,
	#	only direct GPs will be retrieved
	my $apph = $session->apph;
	my $dbh = $apph->dbh;

	print STDERR "Starting get_term_assocs...\n\n";

	my $pairs;    # pairs of term accs and GP ids
	my $parent;   # the parent(s) of each term
	my $term_h;   # term info for all terms
	my $cache;    # save info for caching here

	#	if we are doing a term_assocs query with results output
	#	as html, we want to check that we don't have too many
	#	results. If the format is set, the action is 'override',
	#	or the session type isn't 'term_assoc', we don't do these
	#	checks.
	my $check_results;
	if (!$format && $session->ses_type eq 'term_assocs' && $action ne 'override')
	{	$check_results = 1;
		print STDERR "Check results is ON\n";
	}

	my $max = $session->get_param('max_assocs') || 2000;
	              # max no. of assocs


	my $n_prods = 'n_products';
	$n_prods = 'n_deep_products' if $all_ass;

	my $n_pages;

	my $tmpl = { acc => 1, definition => 1 };
	my $page_size = $session->get_param('page_size');
	if ($page_size eq 'all' || $page || $format)
	{	print STDERR "Looking for cached results...\n";
	#	get the pairs from the cached results
		$cache = $session->get_all_caching_params; # get from file
		$pairs = $cache->{term_product_ids};
		$parent = $cache->{parent_terms};
		if ($pairs)
		{	print STDERR "Using cached results.\n";
			#	get the subset of terms and gps we want to look at
			#	print STDERR "subset before:\n".Dumper(\@subset)."\n";
			$n_pages = $session->get_n_pages(scalar @$pairs, $page_size);
			if ($n_pages != 1)
			{	$pairs = $session->get_subset($pairs);
			}
#			$session->set_param('current_query', 'n_pages', [$n_pages]);
	
			#	get a list of all the terms we need the term info for
			my %to_get;
			@to_get{ map { $_->[0] } @$pairs } = {};

			#	add the query terms
			map { $to_get{$_} = {} } @$term_list;
			
			my $terms = $apph->get_terms({accs => [keys %to_get]}, $tmpl);
			#	put the info into term_h
			map { $term_h->{$_->acc} = $_ } @$terms;
			
			#	save the cache data
			$session->set_all_caching_params($cache);
		}
	}
	
	if (!$pairs || !keys %$parent)
	{	#	this is a new query. Let's start with a few
		#	checks to make sure our terms are OK
		my $term_l;
		if ($session->check_gp_count_ok == 1)
		{	$term_l = _term_check($session, $term_list, {acc=>1, definition=>1, $n_prods=>1});
		}
		else
		{	$term_l = _term_check($session, $term_list, $tmpl);
		}
		return if !$term_l;

		my $roots = $apph->get_ontology_root_terms(-template=>{acc=>1});
		push @$roots, $apph->get_root_term(-template=>{acc=>1});
		my $ontologies = $session->get_ontology_list;
		print STDERR "roots: ". join("\n", map { $_->acc } @$roots)."\n\n";
		print STDERR "onts: ". join(", ", @$ontologies)."\n";
		
		my $err;

		foreach my $term (@$term_l)
		{	#	check the term is an ontology term
			if (!grep { $_ eq $term->namespace } @$ontologies)
			{	print STDERR "Non ontology term: ".$term->name."\n";
				push @{$err->{not_ont_term}}, $term->name." : ".$term->acc;
				next;
			}

			#	check that the term is not one of the root nodes
			if (grep { $term->acc eq $_->acc } @$roots)
			{	print STDERR "Root term: ".$term->name."\n";
				push @{$err->{root_term_assocs}}, $term->name." : ".$term->acc;
				next;
			}

			#	check that the term is not obsolete
			if ($term->is_obsolete == 1)
			{	print STDERR "Obsolete term: ".$term->name."\n";
				push @{$err->{obs_term}}, $term->name." : ".$term->acc;
				next;
			}
			
			#	phew! It's OK. Add it to term_h
			$term_h->{$term->acc} = $term;
		}

		#	we'll keep term_l as a list of valid terms, so let's
		#	reset term_l to be the OK terms stored in term_h
		$term_l = [ values %$term_h ];

		print STDERR "term_l: ".Dumper($term_l)."\n";

		if ($err)
		{	if ($session->ses_type eq 'term_assoc')
			{	if (@$term_l)
				{	#	we've got some terms so just warn about the other probs
					foreach my $msg (keys %$err)
					{	$session->add_message('warning', [$msg, $err->{$msg}]);
					}
				}
				else
				{	foreach my $msg (keys %$err)
					{	$session->add_message('fatal', [$msg, $err->{$msg}]);
					}
					return 0;
				}
			}
			else
			{	return 0;
			}
		}
		
		#	if the filter settings allow us, check whether
		#	we have too many associations
		#	it's OK to have more than the max_assocs value
		#	if check_results is 0 (e.g. if we're getting formatted
		#	results or have been told to override the check)
		if ($session->check_gp_count_ok == 1)
		{	my $sum = 0;
			foreach my $term (@$term_l)
			{	$sum += $term->$n_prods;
		#		last if ($sum > $max);
				if ($sum > $max)
				{	if ($check_results)
					{	#	too many associations. Abort!
						$session->add_message('fatal', ['too_many_assocs', map { $_->name . " ; " . $_->acc } @$term_l ]);
						return term_assoc_die($session, $term_l, $all_ass, 1);
					}
					last;
				}
			}
			
			if ($sum == 0)
			{	print STDERR "No annotations found.\n";
				if ($session->ses_type eq 'term_assoc')
				{	$session->add_message('fatal', ['no_assocs', map { $_->name . " ; " . $_->acc } @$term_l ]);
					return term_assoc_die($session, $term_l, $all_ass, 0);
				}
				else
				{	return 0;
				}
			}
		}

		#	we've got the all clear. Let's get them associations!
		my $accs_of_interest;
		my $acc_to_name;
		
		if ($all_ass == 1)
		{	
		#	old graph-based version
			#	my $graph = $apph->get_graph_by_terms($term_l, -1, { traverse_down => 1, traverse_up => 0, terms=>{acc=>1} });
			#	my $it = $graph->create_iterator;
				# returns a GO::Model::GraphIterator object
			#	while (my $ni = $it->next_node_instance) {
			#		if (!$parents->{$ni->term->acc})
			#		{	push @$terms_from_graph, $ni->term;
			#			$parents->{$ni->term->acc} = $term->acc;
			#		}
			#	}
		
			#	new graph_path-based version
			#	find all terms with @$term_l as parents
			my $results = $dbh->selectall_arrayref(
			"SELECT DISTINCT term2_id, term1_id FROM graph_path WHERE DISTANCE <> 0 AND term1_id IN ("
			.join(",", map { $_->id } @$term_l)
			.")");

			print STDERR "SQL: SELECT DISTINCT term2_id, term1_id FROM graph_path WHERE DISTANCE <> 0 AND term1_id IN ("
			.join(",", map { $_->id } @$term_l)
			.")\n";

			if (@$results)
			{	#	get the accs from this list of IDs
				my %temp_parents;
				my %all_parents;
				my %acc_h;
				
				my %terms_by_id;
				map { $terms_by_id{$_->id} = $_ } @$term_l;
				
				foreach (@$results)
				{	push @{$temp_parents{$_->[0]}}, $terms_by_id{$_->[1]}->acc;
					$all_parents{ $terms_by_id{$_->[1]}->acc } = 1;
				}

				my $id_accs = $dbh->selectall_arrayref("SELECT id, acc, name FROM term WHERE id IN (".join(",", keys %temp_parents).")");

				print STDERR "SQL: SELECT id, acc, name FROM term WHERE id IN (".join(",", keys %temp_parents).")\n";

				#	convert into hash
				foreach (@$id_accs)
				{	$acc_to_name->{$_->[1]} = $_->[2];
				#	create the parent hash info
					$parent->{$_->[1]} = $temp_parents{$_->[0]};
				#	add this acc to acc_h, which will be turned into accs_of_interest
					$acc_h{$_->[1]} = 1;
				}
			
				foreach (@$term_l)
				{	$acc_to_name->{$_->acc} = $_->name;
					$term_h->{$_->acc} = $_;
					$acc_h{$_->acc} = 1;
					if ($temp_parents{$_->acc})
					{	$parent->{$_->acc} = $temp_parents{$_->acc};
					}
					push @{$parent->{$_->acc}}, $_->acc;
				}
				$accs_of_interest = [ keys %acc_h ];
			}
		}
		if (!$accs_of_interest)
		{	#	either we are looking at direct annotations
			#	or the term has no children, so the above query returned
			#	no results
			$accs_of_interest = [ map { $_->acc } @$term_l ];
			map {
				$parent->{$_->acc} = [ $_->acc ];
				$acc_to_name->{$_->acc} = $_->name;
				$term_h->{$_->acc} = $_;
				} @$term_l;
		}

		my $all_pairs = get_assoc_pairs($apph, { terms => $accs_of_interest } );

		print STDERR "got pairs:\n".Dumper(scalar @$all_pairs)."\n";
		if (!$all_pairs || !@$all_pairs)
		{	print STDERR "No pairs found.\n";
		#	if ($format)
		#	{	$session->suicide_message('no_assocs');
		#	}
			$session->add_message('fatal', ['no_assocs', map { $_->name . " ; " . $_->acc } @$term_l ]);
			return term_assoc_die($session, $term_l, $all_ass, 0, $all_pairs);
#			return { root_terms => $term_l } ;
		}

	#	sort 'n' save the results
	#	we want to save our terms sorted by name but with the root terms first
	#	pairs already has the gps sorted correctly. We just need to sort the terms
		my @sorted_pairs;
		if ($format)
	#	don't need to sort if we're preparing data to download
		{	@sorted_pairs = @$all_pairs;
		}
		else
		{	my $idx = 0;
			@sorted_pairs =
				map { [ (split("\0", $_))[3..4] ] }
				sort
				map {
					my $acc = $_->[0];
					join("\0", 
						(grep { $acc eq $_->acc } @$term_l) ? 0 : 1,
						lc $acc_to_name->{$acc},
						sprintf("%08d",$idx++),
						$_->[0],
						$_->[1] );
#									$idx++;
						} @$all_pairs;
										
		#	print STDERR "list:\n".Dumper(\@list)."\n";
		}
#		print STDERR "sorted pairs:\n".Dumper(\@sorted_pairs)."\n";

		if (!$cache)
		#	if there's no cache, set the cache now
		{	$session->set_caching_param('term_product_ids', [@sorted_pairs]);
			$session->set_caching_param('parent_terms', $parent);
		}

		#	check whether we have too many results or not
		if (scalar @sorted_pairs > $max && $check_results)
		{	$session->add_message('fatal', ['too_many_assocs', map { $_->name . " ; " . $_->acc } @$term_l ]);
			return term_assoc_die($session, $term_l, $all_ass, 1, $all_pairs);
		}

		$n_pages = $session->get_n_pages(scalar @sorted_pairs, $page_size);
#		$session->set_param('current_query', 'n_pages', [$n_pages]);
#		$session->set_param('current_query', 'n_results', [scalar(@sorted_pairs)]);

		#	get the subset of terms and gps we want to look at
		@$pairs = @sorted_pairs;
		if ($n_pages != 1)
		{	$pairs = $session->get_subset($pairs);
		}

	#	print STDERR "pairs:\n".Dumper($pairs)."\n";

		#	get the rest of the information for our terms of interest
		my %to_get;
		map { $to_get{ $_->[0] } = 1 } @$pairs;
		my $terms_to_get = $apph->get_terms({accs => [keys %to_get]}, $tmpl);
		#	put the info into term_h
		map { $term_h->{$_->acc} = $_ } @$terms_to_get;
	}

	#	get the gene product data
	my %gp_h;
	@gp_h{ map{ $_->[1] }@$pairs } = ();

	my $product_h;
	my $gp_tmpl = { has_seq => 1 };
	if ($format)
	{	if ($format eq 'go_assoc')
		{	$gp_tmpl = { synonyms => 1 };
		}
		elsif ($format eq 'rdfxml')
		{	
		}
	}

	my $gp_l = get_gp_details($session, { id => [keys %gp_h]}, $gp_tmpl);
	map { $product_h->{$_->id} = $_ } @$gp_l;

	my $assoc_terms = [];
	my $acc_id_list;

	my $last = '';
	foreach (@$pairs) {
		if ($_->[0] ne $last)
		{	if ($term_h->{$_->[0]}) {
				push @$assoc_terms, $term_h->{$_->[0]};
			} else {
				printf STDERR "something is wrong, no term for acc: %s\n",$_->[0];
				$term_h->{$_->[0]}++;
			}
			push @$acc_id_list, { term => $term_h->{$_->[0]}, gps => [ $product_h->{$_->[1]} ] };
			$last = $_->[0];
		}
		else
		{	push @{$acc_id_list->[-1]{gps}}, $product_h->{$_->[1]};
		}
	}
	
#	print STDERR "product_h:\n".Dumper($product_h)."\n";

	my $assocs = get_association_data(-apph=>$apph, -terms=>$assoc_terms, -products=>[ values %$product_h ], '-format'=>$format);

	#	return the results if a format is specified
	if ($format)
	{	return $assocs;
	}

#	print STDERR "associations:\n".Dumper($assocs)."\n";

	my %id_2_acc;
	foreach (@$assoc_terms)
	{	$id_2_acc{ $_->id } = $_->acc;
	}

	my $assoc_h;
	foreach my $assoc (@$assocs) {
	#	go through the association list and group together assocs by term and qualifier
		my $q = 'a';
		if ($assoc->qualifier_list)
		{	$q = join(",", map { $_->name } @{$assoc->qualifier_list});
		}
		
		push @{ $assoc_h->{ $id_2_acc{$assoc->{term_id}} }{ $assoc->gene_product->id }{ $q } }, $assoc;
	}
	
	return { product_h => $product_h, assoc_h => $assoc_h, term_h => $term_h, parent_h => $parent, order => $acc_id_list, n_pages => $n_pages }; #root_terms => [ map { $term_h->{$_} } @$term_list],  };
}

sub term_assoc_die {
	my ($session, $term_l, $all_ass, $max, $pairs) = @_;
	my $apph = $session->apph;
	my $dbh = $apph->dbh;
	my $term_h;
	map { $term_h->{$_->acc} = $_ } @$term_l;

	if ($max)
	{	my $data;
		#	too many assocs: get some useful info
		#	- no. of GPs annot'd directly to term
		#	(WITH filters)
		if ($all_ass)
		{	if ($pairs)
			{	#	count how many GPs are annot'd to the terms
				my %counts;
				map { $counts{$_->acc} = 0 } @$term_l;
				foreach (@$pairs)
				{	$counts{$_->[0]}++ if exists $counts{$_->[0]};
				}
				foreach (@$term_l)
				{	$_->n_products($counts{$_->acc});
				}
			}
			else
			{	#	get the data using the apph
				#	n.b. currently doesn't respect all filters!!
				$apph->get_product_count({terms => $term_l});
			}
		}
		#	- no. of children of term
		my $count_1 = $dbh->selectall_arrayref("SELECT term1_id, COUNT(DISTINCT term2_id) FROM term2term WHERE term1_id IN (".join(", ", map { $_->id } @$term_l).") GROUP BY term1_id");
		my %count1_h;
		map { $count1_h{ $_->[0] } = $_->[1] } @$count_1 if $count_1;
		
		map {
			$data->{$_->acc}{direct_children} = $count1_h{ $_->id } || 0;
			} @$term_l;
		return { term_h => $term_h, data => $data };
	}
	else
	{	#	if there are no associations, see if it is because
		#	we have filters by getting the product count
		#	BUT WITHOUT FILTERS!!
		
		#	horrible hack for the time being
		my $filter_h = $apph->filters;
		$apph->filters({});
		print STDERR "filters: ".Dumper($apph->filters)."\n";

		my $total;
		if ($all_ass)
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
		print STDERR "filters: ".Dumper($apph->filters)."\n";
		
		return { term_h => $term_h, total => $total };
	}
}

sub get_assoc_pairs {
#	possible constraints
#	terms
#	gps
#	filters

#	gps: input is a list of gp ids
#	terms: input is a list of (ordered?) term ids

#	for caching: save term acc - gp id pairs

#	constructor: GPs: sort by term type, term name
#	term: sort by GP symbol, GP full name

	my ($apph, $constr) = rearrange([qw(apph constraints)], @_);
	my $dbh = $apph->dbh;
	my $unique;

	my $tables = ["association", "term"];
	my $where = ["association.term_id = term.id"];
	my $order;

	if ($constr->{terms})
	{	my $acc = $constr->{terms};
		$acc = [$acc] unless (ref($acc) eq 'ARRAY');
		push @$tables, 'gene_product';
		push @$where, 
		"association.gene_product_id = gene_product.id",
		"term.acc IN (".join(",", map { sql_quote($_) } @$acc).")";
		$order = "gene_product.symbol, gene_product.full_name";
	}

	if ($constr->{products})
	{	my $gps = $constr->{products};
		$gps = [$gps] unless (ref($gps) eq 'ARRAY');
		push @$where, "association.gene_product_id IN (".join(",", map{ sql_quote($_) } @$gps).")";
		$order = "term.term_type, term.name";
	}

	my $filters = $apph->filters || {};
	if ($filters && keys %{$filters})
	{	#	if the constraint is terms, we don't need to do the ontology filter
		if ($constr->{terms} && $filters->{ont})
		{	delete $filters->{ont};
		}
		
		#	set the filters
		set_filters($filters, $tables, $where);
	}

	my $sql = "SELECT DISTINCT term.acc, association.gene_product_id FROM "
				.join(", ", @$tables) . " WHERE "
				.join(" AND ", @$where)
				. " ORDER BY $order";

	print STDERR "SQL: $sql\n";
	
	my $results = $dbh->selectall_arrayref($sql);
	
	if (!$results)
	{	return;
	}
	
	return $results;
}

sub get_association_data {
#	my $self = shift;
#	my $dbh = $self->dbh;
	my ($apph, $terms, $products, $format) =
	  rearrange([qw(apph terms products format)], @_);
	my $dbh = $apph->dbh;

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
	if (!$format || $format eq 'go_assoc')
	{	push @$cols, 
			"association.assocdate",
			"association.source_db_id";
	}
	if (!$format)
	{ push @$cols, "association.gene_product_id",
	}
	else
	{	push @$cols, "association.gene_product_id AS gp_id",
	}
	#set the term constraint
	$terms = [$terms] unless (ref($terms));
	unshift @$where,
		"association.term_id in (".join(", ", map { $_->id }@$terms).")";

	#set the product constraint
	$products = [$products] unless (ref($products));
	push @$where,
		"association.gene_product_id IN (".join(",", map { $_->id }@$products).")";

	my $filters = $apph->filters || {};

	#	set the evidence code filters
	my $evcodes = $filters->{evcode};
	my $quals = $filters->{qual};
	my $assbys = $filters->{assby};
	if ($evcodes || $quals || $assbys)
	{	set_filters( { evcode => $evcodes, qual => $quals, assby => $assbys }, $tables, $where);
	}
	my $hl = select_hashlist($dbh, $tables, $where, $cols);

	if (!@$hl) {
		print STDERR "No associations found in get_association_data: error?\n";
		return undef;
	}

	my @assocs = ();
	my @assoc_lookup = ();
	my @assoc_ids = ();

	my ($gp_l, $term_l);
	if ($format)
	{	map { $gp_l->[$_->id] = $_ } @$products;
		map { $term_l->[$_->id] = $_ } @$terms;
	}
	
	foreach my $h (@$hl) {
		my $assoc = $assoc_lookup[$h->{id}];
		if (!$assoc) {
			$assoc = $apph->create_association_obj($h);
			$assoc_lookup[$assoc->id] = $assoc;
			if ($format)
			{	#print STDERR Dumper($assoc);
				if (!$gp_l->[$h->{gp_id}]) {
					print STDERR "Could not find gene product for assoc ".$assoc->id."!\n"
					.Dumper($assoc)."\n";
				}
				else
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
		print STDERR "No associations found in get_association_data: error?\n";
		return 0;
	}
#	get extra association info
print STDERR "getting evidence seq xrefs...\n";
	$apph->_get_evidence_seq_xrefs(\@assocs);
print STDERR "getting qualifiers...\n";
	$apph->get_qualifiers(\@assocs);
print STDERR "getting assigned by...\n";
	$apph->get_assigned_by(\@assocs) if (!$format || $format eq 'go_assoc');

#	print STDERR "assocs: ".Dumper(\@assocs)."\n";

	if ($format)
	{	my $graph = $apph->create_graph_obj;
		foreach (@$terms)
		{	$graph->add_term($_);
		}
		return $graph;
	}

	return \@assocs;
}

sub get_current_state_node_graph {
	my $session = shift;
	my $tree = shift;
	my $format = shift || undef;
	my $apph = $session->apph;
	my $graph;
	my $terms;

	my $tmpl = { acc => 1 };
	if ($format && ($format eq 'obo' || $format eq 'rdfxml'))
	{	$tmpl = { acc => 1, synonym_list => 1, definition => 1 }; #, dbxref_list => 1 };
		if ($format eq 'rdfxml' && $session->check_gp_count_ok == 1)
		{	$tmpl->{n_deep_products} = 1;
		}
	}
	elsif ($session->check_gp_count_ok == 1)
	{	$tmpl->{n_deep_products} = 1;
	}
	
	print STDERR "Template (from Query):\n".Dumper($tmpl)."\n";
	my $root = $apph->get_root_term(-template => $tmpl);
	print STDERR "Got root.\n";

	#show term parentage!!!
	#open_0 mean show all parents without expanding them (only coming from term search page?)
	#get_graph_by_terms_on_path will not work for open_0 by definition
	#as no parent terms in the query

	if ($tree->{open_0} && @{$tree->{open_0}}) {
	#	$graph = $self->_get_ancestor_graph($session,$session->get_param_values('open_0'),$session->get_param_values('open_1'));

		print STDERR "Using get_graph_by_terms\n";
		print STDERR "open_0: ".Dumper($tree->{open_0})."\n";
	
		$terms = $apph->get_terms({acc => $tree->{open_0} }, $tmpl);
		$graph = $apph->get_graph_by_terms(-terms=>$terms, -depth=>0, -template=>{terms => $tmpl});
	
		if ($tree->{open_1})
		{	$apph->extend_graph(-graph=>$graph, -acc=>$_, -depth=>1, -template=>{terms => $tmpl}) foreach (@{$tree->{open_1}});
		}
		print STDERR "Doing get children in the open_0 subroutine\n";
		$apph->_get_n_children_h($graph);
	}
	elsif ($tree->{open_1} && @{$tree->{open_1}}) {
		print STDERR "Using get_graph_by_terms_on_path\n";
		print STDERR "open_1: ".Dumper($tree->{open_1})."\n";
		$terms = $apph->get_terms({acc=>$tree->{open_1}}, $tmpl);
		$graph = $apph->get_graph_by_terms_on_path(-terms=>$terms, -root=>$root, -template=>{terms => $tmpl});
		
#		print STDERR "graph: ".Dumper($graph)."\n";
		
	} else {
		print STDERR "Using get_graph_by_terms with the 'term' parameter\n";
		if ($tree->{term} && @{$tree->{term}})
		{	$terms = $apph->get_terms({acc=>$tree->{term}}, $tmpl);
		}
		else
		{	$terms = [$root];
		}

#		unless (@terms) { # no terms - just show the root node graph
#			push @term_list, $root->acc;
#		}

		if (scalar @$terms != 0)
		{
#			$terms = $apph->get_terms({acc=>\@term_list}, $tmpl);

			my $depth = 0;
			if ($tree->{depth}) {
				$depth = $tree->{depth};
			}
			$graph = $apph->get_graph_by_terms(-terms=>$terms, -depth=>$depth, -template=>{terms=>$tmpl});
		}
	}

	if ($tree->{closed} && @{$tree->{closed}}) {
		foreach my $close (@{$tree->{closed}}) {
			eval {
				$graph->close_below($close);
			};
		}
	}

	print STDERR "seeding nodes...\n";
	$graph->seed_nodes($terms);

#	my $ont = $session->get_param_list('ont');
	my $ont = $apph->filters->{ont};
#	if ($ont) {
	#	my $t_gp = 0;
#		foreach my $c (@{$graph->get_child_terms($root->acc) || []})
#		{	unless (grep { $_ eq $c->type } @$ont)
#			{	$graph->close_below($c->acc);
#				$graph->delete_node($c->acc);
#			}
		#	else {
		#		$t_gp += $c->n_deep_products;
		#	}
#		}
	#	$root->n_deep_products($t_gp); #this is wrong!!
#	}

#new: remove the obsolete nodes from the graph in the summary and graphical view
#	if ($session->ses_type eq 'graphviz')
#	{	
		foreach my $c (@{$graph->get_child_terms($root->acc) || []})
		{	if ($c->is_obsolete || ($ont && !grep { $_ eq $c->type } @$ont))
			{	$graph->close_below($c->acc);
				$graph->delete_node($c->acc);
			}
		}
#	}
	print STDERR "Finished.\n";

#	print STDERR "Doing get children in the get_current_state_node_graph query\n";
#	$apph->_get_n_children_h($graph);
	return $graph;
}

=head2 get_data_for_chart

	Arguments - $session, $acc
	Returns   - data structure or error message
	
	Uses the term parameter from session and gets the data about the number
	of gene products annotated to or below that term. Used by the bar chart.

=cut

sub get_data_for_chart {
	my $session = shift;
	my $acc = shift;
	my $apph = $session->apph;
	my $dbh = $apph->dbh;
	my $tmpl = { acc => 1, n_deep_products => 1};
	my $term_l = _term_check($session, $acc, $tmpl);
	return if (!$term_l);

	my $term = $term_l->[0];

	if ($term->is_obsolete == 1)
	{	$session->suicide_message('obs_term');
	}

	my $ontologies = $session->get_ontology_list;
	#	check the term is an ontology term
	if (!grep { $_ eq $term->namespace } @$ontologies)
	{	$session->suicide_message('not_ont_term');
	}
	
	my $count = $term->n_deep_products;
	if ($count == 0)
	{	$session->suicide_message([ 'no_gp_assocs', $term->name." ; ".$term->acc." or its children"]);
	}

	#	get the children of the term
	my $child_accs = $dbh->selectall_arrayref("SELECT term.acc FROM term2term, term WHERE term.id = term2term.term2_id AND term2term.term1_id = ".$term->id);
	if (!$child_accs)
	{	$session->suicide_message([ 'no_children', $term->name." ; ".$term->acc ]);
		return;
	}
	map { $_ = $_->[0] } @$child_accs;
	my $children = $apph->get_terms({ accs => $child_accs }, $tmpl);

	my $data;
	$data->{parent} = { name => $term->name, acc => $term->acc, count => $count};
	print STDERR "term_data: ".Dumper($term)."\n";

	foreach my $child (@$children) {
		my $percent = sprintf("%.0f", ($child->n_deep_products/$count*100) );
		$data->{children}{$child->acc} = { acc => $child->acc, name => $child->name, count => $child->n_deep_products, percent => $percent };
	}

	my $l = length($count);
	$data->{graph} = [
		map { $data->{children}{ (split("\0", $_))[-1] } }
		sort
			#	{
			#		$b->[1] <=> $a->[1]
			#					||
			#		$a->[2] cmp $b->[2] 
			#	}
		map { join("\0",
					sprintf("%0".$l."d", ($count - $data->{children}{$_}{count})),
					$data->{children}{$_}{name},
					$_ ) }
					keys %{$data->{children}} ];

	delete $data->{children};

	return $data;
}

=head2 _term_check

  Arguments - acc or list of accs
              template (optional)
  returns   - a term list, a redirect or a warning

  Takes a term or list of terms and checks that they
  are present in the ontology. If they aren't, it checks
  alt_ids and returns any matches there.

=cut

sub _term_check {
	my ($session, $accs, $tmpl) =
		rearrange([qw(session accs template)], @_);
	my $apph = $session->apph;
	
	if (!ref($accs)) {
		$accs = [$accs];
	}
=cut
		my $term = $apph->get_term({acc => $accs}, $tmpl);
		return [$term] if ($term);
				
		#	try the synonyms instead
		$term = $apph->get_term({synonym => $accs}, {acc => 1});
		if ($term)
		{	#	redirect to the new URL
			(my $cgi = $session->ses_type) =~ s/_/-/g;
			print "Location: ".$session->get_param('cgi_url')."/$cgi.cgi?term=".$term->acc."&session_id=".$session->get_param('session_id')."\n\n";
				exit;
		}
		$accs = [$accs];
	}
	else
	{	
=cut
		#	a list of terms
		my $n_terms = scalar @$accs;
		my $term_l = $apph->get_terms({accs => $accs}, $tmpl);
	
		if (scalar @$term_l == $n_terms)
		{	return $term_l;
		}

		my %missing;
		@missing{ @$accs } = (1) x @$accs;
		if (@$term_l)
		{	#	check which IDs are missing, and do a synonym search
			foreach (@$term_l)
			{	delete $missing{ $_->acc };
			}
		}
	
		my $n_missing = scalar keys %missing;
		if ($n_missing == 1)
		{	$tmpl = { acc => 1 };
		}
		else
		{	$tmpl = { acc => 1, synonym_list => 1 };
		}
		my $syn_l = $apph->get_terms({ synonyms => [ keys %missing ] }, $tmpl);

		#	we found some terms
		if (@$syn_l)
		{	push @$term_l, @$syn_l;
			my %syn_accs;
			
			if ($n_missing > 1)
			{	#	check which of the IDs we found
				foreach (@$syn_l)
				{	my @alts = $_->synonyms_by_type('alt_id');
					if (@alts)
					{	map { if ($missing{$_}) { delete $missing{$_}; } } @alts;
					}
					last if (!keys %missing);
				}
			}
			else
			{	delete $missing{$_} foreach (keys %missing);
			}
			
			#	check no accs are duplicated
			map { $syn_accs{$_->acc} = $_ } @$term_l;
			#	should we warn the user that we didn't find some of their ids?
			#	if (keys %missing)
			#	{	#	we found some terms but not all of them
			#		$session->add_message('warning', ["term_not_found", [keys %missing]]);
			#	}
				
			(my $cgi = $session->ses_type) =~ s/_/-/g;
			print "Location: ".$session->get_param('cgi_url')
				."/$cgi.cgi?term="
				.join("&term=", keys %syn_accs)
				."&session_id=".$session->get_param('session_id')."\n\n";
			exit;
		}
		
		if (@$term_l)
		{	#	we found some terms but not all of them
			$session->add_message('warning', ["term_not_found", keys %missing]);
			return $term_l;
		}
#	}

	#	otherwise, we didn't find anything.
	$session->add_message('fatal', ["term_not_found", @$accs]);
	return 0;
}

sub get_graph_for_gp {
	my $session = shift;
	my $gps = shift;

	my $apph = $session->apph;
	my $data = get_gp_assocs($session, $gps);
	if (!$data || !$data->{assoc_h})
	{	$session->suicide_message(['no_gp_assocs_gp', $gps]);
	}
	
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
	
	return { graph => $graph, product_h => $data->{product_h} };
}

sub get_nit {
	my $session = shift;
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
	my $template = shift || 'full';
	
	print STDERR "template: ".$template."\n";

	$gps = [$gps] unless (ref($gps) eq 'ARRAY');

	return unless $gps;

	my %gp_h = map {$_->_seqs_obtained(1); $_->id=>$_} @$gps;
	my @gp_ids = keys %gp_h;
	return unless @gp_ids;
	
	my $cols = 'gene_product_seq.gene_product_id, seq.*';
	if ($template eq 'has_seq')
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

	if (@byid && $template eq 'full') {
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

=head2 get_definitions

  Arguments - apph, list of terms objects
  returns   - the terms with defs added

=cut

sub get_definitions {
	my ($apph, $terms) = @_;
	my $dbh = $apph->dbh;

	if (@$terms) {
		my @term_l;
		foreach (@$terms)
		{	$term_l[$_->id] = $_;
		}

		print STDERR "\nGetting term defs...\n";

		my $sql = "SELECT term_id, term_definition FROM term_definition WHERE term_id in (".join(", ", map { sql_quote($_->id) } @$terms).")";
		print STDERR "sql: $sql\n";
		my $sth = $dbh->prepare($sql);
		$sth->execute();

		while (my $d = $sth->fetchrow_arrayref) {
			$term_l[$d->[0]]->definition($d->[1]) if $term_l[$d->[0]];
		}
	}
}

=head2 set_filters

  Arguments - filters (from session->apph or manually set),
              tables and where arrays for an SQL query
  returns   - amended tables and where arrays

  Adds the filter settings to an SQL query

=cut

sub set_filters {
	my $filters = shift;
	my $tables = shift;
	my $where = shift;

#	assumes that the table 'gene_product' already exists

	if (!keys %$filters)
	{	return;
	}

	#	species DB
	my $spdbs = $filters->{speciesdb};
	if ($spdbs) {
		if (!ref($spdbs)) {
			$spdbs = [$spdbs];
		}
		
		if (@$spdbs)
		{	if (! grep { $_ eq 'dbxref' } @$tables)
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
			push @$tables, "species";
			push @$where, "species.id = gene_product.species_id";
			push @$where, "species.ncbi_taxa_id IN (".
				join(",", @$taxids).")";
		}
	}

	#	gp types
	my $types = $filters->{gptype};
	if ($types) {
		if (!ref($types)) {
			$types = [$types];
		}

		if (@$types) {
			push @$tables, "term as gptype";
			push @$where, "gene_product.type_id=gptype.id";
			push @$where, "gptype.name IN (".
				join(",", map { sql_quote($_) } @$types).")";
		}
	}

	#	evidence codes
	my $evcodes = $filters->{evcode};
	if ($evcodes) {
		if (!ref($evcodes)) {
			$evcodes = [$evcodes];
		}

		if (@$evcodes) {
			if (! grep { $_ eq 'association' } @$tables)
			{	push @$tables, "association", "evidence";
				push @$where, "gene_product.id=association.gene_product_id", "evidence.association_id = association.id";
			}
			elsif (! grep { $_ eq 'evidence' } @$tables)
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

	# ontology
	my $onts =  $filters->{ont};
	if ($onts) {
		if (!ref($onts)) {
			$onts = [$onts];
		}

		if (@$onts) {
			if (! grep { $_ eq 'association' } @$tables)
			{	push @$tables, "association", "term";
				push @$where, "gene_product.id=association.gene_product_id", "association.term_id=term.id";
			}
			elsif (! grep { $_ eq 'term' } @$tables)
			{	push @$tables, "term";
				push @$where, "association.term_id=term.id";
			}

			push @$where, "term.term_type IN ("
				.join(",", map { sql_quote($_) } @$onts).")";
		}
	}

	#	assigned by
	my $assby = $filters->{assby};
	if ($assby) {
		if (!ref($assby)) {
			$assby = [$assby];
		}

		if (@$assby) {
			if (! grep { $_ eq 'association' } @$tables)
			{	push @$tables, "association";
				push @$where, "gene_product.id=association.gene_product_id";
			}
			push @$tables, "db";
			push @$where, "association.source_db_id=db.id";

			push @$where, "db.name IN ("
				.join(",", map { sql_quote($_) } @$assby).")";
		}
	}

	#	association negation
	my $negation = $filters->{is_not};
	if ($negation)
	{	if (! grep { $_ eq 'association' } @$tables)
		{	push @$tables, "association";
			push @$where, "gene_product.id=association.gene_product_id";
		}
		push @$where, "association.is_not = 1";
	}


	#	association qualifiers
	my $quals = $filters->{qual};
	if ($quals) {
		if (!ref($quals)) {
			$quals = [$quals];
		}

		if (@$quals) {
			if (! grep { $_ eq 'association' } @$tables)
			{	push @$tables, "association";
				push @$where, "gene_product.id=association.gene_product_id";
			}
			push @$tables, "association_qualifier, term AS qualifier";
			push @$where, "association.id=association_qualifier.association_id", "association_qualifier.term_id=qualifier.id";

			push @$where, "qualifier.name IN ("
				.join(",", map { sql_quote($_) } @$quals).")";
		}
	}
	
	#	assocdate
	my $dates = $filters->{assocdate};
	if ($dates) {
		if (!ref($dates)) {
			$dates = [$dates];
		}
		if (@$dates) {
			if (! grep { $_ eq 'association' } @$tables)
			{	push @$tables, "association";
				push @$where, "gene_product.id=association.gene_product_id";
			}

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
	return;
}

=head2 get_fasta

  Arguments - list of GPs
  returns   - FASTA seqs

  Adds the filter settings to an SQL query

=cut

sub get_fasta {
	my $session = shift;
	my $gps = shift;
	my $apph = $session->apph;
	my $dbh = $apph->dbh;

	my $gp_l = get_gp_details($session, { gpxref => $gps}, { seq => 1 });

	if ($gp_l)
	{	my $gp_h;
		my @no_seqs;
		map { 
			if ($_->seq_list)
			{	$gp_h->{$_->xref->xref_dbname .":". $_->xref->xref_key} = $_;
			}
			else
			{	push @no_seqs,$_->xref->xref_dbname .":". $_->xref->xref_key;
			}
		} @$gp_l;
		
		if (@no_seqs)
		{	$session->add_message('warning', ['no_seq', @no_seqs]);
		}

=cut
	
	#	check for / remove dups
	my %hash;
	@hash{@$gps} = {};
	$gps = [ keys %hash ];



	my $tables = ["gene_product", "dbxref"];
	my $where = ["gene_product.dbxref_id = dbxref.id"];
	push @$where, "(".join(" OR ", 
		map {
				if ($_ =~ /(.*?):(.*)/) {
					"(dbxref.xref_dbname = ".sql_quote($1).
					" AND dbxref.xref_key = ".sql_quote($2).")";
				}
				else {
					"dbxref.xref_key = ".sql_quote($_);
				}
			} @{$gps}). ")";

	my $sql = "SELECT DISTINCT gene_product.id, gene_product.symbol, gene_product.full_name, gene_product.species_id, dbxref.xref_key AS acc, dbxref.xref_dbname AS speciesdb from " . join(", ", @$tables) . " WHERE " . join(" AND ", @$where);

	print STDERR "SQL: $sql\n";

	if ($sql =~ /\(\)/)
	{	print STDERR "Problem with the SQL!\n$sql\n";
		return undef;
	}

	my $sth = $dbh->prepare($sql);
	$sth->execute();

	my %gp_h = ();
	
	while (my $d = $sth->fetchrow_hashref) {
	#	print STDERR "$d:\n".Dumper($d)."\n";
		$d->{full_name} = $d->{symbol} if !$d->{full_name};
		$gp_h{$d->{id}} = $apph->create_gene_product_obj($d);
		$gp_h{$d->{id}}{species_id} = $d->{species_id};
	#	$gp_h{$d->{id}}{type_id} = $d->{type_id} if $d->{type_id};
	}

	if (keys %gp_h) {
		$apph->_get_product_species([values %gp_h]);
		map { $_->_seqs_obtained(1) } values %gp_h;

		my $cols = 'gene_product_seq.gene_product_id, seq.*';
		my $hl = select_hashlist
			($apph->dbh,
			 ["gene_product_seq", "seq"],
			 ["seq.id = seq_id", "gene_product_id in (".join(',', keys %gp_h).")"],
			 [$cols]
			);
	
		my (@seqs, @byid, @has_seq);
		foreach my $h (@$hl) {
			my $seq = $apph->create_seq_obj($h);
			my $p = $gp_h{$h->{gene_product_id}};
			$p->add_seq($seq);
			push @seqs, $seq;
			$byid[$seq->id] = $seq;
		}
	
		if (@byid) {
			$hl = select_hashlist($apph->dbh,
							["dbxref", "seq_dbxref"],
							["dbxref.id = dbxref_id",
							 "seq_id in (".join(", ", map {$_->id} @seqs).")"],
							 "dbxref.*, seq_id");
			map {
				$byid[$_->{seq_id}]->add_xref(GO::Model::Xref->new($_));
				} @$hl;
		}
		
		foreach (keys %gp_h)
		{	if (!$gp_h{$_}->seq)
			{	print STDERR "gp without seq:\n".Dumper($_)."\n";
				delete $gp_h{$_};
			}
		}

		#	check we have all the GPs we were looking for
		if (scalar (keys %gp_h) != scalar @$gps)
		{	print STDERR "scalar gps = ".scalar @$gps."; scalar keys gp_h = ".scalar (keys %gp_h)."\n";
			my @missing;
			my %hash;
			map { $hash{$_->xref->xref_dbname . ":" . $_->xref->xref_key} = 1 } values %gp_h;
			
			foreach (@$gps)
			{	#if (!grep { $xref eq $_->speciesdb.":".$_->acc } values %gp_h)
				if (!$hash{$_})
				{	push @missing, $_;
					print STDERR "Lost $_\n";
				}
			}
			$session->add_message('warning', ['gp_not_found', sort @missing]);
		}

=cut
		return [ 
			map { $gp_h->{ (split("\0", $_))[-1] } }
			sort# { $a->[1] cmp $b->[1] } 
			map { join("\0",
					$gp_h->{$_}->xref->xref_dbname.":".$gp_h->{$_}->xref->xref_key,
					$_) } keys %$gp_h ];
	}
	else
	{	$session->suicide_message(['gp_not_found', @$gps]);
	}
}


=head2 _get_term_count_for_gps

  Arguments - session, GP list, boolean for whether or not to use filters
  Returns   - list of GP ID and term count

=cut

sub _get_term_count_for_gps {
	my $session = shift;
	my $gps = shift;
	my $use_filters = shift || 1;

	my $apph = $session->apph;
	my $dbh = $apph->dbh;

	my $tables = ["association"];
	my $where = ["association.gene_product_id IN (".join(",", map{ $_->id } @$gps).")"];

	if ($use_filters)
	{	my $filters = $apph->filters || {};
		if ($filters && keys %{$filters})
		{	push @$tables, 'gene_product';
			push @$where, 'gene_product.id = association.gene_product_id';
			#	set the filters
			set_filters($filters, $tables, $where);
		}
	}

	my $sql = "SELECT association.gene_product_id, COUNT(DISTINCT association.term_id) FROM "
				.join(", ", @$tables) . " WHERE "
				.join(" AND ", @$where)
				. " GROUP BY association.gene_product_id";

	print STDERR "SQL: $sql\n";
	
	my $results = $dbh->selectall_arrayref($sql);
	
	if (!$results)
	{	return;
	}

	#	pop the results into a list and return 'em
	return [ map { ($_->[0], $_->[1]) } @$results ];
}

=head2 _get_gp_count_for_terms

  Arguments - session, term list, whether to get direct or all assocs,
              boolean for whether or not to use filters
  Returns   - list of GP ID and term count

=cut

sub _get_gp_count_for_terms {
	my $session = shift;
	my $terms = shift;
	my $all_ass = shift;
	my $use_filters = shift || 1;
	
	my $apph = $session->apph;
	my $dbh = $apph->dbh;
	
	if ($all_ass)
	{	if ($session->get_gp_count_ok || !$use_filters)
		{	print STDERR "Getting the deep product count!\n";
			
			my $terms_by_id;
			$terms_by_id->[$_->id] = $_ foreach (@$terms);
			my $countl = $apph->get_deep_product_count({ per_term=>1, terms => $terms });
			foreach (@$countl) 
			{	$terms_by_id->[$_->{term_id}]->n_deep_products($_->{"c"}) if ($terms_by_id->[$_->{term_id}]);
			}
		}
		else
		{	my $filters = $apph->filters || {};
			if ($filters->{ont})
			{	delete $filters->{ont};
			}
			#	find the descendents of each term in the list
			#	this query will also return the term itself
			my $results = $dbh->selectall_arrayref(
			"SELECT DISTINCT term1_id, term2_id FROM graph_path WHERE term1_id IN ("
			.join(", ", map { $_->id } @$terms).")");

			#	gather together the term and its children
			my $term_and_c_list;
			foreach (@$results)
			{	push @{$term_and_c_list->{$_->[0]}}, $_->[1];
			}
			
			my $tables = ["association"];
			
			#	now count the number of distinct GPs annot'd to each term set
			my @result_l;
			foreach (keys %$term_and_c_list)
			{	my $where = ["association.term_id IN (".join(",", @{$term_and_c_list->{$_}}).")"];
				set_filters($filters, $tables, $where);

				my $sql = "SELECT COUNT(DISTINCT association.gene_product_id) FROM "
				.join(", ", @$tables) . " WHERE "
				.join(" AND ", @$where);

				print STDERR "SQL: $sql\n";
				my $results = $dbh->selectall_arrayref($sql);
				$result_l[$_] = $results->[0][0] if ($results);
			}
			#	add the numbers to the results
			foreach (@$terms)
			{	if ($result_l[$_->id])
				{	$_->n_deep_products($result_l[$_->id]);
				}
				else
				{	$_->n_deep_products(0);
				}
			}
		}
	}
	else
	{	my $tables = ["association"];
		my $where = ["association.term_id IN (".join(",", map{ $_->id } @$terms).")"];

		if ($use_filters)
		{	my $filters = $apph->filters || {};
			if ($filters->{ont})
			{	delete $filters->{ont};
			}
			set_filters($filters, $tables, $where);
		}
		my $sql = "SELECT association.term_id, COUNT(DISTINCT association.gene_product_id) FROM "
				.join(", ", @$tables) . " WHERE "
				.join(" AND ", @$where)
				. " GROUP BY association.term_id";

		print STDERR "SQL: $sql\n";
	
		my $results = $dbh->selectall_arrayref($sql);
	
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
}

#	currently unused
sub get_graph_path_pairs {
	my $session = shift;
	my $termset1 = shift;
	my $termset2 = shift;
	my $dbh = $session->apph->dbh;

	my $results = $dbh->selectall_arrayref(
		"SELECT DISTINCT term1_id, term2_id FROM graph_path WHERE term1_id IN ("
		.join(", ", map { $_->id } @$termset1)
		.") AND term2_id IN ("
		.join(", ", map { $_->id } @$termset2)
		.") AND term1_id <> term2_id");

	return $results || $session->suicide_message('No paths could be found for the terms you specified.');
}

1;



