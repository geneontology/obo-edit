package GO::CGI::GOSlimmer;

use Carp;
use DBI;
use GO::AppHandle;
#use GO::Utils qw(rearrange);
use GO::SqlWrapper qw(select_hashlist);
use GO::CGI::Search;
use FileHandle;

use Data::Dumper;
$Data::Dumper::Indent = 1;
#use Time::HiRes qw(gettimeofday);

use Exporter;
@ISA = ('Exporter');
@EXPORT_OK = qw(get_mapping goslimmer);

use strict;

our $user_ga_file; #	whether or not there is a user GA file

=head2 get_mapping

	Arguments - session
	Returns   - a mapping of each term to direct and all GS ancestors

	Maps all the terms in the ontology to the GO slim terms.

=cut

sub get_mapping {
	my $session = shift;
	my $apph = $session->apph;

	my $gsterms = get_slim_terms($session);
	my $roots = $apph->get_ontology_root_terms(-template => { acc => 1 } );

	return slimdown($session, $gsterms, $roots);
}

sub goslimmer {
	my $session = shift;
	my $apph = $session->apph;
	my $dbh = $apph->dbh;
	
	my $gsterms = get_slim_terms($session);

	my $roots = $apph->get_ontology_root_terms(-template => { acc => 1 } );
	
	my $assoc_h = get_assoc_data($session);
	
	#	get the list of terms involved in the association data
	#	$assoc_h->{term_acc_to_id}
	#	$assoc_h->{term_objects}
	my $allterms;
	if ($assoc_h->{term_objects})
	{	$allterms = { map { $_->acc => $_->id } @{$assoc_h->{term_objects}} };
	}
	elsif ($assoc_h->{term_acc_to_id})
	{	$allterms = $assoc_h->{term_acc_to_id};
	}

	#	do the slimming
	my $slimmed = slimdown($session, $gsterms, $roots, $allterms);

	if ($session->ses_type eq 'goslim_minimap')
	{	#	if we are getting the minimap, we only need the data for the terms in allterms
		foreach (keys %$slimmed)
		{	delete $slimmed->{$_} unless $allterms->{$_};
		}
		return $slimmed;
	}

	my $data = $slimmed->[0];
	if ($session->get_param('bucket'))
	{	#	put the bucket terms on to the GO slim term array
		if ($slimmed->[1])
		{	push @$gsterms, @{$slimmed->[1]};
		}
	}
	
	
	#	based on our session type, perform further exciting data transformations
	#	right now our data is in the form
	#	data->{ acc }
	#		direct => [ list of direct GS parents ]
	#		all => [ list of all GS parents ]
	#		bucket => [ list of bucket terms ] (if applicable)
	#	we need to remap the gp data we've got
	#	gp data is in the form
	#	$assoc_h->{assocs_by_acc}{ acc }{ gpxref }

#	assoc => 'new gene association file, containing the most pertinent GO slim accessions',
#	count => 'distinct gene product counts for each slim term',
#	amigo => 'view associations in AmiGO view',
#	download => 'downloadable file in plain text (tab-delimited) format',


	#	print STDERR "data: ".Dumper($data)."\n";
	#	print STDERR "assoc_h: ".Dumper($assoc_h)."\n";

	my $new_assoc_h;
	my %counts;
	
	my $c = 0;
	if ($session->ses_type eq 'goslim_count')
	{	$c = 1;
	}

	foreach my $acc (keys %{$assoc_h->{assocs_by_acc}})
	{	if (!$data->{$acc} && !grep { $acc eq $_->acc }@$roots)
		{	#	something has gone horribly wrong here! Uh-oh...
			print STDERR "No data found for $acc: whazzup wit dat?\n";
			next;
		}
		foreach my $d (@{$data->{$acc}{direct}})
		{	foreach (keys %{$assoc_h->{assocs_by_acc}{$acc}})
			{	if ($c)
				{	if ($user_ga_file)
					{	$new_assoc_h->{$d}{$_} += scalar @{$assoc_h->{assocs_by_acc}{$acc}{$_}};
					}
					else
					{	$new_assoc_h->{$d}{$_} += $assoc_h->{assocs_by_acc}{$acc}{$_};
					}
				}
				else
				{	#	in assoc mode, remap the associations to the new terms
					push @{$new_assoc_h->{$d}{$_}}, @{$assoc_h->{assocs_by_acc}{$acc}{$_}};
				}
			}
		}
	}

	if ($c)
	{	my %gsterms_by_acc;
		map { $gsterms_by_acc{$_->acc} = $_ } @$gsterms;
		
#		print STDERR "gsterms: ".Dumper($gsterms)."\n";
		
		my @results =
			map {
				my $acc = (split("\0", $_))[-1];
				{	term => $gsterms_by_acc{$acc},
					assocs => ($new_assoc_h->{$acc} || {} )
				}
			}
			sort
			map {
				(my $acc = $_->acc) =~ s/(.*?):.*/$1/;
				join("\0", 
				$_->is_obsolete,
				$_->namespace,
				lc $acc,
				lc($_->name),
				$_->acc)
			} @$gsterms;

#		foreach (@results)
#		{	print STDERR $_->{term}->acc." ; ".$_->{term}->name."\n";
#		}

		return \@results;
	}
	elsif ($session->ses_type eq 'goslim_assoc')
	{	return { fmt => 'ga', data => $new_assoc_h } if $user_ga_file;
	
		my %gsterms_by_acc;
		map { $gsterms_by_acc{$_->acc} = $_ } @$gsterms;

		print STDERR "new_assoc_h: ".Dumper($new_assoc_h)."\n";
		print STDERR "assoc_h: ".Dumper($assoc_h)."\n";
		my %ass_id_h;
		#	get the association data for each of those association IDs
		foreach my $acc (keys %$new_assoc_h)
		{	foreach my $gpid (keys %{$new_assoc_h->{$acc}})
			{	foreach (@{$new_assoc_h->{$acc}{$gpid}})
				{	push @{$ass_id_h{$_}}, $acc;
				}
			}
		}

		my $format = 'ga';
		#	we have all the unique ass ids. Now get the association data.
		my $tables =
		[	"association",
			"evidence",
			"dbxref evdbxref"];
		my $where =
		[	"evidence.association_id = association.id",
			"evidence.dbxref_id = evdbxref.id"];
		my $cols =
		[	"association.id",
		#	"association.term_id",
			"association.is_not",
			"evidence.id AS ev_id",
			"evidence.code",
			"evidence.seq_acc",
			"evdbxref.xref_key AS evdbxref_acc",
			"evdbxref.xref_dbname AS evdbxref_dbname"];
		if (!$format || $format ne 'rdfxml')
		{	push @$cols, 
				"association.assocdate",
				"association.source_db_id";
		}
		if (!$format)
		{	push @$cols, "association.gene_product_id",
		}
		else
		{	push @$cols, "association.gene_product_id AS gp_id",
		}
	
		push @$where,
			"association.id IN (".join(",", keys %ass_id_h).")";
	
#		my $filters = $apph->filters || {};
	
		#	set the evidence code filters
#		my $evcodes = $filters->{evcode};
#		my $quals = $filters->{qual};
#		my $assbys = $filters->{assby};
#		if ($evcodes || $quals || $assbys)
#		{	set_filters( { evcode => $evcodes, qual => $quals, assby => $assbys }, $tables, $where);
#		}
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
		{	map { $gp_l->[$_->id] = $_ } @{$assoc_h->{gp_list}};
	#		map { $term_l->[$_->id] = $_ } @$terms;
		}
		
		foreach my $h (@$hl) {
			my $assoc = $assoc_lookup[$h->{id}];
			if (!$assoc) {
				$assoc = $apph->create_association_obj($h);
				$assoc_lookup[$assoc->id] = $assoc;
				if (!$gp_l->[$h->{gp_id}]) {
					print STDERR "Could not find gene product for assoc ".$assoc->id."!\n"
					.Dumper($assoc)."\n";
				}
				else
				{	$assoc->gene_product($gp_l->[$h->{gp_id}]);
				}
				
				#	look up the assoc id in the hash we made earlier
				#	to find out which GS terms it's assoc'd with
				if (!$ass_id_h{$assoc->id})
				{	print STDERR "Could not find GS info for ".$assoc->id."!\n";
				}
				else
				{	foreach (@{$ass_id_h{$assoc->id}})
					{	if ($gsterms_by_acc{$_})
						{	$gsterms_by_acc{$_}->add_association($assoc);
						}
						else
						{	"Could not find term $_ for assoc ".$assoc->id."!\n";
						}
					}
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
			return undef;
		}
		#	get extra association info
		$apph->_get_evidence_seq_xrefs(\@assocs);
		$apph->get_qualifiers(\@assocs);
		$apph->get_assigned_by(\@assocs) if (!$format || $format ne 'rdfxml');
	
		if ($format)
		{	my $graph = $apph->create_graph_obj;
			foreach (@$gsterms)
			{	$graph->add_term($_) if ($_->{association_list});
			}
			return { fmt => 'graph', data => $graph };
		}
	}
}

=head2 get_slim_terms

	Arguments - session
	Returns   - array of term objects (GO slim terms)

	Creates the GO slim terms, either by taking them from the
	'terms' or 'termfile' form input parameters, or by using a
	pre-defined subset from the database

=cut

sub get_slim_terms {
	my $session = shift;
	my $apph = $session->apph;

	#	get the GO slim terms by whatever means, fair or foul
	#	possible sources: box on query form ('terms')
	#	file upload on query form ('termfile')
	#	an existing GO slim ('goslim')
	#	a set of terms chosen elsewhere in AmiGO ('term_basket')
	my $tmpl = { acc => 1 };
	my $term_l = [];

	if ($session->get_param('terms') || $session->get_param('termfile'))
	{	my @list;
		#	term list from the input box on the GO slimmer page
		if ($session->get_param('terms'))
		{	@list = split('\n', $session->get_param_list('terms'));
		}
		#	uploaded file from the GO slimmer page
		else
		{	#turn the file into a list of terms
			my $file = $session->get_cgi->param('termfile');
			my $first_line = <$file>;
			#	check whether it's a list of terms or an ontology file
			#	obo files should start with the line 'format-version: 1.x'
			if ($first_line =~ (/format-version/))
			{	# parse GO-slim and get the slim graph

			#	use GO::Parser;
			#	my $parser = GO::Parser->new({handler=>'obj'});
			#	printf STDERR "Parsing slimfile: $file\n" if $verbose;
			#	$parser->parse($file);
			#	$gslim = $parser->handler->graph;
			#	@term_l = map { $_->acc } @{$gslim->get_all_terms};
				my $line;
				while (defined($line = <$file>)) {
					if ($line =~ /^id: (.*)/) # id line
					{	push @list, $1;
					}
				}
			}
			else
			{	my $line;
				while (defined($line = <$file>)) {
					if ($line =~ /\S/ && $line !~ /#/) # comment line
					{	$line =~ s/\s//g;
						push @list, $line;
					}
				}
			}
			print STDERR "list: ".Dumper(\@list)."\n";
		}

		$term_l = get_valid_terms($session, \@list);
	} 
	#	one of the goslims in the database
	elsif ($session->get_param('goslim'))
	{	$term_l = $apph->get_terms({subset=>$session->get_param('goslim')}, $tmpl);
	}
	#	terms selected elsewhere in AmiGO
	elsif ($session->get_param('term_basket'))
	{	my $accs = $session->get_param_list('term_basket');
		$term_l = $apph->get_terms({accs => $accs}, $tmpl) if ($accs);
	}

	#	remove any obsoletes
	my @gsterms = grep { $_->is_obsolete == 0 } @$term_l;
	
	if (!@gsterms)
	{	#	no terms found. Return fatal
		$session->suicide_message('no_termset');
	}
	return \@gsterms;
}

=head2 slimdown

	Arguments - session, arrays of slim terms, ontology roots,
	            term acc to term ID mapping for all terms to be slimmed
	Returns   - a mapping of each term to direct and all GS ancestors,
	            and, if bucket mode is on and the output format requires it,
	            an array of term objects for the bucket terms.

=cut

sub slimdown {
	my $session = shift;
	my $gsterms = shift; #		array of slim terms (Term objects)
	my $roots = shift;   #		ontology root terms (Term objects)
	my $allterms = shift; #		term acc to term ID mapping for all terms
	my $verbose = 1;

	my $bucket = $session->get_param('bucket');
	
	print STDERR "bucket = ".Dumper($bucket)."\n" if $verbose;
	
	my $apph = $session->apph;
	my $dbh = $apph->dbh;

	my %gsterms_by_id;
	@gsterms_by_id{ map { $_->id } @$gsterms } = @$gsterms;

#	add the ontology root nodes, just to make sure we have them!
#	push @$roots, $apph->get_root_term(-template => $tmpl);
	foreach my $r (@$roots)
	{	if (!grep { $_->acc eq $r->acc } @$gsterms)
		{	push @$gsterms, $r;
			$gsterms_by_id{$r->id} = $r;
			print STDERR "Added root term ".$r->name." ; ".$r->acc." to the GO slim\n" if $verbose;
		}
	}

	my %id_to_acc;		#id to acc mapping
	my %acc_to_id;		#acc to id mapping

	if ($allterms)
	{	%acc_to_id = %$allterms;
	}

	#	add the info from @$gsterms
	foreach (@$gsterms)
	{	$acc_to_id{$_->acc} = $_->id;
	}

	#	get a list of all the terms we'll be looking at, term IDs and accs
	#	(quicker to do this than to do joins with the term table)

	print STDERR "getting the graph_paths between GS terms and all terms\n" if $verbose;

	my $sql = "SELECT DISTINCT term2_id, term1_id FROM graph_path WHERE distance <> 0 AND term1_id IN ("
		.join(",", map { $_->id } @$gsterms)
		.")";
	#	no allterms means get the whole lot
	if ($allterms)
	{	#	we need the relationships between the GO slim terms too,
		#	so use the values of acc_to_id
		$sql .= " AND term2_id IN (".join(",", (values %acc_to_id) ).")";
	}
	print STDERR "SQL: $sql\n" if $verbose;
	my $results = $dbh->selectall_arrayref($sql);

	my %all_gs_ancs;	#key is the term id, values are ids of all GS ancestors
	foreach (@$results)
	{	push @{$all_gs_ancs{$_->[0]}}, $_->[1];
	}

	#	if allterms was undefined, we need to get the id to acc mapping
	#	get the data from the db (quicker this way than doing joins with
	#	the term table)
	if (!$allterms)
	{	print STDERR "Getting the acc and id info for ".(scalar keys %all_gs_ancs)." terms.\n";
		my $accs_ids = $dbh->selectall_arrayref("SELECT acc, id FROM term WHERE id IN (".join(",", keys %all_gs_ancs ).")");

		foreach (@$accs_ids)
		{	$acc_to_id{$_->[0]} = $_->[1];
		}
	}
	%id_to_acc = reverse %acc_to_id;

	print STDERR "Got the id and acc info for ".(scalar keys %id_to_acc)." terms.\n" if $verbose;

	#	now run that jolly old slimming code
	print STDERR "Slimming down the terms...\n" if $verbose;
	my %ref;				#	store relationships we've looked at
	my %par_child;		#	store { ancestor }{ descendent } rels
	my %child_par;		#	store { desc }{ anc } relationships
	my $data;			#	results

	#	go through the GS list and work out the parentage
	foreach my $gs (@$gsterms)
	{	if ($all_gs_ancs{$gs->id})
		{	#	if we have a list with this term plus any of its ancestors,
			#	we know that this term is the most specific
			$ref{ join("_", sort (@{$all_gs_ancs{$gs->id}}, $gs->id) ) } = { direct => [ $gs->acc ],
			all => [ sort map { $id_to_acc{$_} } (@{$all_gs_ancs{$gs->id}}, $gs->id) ] };

			map { 
				$par_child{$_}{$gs->id} = 1;
				$child_par{$gs->id}{$_} = 1;
				} @{$all_gs_ancs{$gs->id}};
		}
		else
		{	#	it's a root node, or something has gone horribly wrong
			print STDERR "No GS ancestors found for ".$gs->acc.", ".$gs->id."\n" if $verbose;
		}
	}
	
	my $count = 0;
	##	this is the real meaty chunks of goslimmer
	foreach my $term (keys %all_gs_ancs)
	{	my $acc = $id_to_acc{$term};

		if ($gsterms_by_id{$term})
		{	#	this is a goslim term!
			$data->{$acc}{direct} = [ $acc ]; # the direct GS ancestor is itself
			$data->{$acc}{all} = [ sort map { $id_to_acc{$_} } (@{$all_gs_ancs{$term}}, $term) ];
			next;
		}

		$data->{$acc}{all} = [ sort map { $id_to_acc{$_} } @{$all_gs_ancs{$term}} ];
	#	if (scalar @{$all_gs_ancs{$term}} == 1)
	#	{	#	unlikely but...
	#		$data->{$id_to_acc{$term}}{direct} = $data->{$acc}{all};
	#		if ($bucket && $par_child{$a})
	#		{	push @{$data->{$acc}{bucket}}, $_;
	#		}
	#		next;
	#	}

		my $str = join "_", sort @{$all_gs_ancs{$term}};
		if ($ref{$str})
		{	#	we've already seen this combination of GS terms before
			$data->{$acc} = $ref{$str};
			$count++;
			next;
		}

		#	we did our best but we're going to have to work this one out the hard way
		my @list = @{$all_gs_ancs{$term}};
		
		SLIMDOWN_LOOP:
		while (@list)
		{	my $a = pop @list;
			my @list2 = ();
			while (@list)
			{	my $b = pop @list;
				if ($par_child{$a}{$b})
				{	#	a is an ancestor of b
					#	forget about a, go on to the next list item
					push @list, $b;
					push @list, @list2 if (@list2);
					next SLIMDOWN_LOOP;
				}
				elsif ($child_par{$a}{$b})
				{	#	b in an ancestor of a
					#	forget about b, look at the next in the list
					next;
				}
				else
				{	#	a and b aren't related
					#	keep b
					push @list2, $b;
					next;
				}
			}
			#	if a is still around, it must be a descendent of
			#	all the terms we've looked at, so it can go on our
			#	descendent list
			push @{$data->{$acc}{direct}}, $id_to_acc{$a};
			
			#	if we have a list2, transfer it back to @list
			push @list, @list2 if (@list2);
		}
		
		$data->{$acc}{direct} = [sort @{$data->{$acc}{direct}}];
		
		#	if bucket mode is on, have a look and see if our
		#	direct GS parents are leaf nodes or not
		#	any terms that have a non-leaf GS node as a direct parent
		#	will go into a bucket
		#	leaf terms don't appear as keys in $par_child
		if ($bucket)
		{	foreach (@{$data->{$acc}{direct}})
			{	if ($par_child{ $acc_to_id{$_} })
				{	push @{$data->{$acc}{bucket}}, $_;
				#	should we remove those terms from the direct list?
					$_ =~ s/.*?:/SLIM:/;
				}
			}
		}
		
		#	store the combination of terms in case we come across it again
		$ref{$str} = $data->{$acc};
	}
	
	if ($session->ses_type =~ /map$/)
	{	return $data;
	}
	
	print STDERR "$count terms used a referenced direct anc set\n" if $verbose;

	#	if buckets were used and the output mode requires it,
	#	create fake bucket objects
	#	bucket terms are those which appear as keys in par_child
	my $bucket_arr;
	foreach (keys %par_child)
	{	my $base_term = $gsterms_by_id{$_};
		my $hash;
		foreach my $p ('name', 'acc', 'term_type', 'is_obsolete')
		{	$hash->{$p} = $base_term->$p;
		}
		$hash->{name} = 'OTHER '.$hash->{name};
		$hash->{acc} =~ s/.*?:/SLIM:/;
		push @$bucket_arr, $apph->create_term_obj($hash);
	}

	return [$data, $bucket_arr];
}

sub get_assoc_data {
	my $session = shift;
	my $dbh = $session->apph->dbh;

	my $verbose = 1;
	my $gplist; #	list of gene products
	my $assoc_h; #	associations

	#	possible sources for our association data:
	#	uploaded association file or gene list ('gpfile')
	##	n.b. may only want data for a subset of these!!
	#	list of gpxrefs from query form ('gps')
	#	list of gpxrefs from elsewhere in AmiGO ('gp_basket')

	#	if it's an association file, we can get the data
	#	directly from that without worrying about the database
	if ($session->get_param('gpfile'))
	{	#this might be a gene association file or a list of dbxrefs
		my $assocfile = $session->get_cgi->param('gpfile');
		my $file_name = $session->get_param('gpfile');

		my $fh;
		my $is_assoc_file = 0;
		if ($file_name =~ /\.(Z|gz)$/) {
			print STDERR "Uncompressing and mapping $assocfile to slim\n" if $verbose;
			$fh = FileHandle->new("gzip -dc $assocfile|") || 
				$session->suicide_message("Cannot open assocfile $assocfile: $!");
		}
		else {
			printf STDERR "Mapping $assocfile to slim\n" if $verbose;
			$fh = $assocfile ||
				$session->suicide_message("Cannot open assocfile $assocfile: $!");
		}

		print STDERR "fh: ".Dumper($fh)."\n";

		if ($file_name =~ /\.(Z|gz)$/ || $file_name =~ /association/)
		{	$is_assoc_file = 1;
		}
		else
		{	my $first_line = <$fh>;
			if ($first_line =~ /\t.*?\t/)
			{	#	tab delimited data
				$is_assoc_file = 1;
			}
		}

		if ($is_assoc_file == 1)
		{	print STDERR "Assuming that the file is in GA format\n";
			#	it's a gene association file!
			#	parser from map2slim

			while(<$fh>) {
				next if /^\!/;
				chomp;
				next unless $_;
				my @cols = split('\t', $_);
				my $is_not = $cols[3];
				my $acc = $cols[4];
				my $gpxref = $cols[0].":".$cols[1];
			
				next if $is_not && $is_not =~ /^not$/i;	# skip NOT assocs
			
				if (!$acc || $gpxref !~ /\S:\S/) {
					printf STDERR "WARNING! NO ACCESSION: $_\n" if $verbose;
					next;
				} else {
					#	store indexed by acc then gpxref
				#	if ($session->ses_type eq 'goslim_count')
				#	{	$assoc_h->{assocs_by_acc}{$acc}{$gpxref}++;
				#	}
				#	else
				#	{
						push @{$assoc_h->{assocs_by_acc}{$acc}{$gpxref}}, $_;
				#	}
				}
			}
			close($fh) || $session->suicide_message("Problem reading association file $assocfile: $!");

	#		print STDERR "assoc_h: ".Dumper($assoc_h)."\n";
			if (!keys %{$assoc_h->{assocs_by_acc}})
			{	$session->suicide_message('no_assocset');
			}

			#	these are the terms mentioned in the assocfile
			#	get the terms from the database
			#	(tho we really only need IDs and accs)
			my ($terms, $alt_ids, $missing) = get_valid_terms($session, [keys %{$assoc_h->{assocs_by_acc}}]);

			$assoc_h->{term_objects} = $terms if $terms;
			
			if ($alt_ids)
			{	#	update the alt_ids to the current IDs
				foreach my $acc (keys %$alt_ids)
				{	if ($assoc_h->{assocs_by_acc}{$acc})
					{	foreach (keys %{$assoc_h->{assocs_by_acc}{$acc}})
					#	{#	if ($session->ses_type eq 'goslim_count')
						#	{	$assoc_h->{assocs_by_acc}{ $alt_ids->{$acc} }{$_} += $assoc_h->{assocs_by_acc}{$acc}{$_};
						#	}
						#	else
							{	push @{$assoc_h->{assocs_by_acc}{ $alt_ids->{$acc} }{$_}}, @{$assoc_h->{assocs_by_acc}{$acc}{$_}};
							}
					#	}
						delete $assoc_h->{assocs_by_acc}{$acc};
					}
				}
			}
			
			if (keys %$missing)
			{	#	these terms have disappeared - may as well delete the associations
				$session->add_message('warning', ["term_not_found", keys %$missing]);
				foreach (keys %$missing)
				{	delete $assoc_h->{assocs_by_acc}{$_};
				}
			}
			
			if (!keys %{$assoc_h->{assocs_by_acc}})
			{	$session->suicide_message('no_valid_assocs');
			}
			
	#		print STDERR "assoc_h now: ".Dumper($assoc_h)."\n";
			$user_ga_file = 1;
			return $assoc_h;
		}
		else
		{	print STDERR "Assuming file is a list of gpxrefs\n";
			#	file is a list of gp identifiers
			while(<$fh>) {
				chomp;
				$_ =~ s/\s{2,}/ /g;
				next unless $_;
				push @$gplist, $_;
			}
			close($fh) || $session->suicide_message("Problem reading assocfile $assocfile: $!");
		}
	}
	elsif ($session->get_param('gps'))
	{	$gplist = $session->get_param_list('gps');
	}
	elsif ($session->get_param('gp_basket'))
	{	$gplist = $session->get_param_list('gp_basket');
	}

	if (!$gplist)
	{	$session->suicide_message('no_gpset');
	}

	#	if it's from elsewhere, we don't know
	#	for now, assume that it is a list of gpxrefs

	if ($gplist)
	{	#	get the gp ids and the gpxrefs
		my %gpxref_to_id;
	
	#	if the gplist came from the gpbasket, we know it's gpxrefs
		if ($session->get_param('gp_basket'))
		{	#	we should really go directly to getting either
			#	the associations or the counts here
			
			my $sql = "SELECT DISTINCT gene_product.id, CONCAT(xref_dbname, ':', xref_key) FROM gene_product, dbxref WHERE gene_product.dbxref_id=dbxref.id AND (".
			join(" OR ",
				map {
					if ($_ =~ /(.+?):(.+)/) {
						"(dbxref.xref_dbname = ".sql_quote($1).
						" AND dbxref.xref_key = ".sql_quote($2).")";
					}
					else
					{	"dbxref.xref_key=".sql_quote($_);
					}
				} @$gplist)
			.")";
			my $results = $dbh->selectall_arrayref($sql);

			#	convert into hash
			foreach (@$results)
			{	$gpxref_to_id{$_->[1]} = $_->[0];
			}
		}
		else
		{	my $tmpl = {};
			if ($session->ses_type eq 'goslim_assoc')
			{	$tmpl = { species => 1, type => 1, synonym => 1 };
			}
			#	to do: search for the GPs using the search module
			#	set up the parameters for the search
			my $search = new GO::CGI::Search($session, { get_relevance => 0 });
			my $results = $search->get_results_list($session, 
				{	search_constraint => 'gp',
					search_fields => ['full_name', 'symbol', 'gpxref', 'product_synonym'],
					query => $gplist,
					exact_match => 1,
					template => $tmpl,
				});
			if ($results->{fatal}) #	fatal error
			{	if ($results->{fatal} eq 'no_valid_query')
				{	$session->suicide_message('no_valid_query');
				}
				elsif ($results->{fatal} eq 'no_search_results')
				{	$session->suicide_message('gps_not_in_db');
				}
			}
			if ($results->{lost})
			{	$session->add_message('warning', ['gp_not_found', @{$results->{lost}}]);
			}
			#	convert into hash
			
			print STDERR "search results: ".Dumper($results)."\n";
			
			foreach (@{$results->{found}})
			{	$gpxref_to_id{$_->xref->xref_dbname.":".$_->xref->xref_key} = $_->id;
				$assoc_h->{gp_list} = $results->{found};
			}
		}

		#	get the terms with which they are associated
		my $cols = 'gene_product.id, term.acc, term.id';
		my $group = '';
		if ($session->ses_type eq 'goslim_count')
		{	$cols .= ', COUNT(*)';
			$group = ' GROUP BY term.acc, gene_product.id';
		}
		else
		{	$cols .= ', association.id';
		}
		
		my $sql = "SELECT $cols FROM association, term, gene_product WHERE association.term_id = term.id AND association.is_not=0 AND association.gene_product_id = gene_product.id AND gene_product.id IN ("
		.join(",", values %gpxref_to_id)
		.")$group";
		print STDERR "SQL: $sql\n";

		my $gp_term_assocs = $dbh->selectall_arrayref($sql);
		
		if (!$gp_term_assocs || !@$gp_term_assocs)
		{	print STDERR "No pairs found. Aborting...\n";
			$session->suicide_message('no_assocs_gp');
		}
		
		if ($session->ses_type eq 'goslim_count')
		{	foreach (@$gp_term_assocs)
			{	$assoc_h->{assocs_by_acc}{$_->[1]}{$_->[0]} += $_->[3];
				$assoc_h->{term_acc_to_id}{$_->[1]} = $_->[2];
			}
		}
		else
		{	#	just add association IDs for now - fill in the details later
			foreach (@$gp_term_assocs)
			{	push @{$assoc_h->{assocs_by_acc}{$_->[1]}{$_->[0]}}, $_->[3];
				$assoc_h->{term_acc_to_id}{$_->[1]} = $_->[2];
			}
		}
		
		print STDERR "assoc_h: ".Dumper($assoc_h)."\n";
	}
	return $assoc_h;
}

sub get_valid_terms {
	my $session = shift;
	my $accs = shift;
	my $apph = $session->apph;

	my $alt_ids; #	accs which are now 2ndary IDs
	my $missing; #	accs which couldn't be found

	my $term_l = $apph->get_terms({ accs=>$accs }, { acc => 1 });
#	my $sql = "SELECT acc, id FROM term WHERE acc IN (".join(",", map { sql_quote($_) } @$accs).")";
#	print STDERR "SQL: $sql\n";
#	my $results = $dbh->selectall_arrayref($sql);

	#	check that we found all the terms we were looking for
	if (!$term_l || scalar @$term_l != scalar @$accs)
	{	#	we're missing some terms! Uh-oh...
		#	check which IDs are missing, and do a synonym search
		
		$missing->{$_} = 1 foreach (@$accs);

		if (@$term_l)
		{	delete $missing->{$_->acc} foreach (@$term_l);
		}

		my $syn_l = $apph->get_terms({ synonyms => [ keys %$missing ] }, { acc => 1, synonym_list => 1 });

		print STDERR "Alt_id search results: ".Dumper($syn_l);

#		my $sql = "SELECT acc, id, acc_synonym FROM term_synonym, term WHERE term_synonym.acc_synonym IN (".join(",", map { sql_quote($_) } keys %$missing).") AND term_synonym.term_id = term.id";
#		print STDERR "SQL: $sql\n";
#		my $syn_l = $dbh->selectall_arrayref($sql);

		#	we found some terms
		if (@$syn_l)
		{	push @$term_l, @$syn_l;

			#	check which of the IDs we found
			foreach my $s (@$syn_l)
			{	my $alts = $s->synonyms_by_type('alt_id');
				print STDERR "alt ids: ".Dumper($alts)."\n";
				if (@$alts)
				{	map
					{	if ($missing->{$_})
						{ delete $missing->{$_}; $alt_ids->{$_} = $s->acc }
					} @$alts;
				}
				last if (!keys %$missing);
			}

			#	check no accs are duplicated
			my %syn_accs;
			map { $syn_accs{$_->acc} = $_ } @$term_l;
			$term_l = [values %syn_accs];
		}
		if (keys %$missing)
		{	#	we found some terms but not all of them
			$session->add_message('warning', ["term_not_found", keys %$missing]);
		}
	}
	
	#	should we return data about which terms correspond with which alt_ids?
	
	return ($term_l, $alt_ids, $missing);
}

1;
