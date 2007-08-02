#!/usr/local/bin/perl
#
=head1 SYNOPSIS

package GO::CGI::Session;

=head2 Usage

Session manager for AmiGO (go.cgi)

=cut
package GO::CGI::Session;

use strict;

use GO::AppHandle;
use GO::Utils qw(rearrange);
use GO::CGI::NameMunger;
use FileHandle;
use DirHandle;
use FreezeThaw qw (freeze thaw);
use Data::Dumper;
$Data::Dumper::Indent = 1;

=head2 new

-args -out  Filehandle;
      -q    CGI,
      -data GO::Model::Graph

returns  GO::CGI::Session;

=cut

sub new {

	print STDERR "Starting a new session!\n";

	my $class = shift;
	my $self = {};
	bless $self, $class;
	my ($q, $out, $data, $no_update, $ses_type, $read_only) = rearrange([qw(q out data no_update ses_type read_only)], @_);

	$self->{'cgi'} = $q;
#	$self->{'out'} = $out;
#	$self->{'data'} = $data;
	$self->ses_type($ses_type);
	
	# $self->{'params'} is now what holds the data for performing
	# a query. The CGI values will no longer be directly used.
	# As such, $self->{'params'}->{n} is what will be returned by get_param
	# and get_param_hash.

	unless ($no_update) {
		$self->__clear_sessions;
		if ($q->param('session_id'))
		{	my $ses_id = $q->param('session_id');
			#	check the session ID is OK before we do anything else
			if ($self->is_valid_session_id($ses_id))
			{	$self->__load_session($ses_id);
			}
			else
			{	#	invalid session ID. Delete it!
				$q->delete('session_id');
			}
		}
		$self->__set_session_type;
		$self->__synchronize_session;
		$self->__save_session unless ($read_only);
	}
	else
	{	if (!$self->get_param('session_id'))
		{	#	create a session ID
			$self->__create_session_id;
		}
	}
	return $self;
}

sub __set_session_type {
	my $self = shift;

	if ($self->ses_type)
	{	return;
	}
	else
	{	print STDERR "\n\nNO SESSION TYPE SET!\n\n";
	}
=cut
	my $q = $self->get_cgi;
	my %params = $q->Vars;
	my $ses_type;
	my $queries;

	if ($params{'search_constraint'} && $params{'search_constraint'} eq 'terms')
	{	$params{'search_constraint'} = 'term';
		$q->param(-name=>'search_constraint', -values=>'term');
	}

	if ($params{'query'} || $params{'idfile'})
	{	$queries = 1;
	}
#	$self->__trim_filters;

#	print STDERR Dumper(\%params);
#	print STDERR "Params pre-munging: ".Dumper($self->{'params'})."\n\n";

	if (defined $queries)
	{	print STDERR "Queries is defined!\n";
		if ($params{'search_constraint'})
		{	if ($params{'search_constraint'} eq 'term')
			{	if ($params{'action'})
				{	if ($params{'action'} eq 'query' || $params{'action'} eq 'sort')
					{	$ses_type = 'term_search';
					}
					elsif ($params{'action'} eq 'minus_node'
							|| $params{'action'} eq 'plus_node'
							|| $params{'action'} eq 'replace_tree')
					{	#	this is a standard browse
						$ses_type = 'browse';
					}
					elsif ($params{'action'} eq 'dotty')
					{	if ($params{'graph_format'} && $params{'graph_format'} eq 'dottext')
						{	$ses_type = 'term_graph_dot';
						}
						else
						{	$ses_type = 'term_graph';
						}
					}
				}
				elsif ($params{'view'} && $params{'view'} eq 'details')
				{	$ses_type = 'term_details';
				}
				elsif ($params{'view'} && $params{'view'} eq 'assoc')
				{	$ses_type = 'term_assoc';
					if ($params{'format'})
					{	if ($params{'format'} eq 'xml')
						{	$ses_type .= '_rdf_xml';
						}
						elsif ($params{'format'} eq 'go_assoc')
						{	$ses_type .= '_ga';
						}
					}
				}
			}
			elsif ($params{'search_constraint'} eq 'gp')
			{	$ses_type = 'gp_search';
			}
		}
		elsif ($params{'action'} eq 'summary')
		{	$ses_type = 'pie_chart_summary';
		}
					
		#	this is to remap those query=GO:xxx which are coming from somewhere or other
		if (!$ses_type)
		{	if (!$params{'view'} && !$params{'action'} && !$params{'link'} && !$params{'format'})
			{	# do a term search as default as we have no real way of telling if a string is a gp name or a go term name
				$ses_type = 'term_search';
			}
		}
	}
	elsif ($params{'gp'})
	{	print STDERR "Could not find any queries.\n";
		if ($params{'format'} && $params{'format'} eq 'fasta')
		{	$ses_type = 'fasta';
		}
		elsif ($params{'view'} && $params{'view'} eq 'details')
		{	#	set the gp_comp bit later
			$ses_type = 'gp_details';
		}
		#	possible craziness: send to the GP search
		if (!$ses_type)
		{	my $values = $q->param('gp');
			$q->param(-name=>'query', -values=>$values);
			$ses_type = 'gp_search';
		}
	}
	elsif ($params{'action'})
	{	if ($params{'action'} eq 'replace_tree'
			|| $params{'action'} eq 'minus_node'
			|| $params{'action'} eq 'plus_node')
		{	#	this is a standard browse
			$ses_type = 'browse';
		}
		elsif ($params{'action'} eq 'dotty')
		{	#	graphical view
			#	without any known session, returns the 'all' node
			if ($params{'graph_format'} && $params{'graph_format'} eq 'dottext')
			{	$ses_type = 'term_graph_dot';
			}
			else
			{	$ses_type = 'term_graph';
			}
		}
		elsif ($params{'action'} eq 'get_job_by_id' || $params{'action'} eq 'blast')
		{	#	blast pages
			$self->__copy_fields(-fields=>['threshhold', 'maxhits', 'blast_filter']);
		}
	}
	elsif ($params{'format'})
	{	if ($params{'format'} eq 'go_ff')
		{	#	browse, in GO FF format
			#	will only return the root node without a session_id
			$ses_type = 'go_ff';
		}
		elsif ($params{'format'} eq 'xml')
		{	#	browse, in XML format
			#	will only return the root node without a session_id
			$ses_type = 'rdf_xml';
		}
	}
	elsif ($params{'link'} && $params{'link'} eq 'static')
	{	#	permalink
		$ses_type = 'browse';
	}
	elsif ($params{'advanced_query'})
	{	$ses_type = 'advanced_query';
	}

	if (!$ses_type)
	{	$ses_type = 'front';
	}

	print STDERR "ses_type: $ses_type\n";

	$self->ses_type($ses_type);
=cut
}

sub __synchronize_session {
	my $self = shift;

	my $release_date = $self->apph->instance_data->{release_name};
	$self->{release_date} = $release_date;

	my $q = $self->get_cgi;
	my %params = $q->Vars;
	my $ses_type = $self->ses_type;

	$self->{'params'}->{'current_query'} = {};

	print STDERR "Params: ".Dumper($self->{'params'})."\n\n";

	foreach my $param (keys %{$self->{'cgi'}->Vars}) {
		my @values = split ("\0", $self->{'cgi'}->Vars->{$param});
		$self->__set_param(-query=>'current_query',
		                   -field=>$param,
		                   -values=>\@values);
	}


	if (!defined($self->{'params'}->{'1'})) {
		$self->{'params'}->{'1'} = {};
	}

#	$self->__copy_fields(-fields=>[&_persisting_params]);
#	$self->__delete_fields(-query=>'current_query', -fields=>[&_persisting_params]);
	$self->__transfer_fields(-fields=>[&_persisting_params]);

	if ($ses_type ne 'front')
	{	$self->__trim_and_set_filters;
	}
	else
	{	$self->__transfer_fields([&_filter_fields]);
	}

	if (!defined($self->get_param('session_id', '1'))) {
	#	$self->__set_param(-query=>'1',
	#	                   -field=>'session_id',
	#	                   -values=>[$self->__create_session_id]);
		$self->__create_session_id;
	}
	
	if ($self->get_param('format'))
	{	$self->__set_param('current_query', 'page_size', ['all']);
	}

	print STDERR "Params post-munging: ".Dumper($self->{'params'})."\n\n";
}

sub graph_sync {
	my $self = shift;
	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, &_blast_params]);

#print STDERR "session: ".Dumper($self)."\n";

	my $action = $self->get_param('action', 'current_query');
	if ($self->ses_type eq 'graphviz' || $self->get_param('format'))
	{	$action = 'permalink';
	}
	if ($action) {
		if ($action eq 'reset-tree') {
#			print STDERR "session: ".Dumper($self)."\n";
			$self->__delete_fields(-query=>'1', -fields=>[&_tree_params]);
			$self->__set_param(-field=>'last_action', -values=>["reset the tree"]);
			$self->__transfer_fields(-fields=>[&_tree_params]);
		}

		if ($action eq 'minus_node') {
			my $term = $self->get_param('term');
			$self->__set_param(-field=>'action_node', -values=>[$term], -query=>'current_query');
			$self->__append_field(-field=>'term', -to_field=>'closed');
			$self->__remove_values(-field=>'term', -to_field=>'open_1');
		#keep open_0, otherwise, minus action show only root and root direct children
	#	 $self->__remove_values(-field=>'query', -to_field=>'open_0');
	
			$self->__set_param(-field=>'last_action', values=>["closed $term"]);
	
		} elsif ($action eq 'plus_node') {
			##	opened a node
			my $term = $self->get_param('term');
			$self->__set_param(-field=>'action_node', -values=>[$term], -query=>'current_query');
			$self->__remove_values(-field=>'term', -to_field=>'closed');
			if ($self->get_param('depth') && $self->get_param('depth') == 0) {
				$self->__append_field(-field=>'term', -to_field=>'open_0');
			} else {
				$self->__append_field(-field=>'term', -to_field=>'open_1');
			$self->__set_param(-field=>'last_action', -values=>["opened $term"]);
			}
		} elsif ($action eq 'permalink') {

			my @auto_replace = (
								'open_0',
								'open_1',
		#						'closed',
		#						'session_id',
		#					#	'search_constraint',
							);
			foreach my $replace (@auto_replace) {
				if ($self->get_param($replace)) {
					$self->__transfer_fields(-fields=>[$replace]);
				}
			}
		
		
		} elsif ($action eq 'set-tree') {
			$self->__set_param(-field=>'last_action', -values=>["set the tree"]);
			if ($self->get_param('term')) {
				print STDERR "found param term\n";
				$self->__append_field(-field=>'term', -to_field=>'open_0');
				$self->__move_field(-field => 'term',
				                    -to_field => 'open_0');
			}
		}
	}
	else {
	#	start afresh with a beautiful new tree
		$self->__delete_fields(-query=>'1', -fields=>[&_tree_params]);
		$self->__set_param(-field=>'last_action', -values=>["reset the tree"]);
		
		#	if there are any 'term' parameters, put them in the tree
	#	if ($self->get_param('term')) {
	#		$self->__append_field(-field=>'term', -to_field=>'open_0');
	#	}
	}

	my $root_node = $self->get_param('ROOT_NODE') || $self->apph->get_root_term(-template=>{acc=>1})->acc || 'all';

	if ($self->__has_value(-field=>'closed', -value=>$root_node)) {

#		$self->__append_param(-query=>'1',
#				-field=>'open_0',
#				-values=>[$root_node]);

	} else {
		$self->__append_param(-query=>'1',
									 -field=>'open_1',
									 -values=>[$root_node]);
	}
	$self->__delete_fields(-query=>'current_query', -fields=>['term']);

#	print STDERR "session post-graph-sync: ".Dumper($self->{params})."\n";

}

sub search_sync {
	my $self = shift;

	if ($self->get_cgi->param('action') && $self->get_cgi->param('action') eq 'new-search')
	{	$self->__delete_fields(-query=>'1', -fields=>[&_search_params]);
		#	delete cached results
		$self->__delete_cached_results;
	}
	
	$self->__copy_fields(-fields=>['search_constraint']);
	$self->__copy_or_delete(-fields=>['exact_match', 'termfields', 'gpfields', 'sppfields']);
#	$self->__delete_fields(-query=>'1', -fields=>['trim', 'xpand']);

	my %q_hash;
	if ($self->get_cgi->param('query'))
	{	my @queries = $self->get_cgi->param('query');
		foreach my $q (@queries) {
			foreach ( split /(\n|\0)/, $q )
			{	#	get rid of any tracts of whitespace
				s/(\t|\s{2,})/ /g;
				s/^\s*(\S+.*?)\s*$/$1/;
				$q_hash{$_}++ if (/\w/);
			}
		}
	}

	my $file = $self->get_cgi->param('idfile') || undef;
	if ($file)
	{	print STDERR "Found an idfile!\n";
		while (<$file>) {
			#	get rid of any tracts of whitespace
			s/(\t|\s{2,})/ /g;
			s/^\s*(\S+.*?)\s*$/$1/;
			$q_hash{$_}++ if /\w/;
		}
	}
	
	if (keys %q_hash)
	{	$self->set_param('current_query', 'query', [keys %q_hash]);
	}
	else
	{	$self->suicide_message('no_valid_query');
	}
}

sub adv_search_sync {
	my $self = shift;
	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, &_tree_params]);
}

sub term_sync {
	my $self = shift;
#	print STDERR "session before sync:\n".Dumper($self)."\n";

#	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, 'xpand', 'trim']);
	if ($self->get_cgi->param('closed')) {
		my $term = $self->get_cgi->param('closed');
		$self->__set_param(-field=>'last_action', values=>["closed $term"]);
		$self->__set_param(-field=>'action_node', -values=>[$term], -query=>'current_query');
		$self->__remove_values(-field=>'closed',-to_field=>'open_1');
		$self->__append_field(-field=>'closed', -to_field=>'closed');
		$self->__delete_fields(-query=>'current_query', -fields=>['closed']);

	} elsif ($self->get_cgi->param('open_1')) {
		my $term = $self->get_cgi->param('open_1');
		$self->__set_param(-field=>'last_action', values=>["opened $term"]);
		$self->__set_param(-field=>'action_node', -values=>[$term], -query=>'current_query');
		$self->__remove_values(-field=>'open_1',-to_field=>'closed');
		$self->__append_field(-field=>'open_1', -to_field=>'open_1');
		$self->__delete_fields(-query=>'current_query', -fields=>['open_1']);

#	} elsif ($self->get_cgi->param('trim')) {
#		$self->__remove_values(-field=>'trim',-to_field=>'xpand');
#		$self->__append_field(-field=>'trim', -to_field=>'trim');
#		$self->__delete_fields(-query=>'current_query', -fields=>['trim']);

#	} elsif ($self->get_cgi->param('xpand')) {
#		$self->__remove_values(-field=>'xpand',-to_field=>'trim');
#		$self->__append_field(-field=>'xpand', -to_field=>'xpand');
#		$self->__delete_fields(-query=>'current_query', -fields=>['xpand']);

	} else {
		#hmm when to clear up, change filter should not!! how to check that?
		$self->__set_param(-field=>'last_action', -values=>["set the tree"]);
		unless ($self->get_cgi->param('ch_filter')) {
			$self->__remove_values(-field=>'closed',-to_field=>'closed');
			$self->__delete_fields(-query=>'1',-fields=>['closed']);

			$self->__remove_values(-field=>'open_1',-to_field=>'open_1');
			$self->__delete_fields(-query=>'1',-fields=>['open_1']);
		}
	}
	$self->__delete_fields(-query=>'current_query', -fields=>['open_0']);
#	print STDERR "\nsession after sync:\n".Dumper($self)."\n";
}

sub term_assoc_sync {
	my $self = shift;
	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, &_tree_params]);
}

sub gp_sync {
	my $self = shift;
	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, &_tree_params]);
}

sub blast_sync {
	my $self = shift;
	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, &_tree_params]);
	$self->__copy_or_delete(-fields=>['use_filters']);
	$self->__transfer_fields(-fields=>[&_blast_params]);
}

sub slim_sync {
	my $self = shift;
	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, &_tree_params]);
	#	terms and gps
	#	remove blank lines
	#	get rid of spaces
	#	delete field if there's no content
	foreach my $f ('terms', 'gps')
	{	my @list = $self->get_cgi->param($f);
		if (@list)
		{	my @new_list;
			map {
				foreach (split(/(\n|\0)/, $_))
				{	$_ =~ s/\s{2,}/ /g;
					$_ =~ s/^\s|\s$//;
					if (/\w/)
					{	push @new_list, $_;
					}
				}
			} @list;
			$self->__delete_fields(-query=>'current_query', -fields=>[$f]);
			$self->__set_param(-query=>'current_query', -field=>$f, -values=>[@new_list]);
		}
	}
}

sub prefs_sync {
	my $self = shift;
	foreach my $f (&_filter_fields)
	{	my $v = $self->get_cgi->param('disable_'.$f.'_filter');
		if ($v)
		{	$self->set_param(-query=>'user', -field=>'disable_'.$f.'_filter', -values=>1);
			$self->__delete_fields(-fields=>[$_]);
		}
	}
	if ($self->get_cgi->param('tree_view') eq 'compact')
	{	$self->set_param(-query=>'1', -field=>'tree_view', -values=>['compact']);
		$self->__delete_fields(-fields=>['tree_view']);
	}
}

sub _filter_fields {
	return qw(evcode speciesdb taxid ont gptype qual assby);
}

sub _persisting_params {
#	return qw(evcode speciesdb taxid ont gptype qual assby reltype show_associations term_context termsort gpsort sppsort session_id);
	return qw(reltype show_associations term_context termsort gpsort sppsort session_id);
}

sub _search_params {
	return qw(termfields gpfields sppfields search_constraint exact_match query);
}

sub _tree_params {
	return qw(open_0 open_1 closed last_action);
}

sub _blast_params {
	return qw(threshold maxhits blast_filter);
}

=head2 __trim_and_set_filters

	Usage: $session->__trim_and_set_filters;

	Sets the apph filters, moves the filter params
	from 'current_query' to '1'.
	Checks that each field is enabled globally
	and that the user hasn't disabled it; also ensures that
	if 'all' is selected as an option, the filter isn't set
	The evcode filter has an additional check for 'aca', a deprecated
	code meaning 'all curator approved'.
=cut

sub __trim_and_set_filters {
	my $self = shift;
	my $apph = $self->apph;

	my %filters;
	my $action = $self->get_param('action');

	if ($action)
	{	if ($action eq 'reset-filters')
		{	$apph->filters(\%filters);
			$self->__delete_fields(-query=>1, -fields=>[&_filter_fields]);
			$self->__delete_fields(-query=>'current_query', -fields=>[&_filter_fields]);
		#	foreach (&_filter_fields)
		#	{	delete $self->{params}{1}{$_};
		#	}
			
			#	delete any cached results
			$self->__delete_cached_results;
			return;
		}
		elsif ($action eq 'filter')
		{	#	find out if this filter set is a subset of the old set
			#	if (!$self->{params}{1}{$f})
			#	{	#	no filters in the previous session
			#		$self->{use_cache} = 1;
			#	}
			#	else
			#	{	foreach my $f (&_filter_fields)
			#		{	if ($self->{params}{1}{$f} && !$self->get_cgi->
			#
			#
			#
			#	delete any cached results
			$self->__delete_cached_results;
		}
	}

	my $to_delete;
	foreach my $f qw(speciesdb ont taxid gptype assby qual)
	{	#	check the filter is enabled (both globally and by the user)
		if ($self->get_param('show_'.$f.'_filter') && !$self->get_user_params('disable_'.$f.'_filter'))
		{	my $p_list = $self->get_param_list($f);
			print STDERR "f: $f; list: ".join(", ", @{$p_list || []})."\n";
			if (!$p_list  # no values present
				|| grep { $_ eq 'all' } @$p_list)  # the list contains 'all'
			{	#	delete the param
				push @$to_delete, $f;
			}
			elsif ($self->get_param_list_size($f) == scalar @$p_list)
			#	slight cheat here: param list size is the same as
			#	number of selected params
			{	push @$to_delete, $f;
			}
			else
			{	print STDERR "p_list size: ".(scalar @$p_list)."; param list size: ".$self->get_param_list_size($f)."\n";
				
				$filters{$f} = $p_list;
				$self->set_param(-query=>'1', -field=>$f, -values=>$p_list);
				print STDERR "set $f filters.\n";
			}
		}
	}

	#	evcode
	my $f = 'evcode';
	if ($self->get_param('show_'.$f.'_filter') && !$self->get_user_params('disable_'.$f.'_filter'))
	{	my $p_list = $self->get_param_list($f);
	#	print STDERR "f: $f; list: ".join(", ", @{$p_list || []})."\n";
		if (!$p_list  # no values present
			|| grep { $_ eq 'all' } @$p_list)  # the list contains 'all'
		{	#	delete the param
			push @$to_delete, $f;
		} elsif (grep { $_ eq 'aca' } @$p_list) {
			#	parameter setting 'all curator approved' is on
			#	Ignore it unless IEAs are loaded
			if ($self->get_param('IEAS_LOADED') && !grep { $_ eq 'iea' } @$p_list) {
				#	IEAs are loaded, ACA is the search set, IEA not in the search set
				print STDERR "Param IEAS LOADED is ON\n";
				$filters{'evcode'} = ['!IEA'];
			} else {
				#	IEA *and* ACA are on ==> redundant parameter
				push @$to_delete, $f;
			}
		}
		elsif ($self->get_param_list_size($f) == scalar @$p_list)
		#	slight cheat here: param list size is the same as
		#	number of selected params
		{	push @$to_delete, $f;
		}
		else
		{	$filters{$f} = $p_list;
			$self->set_param(-query=>'1', -field=>$f, -values=>$p_list);
			print STDERR "set $f filters.\n";
		}
	}

	$self->__delete_fields(-query=>'1', -fields=>$to_delete) if $to_delete;
	$self->__delete_fields(-query=>'current_query', -fields=>[&_filter_fields]);
	$apph->filters(\%filters);

	print STDERR "Set apph filters!\n";

}

sub __remove_values {
	my $self = shift;
	my ($field, $to_field, $query, $to_query) =
		rearrange([qw(field to_field query to_query)], @_);
  
	if (!defined($to_field)) {
		$to_field = $field;
	}
	if (!defined($query)) {
		$query = 'current_query';
	}
	if (!defined($to_query)) {
		$to_query = '1';
	}
	my @new_list;
	my $values = $self->get_param_values(-query=>$to_query, -field=>$to_field);
	my $val = $self->get_param_values(-query=>$query, -field=>$field);
	my $is_in;
	foreach my $v (@$values) {
		$is_in = 0;
		foreach my $value (@$val) {
			if ($v eq $value) {
				$is_in = 1;
			}
		}
		if (!$is_in) {
			push @new_list, $v;
		}
	}
	$self->__set_param(-query=>$to_query,
	                   -field=>$to_field,
	                   -values=>\@new_list);
}

sub __delete_fields {
	my $self = shift;
	my ($query, $fields) = rearrange([qw(query fields)], @_);
  
	if (!defined($query)) {
		$query = 'current_query';
	}

	foreach my $field(@$fields) {
		delete $self->{params}{$query}{$field};
	#	$self->__set_param(-query=>$query,
	#	                   -field=>$field,
	#	                   -values=>undef);
	}
}

sub __copy_fields {
	my $self = shift;
	my ($fields) = rearrange([qw(fields)], @_);

	foreach my $field (@$fields) {
		if ($self->get_param($field, 'current_query')) {
			my $values = $self->get_param_values(-query=>'current_query',
			                                     -field=>$field);
			my @v = thaw freeze $values;
			my $v = $v[0];
			$self->__set_param(-query=>'1', -field=>$field, -values=>$v);
		}
	}
}

#	transfer field values from one query to another
#	by default, transfer is from 'current query' to '1'
sub __transfer_fields {
	my $self = shift;
	my ($fields, $from, $to) = rearrange([qw(fields from to)], @_);
	$from = 'current_query' unless $from;
	$to = '1' unless $to;

	foreach my $field (@$fields) {
		if ($self->get_param($field, $from)) {
			my $values = $self->get_param_values(-query => $from,
			                                     -field => $field);
			my @v = thaw freeze $values;
			my $v = $v[0];
			$self->__set_param(-query => $to, -field=> $field, -values=>$v);
#			delete $self->{params}{$from}{$field};
		}
	}
	$self->__delete_fields(-query=>$from, -fields=>$fields);
}

sub __move_field {
	my $self = shift;
	my ($field, $to_field) = rearrange([qw(field to_field)], @_);

	if ($self->get_param($field, 'current_query')) {
		my @values = @{$self->get_param_values(-query=>'current_query',
		                                     -field=>$field)};
		$self->__set_param(-query=>'1', -field=>$to_field, -values=> [@values] );
	}
}

sub __copy_or_delete {
	my $self = shift;
	my ($fields) = rearrange([qw(fields)], @_);

	foreach my $field(@$fields) {
		if ($self->get_param($field, 'current_query')) {
			$self->__copy_fields(-fields=>[$field]);
		} else {
			$self->__delete_fields('1', [$field]);
#			delete $self->{param}{1}{$field};
		#	$self->__set_param(-query=>'1', -field=>$field, -values=>[]);
		}
	}
}

sub __append_fields {
	my $self = shift;
	my ($fields) = rearrange([qw(fields)], @_);
  
	foreach my $field(@$fields) {
		$self->__append_field(-field=>$field);
	}
}

sub __append_field {
	my $self = shift;
	my ($field, $to_field, $query, $to_query) =
		rearrange([qw(field to_field query to_query)], @_);

	if (!defined($to_field)) {
		$to_field = $field;
	}
	if (!defined($query)) {
		$query = 'current_query';
	}
	if (!defined($to_query)) {
		$to_query = '1';
	}

	my $values = $self->get_param_values(-query=>$query, -field=>$field);
	if (defined($self->get_param_values(-query=>$to_query, -field=>$to_field))) {
		my $is_in = 0;
		foreach my $value(@$values) {
			$is_in = 0;
			foreach my $v (@{$self->get_param_values(-query=>$to_query, -field=>$to_field)}) {
				if ($v eq $value) {
					$is_in = 1;
				}
			}
			if (!$is_in) {
				push (@{$self->get_param_values(-query=>$to_query, -field=>$to_field)}, $value);
			}
		}
	} else {
		my @v = thaw freeze $values;
		my $v = $v[0];
		$self->__set_param(-query=>'1', -field=>$to_field, -values=>$v);
	}
}

sub __append_param {
	my $self = shift;
	my ($query, $field, $values) =
		rearrange([qw(query field values)], @_);
	
	if (!defined($query)) {
		$query = 1;
	}

	if (defined($self->get_param_values(-query=>$query, -field=>$field))) {
		my $is_in = 0;
		foreach my $value(@$values) {
			$is_in = 0;
			foreach my $v (@{$self->get_param_values(-query=>$query, -field=>$field)}) {
	if ($v eq $value) {
		$is_in = 1;
	}
			}
			if (!$is_in) {
	push (@{$self->get_param_values(-query=>$query, -field=>$field)}, $value);
			}
		}
	} else {
		$self->__set_param(-query=>$query, -field=>$field, -values=>$values);
	}
}

sub __set_param {
	my $self = shift;
	my ($query, $field, $values) = rearrange([qw(query field values)], @_);

	if (!defined($query)) {
		$query = 1;
	}
	$self->{'params'}->{$query}->{$field} = $values;
}
*set_param = \&__set_param;

sub get_params {
	my $self = shift;
	my $field = shift;
	return $self->{'params'}->{1}->{$field};
}

sub get_current_params {
	my $self = shift;
	my $field = shift;
	return $self->{'params'}->{'current_query'}->{$field};
}

sub get_user_params {
	my $self = shift;
	my $field = shift || undef;
	if ($field)
	{	if ($self->{params}{user}{$field})
		{	return $self->{params}{user}{$field};
		}
		elsif (defined($ENV{uc("GO_$field")}))
		{	return $ENV{uc("GO_$field")};
		}
	}
	else
	{	return $self->{params}{user};
	}
}

sub get_param_list {
	my $self = shift;
	my $field = shift;
#	print STDERR "get_param_list: field = $field\n";

	if ($self->{'params'}->{'current_query'}->{$field} &&
		scalar @{$self->{'params'}->{'current_query'}->{$field}} > 0 && 
		$self->{'params'}->{'current_query'}->{$field}->[0]) {
		return $self->{'params'}->{'current_query'}->{$field};
	} elsif ($self->{'params'}->{1}->{$field}) {
		return $self->{'params'}->{1}->{$field};
	}
	elsif (defined($self->{user}->{$field})) {
		return $self->{user}->{$field};
	}
	return undef;
}

sub get_param_values {
	my $self = shift;
	my ($field, $query) = rearrange([qw(field query)], @_);
	if (!defined($query)) {
		$query = 1;
	}
	return $self->{'params'}->{$query}->{$field};
}

sub __remove_acc_from_field {
	my $self = shift;
	my ($acc, $field) = rearrange([qw(acc field)], @_);
	my $q = $self->get_cgi;

	my @value_list;
	my @q_l = split "\0", $self->get_param_hash->{$field};
	foreach my $value(@q_l) {
		$value =~ s/^\s*(.*?)\s*$/$1/;
#		$value =~ s/^GO:?//;
#		$value =~ s/^0*//;
		if ($value ne $acc) {
			push @value_list, $value;
		}
	}
	if (scalar(@value_list) > 0) { 
		$q->param(-name=>$field, -values=>\@value_list);
	} else {
		$q->param(-name=>$field, -values=>'');
	}
}

sub __add_values_to_field {
	my $self = shift;
	my ($values, $field) = rearrange([qw(values field)], @_);
	my $q = $self->get_cgi;

	my @new_values = split "\0", $self->get_param_hash->{$field};
	push @$values, @new_values;
	$q->param(-name=>$field, -values=>\@new_values);

}

sub __has_value {
	my $self = shift;
	my ($field, $value) = rearrange([qw(field value)], @_);

	my $new_values = $self->get_param_hash->{$field};
	if ($new_values)
	{	foreach my $v (split "\0", $new_values)
		{	if ($v eq $value) {return 1};
		}
	}
	return 0;
}

sub __is_inside {
	my $self = shift;
	my ($acc, $array) = @_;

	foreach my $node (@$array) {
		if ($acc eq $node) {
			return 1;
		}
	}
	return 0;
}

=head2 get_cgi

args     none
returns  CGI;

=cut

sub get_cgi {
	my $self = shift;
	return $self->{'cgi'};
}

=head2 get_param

 Usage: my $dbname = $session->get_param("dbname");

will return, if defined, in this order:

environment variables of that name, ie GO_DBNAME
parameters from the "current_query"
parameters from query "1"
user params
default values

=cut

sub get_param {
	my $self = shift;
	my $pname = shift;
	my $query = shift;


	if (defined($ENV{uc("GO_$pname")})) {
		return $ENV{uc("GO_$pname")};
	} elsif (defined($query)) {
		if (defined ($self->get_param_values(-query=>$query,#'current_query',
		                                     -field=>$pname))) {
			eval {
				return ${$self->get_param_values(-query=>$query,
				                                 -field=>$pname)}[0];
			};
		} else {
			return undef;
		}
	}
	elsif (scalar(@{$self->get_param_values(-query=>'current_query',
	                                        -field=>$pname) || []})) {
		return ${$self->get_param_values(-query=>'current_query',
		                                 -field=>$pname)}[0];
	}
	elsif (defined ($self->get_param_values(-query=>'1',
	                                        -field=>$pname))) {
		return ${$self->get_param_values(-query=>'1',
	                                    -field=>$pname)}[0];
	}
	elsif (defined($self->{user}->{$pname})) {
		return $self->{user}->{$pname};
	}
	elsif (defined($self->default_val($pname))) {
		return $self->default_val($pname);
	}
	else {
		return undef;
	}
}

sub default_val {
	my $self = shift;
	my $pname = shift;
	my %defaults =
	(
		dbname => "go",
		dbhost => "localhost",
		view => "tree",
		page_size => 50,
		search_constraint => 'term',
#		termfields => ['name', 'term_synonym'],
#		gpfields => ['symbol', 'full_name', 'product_synonym'],
		termsort => 'rel',
		gpsort => 'rel',
		sppsort => 'rel',
		show_associations => 'list',
		term_context => 'parents',
		threshold => 0.1,
		maxhits => 50,
		blast_filter => 'on',
		max_selected_gps => 5,
		session_dir => 'sessions',
		
	);
	return $defaults{$pname};
}

sub apph {
	my $self = shift;
	if (!$self->{'apph'}) {
		my $dbname = $self->get_param("dbname");
		my $dbhost = $self->get_param("dbhost");
		my $dbuser = $self->get_param("dbuser");

		# DBPASS and DBAUTH both work.
		my $dbpass = $self->get_param("dbpass");
		my $dbauth = $self->get_param("dbauth");

		my $dbsocket = $self->get_param("dbsocket");
		my $dbport = $self->get_param("dbport");
		my %params = (-dbname => $dbname,
							-dbhost => $dbhost,
						);

		if (defined ($dbuser)) { $params{-dbuser} = $dbuser; }
		# DBPASS and DBAUTH both work.
		if (defined ($dbpass)) { $params{-dbauth} = $dbpass; }
		if (defined ($dbauth)) { $params{-dbauth} = $dbauth; }
	
		if (defined ($dbsocket)) { $params{-dbsocket} = $dbsocket; }
		if (defined ($dbport)) { $params{-dbport} = $dbport; }
	
		$self->{'apph'} = GO::AppHandle->connect(%params);
	}
	return $self->{'apph'};
}

sub munger {
	my $self = shift;
	if (!$self->{'munger'}) {
		$self->{'munger'} = GO::CGI::NameMunger->new();
	}
	return $self->{'munger'};
}

=head2 set_cgi

args     CGI;
returns  none

=cut

sub set_cgi {
    my $self = shift;
    my $cgi = shift;

    $self->{'cgi'} = $cgi;
}

=head2 get_param_hash

args     none
returns  hash table of params

If a value is given for $hash (which query you want -
currently "current_query" or "1") only values from that
query will be returned.  Otherwise, cgi values have priority,
followed by current_query, followed by '1' 

NOTE:  For legacy reasons, this returns multiple results
  as a \0 separated string rather than as an array, so to
  loop thru the results do:

foreach my $value(split("\0", $session->get_param_hash->{'query'})) {}

=cut

sub get_param_hash {
	my $self = shift;
	my $hash = shift;
	my $params = shift;

#	if (!defined($params)) {
#	$params = {};
#	}

	if ($hash) {
		foreach my $param(keys %{$self->{'params'}->{$hash}}) {
			if (!defined($params->{$param})) {
				foreach my $value(@{$self->{'params'}->{$hash}->{$param}}) {
					if ($params->{$param}) {
						$params->{$param} .= "\0$value";
					} else {
						$params->{$param} .= "$value";
					}
				}
			}
		}
		return $params;
	} else {
		$params = $self->get_param_hash('current_query', $params);
		$params = $self->get_param_hash('1', $params);
	}
	return $params;
}

=head2 set_output

args     Filehandle
returns  none

=cut

sub set_output{
	my $self = shift;
	my $out = shift;
	$self->{'out'} = $out;
}

=head2 get_output

args     none
returns  Filehandle

=cut


sub get_output{
	my $self = shift;
	return $self->{'out'};
}

=head2 set_data

    args     GO::Model::*;
returns  none

=cut

sub set_data{
	my $self = shift;
	my $data = shift;
	$self->{'data'} = $data;
}

=head2 get_data

args     none
returns  GO::Model::*;

=cut

sub get_data{
	my $self = shift;
	return $self->{'data'};
}

=head2 get_session_settings_urlstring

args     optional:  array_ref of query_values to pass along
returns  string

    returns a url string with the values of all the parameters
    specified in @settings_to_pass_along in the familiar &param=value
    format.

=cut

sub get_session_settings_urlstring {
	my $self = shift;
	my $settings_to_pass_along = shift;

	if (!$settings_to_pass_along) {
		$settings_to_pass_along = 
		[
			'session_id',
			'evcode',
			'exact_match',
			'speciesdb',
			'taxid',
#			'search_descriptions',
			'ont',
		];
	}
	my $setting_pass_alongs = "";
	foreach my $value (@$settings_to_pass_along) {
		foreach my $param (split ('\0', $self->get_param_hash->{$value})) {
			$setting_pass_alongs .= "&amp;$value=$param";
		}
	}
	return $setting_pass_alongs;
}

=head2 get_session_querylist_urlstring

args     none
returns  string

    much the same as get_session_settings_urlstring except
    its specialized for the query param

=cut

sub get_session_querylist_urlstring {
	my $self = shift;
	my $query_extension = "";

	print STDERR "query: ".$self->get_param_hash->{'query'}."\n";

	foreach my $query_value(split ('\0', $self->get_param_hash->{'query'})) {
		$query_extension .= "&amp;query=$query_value";
	}
	foreach my $query_value(split ('\0', $self->get_param_hash->{'closed'})) {
		$query_extension .= "&amp;closed=$query_value";
	}
	return $query_extension;
}

#get / set session type
sub ses_type {
	my $self = shift;
	$self->{_ses_type} = shift @_ if (@_);
	return $self->{_ses_type};
}

#communicate query warning message
sub message {
	my $self = shift;
	$self->{_message} = shift @_ if (@_);
	return $self->{_message};
}

#	add a warning message
sub add_message {
	my $self = shift;
	my ($msg_type, $msg) = @_;
	push @{$self->{params}{current_query}{_msg}{$msg_type}}, $msg;
}

sub get_message {
	my $self = shift;
	my $class = shift || undef;

	if ($class)
	{	return $self->{params}{current_query}{_msg}{$class} || undef;
	}
	else
	{	return $self->{params}{current_query}{_msg};
	}
}

sub __create_session_id {
	my $self = shift;
	my $session_id = "";
	$session_id = int(rand(10000));
	$session_id .= "amigo";
	$session_id .= time;
	$self->set_param(-field=>'session_id', -query=>'1', -values=>[$session_id]);
#	my $q = $self->get_cgi;
#	$q->param(-name=>'session_id', -value=>$session_id);
}

sub is_valid_session_id {
	my $self = shift;
	my $session_id = shift;
	if ($session_id =~ /^\d+amigo\d+$/)
	{	return 1;
	}
	return 0;
}

sub __equal_accs {
	my $self = shift;
	my ($acc1, $acc2) = @_;

	if ($acc1 =~ m/^GO:?/) {
	} else {
		$acc1 = $self->__make_go_from_acc($acc1);
	}
	if ($acc2 =~ m/^GO:?/) {
	} else {
		$acc2 = $self->__make_go_from_acc($acc2);
	}
	if ($acc1 eq $acc2) {
		return 1;
	} else {
	return 0;
	}
}

sub __make_go_from_acc {
	my $self = shift;
	my $acc = shift;
	
	return sprintf "GO:%07d", $acc;
}

sub __transfer_cache {
	my $self = shift;
	return if ($self->{__cache_transferred});
	my $query = 'cache_result';
	$self->{backup}->{$query} = $self->{cache}->{$query};
	$self->{__cache_transferred} = 1;
}

sub set_caching_param {
	my $self = shift;
	my ($field, $values) = rearrange([qw(field values)], @_);
	my $query = 'cache_result';

	$self->{params}->{$query}->{$field} = $values;
#	$self->{$query}->{$field} = $values;
	$self->__transfer_cache;
	$self->{cache}->{$query}->{$field} = $values;
}

sub set_all_caching_params {
	#	set all the caching params in one fell swoop
	my $self = shift;
	my $cache_ref = shift;
	my $query = 'cache_result';

	$self->{params}->{$query} = $cache_ref;
#	$self->{$query} = $cache_ref;
	$self->__transfer_cache;
	$self->{cache}->{$query} = $cache_ref;
}

sub get_caching_param {
	my $self = shift;
	my ($field) = rearrange([qw(field)], @_);
	my $query = 'cache_result';
	return $self->{cache}->{$query}->{$field} || $self->get_backup_param($field, $query, @_);
}

sub get_all_caching_params {
	my $self = shift;
	my $query = 'cache_result';
	return $self->{cache}->{$query} || $self->get_all_backup_params;
}

sub get_backup_param {
	my $self = shift;
	my ($field) = rearrange([qw(field)], @_);
	my $query = 'cache_result';
	unless ($self->{__backup_loaded}) {
	#	$self->__load_backup;
		$self->__load_cached_results;
	}
	return $self->{backup}->{$query}->{$field};
}

sub get_all_backup_params {
	my $self = shift;
	my $query = 'cache_result';
	unless ($self->{__backup_loaded}) {
	#	$self->__load_backup;
		$self->__load_cached_results;
	}
	return $self->{backup}->{$query};
}

sub __clear_sessions {
	my $self = shift;

	## Clean out sessions in here
	my $session_dir = $self->get_param('session_dir');

	## Clean out temporary images in here.
	my $html_dir = $self->get_param('html_dir') || "../amigo";
	my $tmp_image_dir = $html_dir.'/tmp_images';

	my $time = time;
	my $max_sessions = $self->get_param('MAX_SESSIONS') || 300; #inc by 100 as we have backup param as well
	my $session_timeout = $self->get_param('SESSION_TIMEOUT') || 7200;

	foreach my $dir ($session_dir, $tmp_image_dir) {
		my $dh = new DirHandle($dir);
		my @file_l = split ('\n', `ls $dir`);
		if ((scalar @file_l) > $max_sessions) {
			while (my $ses = $dh->read) {
				my @stat = lstat("$dir/$ses");
				my $a = $stat[9];
				if ($time - $a > $session_timeout ) {
					eval {
						if ($ses && !($ses =~ /\W/)) {
							if ($ses ne '.' && $ses ne '..' && $ses ne 'data' && $ses ne '') {
								`rm -rf $dir/$ses`;
								my $data_dir = $self->get_param('data_dir');
								my $command = "rm -rf $data_dir/$ses"."_blast";
								`$command`;
							}
						}
					};
				}
			}
		}
	}
}

sub __load_backup {
	my $self = shift;
	my $session_id = shift || $self->get_param('session_id');
	my $session_dir = $self->get_param('session_dir');
	unless ($session_id) {
		$session_id = $self->get_cgi->Vars->{'session_id'};
	}
	my $read_file = new FileHandle;
	my $file;
	my $session_file = "cache_".$session_id;
	if ($read_file->open("< $session_dir/$session_file")) {
		my @lines = $read_file->getlines;
		foreach my $line (@lines) {
			$file .= $line;
		}
		$read_file->close;
	}
	if ($file) {
	#	chmod 0777, $session_id;
		my $VAR1;
		eval $file;
		$self->{backup} = $VAR1;
	}
	$self->{__backup_loaded};
}

sub __load_session {
	print STDERR "Loading session!\n";
	my $self = shift;
	my $session_id = shift;
	my $session_dir = $self->get_param('session_dir');

	unless ($session_id) {
		$session_id = $self->get_cgi->Vars->{'session_id'};
	}
	if ($session_id)
	{	my $read_file = new FileHandle;
		my $file;
		my $data_dir = $session_id .= "_data";
#		my $session_file = "ses_".$session_id;
#		if ($read_file->open("< $session_dir/$session_file")) {
		my $session_file = "params";
		if ($read_file->open("< $session_dir/$data_dir/$session_file")) {
			my @lines = $read_file->getlines;
			foreach my $line (@lines) {
				$file .= $line;
			}
			$read_file->close;
		}
		if ($file) {
		#	chmod 0777, $session_id;
			my $VAR1;
			eval $file;
			$self->{params} = $VAR1;
		}
		
# move non key stuff to extra space and it won't be saved to disk as some of them are quite big
# to avoid carry this extra bag (e.g. diff query from caching type of query with same session)
# and we put this extra as back as well for one session has 2 diff caching
# use _set_param for these stuff if want to save to disk
		
		foreach my $q (keys %{$self->{params} || {}}) {
			unless (grep { $q eq $_ } qw(1 current_query user)) {
				$self->{cache}->{$q} = $self->{params}->{$q};
				delete $self->{params}->{$q};
			}
		}
	}
}

sub __save_session {
	print STDERR "Saving session!\n";
	my $self = shift;
	my $save_backup = shift;
	my $session_dir = $self->get_param('session_dir');

	## Save session to disk.
	my $file = new FileHandle;
	my $session_id = $self->get_param('session_id');
	
	my $cache = $self->{params}{cache_result};
	delete $self->{params}{cache_result};
	
	if ($session_id) {
#		my $session_file = 'ses_'.$session_id;
#		if ($file->open("> $session_dir/$session_file")) {
#			print $file Dumper($self->{params});
#			$file->close;
#		}

		my $data_dir = $session_id."_data";
		#	see if we have a data directory for this session already
		if (!new DirHandle("$session_dir/$data_dir")) {
			print STDERR "Eval-ing the create data_dir command\n";
			eval {
				mkdir("$session_dir/$data_dir", 0755);
				`chmod a+rw $session_dir/$data_dir`;
			};
			if ($@)
			{	print STDERR "Error: $@";
				$self->suicide_message("AmiGO configuration error: $@");
			}
		}
		if ($file->open("> $session_dir/$data_dir/params")) {
			print $file Dumper($self->{params});
			$file->close;
		}

		$self->{params}{cache_result} = $cache unless !$cache;

		if ($save_backup) {
#			return unless $cache;
			return unless $self->{params}{cache_result};

			print STDERR "Finding cache results to save...\n";

		#	return unless ($self->{cache_result});

		#	my $data_dir = $session_id."_data";
			#	see if we have a data directory for this session already
		#	if (!new DirHandle("$session_dir/$data_dir")) {
		#		print STDERR "Eval-ing the create data_dir command\n";
		#		eval {
		#			mkdir("$session_dir/$data_dir", 0755);
		#			`chmod a+rw $session_dir/$data_dir`;
		#		};
		#		if ($@)
		#		{	print STDERR "Error: $@";
		#			$self->suicide_message("AmiGO configuration error: $@");
		#		}
		#	}

			my $fh = new FileHandle;
			my $cache_file = $self->ses_type . "_cache";
			if (!($cache_file =~ /\W/)) {
				if ($fh->open("> $session_dir/$data_dir/$cache_file")) {
					print $fh Dumper($self->{params}{cache_result});
				#	print $fh Dumper($self->{cache_result});
					$fh->close;
				}
			}
		}
=cut for the time being
		if ($save_backup) {
			return unless ($self->{backup});
			my $qt = 'cache_result';
			#check backup has same values as param;
			map{delete $self->{backup}->{$qt}->{$_}}keys %{$self->{params}->{$qt} || {}};
			return unless (grep{not exists $self->{params}->{$qt}->{$_}}keys %{$self->{backup}->{$qt} || {}});
			my $fh = new FileHandle;
			my $cache_file = 'cache_'.$session_id;
			if (!($cache_file =~ /\W/)) {
				if ($fh->open("> $session_dir/$cache_file")) {
					print $fh Dumper($self->{'backup'});
					$fh->close;
				}
			}
		}
=cut
	}
}
*save_session = \&__save_session;

sub __save_params {
	my $self = shift;
	my ($query, $params) = rearrange([qw(query params)], @_);

	require "GO/CGI/Session.pm";

	my $session_id = $self->get_param('session_id');
	my $new_session = GO::CGI::Session->new(-no_update=>1);
	$new_session->__load_session($session_id);

	foreach my $param (@$params) {
		my $values = $self->get_param_values(-query=>$query, -field=>$param);
		$new_session->__set_param(-query=>$query,
		                          -field=>$param,
		                          -values=>$values);
	}
	$new_session->__save_session;
}

sub __load_cached_results {
	my $self = shift;
	my $ses_type = shift || $self->ses_type;
	my $ses_data_dir = $self->get_session_data_dir;
	if (!$ses_type)
	{	print STDERR "No session type set; cannot save results\n";
		return;
	}

	$ses_type .= "_cache";
	my $read_file = new FileHandle;

	print STDERR "Looking for $ses_data_dir/$ses_type \n";
	my $file;
	if ($read_file->open("< $ses_data_dir/$ses_type")) {
		my @lines = $read_file->getlines;
		foreach my $line (@lines) {
			$file .= $line;
		}
		$read_file->close;

		if ($file) {
		#	chmod 0777, $session_file;
			my $VAR1;
			eval $file;
			$self->{backup}{cache_result} = $VAR1;
		}
	}
	$self->{__backup_loaded};
}

sub __save_cached_results {
	my $self = shift;
	my $ses_type = shift || $self->ses_type;
	my $ses_data_dir = $self->get_session_data_dir;
	if (!$ses_type)
	{	print STDERR "No session type set; cannot save results\n";
		return;
	}

	if ($self->{cache}{cache_result})
	{	$ses_type .= "_cache";
		my $file = new FileHandle;
		if ($file->open("> $ses_data_dir/$ses_type")) {
			print $file Dumper($self->{cache}{cache_result});
			$file->close;
		}
	}
}

sub __delete_cached_results {
	my $self = shift;
	my $ses_type = shift || $self->ses_type;
	if (!$ses_type)
	{	print STDERR "No session type set\n";
		return;
	}

	my $ses_data_dir = $self->get_session_data_dir;
	$ses_type .= "_cache";

	print STDERR "Looking for $ses_data_dir/$ses_type...\n";
	eval {
		if (-f "$ses_data_dir/$ses_type") {
			`rm -rf $ses_data_dir/$ses_type`;
		}
	};
	#	delete any other bits of cache hanging around
	delete $self->{params}{cache_result};
	delete $self->{cache}{cache_result};
	delete $self->{backup}{cache_result};
}

sub save_to_disk {
	my $self = shift;
	my $data = shift;
	my $name = shift;
	require DirHandle;
	
	my $session_id = $self->get_param('session_id');
	if (!($session_id =~ /\W/)) {
		my $job_file = int(rand(10000));
		my $key_dir = "keys_".$session_id;
		my $session_id = $self->get_param('session_id');
		if (!new DirHandle("sessions/$key_dir")) {
			mkdir("sessions/$key_dir");
		}
		my $file = new FileHandle;
		my %seq_hash;
		if ($file->open("> sessions/$key_dir/$job_file")) {
			$file->print($data);
			$file->close;
		}
	return \%seq_hash;
	}
}

sub save_graphviz_image {
	my $self = shift;
	my $graphviz = shift;
	my $ses_id = $self->get_param('session_id');
	my $html_dir = $self->get_param('html_dir');

	if (!$ses_id || !$html_dir)
	{	print STDERR "Session ID or html directory not set!\n";
		$self->suicide_message("AmiGO configuration error.");
	}

	my $num = int(rand(1000));
	my $file_name = $ses_id."$num.png";
	my $tmp_img_dir = $html_dir.'/tmp_images';

	if (!new DirHandle($tmp_img_dir)) {
		print STDERR "Eval-ing the create tmp_img_dir command\n";
		eval {
			mkdir($tmp_img_dir, 0755);
			`chmod a+rw $tmp_img_dir`;
		};
		if ($@)
		{	print STDERR "Error: $@";
			$self->suicide_message("AmiGO configuration error: $@");
		}
	}

	my $img_url;
	my $html;

	my $fh = new FileHandle "> $tmp_img_dir/$file_name";
	print $fh $graphviz->as_png;
	$fh->close;
	chmod (0666, "$tmp_img_dir/$file_name");
	my $i_url = $self->get_param('html_url');
	$img_url = "$i_url/$tmp_img_dir/$file_name";

	$html = $graphviz->as_cmapx;
	#	remove the \ns from the title attributes
	$html =~ s/\\n/ /g;
#	if ($@)
#	{	print STDERR "Error!" . $@ ;
#	}
	
	return ($img_url, $html);
}

# unused
sub bootstrap_tree_view {
	my $self = shift;
	my $graph = $self->get_data;
	require "GO/Model/TreeIterator.pm";

	my $nit = GO::Model::TreeIterator->new($graph);
	$nit->set_bootstrap_mode;

	my @new_open_0;
	my $root_node = $self->get_param('ROOT_NODE') || $self->apph->get_root_term->acc || 'all';

	if ($self->get_param_values(-field=>'open_0')) {
		foreach my $value (@{$self->get_param_values(-field=>'open_0')}) {
			my @values = split(',', $value);
			if (scalar @values == 1 && $value ne $root_node) {
				while (my $ni = $nit->next_node_instance) {
					my $new_acc;
					if ($value =~ m/^(\d*){1}$/) {
						if ($ni->term->acc == $value) {
							foreach my $v(@{$nit->get_current_path}) {
								$new_acc .= $v.",";
							}
						}
					} else {
						if ($ni->term->public_acc eq $value) {
							foreach my $v(@{$nit->get_current_path}) {
								$new_acc .= $v.",";
							}
						}
					}
					chop $new_acc;
					push @new_open_0, $new_acc;
				}
				$nit->reset_cursor;
			} else {
			push @new_open_0, $value;
			}
		}
	}
	$self->__set_param(-field=>'open_0',
	                   -query=>'1',
	                   -values=>\@new_open_0);
}

sub get_search_field_list {
	my $self = shift;
	my $sc = shift;
	my $field = shift || 'srch_options';

	require GO::CGI::Search;

	$sc =~ s/fields//;
	return GO::CGI::Search::_search_field_list($sc, $field) || undef;
}

sub get_reltype_list {
	my $self = shift;
	my @a = keys %{$self->get_misc_hash->{reltype}};
	unless (scalar(@a)) {@a = qw(is_a part_of)}
	return ['all', @a];
}

sub get_ontology_list {
	my $self = shift;
	my @a = grep{$_}keys %{$self->get_misc_hash->{ontology}};
	unless (scalar(@a)) {@a = qw(cellular_component biological_process molecular_function)}
#	return ['all', sort{$a cmp $b}@a];
	return [@a];
}
*get_ont_list = \&get_ontology_list;

sub get_gptype_list {
	my $self = shift;
	my @a = grep{$_}keys %{$self->get_misc_hash->{gptype}};
	unless (scalar(@a)) {@a = qw(gene protein)}
	return [@a];
}

sub get_qual_list {
	my $self = shift;
	my @a = grep{$_}keys %{$self->get_misc_hash->{qual}};
	unless (scalar(@a)) {@a = qw(contributes_to not)}
	return [@a];
}

sub get_assby_list {
	my $self = shift;
	my @a = grep{$_}keys %{$self->get_misc_hash->{assby}};
	unless (scalar(@a)) {@a = qw(db1 db2)}
	return [@a];
}

sub get_speciesdb_list {
	my $self = shift;
	my @a = keys %{$self->get_misc_hash->{speciesdb}};
	unless (scalar(@a)) {
		@a = ('fb', 'sgd', 'mgi', 'genedb_spombe', 'uniprot', 'tair', 'ddb', 'wb', 'ensembl', 'rgd', 'tigr_cmr', 'tigrfams', 'tigr_ath1', 'tigr_tba1', 'gr', 'genedb_tsetse', 'genedb_tbrucei', 'genedb_pfalciparum', 'genedb_lmajor', 'zfin');
	}
#	return ['all', @a];
	return [@a];
}
*get_datasource_list = \&get_speciesdb_list;

sub get_evidence_code_list {
	my $self = shift;
	my @a = keys %{$self->get_misc_hash->{evcode}};
	unless (scalar(@a)) {@a = ('IC','IDA','IEP','IGI','IMP','IPI','ISS','NAS','ND','NR','RCA','TAS')}
#	return ['all', @a];
	return [@a];
}
*get_evcode_list = \&get_evidence_code_list;

sub get_evcode_sets {
	my $self = shift;
	my %sets = 
	(	'exp' => ['IMP','IGI','IPI','IDA','IEP'],
		'aca' => ['IC','IDA','IEP','IGI','IMP','IPI','ISS','NAS','ND','NR','RCA','TAS'],
	);
	return \%sets;
}

sub get_misc_hash {
	my $self = shift;

	if ($self->{'misc_hash'}) {
		return $self->{'misc_hash'};
	} else {
		if (-f "misc_keys.pl") {
			open (FILE, 'misc_keys.pl');
			my $hash;
			while (<FILE>) {
				$hash .= $_;
			}
			my ($a) = thaw $hash;
			$self->{'misc_hash'} = $a;
			return $a;
		} else {
			$self->{'misc_hash'} = {};
			return {};
		}
	}
}

sub get_species_hash {
	my $self = shift;

	if ($self->{'species_hash'}) {
		return $self->{'species_hash'};
	} else {
		if (-f "spec_keys.pl") {
			open (FILE, 'spec_keys.pl');
			my $hash;
			while (<FILE>) {
				$hash .= $_;
			}
			my ($a) = thaw $hash;
			$self->{'species_hash'} = $a;
			return $a;
		} else {
			$self->{'species_hash'} = {};
			return {};
		}
	}
}

sub get_taxid_species_list_str {
	my $self = shift;
	my $hash = $self->get_species_hash;
	my @all = qw("all" "All");
#	my @all;
	map{push @all, $hash->{$_},"\"$_\""}sort{$a cmp $b}keys %$hash;
	return join(",",@all);
}

sub get_reverse_species_hash {
	my $self = shift;
	if ($self->{'species_hash'}) {
		my %species = reverse %{$self->{'species_hash'}};
		return \%species;
	} else {
		open (FILE, 'spec_keys.pl');
		my $hash;
		while (<FILE>) {
			$hash .= $_;
		}
		my ($a) = thaw $hash;
		$self->{'species_hash'} = $a;
		my %species = reverse %$a;
		return \%species;
	}
}

sub get_species_keys {
	my $self = shift;

	my $hash = $self->get_species_hash;

	my @a = sort keys %$hash;
	return \@a;
}
*get_species_key_list = \&get_species_keys;

sub get_species_taxids {
	my $self = shift;

	my $hash = $self->get_species_hash;

	my @a = sort values %$hash;
	return \@a;
}
*get_taxid_list = \&get_species_taxids;

sub get_species_acc {
	my $self = shift;
	my $sacc = shift;

	return $self->get_species_hash->{$sacc};
}

sub get_species_name {
	my $self = shift;
	my $taxa_id = shift || return;
	my $h = $self->get_species_hash;
	#for autocomplete, only one species
	my @a = grep{$h->{$_} == $taxa_id->[0]}keys %$h;
#	 printf STDERR "species: $a[0], taxa id:$taxa_id\n";
	return $a[0];
}

sub get_param_list_size {
	my $self = shift;
	my $param = shift;
	
	my $sub = "get_".$param."_list";
	my $list = [];
	eval {
		$list = $self->$sub;
	};
	return scalar @$list;
}

sub set_single_option {
	my $self = shift;
	my $opt = shift;
	my $data;

	my $option_h = {
		term_context => ['parents', 'sibling'],
		search_constraint => ['term', 'gp'],
		show_associations => ['list','direct'],
		tree_view => ['full', 'compact'],
	};
	
	if (!$option_h->{$opt})
	{	return;
	}
	
	#	get the params and the human names
	my @option_l = map { $_ = { value => $_, label => $self->munger->get_human_name($_) } } @{$option_h->{$opt}};

	my @select;
	my $selected = $self->get_param($opt);
	if ($selected)
	{	@select = grep { $_->{value} eq $selected } @option_l;
	}

	if (!@select)
	{	#	use the first value in the list
		$option_l[0]{selected} = 1;
	}
	else
	{	$select[0]{selected} = 1;
	}

#	print STDERR "data: ".Dumper(\@option_l)."\n";
	return \@option_l;
}

sub set_multi_option {
	my $self = shift;
	my $opt = shift;
	my $selected_only = shift || undef;
	my %data;

	my $all = 0;
	my $selected = $self->get_param_list($opt);
	if (($opt eq 'termfields' || $opt eq 'gpfields') && !$selected)
	{	$selected = $self->get_search_field_list($opt, 'default');
	}

	my $option_l;
	if ($selected_only)
	{	if ($selected)
		{	$option_l = [ map { $_ } @$selected ];
		}
	}
	else
	{	#	get the params and the human names
		$option_l = $self->get_param_list_by_param($opt);
		if (!@$option_l)
		{	return;
		}
	}

	my @select;
	if (!$selected || $selected->[0] eq 'all')
	{	#	wait
		$all = 1;
	}
	else
	{	foreach my $s (@$selected)
		{	push @select, (grep { $_->{value} eq $s } @{$option_l});
		}
		
		if (!@select)
		{	$all = 1;
			last;
		}
		foreach (@select)
		{	$_->{selected} = 1;
			#	get the URL
			if ($opt eq 'ont' || $opt eq 'evcode')
			{	$_->{url} = $self->munger->get_GO_doc_url($opt, $_->{value});
			}
			elsif ($opt eq 'taxid')
			{	$_->{url} = $self->munger->get_url('ncbi_taxid', $_->{value});
			}
			elsif ($opt eq 'speciesdb')
			{	$_->{url} = $self->munger->get_db_url($_->{value});
			}
		}
	}
	
	if ($opt ne 'termfields' && $opt ne 'gpfields')
	{	if ($all == 1)
		{	unshift @$option_l, { value => 'all', label => 'All', selected => 1 };
		}
		else
		{	unshift @$option_l, { value => 'all', label => 'All'};
		}
	}
	
	%data = (
		data => $option_l,
		title => $self->munger->get_human_name($opt),
	);
	$data{selected} = \@select unless (!@select);
	
	return \%data;
}

sub get_param_list_by_param {
	my $self = shift;
	my $param = shift;

#taxid evcode ont gptype speciesdb

	if ($param eq 'ont')
	{	my $ont = [sort { $a cmp $b } @{$self->get_ont_list}];
		map { my $temp;
				($temp = $_) =~ s/_/ /g;
				$_ = { value => $_, label => $temp } } @$ont;
		return $ont;
#		foreach (@$ont)
#		{	$all{ont}{$_} = { url => $munger->get_GO_doc_url('ont', $_) };
#		}
	}
	elsif ($param eq 'evcode')
	{
		my $evcode = [sort { $a cmp $b } @{$self->get_evcode_list}];
	#	map { $_ = { value => $_, label => $self->{munger}->get_full_name($_) } } @$evcode;
		if ($self->ses_type eq 'prefs' || $self->ses_type eq 'advanced_query') {
			map { $_ = { value => $_, label => $self->munger->get_full_name($_) } } @$evcode;
		}
		else {
			map { $_ = { value => $_, label => $_, human => $self->munger->get_full_name($_) } } @$evcode;
		}
		return $evcode;
		
#		foreach (@$evcode)
#		{	$all{evcode}{$_} =
#			{	url => $munger->get_GO_doc_url('evcode', $_),
#				human => $munger->get_full_name($_),
#			};
#		}
	}
	elsif ($param eq 'gptype')
	{	
		my $gptype = [sort { $a cmp $b } @{$self->get_gptype_list}];
		map { $_ = { value => $_, label => $_ } } @$gptype;
		return $gptype;
#		foreach (@$gptype)
#		{	$all{gptype}{$_} = undef;
	#		{	human => $_,
	#		};
#		}
	}
	elsif ($param eq 'assby')
	{	
		my $assby = [sort { $a cmp $b } @{$self->get_assby_list}];
		map { $_ = { value => $_, label => $self->munger->get_human_name($_) } } @$assby;
		return $assby;
	}
	elsif ($param eq 'qual')
	{	
		my $qual = [sort { $a cmp $b } @{$self->get_qual_list}];
		map { $_ = { value => $_, label => $self->munger->get_human_name($_) } } @$qual;
		return $qual;
	}
	elsif ($param eq 'speciesdb')
	{	my $speciesdb = [sort { $a cmp $b } @{$self->get_speciesdb_list}];
		map { $_ = { value => $_, label => $self->munger->get_human_name($_) } } @$speciesdb;
		return $speciesdb;
#		foreach (@$speciesdb)
#		{	$all{speciesdb}{$_} =
#			{	#url => $munger->get_db_url($_),
#				human => $munger->get_human_name($_),
#			};
#		}
	}
	elsif ($param eq 'taxid')
	{	my $taxid = $self->get_reverse_species_hash;
		my @taxa = map { $_ = { value => $_, label => $taxid->{$_} } } sort { $taxid->{$a} cmp $taxid->{$b} } (keys %$taxid);

		#	check whether we have any custom species filters
		my $custom = $self->get_param_list('custom_taxid') || undef;
		if ($custom && @$custom)
		{	#	get the taxon info from the db
			my $apph = $self->apph;
			my $dbh = $apph->dbh;
			my $results = $dbh->selectall_arrayref("SELECT 'value', ncbi_taxa_id, 'label', CONCAT(genus,' ',species) FROM species WHERE ncbi_taxa_id IN (".join(",", @$custom).") ORDER BY genus, species");
			if ($results)
			{	print STDERR "results:\n".Dumper($results)."\n";
			#	foreach (@$results)
			#	{	print STDERR "result:\n".Dumper($_)."\n";
			#		my %hash = @$_;
			#		print STDERR "hash:\n".Dumper(\%hash)."\n";
			#		$_ = \%hash;
			#		print STDERR "result:\n".Dumper($_)."\n";
			#		
			#	}
				map { my %hash = @$_; $_ = \%hash } @$results;
			#	map { $_ = \%{@$_} } @$results;
				print STDERR "results:\n".Dumper($results)."\n";
				unshift @taxa, @$results;
			}
		}
	#	print STDERR "taxa:\n".Dumper(\@taxa)."\n";
		
		return \@taxa;

#		foreach (
#		{	push($tax_a->{taxid}{$_} =
#			{	#url => 'http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id='.$_,
#				human => $taxid->{$_},
#			};
#		}
	}
	elsif ($param eq 'termfields' || $param eq 'gpfields')
	{	my $fields = $self->get_search_field_list($param);
		push @$fields, 'all';
		map { $_ = { value => $_, label => $self->munger->get_field_name($_) } } @$fields;
		return $fields;
	}
}

sub get_data_for_filters {
	my $self = shift;
	
#	print STDERR "self->{munger}:\n".Dumper($self->{munger})."\n\n";
#	print STDERR "self:\n".Dumper($self)."\n\n";
	
	my $ses_type = $self->ses_type;
	my $filterdata;
#	our full list of filters
	my @all_filters = ('ont', 'speciesdb', 'taxid', 'evcode', 'gptype', 'assby', 'qual');
#	active filters
	my @active = grep { $self->get_param('show_'.$_.'_filter') && $self->get_param('show_'.$_.'_filter') == 1 } @all_filters;

	unless ($ses_type =~ /goslim/)
	{	foreach my $f (@active)
		{	$filterdata->{$f} = $self->set_multi_option($f);
		}
	}
	
	$filterdata->{search_constraint} = $self->set_single_option('search_constraint');
	return $filterdata;
}

sub get_searchfield_data {
	my $self = shift;
	my $data;
	
	foreach my $f ('termfields', 'gpfields')
	{	$data->{$f} = $self->set_multi_option($f);
	}
	
	return $data;
}



=head2 check_gp_count_ok

  Arguments - session
  returns   - 1 or 0

  Looks at the session filters to see if the gp count
  for a term will be correct

=cut

sub check_gp_count_ok {
	my $self = shift;
	if ($self->{'gp_count_ok'})
	{	return $self->{'gp_count_ok'};
	}
	
	my $filters = $self->apph->filters;
	if (!$filters)
	{	$self->{'gp_count_ok'} = 1;
		return 1;
	}

	#	if the filter settings allow us, check whether
	#	the gp count will be correct or not
	foreach my $f qw(gptype taxid evcode assby qual)
	{	if ($filters->{$f})
		{	$self->{'gp_count_ok'} = 0;
			return 0;
		}
	}
	
	$self->{'gp_count_ok'} = 1;
	return 1;
}

=head2 suicide_message

  Arguments - session, message (optional)
  
  bows gracefully out of the current query,
  outputting a page with the error message(s) on it.

=cut

sub suicide_message {
	my $self = shift;
	my $msg = shift || undef;
#	my $return = shift;

	require GO::Template::Template;
	
	$self->add_message('fatal', $msg) if $msg;

	print "Content-type:text/html\n\n";
	my $vars = {
		session => $self,
		page_name => $self->ses_type,
		session_id => $self->get_param('session_id'),
		html_url => $self->get_param('html_url'),
		image_dir => $self->get_param('html_url').'/images',
		filterdata => { search_constraint => $self->set_single_option('search_constraint')},
	};

	GO::Template::Template->process_template($self, "amigo_message.tmpl", $vars);
#	$self->__save_session(1);
	exit;
}

=head2 get_subset

  Arguments - session, list, page number (optional), page size (optional)
  returns   - the subset of the input list that would appear on page X

=cut

sub get_subset {
	my $self = shift;
	my $fullset = shift;
	my $page_n = shift || $self->get_param('page') || 1;
	my $page_size = shift || $self->get_param('page_size');
	print STDERR "get subset: page number: $page_n; list size = ".scalar(@$fullset)."\n";
	my @subset = @$fullset;
	my ($from, $to) = (($page_n - 1) * $page_size, $page_size * $page_n - 1);

	if ($to >= scalar(@$fullset)) {
		$to = scalar(@$fullset) - 1;
	}
	@subset = @$fullset[$from..$to];
	return \@subset;
}

=head2 get_n_pages

  Arguments - session, no of results, page size (optional)
  returns   - number of pages required to show the results

=cut

sub get_n_pages {
	my $self = shift;
	my $n_accs = shift;
	my $page_size = shift || $self->get_param('page_size');
	my $n_pages = 1;
	if ($page_size ne 'all')
	{	$n_pages = int($n_accs/$page_size);
		$n_pages++ if ($n_accs % $page_size);
	}
	return $n_pages;
}


=head2 get_session_data_dir

usage : my $ses_data_dir = $session->get_session_data_dir;

Returns the location of the data directory for the session
containing cache files and saved parameters.

=cut

sub get_session_data_dir {
	my $self = shift;

	my $ses_dir = $self->get_param('session_dir');
	my $ses_id = $self->get_param('session_id');

	if (!$ses_id)
	{	print STDERR "No session ID set!\n";
		$self->suicide_message("AmiGO configuration error.");
	}

	my $ses_data_dir = $ses_dir.'/'.$ses_id.'_data';

	if (!new DirHandle($ses_data_dir)) {
		print STDERR "Eval-ing the create data_dir command\n";
		eval {
			mkdir($ses_data_dir, 0755);
			`chmod a+rw $ses_data_dir`;
		};
		if ($@)
		{	print STDERR "Error: $@";
			$self->suicide_message("AmiGO configuration error: $@");
		}
	}
	return $ses_data_dir;
}


=head2 get_blast_results_dir

usage : my $blast_dir = $session->get_blast_results_dir;

Returns the location of the blast results directory

=cut

sub get_blast_results_dir {
	my $self = shift;
	my $ses_data_dir = $self->get_session_data_dir;
	my $blast_dir = $ses_data_dir.'/blast_dir';

	if (!new DirHandle($blast_dir)) {
		print STDERR "Eval-ing the create data_dir command\n";
		eval {
			mkdir($blast_dir, 0755);
			`chmod a+rw $blast_dir`;
		};
		if ($@)
		{	print STDERR "Error: $@";
			$self->suicide_message("AmiGO configuration error: $@");
		}
	}
	return $blast_dir;
}

=head2 get_vars

usage : my $vars = $session->get_vars(\%var_hash)

Returns the standard set of vars required by most templates,
plus any other vars in %var_hash

=cut

sub get_vars {
	my $session = shift;
	my $other_vars = shift;
	return
	{	%$other_vars,
		session => $session,
		munger => $session->munger,
		filterdata => $session->get_data_for_filters,
		session_id => $session->get_param('session_id'),
		html_url => $session->get_param('html_url'),
		image_dir => $session->get_param('html_url').'/images',
		page_name => $session->ses_type,
	};
}


1;
