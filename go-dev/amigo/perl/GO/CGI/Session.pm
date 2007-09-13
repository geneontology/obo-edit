=head1 SYNOPSIS

package GO::CGI::Session;

=head2 Usage

Session manager for AmiGO (go.cgi)

=cut
package GO::CGI::Session;

use strict;
use DirHandle;
use FileHandle;
use FreezeThaw qw (freeze thaw);
use Data::Dumper;
use Exporter;

use GO::AppHandle;
use GO::Utils qw(rearrange);
use GO::CGI::Utilities qw(:all);
use GO::CGI::NameMunger;

$Data::Dumper::Indent = 1;

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
@ISA = ('Exporter');
@EXPORT = qw(process_page_template);
@EXPORT_OK = qw(process_page_template);

=head2 new

-args -ses_type   session type (defaults to 'amigo_message')
      -q          CGI,
      -read_only  don't save the session after loading and syncing
      -temp       create a temporary session (e.g. for outputting
                  fatal error messages)

returns  GO::CGI::Session;

=cut

sub new {
	print STDERR "Starting a new session!\n";
	my $class = shift;
	my $self = {};
	bless $self, $class;
	my ($q, $ses_type, $read_only, $temp) = rearrange([qw(q ses_type read_only temp)], @_);

	# $self->{'params'} is now what holds the data for performing
	# a query. The CGI values will no longer be directly used.
	$self->set_cgi($q);

	#	set the session type
	$ses_type = 'amigo_message' if !$ses_type;
	$self->ses_type($ses_type);
	
	print STDERR "session type: ".$self->ses_type."\n";
	
	$self->__clear_sessions;
	if ($q->param('session_id'))
	{	my $ses_id = $q->param('session_id');
		#	check the session ID is OK before we do anything else
		if ($self->is_valid_session_id($ses_id))
		{	$self->id($ses_id);
			$self->__load_session($ses_id);
		}
		else
		{	#	invalid session ID. Delete it!
			$q->delete('session_id');
		}
	}
	else
	{	$self->__create_session_id;
	}

	my $apph = $self->apph;
	$self->{release_date} = $apph->instance_data->{release_name};

	return $self if $temp;

	$self->__synchronize_session($q);
	$self->__save_session unless $read_only;

	return $self;
}


##### Session IDs #####

#get / set session ID
sub id {
	my $self = shift;
	if (@_)
	{	$self->{_id} = shift;
	}
	return $self->{_id};
}

sub is_valid_session_id {
	my $self = shift;
	my $session_id = shift;
	if ($session_id =~ /^\d+amigo\d+$/)
	{	return 1;
	}
	return 0;
}

sub __create_session_id {
	my $self = shift;
	my $session_id = "";
	$session_id = int(rand(10000));
	$session_id .= "amigo";
	$session_id .= time;
	$self->id($session_id);
}


##### Other handy things #####

sub apph {
	my $self = shift;
	if (!$self->{'apph'}) {
		my $dbname = $self->get_default_param("dbname");
		my $dbhost = $self->get_default_param("dbhost");
		my $dbuser = $self->get_default_param("dbuser");

		# DBPASS and DBAUTH both work.
		my $dbpass = $self->get_default_param("dbpass");
		my $dbauth = $self->get_default_param("dbauth");

		my $dbsocket = $self->get_default_param("dbsocket");
		my $dbport = $self->get_default_param("dbport");
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

#get / set session type
sub ses_type {
	my $self = shift;
	$self->{_ses_type} = shift @_ if (@_);
	return $self->{_ses_type};
}

#get / set gp_count_ok
sub gp_count_ok {
	my $self = shift;
	$self->{_gp_count_ok} = shift @_ if (@_);
	return $self->{_gp_count_ok};
}

sub default_val {
	my $self = shift;
	my $pname = shift;
	
#	print STDERR "pname: $pname\n";
	
	my %defaults =
	(	dbname => "go",
		dbhost => "localhost",
		search_constraint => 'term',
		view => "tree",
#		termfields => ['name', 'term_synonym'],
#		gpfields => ['symbol', 'full_name', 'product_synonym'],
		termsort => 'rel',
		gpsort => 'rel',
		sppsort => 'rel',
		term_assocs => 'all',
		term_context => 'parents',
		threshold => 0.1,
		maxhits => 50,
		blast_filter => 'on',
		max_selected_gps => 10,
		session_dir => 'sessions',
		graph_bgcolor => 'white',
		graph_textcolor => 'blue',
		layout => 'vertical',
		tree_view => 'full',
		use_full_spp_names => 'yes',
	);
	return $defaults{$pname} || undef;
}


##### Getting and setting parameters #####

=head2 get_param

Usage: my $dbname = $session->get_param("dbname");

Looks in the following places for the param and returns it if found

CGI
current parameters
saved parameters
environment variables
default Session values

=cut

sub get_param {
	my $self = shift;
	my $param = shift;
	
	return #$self->get_cgi_param($param) ||
		$self->get_current_param($param) ||
		$self->get_saved_param($param);
}

=head2 get_cgi_param

Usage: my $dbname = $session->get_cgi_param("dbname");

Gets a parameter from the cgi

=cut

sub get_cgi_param {
	my $self = shift;
	my $param = shift || return;
	
	my @list = $self->get_cgi->param($param);
	
	return undef unless @list;
	
	return [ @list ];
}

=head2 get_current_param

Usage: my $dbname = $session->get_current_param("dbname");

Returns the parameter from the 'current' parameter hash

=cut

sub get_current_param {
	my $self = shift;
	my $param = shift || return;

	return unless $self->{params}{current};
	return $self->{params}{current}{$param} || undef;
}

=head2 get_saved_param

Usage: my $dbname = $session->get_saved_param("dbname");

Looks in the following places for the param and returns it if found

saved parameters
environment variables
default Session values

=cut

sub get_saved_param {
	my $self = shift;
	my $param = shift || return;

#	print STDERR "param: $param\n";

	return unless $self->{params}{saved};

	return $self->{params}{saved}{$param} || $self->get_default_param($param);
}

=head2 get_default_param

Usage: my $dbname = $session->get_default_param("dbname");

Looks in the following places for the param and returns it if found

environment variables
default Session values

=cut

sub get_default_param {
	my $self = shift;
	my $param = shift || return;

#	if (defined ($ENV{uc("GO_$param")}))
#	{	return $ENV{uc("GO_$param")};
#	}
	return GO::CGI::Utilities::get_environment_param($param) || $self->default_val($param);
}

=head2 set_current_param

Arguments:  session
            parameter name
            parameter value (optional)

Usage: $session->set_current_param("dbname", "fruitloop");

Sets a parameter in the 'current' parameter hash. If the value
is undefined, deletes the current value of that parameter

=cut

sub set_current_param {
	my $self = shift;
	my $param = shift;
	my $value = shift;
	
	return unless $param;

	print STDERR "param = $param; value = ".Dumper($value);

	if ($value)
	{	$self->{params}{current}{$param} = $value;
	}
	else
	{	delete $self->{params}{current}{$param} if $self->{params}{current}{$param};
	}
}

=head2 set_saved_param

Arguments:  session
            parameter name
            parameter value (optional)

Usage: $session->set_saved_param("dbname", "fruitloop");

Sets a parameter in the 'saved' parameter hash. If the value
is undefined, deletes the current value of that parameter

=cut

sub set_saved_param {
	my $self = shift;
	my $param = shift;
	my $value = shift;
	
	print STDERR "param: $param; value: $value\n";
	
	return unless $param;
	if ($value)
	{	if ($self->get_default_param($param) && $self->get_default_param($param) eq $value)
		{	delete $self->{params}{saved}{$param};
		}
		else
		{	$self->{params}{saved}{$param} = $value;
		}
	}
	else
	{	delete $self->{params}{saved}{$param} if $self->{params}{saved}{$param};
	}
}

=head2 delete_current_param

Arguments:  session
            parameter name

Usage: $session->delete_current_param("dbname");

Deletes the value in the 'current' param hash for the named parameter

=cut

sub delete_current_param {
	my $self = shift;
	my $param = shift;
	return unless $param;

	delete $self->{params}{current}{$param} if $self->{params}{current}{$param};
}

=head2 delete_saved_param

Arguments:  session
            parameter name

Usage: $session->delete_saved_param("dbname");

Deletes the value in the 'saved' param hash for the named parameter

=cut

sub delete_saved_param {
	my $self = shift;
	my $param = shift;
	return unless $param;

	delete $self->{params}{saved}{$param} if $self->{params}{saved}{$param};
}

=head2 delete_param

Arguments:  session
            parameter name

Usage: $session->delete_param("dbname");

Deletes the values from the 'current' and 'saved' param
hashes for the named parameter

=cut

sub delete_param {
	my $self = shift;
	my $param = shift || return;
	$self->delete_current_param($param);
	$self->delete_saved_param($param);
}

=head2 delete_all_params

Arguments:  session
            parameter type ('current' or 'saved')

Usage: $session->delete_all_params("saved");

Deletes the all values from the 'current' or 'saved' param
hashes. If the parameter type is undefined, deletes both the
current and saved params.

=cut

sub delete_all_params {
	my $self = shift;
	my $param = shift;

	if (!defined($param))
	{	$self->{params}{$_} = {} foreach (keys %{$self->{params}});
	}
	else
	{	if ($self->{params}{$param})
		{	$self->{params}{$param} = {};
		}
	}
}

=head2 get_cgi

args     none
returns  CGI;

=cut

sub get_cgi {
	my $self = shift;
	return $self->{'cgi'};
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


##### Loading / saving / deleting files #####

=head2 get_session_data_dir

usage : my $ses_data_dir = $session->get_session_data_dir;

Returns the location of the data directory for the session
containing cache files and saved parameters.

=cut

sub get_session_data_dir {
	my $self = shift;

	my $ses_dir = $self->get_default_param('session_dir');
	my $ses_id = $self->id;

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

sub __clear_sessions {
	my $self = shift;

	print STDERR "__clearing sessions...\n";

	## Clean out sessions in here
	my $session_dir = $self->get_default_param('session_dir');

	## Clean out temporary images in here.
	my $html_dir = $self->get_default_param('html_dir') || "../amigo";
	my $tmp_image_dir = $html_dir.'/tmp_images';

	my $time = time;
	my $max_sessions = $self->get_default_param('max_sessions') || 300; #inc by 100 as we have backup param as well
	my $session_timeout = $self->get_default_param('session_timeout') || 7200;

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
								my $data_dir = $self->get_default_param('data_dir');
								my $command = "rm -rf $data_dir/$ses"."_blast";
								`$command`;
							}
						}
					};
				}
			}
		}
	}
	print STDERR "Done!\n";
}

sub __load_session {
	print STDERR "Loading session!\n";
	my $self = shift;
	my $session_id = shift || $self->id;
	my $ses_data_dir = $self->get_session_data_dir;

	if (!$session_id)
	{	print STDERR "Warning: no session ID found. Error?\n";
		return;
	}

	my $read_file = new FileHandle;

	print STDERR "Looking for $ses_data_dir/params\n";
	my $file;
	if ($read_file->open("< $ses_data_dir/params")) {
		my @lines = $read_file->getlines;
		foreach my $line (@lines) {
			$file .= $line;
		}
		$read_file->close;

		if ($file) {
			my $VAR1;
			eval $file;
			$self->{params} = $VAR1;
#			return $VAR1;
		}
	}
	if ($@)
	{	print STDERR "Error: $@";
	}

# move non key stuff to extra space and it won't be saved to disk as some of them are quite big
# to avoid carry this extra bag (e.g. diff query from caching type of query with same session)
# and we put this extra as back as well for one session has 2 diff caching
# use _set_param for these stuff if want to save to disk
	foreach my $q (keys %{$self->{params} || {}}) {
		unless (grep { $q eq $_ } qw(saved current)) {
			$self->{cache}->{$q} = $self->{params}->{$q};
			delete $self->{params}->{$q};
		}
	}
}

sub __save_session {
	print STDERR "Saving session!\n";
	my $self = shift;
	if (@_)
	{	$self->save_cached_results(@_);
	}
	my $ses_data_dir = $self->get_session_data_dir;

	my $file = new FileHandle;
	if ($file->open("> $ses_data_dir/params")) {
		print $file Dumper($self->{params});
		$file->close;
	}
}
*save_session = \&__save_session;

sub load_cached_results {
	my $self = shift;
	my $ses_type = shift || $self->ses_type;
	if (!$ses_type)
	{	print STDERR "No session type set; cannot retrieve results\n";
		return;
	}
	my $ses_data_dir = $self->get_session_data_dir;

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
			return $VAR1;
#			$self->{backup}{cache_result} = $VAR1;
		}
	}
	if ($@)
	{	print STDERR "Error: $@";
		$self->suicide_message("AmiGO configuration error: $@");
	}
	return;
#	$self->{__backup_loaded};
}

sub save_cached_results {
	my $self = shift;
	my $results_to_cache = shift;
	my $ses_type = shift || $self->ses_type;
	my $ses_data_dir = $self->get_session_data_dir;
	if (!$ses_type)
	{	print STDERR "No session type set; cannot save results\n";
		return;
	}

#	print STDERR "results to cache: ".Dumper($results_to_cache)."\n";

	if ($results_to_cache)
	{	print STDERR "Ready for some caching action!\n";
		$ses_type .= "_cache";
		my $file = new FileHandle;
		if ($file->open("> $ses_data_dir/$ses_type")) {
			print $file Dumper($results_to_cache);
			$file->close;
		}
	}
}

sub delete_cached_results {
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

sub delete_all_cached_results {
	my $self = shift;
	my $ses_data_dir = $self->get_session_data_dir;
	eval {
			`rm -rf $ses_data_dir/*_cache`;
	};
}

sub save_graphviz_image {
	my $self = shift;
	my $graphviz = shift;
	my $ses_id = $self->id;
	my $html_dir = $self->get_default_param('html_dir');

	if (!$ses_id || !$html_dir)
	{	print STDERR "Session ID or html directory not set!\n";
		$self->suicide_message("AmiGO configuration error.");
	}

	my $num = int(rand(1000));
	my $file_name = $ses_id."$num.png";
	my $tmp_img_dir = $html_dir.'/tmp_images/'.$ses_id;

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
	$img_url = "tmp_images/$ses_id/$file_name";

	print STDERR "img_url: $img_url\n";

	$html = $graphviz->as_cmapx;
	#	remove the \ns from the title attributes
	$html =~ s/\\n/ /g;
#	if ($@)
#	{	print STDERR "Error!" . $@ ;
#	}
	
	return ($img_url, $html);
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
#	new!
			if ($self->get_saved_param('use_full_spp_names') eq 'no')
			{	#	truncate the genus to a single letter
				foreach my $h (values %$a)
				{	if ($h->[1])
					{	$h = substr($h->[0], 0, 1).". ".$h->[1];
					}
					else
					{	$h = $h->[0];
					}
				}
			}
			else
			{	$_ = join(" ", @$_) foreach values %$a;
			}

			$self->{'species_hash'} = $a;
			return $a;
		} else {
			$self->{'species_hash'} = {};
			return {};
		}
	}
}


##### Synchronizing sessions #####

sub __synchronize_session {
	my $self = shift;
	my $q = shift || $self->get_cgi;

	print STDERR "Starting __sync_session\n";
#	print STDERR "session before: ".Dumper($self)."\n";

	$self->{params}{saved} = {} unless $self->{params}{saved};
	$self->{params}{current} = {};
#	for ('current', 'saved')
#	{	$self->{params}{$_} = {} if !$self->{params}{$_};
#	}
	my %params = $q->Vars;
	my $ses_type = $self->ses_type;
#	my $ses_params = $self->get_ses_type_params;
	
	#	set the persisting parameters in the 'saved' param hash
	foreach my $p (keys %params) {
	#	my @values = split ("\0", $params{$p});
		if (grep { $p eq $_ } &_persisting_params)
		{	$self->set_saved_param($p, $params{$p});
			#$self->set_saved_param($p, \@values);
		}
		elsif (grep { $p eq $_ } &_current_params)
	#	elsif (grep { $param eq $_ } @$ses_params)
		{	$self->set_current_param($p, $params{$p});
		}
	}

	#	check and set the apph filters
	$self->__trim_and_set_filters($q);

#	print STDERR "Params: ".Dumper($self->{'params'})."\n\n";

	#if ($self->get_param('format'))
	#{	$self->set_current_param('page_size', 'all');
	#}
#	print STDERR "session after: ".Dumper($self->{params})."\n";
}

sub search_sync {
	my $self = shift;
	my $q = $self->get_cgi;
	my %params = $q->Vars;
	
	my $action = $q->param('action');
	if ($action && $action eq 'new-search' || $action eq 'advanced_query')
	{	#	delete cached results
		$self->delete_cached_results;
	#	$self->delete_param($_) foreach &_search_params;
	}

	my $sc = $q->param('search_constraint') || $self->get_default_param('search_constraint');
	$self->set_current_param('search_constraint', $sc);

	my $exact = $q->param('exact_match');
	if ($exact && $exact eq 'yes')
	{	$self->set_current_param('exact_match', 1);
	}
	
	foreach my $p ('termfields', 'gpfields', 'sppfields')
	{	my @list = split( /\0|,/, $params{$p}) if $params{$p};
		$self->set_current_param($p, [@list]) if @list;
	}
	
	my %q_hash;
	if ($params{query})
	{	my @queries = split /(\n|\0)/, $params{query};
		foreach my $q (@queries)
#		{	foreach ( split /(\n|\0)/, $q )
			{	#	get rid of any tracts of whitespace
				$q =~ s/(\t|\s{2,})/ /g;
				$q =~ s/^\s*(\S+.*?)\s*$/$1/;
				$q_hash{$q}++ if $q =~ /\w/;
			}
		#}
	}

	my $file = $q->param('idfile') || undef;
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
	{	$self->set_current_param('query', \%q_hash);
	}
}

sub search_sync_old {
	my $self = shift;
	my $q = $self->get_cgi;
	my %params = $q->Vars;
	
	my $action = $self->get_current_param('action');
	if ($action && $action eq 'new-search' || $action eq 'advanced_query')
	{	
		#	delete cached results
		$self->delete_cached_results;
		$self->delete_param($_) foreach &_search_params;
#		$self->__delete_fields(-query=>'1', -fields=>[&_search_params]);
	}

	
	$self->__copy_fields(-fields=>['search_constraint']);
	$self->__copy_or_delete(-fields=>['exact_match', 'termfields', 'gpfields', 'sppfields']);

	my %q_hash;
	if ($params{query})
	{	my @queries = split /(\n|\0)/, $params{query};
		foreach my $q (@queries)
#		{	foreach ( split /(\n|\0)/, $q )
			{	#	get rid of any tracts of whitespace
				s/(\t|\s{2,})/ /g;
				s/^\s*(\S+.*?)\s*$/$1/;
				$q_hash{$_}++ if (/\w/);
			}
		#}
	}

	my $file = $q->param('idfile') || undef;
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
	{	$self->set_current_param('query', \%q_hash);
	}
}

sub graph_sync {
	my $self = shift;

	#	the params that we are interested in are
	#	action, format, term, open_0, open_1, closed

	#print STDERR "session: ".Dumper($self)."\n";
	my $cgi = $self->get_cgi;
	my %cgi_params = $cgi->Vars;

#	foreach (&_tree_params)
#	{	my $v = $self->get_current_param($_);
#		$tree->{$_} = $v if $v;
#	}

	my $action = $cgi->param('action'); #$self->get_current_param('action');
	if ($self->ses_type eq 'graphviz' || $cgi->param('format'))
	{	$action = 'permalink';
	}
	my $tree;
	my @tree_params = &_tree_params;

	print STDERR "tree pre: ".Dumper($tree);


	print STDERR "action: ". ( $action || 'undefined' )."\n";

	if (!$action || $action eq 'reset-tree' || $action eq 'set-tree')
	{	#	dump the tree
		$self->delete_current_param($_) foreach @tree_params;
	}
	else
	{	#	see if we've got a saved tree, and if so, load it
		foreach (@tree_params)
		{	$tree->{$_} = $self->get_current_param($_);
		}
#		$tree = $self->load_cached_results('tree');
	}

	if ($action) {
		if ($action eq 'reset-tree') {
			if ($cgi_params{term})
			{	$tree->{open_0} = GO::CGI::Utilities::get_valid_list($cgi_params{term});
			}
		}
		elsif ($action eq 'set-tree') {
			#	delete all current tree settings and
			#	replace them with those from the cgi
			foreach my $p (@tree_params)
			{	#	move the params from the cgi in
				if ($cgi_params{$p})
				{	$tree->{$p} = GO::CGI::Utilities::get_valid_list($cgi_params{$p});
				}
			}

			if ($cgi_params{term})
			{	$tree->{open_0} = GO::CGI::Utilities::get_valid_list($cgi_params{term});
			}

		}
		elsif ($action eq 'minus_node') {
			#	the term in 'target' has been closed
			my $target = $cgi->param('target');
			print STDERR "closing node target, $target\n";
			#	add it to the list of closed nodes
			$tree->{closed} = GO::CGI::Utilities::add_value_to_list($tree->{closed} || undef, $target);
			#	remove it from the open_1 node list (if it is present)
			$tree->{open_1} = GO::CGI::Utilities::remove_value_from_list($tree->{open_1} || undef, $target);
		}
		elsif ($action eq 'plus_node') {
			#	the term in 'target' has been opened
			my $target = $cgi->param('target');
			print STDERR "opening node target, $target\n";
			#	add it to the list of open nodes
			$tree->{closed} = GO::CGI::Utilities::remove_value_from_list($tree->{closed} || undef, $target);
			#	remove it from the closed node list (if it is present)
			$tree->{open_1} = GO::CGI::Utilities::add_value_to_list($tree->{open_1} || undef, $target);
		}
		#	other actions - e.g. filtering, permalink: do nothing
	}
	
	print STDERR "tree post: ".Dumper($tree)."\n";
	
	$self->set_current_param($_, $tree->{$_}) foreach @tree_params;
	$self->save_cached_results($tree, 'tree');
	
#			print STDERR "session: ".Dumper($self)."\n";
#			$self->__delete_fields(-query=>'1', -fields=>[&_tree_params]);
#			$self->__transfer_fields(-fields=>[&_tree_params]);

=cut
		}

		if ($action eq 'minus_node') {
			my $target = $self->get_param('target');
			$self->__append_field(-field=>'target', -to_field=>'closed');
			$self->__remove_values(-field=>'target', -to_field=>'open_1');
		} elsif ($action eq 'plus_node') {
			my $target = $self->get_param('target');
			$self->__remove_values(-field=>'target', -to_field=>'closed');
#			if ($self->get_param('depth') && $self->get_param('depth') == 0) {
#				$self->__append_field(-field=>'target', -to_field=>'open_0');
#			} else {
				$self->__append_field(-field=>'target', -to_field=>'open_1');
#			$self->__set_param(-field=>'last_action', -value=>["opened $target"]);
#			}
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
#			$self->__set_param(-field=>'last_action', -value=>["set the tree"]);
			if ($self->get_param('term')) {
				print STDERR "found param term\n";
				$self->__append_field(-field=>'term', -to_field=>'open_0');
				$self->__move_field(-field => 'term',
				                    -to_field => 'open_0');
			}
		}
		#	other actions - e.g. filter: no change to the tree
	}
	else {
	#	start afresh with a beautiful new tree
		$self->__delete_fields(-query=>'1', -fields=>[&_tree_params]);
#		$self->__set_param(-field=>'last_action', -value=>["reset the tree"]);
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
	$self->__delete_fields(-query=>'current', -fields=>['term']);

#	print STDERR "session post-graph-sync: ".Dumper($self->{params})."\n";
=cut
}

sub new_graph_sync {
	my $self = shift;
	my $params = shift;

	#	the params that we are interested in are
	#	action, format, term, open_0, open_1, closed

	print STDERR "params in: ".Dumper($params)."\n";

	my $action = $params->{action};

	my $tree = $params->{tree};
	
	if (!$action && ($params->{'format'} || $self->ses_type eq 'graphviz'))
	{	$action = 'permalink';
	}
	print STDERR "action: ". ( $action || 'undefined' )."\n";

	my @tree_params = &_tree_params;

	if (!$action)
	{	#	dump the tree
		print STDERR "no action: dumping the tree\n";
		undef $tree;
	}
	elsif ($action eq 'reset-tree') {
		# this only happens in the term-details cgi
#		$tree->{open_0} = GO::CGI::Utilities::get_valid_list($params->{term}) if $params->{term};
	}
	elsif ($action eq 'set-tree') {
		# this only happens in the term-details cgi
		$tree->{open_0} = GO::CGI::Utilities::get_valid_list($tree->{term}) if $tree->{term};
		delete $tree->{term}
	}
	elsif ($action eq 'minus_node') {
		#	the term in 'target' has been closed
		my $target = $params->{target};
		print STDERR "closing node target, $target\n";
		#	add it to the list of closed nodes
		$tree->{closed} = GO::CGI::Utilities::add_value_to_list($tree->{closed} || undef, $target);
		#	remove it from the open_1 node list (if it is present)
		$tree->{open_1} = GO::CGI::Utilities::remove_value_from_list($tree->{open_1} || undef, $target);
	}
	elsif ($action eq 'plus_node') {
		#	the term in 'target' has been opened
		my $target = $params->{target};
		print STDERR "opening node target, $target\n";
		#	add it to the list of open nodes
		$tree->{closed} = GO::CGI::Utilities::remove_value_from_list($tree->{closed} || undef, $target);
		#	remove it from the closed node list (if it is present)
		$tree->{open_1} = GO::CGI::Utilities::add_value_to_list($tree->{open_1} || undef, $target);
	}
	else #	other actions, e.g. filtering, permalink, set tree
	{	#	get the params from the cgi
		foreach my $p (@tree_params)
		{	#	move the params from the cgi in
			$tree->{$p} = GO::CGI::Utilities::get_valid_list($tree->{$p}) if $tree->{$p};
		}
#		$tree->{open_0} = GO::CGI::Utilities::get_valid_list($params->{term}) if ($params->{term});
	}

	foreach (keys %$tree)
	{	delete $tree->{$_} unless defined $tree->{$_};
	}
	
	print STDERR "tree post: ".Dumper($tree)."\n";

	return $tree;
#	$self->set_current_param($_, $tree->{$_}) foreach @tree_params;
#	$self->save_cached_results($tree, 'tree');
}


sub term_sync {
	my $self = shift;
#	print STDERR "session before sync:\n".Dumper($self)."\n";
	my $cgi = $self->get_cgi;
	my %cgi_params = $cgi->Vars;
	my $action = $cgi->param('action'); #$self->get_current_param('action');
	if ($self->ses_type eq 'graphviz' || $cgi->param('format'))
	{	$action = 'permalink';
	}
	my $tree;
	my @tree_params = &_tree_params;

	print STDERR "tree pre: ".Dumper($tree);


	print STDERR "action: ". ( $action || 'undefined' )."\n";

	if (!$action || $action eq 'reset-tree' || $action eq 'set-tree')
	{	#	dump the tree
	}
	else
	{	#	see if we've got a saved tree, and if so, load it
		$tree = $self->load_cached_results('tree');
	}

	delete $tree->{open_0} if $tree->{open_0};

	if ($action) {
		if ($action eq 'minus_node') {
			#	the term in 'target' has been closed
			my $target = $cgi->param('target');
			print STDERR "closing node target, $target\n";
			#	add it to the list of closed nodes
			$tree->{closed} = GO::CGI::Utilities::add_value_to_list($tree->{closed} || undef, $target);
			#	remove it from the open_1 node list (if it is present)
			$tree->{open_1} = GO::CGI::Utilities::remove_value_from_list($tree->{open_1} || undef, $target);
		}
		elsif ($action eq 'plus_node') {
			#	the term in 'target' has been opened
			my $target = $cgi->param('target');
			print STDERR "opening node target, $target\n";
			#	add it to the list of open nodes
			$tree->{closed} = GO::CGI::Utilities::remove_value_from_list($tree->{closed} || undef, $target);
			#	remove it from the closed node list (if it is present)
			$tree->{open_1} = GO::CGI::Utilities::add_value_to_list($tree->{open_1} || undef, $target);
		}
		#	other actions - e.g. set / reset filters: do nothing
	} else {
		#hmm when to clear up, change filter should not!! how to check that?
		unless ($self->get_cgi->param('ch_filter')) {

			undef $tree;
		#	$self->__remove_values(-field=>'closed',-to_field=>'closed');
		#	$self->__delete_fields(-query=>'1',-fields=>['closed']);

		#	$self->__remove_values(-field=>'open_1',-to_field=>'open_1');
		#	$self->__delete_fields(-query=>'1',-fields=>['open_1']);
		}
	}
	
	
#	$self->__delete_fields(-query=>'current', -fields=>['open_0']);
#	print STDERR "\nsession after sync:\n".Dumper($self)."\n";
	
	print STDERR "tree post: ".Dumper($tree)."\n";
	
	$self->set_current_param($_, $tree->{$_}) foreach @tree_params;
	$self->save_cached_results($tree, 'tree');
	
}

sub adv_search_sync {
	my $self = shift;
	$self->delete_all_params('current');
#	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, &_tree_params]);
}

sub term_assoc_sync {
	my $self = shift;
	foreach (&_search_params, &_tree_params, 'gp')
	{	$self->delete_current_param($_);
	}
#	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, &_tree_params]);
}

sub gp_sync {
	my $self = shift;
	foreach (&_search_params, &_tree_params, 'term')
	{	$self->delete_current_param($_);
	}
}

sub blast_sync {
	my $self = shift;
	foreach (&_search_params, &_tree_params, 'term', 'gp')
	{	$self->delete_current_param($_);
	}
	$self->set_current_param($self->get_cgi->param('use_filters'));

#	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, &_tree_params]);
#	$self->__copy_or_delete(-fields=>['use_filters']);
#	$self->__transfer_fields(-fields=>[&_blast_params]);
}

sub prefs_sync {
	my $self = shift;
	foreach my $f (&_filter_fields)
	{	my $v = $self->get_cgi->param('disable_'.$f.'_filter');
		if ($v)
		{	$self->set_param(-query=>'user', -field=>'disable_'.$f.'_filter', -value=>1);
			$self->__delete_fields(-fields=>[$f]);
		}
	}
	if ($self->get_cgi->param('tree_view') eq 'compact')
	{	$self->set_param(-query=>'1', -field=>'tree_view', -value=>['compact']);
		$self->__delete_fields(-fields=>['tree_view']);
	}
}

=head2 __trim_and_set_filters

	Usage: $session->__trim_and_set_filters;

	Sets the apph filters, moves the filter params
	from 'current' to '1'.
	Checks that each field is enabled globally
	and that the user hasn't disabled it; also ensures that
	if 'all' is selected as an option, the filter isn't set
	The evcode filter has an additional check for 'aca', a deprecated
	code meaning 'all curator approved'.
=cut

sub __trim_and_set_filters {
	my $self = shift;
	my $cgi = shift || $self->get_cgi;
	my %params = $cgi->Vars;
	my $apph = $self->apph;

	print STDERR "saved before: ".Dumper($self->{params}{saved});

	my %filters;
	my $action = $cgi->param('action');

	if ($action)
	{	if ($action eq 'reset-filters')
		{	$apph->filters(\%filters);
			$self->delete_param($_) foreach &_filter_fields;
			#	delete any cached results
			$self->delete_cached_results;
			return;
		}
		elsif ($action eq 'filter')
		{	#	delete any cached results
			$self->delete_cached_results;
		}
	}

	#	get the active filter fields
	my $active_filters = $self->get_active_filter_fields;

	my $evcode = 0;
	if (GO::CGI::Utilities::get_environment_param('ieas_loaded') && grep { 'evcode' } @$active_filters && $params{'evcode'})
	{	#	evcode has to be handled slightly differently if IEAs
		#	are present, so remove it from the list of filters
		$evcode = 1;
		my @other_filters = grep { $_ ne 'evcode' } @$active_filters;
		$active_filters = [ @other_filters ];
	}
	
	print STDERR "active filters (possibly minus evcode): ".Dumper($active_filters)."\n";

	my $to_delete;
	foreach my $f (@$active_filters)
	{	if (!$params{$f})
		{	#	set the filter using the values in the saved params
			$filters{$f} = $self->get_saved_param($f) if $self->get_saved_param($f);
			next;
		}
		my $p_list = GO::CGI::Utilities::get_valid_list( [split("\0", $params{$f})] );
		print STDERR "f: $f; list: ".Dumper($p_list)."\n";
		if (!@$p_list  # no values present
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
		{#	print STDERR "p_list size: ".(scalar @$p_list)."; param list size: ".$self->get_param_list_size($f)."\n";
			$filters{$f} = \@$p_list;
			$self->set_saved_param($f, \@$p_list);
#			$self->set_param(-query=>'1', -field=>$f, -value=>$p_list);
			print STDERR "set $f filters.\n";
		}
	}

	#	evcode
	if ($evcode)  # this will only be on if IEAs are loaded
	{	my $p_list = GO::CGI::Utilities::get_valid_list( split("\0", $params{'evcode'}) );
			#	print STDERR "f: $f; list: ".join(", ", @{$p_list || []})."\n";
		if (!@$p_list ||  # no values present
			grep { $_ eq 'all' } @$p_list)  # the list contains 'all'
		{	#	delete the param
			push @$to_delete, 'evcode';
		} elsif (grep { $_ eq 'aca' } @$p_list) {
			#	parameter setting 'all curator approved' is on
			if (!grep { $_ eq 'iea' } @$p_list) {
				#	IEAs are loaded, ACA is the search set, IEA not in the search set
				print STDERR "Param IEAS LOADED is ON\n";
				$filters{'evcode'} = ['!IEA'];
			} else {
				#	IEA *and* ACA are on ==> redundant parameter
				push @$to_delete, 'evcode';
			}
		}
		elsif ($self->get_param_list_size('evcode') == scalar @$p_list)
		#	slight cheat here: param list size is the same as
		#	number of selected params
		{	push @$to_delete, 'evcode';
		}
		else
		{	$filters{'evcode'} = \@$p_list;
			$self->set_saved_param('evcode', \@$p_list);
			print STDERR "set evcode filters.\n";
		}
	}

	$self->delete_saved_param($_) foreach @$to_delete;
	
#	$self->get_cgi->delete($_) foreach @$to_delete;
	
#	$self->delete_current_param($_) foreach @$active_filters;
	$apph->filters(\%filters);

	print STDERR "apph filters: ".Dumper(\%filters);
	print STDERR "Set apph filters!\n";

	#	check whether it's going to be ok to get gp counts
	if (!keys %filters)
	{	$self->gp_count_ok(1);
	}
	else
	{	my @gp_count_correct_fields = &_gp_count_correct_fields;
		
		print STDERR "gp count correct fields: ".Dumper(\@gp_count_correct_fields)."\n";
		
		#	check if there are filters which aren't in gp_count_correct_fields
		my $set;
		foreach my $f (keys %filters)
		{	print STDERR "f = $f\n";
			if (!grep { $f eq $_ } @gp_count_correct_fields)
			{	print STDERR "$f is not in gp_count_correct\n";
				$self->gp_count_ok(0);
				$set = 1;
				last;
			}
		}
		$self->gp_count_ok(1) if !$set;
	}
	
	print STDERR "gp_count_ok: ".$self->gp_count_ok."\n";
	print STDERR "saved after: ".Dumper($self->{params}{saved});

}



##### Useful parameter lists #####

sub _global_current_params {
	return (qw(page page_size format action term gp), &_filter_fields);
}

sub _current_params {
	return qw(term_assocs term_context show_blast_scores);
}

# these are all single scalar values
sub _persisting_params {
#	return qw(reltype show_associations term_context termsort gpsort sppsort session_id);
	my @list = qw(termsort gpsort sppsort tree_view);
	push @list, &_blast_params, &_graphviz_params, map { 'disable_'.$_.'_filter' } &_filter_fields;
	return @list;
}

sub _search_params {
	return qw(termfields gpfields sppfields search_constraint exact_match query);
	#	action format
}

sub _tree_params {
	return qw(open_0 open_1 closed);
}

sub _blast_params {
	return qw(threshold maxhits blast_filter use_filters);
}

sub _graphviz_params {
	return qw(layout graph_bgcolor graph_textcolor);
}

sub _term_assoc_params {
	
}

=head2 _filter_fields

A list of all the fields that can be filtered by AmiGO

=cut

sub _filter_fields {
	return (&_gp_filters, &_assoc_filters, &_ont_filters);
}

=head2 _gp_count_correct_fields

The fields for which the gp count is correctly calculated

=cut

sub _gp_count_correct_fields {

	return qw(ont speciesdb); # taxid);

}

=head2 _gp_filters

Filterable gene product fields

=cut

sub _gp_filters {
	return qw(speciesdb taxid gptype);
}

=head2 _assoc_filters

Filterable association fields

=cut

sub _assoc_filters {
	return qw(evcode qual assby); #assocdate);
}

=head2 _ont_filters

Filterable ontology fields

=cut

sub _ont_filters {
	return ('ont');
}


##### Getting parameter lists #####

=head2 get_active_filter_fields

Usage: my $active = $session->get_active_filter_fields;

Returns a ref to an array of the filter fields
enabled in the current AmiGO session

=cut

sub get_active_filter_fields {
	my $self = shift;
	if ($self->{active_filter_fields})
	{	return $self->{active_filter_fields};
	}
	
	print STDERR "getting active filter fields...\n";
	my @active;
	foreach (&_filter_fields)
	{	push @active, $_ if (GO::CGI::Utilities::get_environment_param('show_'.$_.'_filter') && !$self->get_saved_param('disable_'.$_.'_filter'));
	}
	$self->{active_filter_fields} = [@active];
	return $self->{active_filter_fields};
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
#	my @a = keys %{$self->get_misc_hash->{reltype}};
	my @a = @{$self->get_misc_hash->{reltype}};
	unless (scalar(@a)) {@a = qw(is_a part_of)}
	return ['all', @a];
}

sub get_ontology_list {
	my $self = shift;
#	my @a = grep{$_}keys %{$self->get_misc_hash->{ontology}};
	my @a = @{$self->get_misc_hash->{ontology}};
	unless (scalar(@a)) {@a = qw(cellular_component biological_process molecular_function)}
#	return ['all', sort{$a cmp $b}@a];
	return [@a];
}
*get_ont_list = \&get_ontology_list;

sub get_gptype_list {
	my $self = shift;
#	my @a = grep{$_}keys %{$self->get_misc_hash->{gptype}};
	my @a = @{$self->get_misc_hash->{gptype}};
	unless (scalar(@a)) {@a = qw(gene protein)}
	return [@a];
}

sub get_qual_list {
	my $self = shift;
#	my @a = grep{$_}keys %{$self->get_misc_hash->{qual}};
	my @a = @{$self->get_misc_hash->{qual}};
	unless (scalar(@a)) {@a = qw(contributes_to not)}
	return [@a];
}

sub get_assby_list {
	my $self = shift;
#	my @a = grep{$_}keys %{$self->get_misc_hash->{assby}};
	my @a = @{$self->get_misc_hash->{assby}};
	unless (scalar(@a)) {@a = qw(db1 db2)}
	return [@a];
}

sub get_speciesdb_list {
	my $self = shift;
#	my @a = keys %{$self->get_misc_hash->{speciesdb}};
	my @a = @{$self->get_misc_hash->{speciesdb}};
	unless (scalar(@a)) {
		@a = ('fb', 'sgd', 'mgi', 'genedb_spombe', 'uniprot', 'tair', 'ddb', 'wb', 'ensembl', 'rgd', 'tigr_cmr', 'tigrfams', 'tigr_ath1', 'tigr_tba1', 'gr', 'genedb_tsetse', 'genedb_tbrucei', 'genedb_pfalciparum', 'genedb_lmajor', 'zfin');
	}
#	return ['all', @a];
	return [@a];
}
*get_datasource_list = \&get_speciesdb_list;

sub get_evidence_code_list {
	my $self = shift;
#	my @a = keys %{$self->get_misc_hash->{evcode}};
	my @a = @{$self->get_misc_hash->{evcode}};
	unless (scalar(@a)) {@a = ('IC','IDA','IEP','IGI','IMP','IPI','ISS','NAS','ND','NR','RCA','TAS')}
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

=comment
#	these subs are unused

sub get_taxid_species_list_str {
	my $self = shift;
	my $hash = $self->get_reverse_species_hash;
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

	my $hash = $self->get_reverse_species_hash;

	my @a = sort keys %$hash;
	return \@a;
}
*get_species_key_list = \&get_species_keys;

sub get_species_taxids {
	my $self = shift;

	my $hash = $self->get_reverse_species_hash;

	my @a = sort values %$hash;
	return \@a;
}
*get_taxid_list = \&get_species_taxids;

sub get_species_acc {
	my $self = shift;
	my $sacc = shift;

	return $self->get_reverse_species_hash->{$sacc};
}

sub get_species_name {
	my $self = shift;
	my $taxa_id = shift || return;
	my $h = $self->get_reverse_species_hash;
	#for autocomplete, only one species
	my @a = grep{$h->{$_} == $taxa_id->[0]}keys %$h;
#	 printf STDERR "species: $a[0], taxa id:$taxa_id\n";
	return $a[0];
}
=cut

sub get_list {
	my $self = shift;
	my $param = shift;
#	print STDERR "get_list: param: $param\n";
	my %lists = (
		ont => $self->get_ontology_list,
		gptype => $self->get_gptype_list,
		assby => $self->get_assby_list,
		qual => $self->get_qual_list,
		speciesdb => $self->get_speciesdb_list,
		evcode => $self->get_evcode_list,
		maxhits => ['10', '20', '30', '40', '50', '60', '80', '100', '200', '500', '1000'],
		threshold => ['100', '10', '1.0', '0.1', '0.01', '0.001', '1e-5', '1e-25', '1e-50', '1e-100'],
		color => [ 'beige', 'black', 'blue', 'forestgreen', 'maroon', 'navy', 'pink', 'purple', 'red', 'skyblue', 'white', 'yellow' ],
		term_context => ['parents', 'sibling'],
		search_constraint => ['term', 'gp'],
		term_assocs => ['all','direct'],
		tree_view => ['full', 'compact'],
		blast_filter => ['on', 'off'],
		use_filters => ['on', 'off'],
		layout => ['vertical', 'horizontal'],
#		graph_bgcolor => $self->get_list('color'),
#		graph_textcolor => $self->get_list('color'),
	);
	
#	print STDERR "lists{param} = ".Dumper($lists{$param})."\n";
	
	return $lists{$param};
}

##### Utility functions for parameter lists #####

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

sub get_param_list_by_name {
	my $self = shift;
	my $param = shift;
	print STDERR "param = $param\n";
	
	my $sub = 'get_'.$param.'_list';
	my $list = [];
	eval {
		$list = $self->$sub;
	};
	return $list;
}

##### Preparing page templates #####

=head2 process_page_template

	Prepare the page for lift-off

	Arguments - vars - hash containing data to go into the template
	            session (if created)
	            session_type (if not in session->ses_type)

	Returns -   the standard set of vars required by most templates,
               plus any other vars in %var_hash


=cut

sub process_page_template {
	my ($vars, $self, $ses_type) = @_;

	$self = GO::CGI::Session->new(-read_only=>1) if !$self;
	$ses_type = $self->ses_type if !$ses_type;

	foreach (keys %$vars)
	{	if (!defined $vars->{$_})
		{	delete $vars->{$_};
		}
	}

	my $tmpl_vars = 
	{	%$vars,
		filterdata => $self->get_data_for_filters,
		search_constraint_data => $self->set_option('search_constraint'),
		session => $self,
		munger => $self->munger,
		session_id => $self->id,
		page_name => $self->ses_type,
		release_date => $self->{release_date},
		's' => 4,
		html_url => $self->get_default_param('html_url'),     # environment
		show_blast => $self->get_default_param('show_blast'), # environment
		amigo_url => $self->get_default_param('cgi_url'),
		image_dir => $self->get_default_param('html_url').'/images',  # env
		template_paths => $self->get_default_param('template_paths'), # env
	};

	print "Content-type:text/html\n\n";
	GO::Template::Template->process_template($ses_type.".tmpl", $tmpl_vars);
}


=cut
sub set_single_option {
	my $self = shift;
	my $param = shift;
	my $data;

	my $option_h = {
		term_context => ['parents', 'sibling'],
		search_constraint => ['term', 'gp'],
		term_assocs => ['all','direct'],
		tree_view => ['full', 'compact'],
		blast_filter => ['on', 'off'],
		use_filters => ['on', 'off'],
		layout => ['vertical', 'horizontal'],
		
	};
	
	if (!$option_h->{$param})
	{	return;
	}
	
	#	get the params and the human names
	my @option_l = map { $_ = { value => $_, label => $self->munger->get_human_name($_) } } @{$option_h->{$param}};

	my @select;
	my $selected = $self->get_param($param);
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
=cut
sub set_option {
	my $self = shift;
	my $param = shift;
	my $selected_only = shift || undef;
	my $all = 0;

	print STDERR "param: $param". ($selected_only ? " selected_only" : "") . "\n";

	#	get the current values of the parameter
	my $selected = $self->get_param($param) || $self->get_cgi_param($param);
#	print STDERR "selected: ".Dumper($selected)."\n";

	#	if there are no selected values...
	if (!$selected)
	{	#	get the defaults
		if ($param eq 'termfields' || $param eq 'gpfields')
		{	$selected = $self->get_search_field_list($param, 'default');
		}
		#	for other fields, if selected_only is on, return nothing
		#	(no values for that parameter)
		elsif ($selected_only)
		{	return;
		}
	}

	if ($selected && !ref($selected))
	{	$selected = [ $selected ];
	}
	
	#	get the human names for the param values
	#	and the rest of the values, if appropriate (i.e. selected_only NOT on)
	my $option_l;

###	BIG INSERT!

	if ($param eq 'evcode')
	{	my $list = $self->get_evcode_list;
	
		my @sorted = 
		map
		{	my @list = split("\0", $_);
			if ($self->ses_type eq 'prefs' || $self->ses_type eq 'advanced_query') {
				{ label => $list[0], value => $list[1] };
			}
			else {
				{ human => $list[0], label => $list[1], value => $list[1] };
			}
		}
		sort
		map
		{	join("\0", $self->munger->get_full_name($_), $_);
		} @$list;

		$option_l = [@sorted];
	}
	elsif ($param eq 'taxid')
	{	#my $taxid = $self->get_reverse_species_hash;
		my $hash = $self->get_species_hash;
		$option_l = [ 
		map { my @temp = split("\0", $_);
				{ value => $temp[1], label => $temp[0] };
			}
		sort
		map { join("\0", $hash->{$_}, $_) }
		keys %$hash ];

=cut
		#	check whether we have any custom species filters
		my $custom = $self->get_param_list('custom_taxid') || undef;
		if ($custom && @$custom)
		{	#	get the taxon info from the db
			my $apph = $self->apph;
			my $dbh = $apph->dbh;
			my $results = $dbh->selectall_arrayref("SELECT 'value', ncbi_taxa_id, 'label', CONCAT(genus,' ',species) FROM species WHERE ncbi_taxa_id IN (".join(",", @$custom).") ORDER BY genus, species");
			if ($results)
			{	print STDERR "results:\n".Dumper($results)."\n";
				map { my %hash = @$_; $_ = \%hash } @$results;
			#	map { $_ = \%{@$_} } @$results;
				print STDERR "results:\n".Dumper($results)."\n";
				unshift @taxa, @$results;
			}
		}
=cut
#		print STDERR "taxa:\n".Dumper(\@taxa)."\n";
		
#		$option_l = [@taxa];
	}
	elsif ($param =~ /(term|gp|spp)fields/)
	{	my $fields = $self->get_search_field_list($param);
		push @$fields, 'all';
		$option_l = [ map { { value => $_, label => $self->munger->get_field_name($_) } } @$fields ];
	}
	elsif ($param eq 'maxhits' || $param eq 'threshold' || $param eq 'blast_filter' || $param eq 'tree_view' || $param eq 'term_context' || $param eq 'term_assocs')
	{	#print STDERR "using the long or list option; param: ".$param."\n";
		my $list = $self->get_list($param);
	#	print STDERR "list: ".Dumper($list);
		
		$option_l = [ map { { label => $_, value => $_ } } @$list ];
	}
#	elsif ($param eq 'graph_textcolor' || $param eq 'graph_bgcolor')
#	{	my $list = $self->get_param_list_by_name('color');
#		$option_l = [ map { { label => $self->munger->get_human_name($_), value => $_ } } @$list ];
#	}
	else
	{	#print STDERR "param: $param\n";
		my $list = $self->get_list($param);
	#	print STDERR "list: ".Dumper($list)."\n";
		$option_l = [ map { { label => $self->munger->get_human_name($_), value => $_ } } @$list ];
	}

###	END BIG INSERT
	my @select;
	if ((!$selected || $selected->[0] eq 'all') && grep { $_ eq $param } @{$self->get_active_filter_fields})
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
			if ($param eq 'ont' || $param eq 'evcode')
			{	$_->{url} = $self->munger->get_GO_doc_url($param, $_->{value});
			}
			elsif ($param eq 'taxid')
			{	$_->{url} = $self->munger->get_url('ncbi_taxid', $_->{value});
			}
			elsif ($param eq 'speciesdb')
			{	$_->{url} = $self->munger->get_db_url($_->{value});
			}
		}
	}


	my $param_data = { title => $self->munger->get_human_name($param) };
	$param_data->{selected} = \@select unless (!@select);

	if ($selected_only)
	{	return $param_data;
	}

	#	Add an 'all' option if appropriate
	if (grep { $_ eq $param } (@{$self->get_active_filter_fields}))#, 'termfields', 'gpfields', 'sppfields'))
	{	if ($all == 1)
		{	unshift @$option_l, { value => 'all', label => 'All', selected => 1 };
		}
		else
		{	unshift @$option_l, { value => 'all', label => 'All'};
		}
	}
	
	$param_data->{data} = $option_l;
	return $param_data;
}

=head2 get_data_for_filters

usage : my $filterdata = $session->get_data_for_filters()

  Arguments - session,
              hashref containing
              selected => [ list of filters ] OR '*'
              
  Returns   - 'selected' returns only the active parameters for that filter
              'all' returns the list of all possible parameters
              
              e.g. the ontology filter
              say we have three ontologies, 'component', 'function' and 'process'
              currently we are filtering so that only terms in the 'component'
              ontology are shown
              selected => [ 'ontology' ] would return 'component'
              otherwise would return 'component', 'function', 'process'
              
              if nothing is specified, it will default to getting all the data
              for all filters

=cut

sub get_data_for_filters {
	my $self = shift;
#	my $filter_h = shift;
#	my $extra = shift;
	my $ses_type = $self->ses_type;
	my $filterdata;
	print STDERR "Running get_data_for_filters.\n";

#	print STDERR Dumper($filter_h);

#	these session types don't need to display any filter data
	if (grep { $ses_type eq $_ } qw(front amigo_message blast_waiting gp_details vocab_details spp_search graphviz))
	{	return $filterdata;
	}
	
#	active filters
	my @active = @{$self->get_active_filter_fields};

	print STDERR "active filters: ".join(", ", @active)."\n";

#	not all pages need all the filter data
#	any page which shows the current filters needs the 'selected' filter data
#	for pages where a certain filter parameter can be set, you need all the
#	filter data

#	ses_types which allow you to filter on all fields:
#	gp_search, blast_query, prefs, advanced_query

	my $allow_filter_by = {
	#	gp_count_correct fields
		term_details => [&_gp_count_correct_fields],
		term_chart => [&_gp_count_correct_fields],

	#	gp_count_correct fields, ont
		subset_details => [&_gp_count_correct_fields, 'ont'],
		browse => [&_gp_count_correct_fields, 'ont'],

	#	all fields except ont
		term_assoc => [ grep { $_ ne 'ont' } @active ],

		#	filter on ont
		term_search => ['ont'],
#		gp_assoc_view => ['ont'],		#	ont only

		#	ont, qual, assby, evcode
		gp_assoc => ['ont', 'qual', 'assby', 'evcode'],

		#	just show the filter, don't allow filtering
		blast_submit => [0],

	};

	if (!$allow_filter_by->{$ses_type})
	{	#print STDERR "No selected found. Getting all data...\n";
		#	no specific data required, so we'll just get everything
		foreach my $f (@active)
		{	$filterdata->{$f} = $self->set_option($f);
		}
	}
	else
	{	#print STDERR "Found some selected. Getting data...\n";
		foreach my $f (@active)
		{	if (grep { $f eq $_ } @{$allow_filter_by->{$ses_type}})
			{	#print STDERR "Found $f in filter_h!\n";
				$filterdata->{$f} = $self->set_option($f);
			}
			else
			{	#print STDERR "Didn't find $f in filter_h!\n";
				$filterdata->{$f} = $self->set_option($f, 'selected_only');
			}
		}
	}

#	unless ($ses_type =~ /goslim/)
#	{	foreach my $f (@active)
#		{	$filterdata->{$f} = $self->set_option($f);
#		}
#	}

#	print STDERR "filterdata: ".join(", ", keys %$filterdata)."\n";
#	print STDERR "filterdata: ".Dumper($filterdata)."\n";

	return $filterdata;
}

#sub get_searchfield_data {
#	my $self = shift;
#	my $data;
#	
#	foreach my $f ('termfields', 'gpfields')
#	{	$data->{$f} = $self->set_option($f);
#	}
#	
#	return $data;
#}

#sub get_graphviz_data {
#	my $self = shift;
#	my $data;
#
#	my $default_graph_bgcolor = 'white';
#	my $default_graph_textcolor = 'blue';
#	
#	foreach my $x ('graph_bgcolor', 'graph_textcolor', 'layout')
#	{	$data->{$x} = $self->set_option($x);
#	}
#	
#	print STDERR "graphviz data: ".Dumper($data)."\n";
#	
#	return $data;
#}

1;
