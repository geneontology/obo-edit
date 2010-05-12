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
use Template;
#use Template::Constants qw( :all );

use GO::AppHandle;
use GO::Utils qw(rearrange);
use GO::CGI::Utilities qw(:all);
use GO::CGI::NameMunger;

$Data::Dumper::Indent = 1;

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
@ISA = ('Exporter');
@EXPORT = qw(process_page_template);
@EXPORT_OK = qw(process_page_template);

our $verbose = get_environment_param('verbose');

=head2 new

-args -ses_type   session type (defaults to 'amigo_message')
      -q          CGI,
      -read_only  don't save the session after loading and syncing
      -temp       create a temporary session (e.g. for outputting
                  fatal error messages)

returns  GO::CGI::Session;

=cut

sub new {

  print STDERR "Session::new: Starting!\n" if $verbose;
  my $class = shift;
  my $self = {};
  bless $self, $class;
  my ($q, $ses_type, $read_only, $temp, $apph) =
    rearrange([qw(q ses_type read_only temp apph)], @_);

  #	set the session type
  $ses_type = 'amigo_message' if !$ses_type;
  $self->ses_type($ses_type);

  print STDERR "session type: ".$self->ses_type."\n" if $verbose;

  # $self->__clear_sessions;
  if ($q && $q->param('session_id')){
    my $ses_id = $q->param('session_id');
    # check the session ID is OK before we do anything else
    if ($self->is_valid_session_id($ses_id)){
      $self->id($ses_id);
      $self->__load_session($ses_id) unless $temp;
    }else{
		  #	invalid session ID. Delete it!
		  $q->delete('session_id');
		  $self->__create_session_id;
		}
	}
	else
	{	$self->__create_session_id;
	}

	if ($apph)
	{	$self->{apph} = $apph;
	}
	else
	{	$apph = $self->apph;
	}

	# $self->{'params'} is now what holds the data for performing
	# a query. The CGI values will no longer be directly used.
	$self->set_cgi($q);

	return $self if $temp;

	$self->__synchronize_session($q);
	$self->__save_session;# unless $read_only;

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
	my $session_id = &__create_amigo_session_id;
	$self->id($session_id);
}

## non-OO version of the above
sub __create_amigo_session_id {
	my $session_id = "";
	$session_id = int(rand(10000));
	$session_id .= "amigo";
	$session_id .= time;
	return $session_id;
}


##### Other handy things #####

sub apph {
	my $self = shift;
	if (!$self->{'apph'}) {
		$self->{'apph'} = create_apph;
	}
	return $self->{'apph'};
}

#sub munger {
#	my $self = shift;
#	if (!$self->{'munger'}) {
#		$self->{'munger'} = GO::CGI::NameMunger->new();
#	}
#	return $self->{'munger'};
#}

#get / set session type
sub ses_type {
	my $self = shift;
	$self->{_ses_type} = shift @_ if (@_);
	return $self->{_ses_type};
}

#get / set gp_count_ok
sub gp_count_ok {
	my $self = shift;
	if (@_)
	{	my $val = shift;
		$self->{_gp_count_ok} = $val;
		$self->show_counts('gp', 1) if $val ne '0';
	}
	return $self->{_gp_count_ok};
}

sub show_counts {
	my $self = shift;
	my $count_type = shift;
	if (@_)
	{	$self->{"_show_".$count_type."_counts"} = shift;
	}
	elsif ($self->{"_show_".$count_type."_counts"})
	{	#
	}
	else
	{	$self->{"_show_".$count_type."_counts"} = $self->get_saved_param("calculate_".$count_type."_counts") || 0;
	}
	return $self->{"_show_".$count_type."_counts"};
}

sub amigo_default {
	my $pname = shift;
	
#	print STDERR "pname: $pname\n" if $verbose;
	
	my %defaults =
	(	dbname => "go",
		dbhost => "localhost",
		search_constraint => 'term',
		view => "tree",
#		termfields => ['name', 'term_synonym'],
#		gpfields => ['symbol', 'full_name', 'product_synonym'],
#		termsort => 'rel',
#		gpsort => 'rel',
#		sppsort => 'rel',
		term_assocs => 'all',
		term_context => 'parents',
		threshold => 0.1,
		maxhits => 50,
		blast_filter => 'on',
		max_selected_gps => 50,
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

#	print STDERR "param: $param\n" if $verbose;

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
	return get_environment_param($param) || amigo_default($param);
}

#	non-OO version of the above
sub get_amigo_default_param {
	my $param = shift || return;
	return get_environment_param($param) || amigo_default($param);
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

	print STDERR "param = $param; value = ".Dumper($value) if $verbose;

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
	
	print STDERR "param: $param; value: $value\n" if $verbose;
	
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
	{	print STDERR "No session ID set!\n" if $verbose;
		$self->suicide_message("AmiGO configuration error: no session ID!");
	}

	my $ses_data_dir = $ses_dir.'/'.$ses_id.'_data';

	if (!new DirHandle($ses_data_dir)) {
		print STDERR "Eval-ing the create dir command\n" if $verbose;
		eval {
			mkdir($ses_data_dir, 0755);
			`chmod a+rw $ses_data_dir`;
		};
		if ($@)
		{	print STDERR "Error: $@" if $verbose;
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
		print STDERR "Eval-ing the create dir ( $blast_dir ) command\n" if $verbose;
		eval {
			mkdir($blast_dir, 0755);
			`chmod a+rw $blast_dir`;
		};
		if ($@)
		{	print STDERR "Error: $@" if $verbose;
			$self->suicide_message("AmiGO configuration error: $@");
		}
	}
	return $blast_dir;
}

=head2 get_tmp_images_dir

usage : my $tmp_images_dir = $session->get_tmp_images_dir;

Returns the location of the temporary images directory
(for storing GraphViz images)

=cut

sub get_tmp_images_dir {
	my $self = shift;
	return $self->get_default_param('temp_image_dir');
}

sub __load_session {
	print STDERR "Session::__load_session: Starting!\n" if $verbose;
	my $self = shift;
	my $session_id = shift || $self->id;
	my $ses_data_dir = $self->get_session_data_dir;

	if (!$session_id)
	{	print STDERR "Warning: no session ID found. Error?\n" if $verbose;
		return;
	}

	my $read_file = new FileHandle;

	print STDERR "Looking for $ses_data_dir/params\n" if $verbose;
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
	{	print STDERR "Error: $@" if $verbose;
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
	print STDERR "Session::__save_session: Starting!\n" if $verbose;
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
	print STDERR "Session::load_cached_results: Starting!\n" if $verbose;
	my $self = shift;
	my $ses_type = shift || $self->ses_type;
	if (!$ses_type)
	{	print STDERR "No session type set; cannot retrieve results\n" if $verbose;
		return;
	}
	my $ses_data_dir = $self->get_session_data_dir;

	$ses_type .= "_cache";
	my $read_file = new FileHandle;

	print STDERR "Looking for $ses_data_dir/$ses_type \n" if $verbose;
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
	{	print STDERR "Error: $@" if $verbose;
		$self->suicide_message("AmiGO configuration error: $@");
	}
	return;
#	$self->{__backup_loaded};
}

sub save_cached_results {
	print STDERR "Session::save_cached_results: Starting!\n" if $verbose;
	my $self = shift;
	my $results_to_cache = shift;
	my $ses_type = shift || $self->ses_type;
	my $ses_data_dir = $self->get_session_data_dir;
	if (!$ses_type)
	{	print STDERR "No session type set; cannot save results\n" if $verbose;
		return;
	}

#	print STDERR "results to cache: ".Dumper($results_to_cache)."\n" if $verbose;

	if ($results_to_cache)
	{	print STDERR "Ready for some caching action!\n" if $verbose;
		$ses_type .= "_cache";
		my $file = new FileHandle;
		if ($file->open("> $ses_data_dir/$ses_type")) {
			print $file Dumper($results_to_cache);
			$file->close;
		}
	}
}

sub delete_cached_results {
	print STDERR "Session::delete_cached_results: Starting!\n" if $verbose;
	my $self = shift;
	my $ses_type = shift || $self->ses_type;
	if (!$ses_type)
	{	print STDERR "No session type set\n" if $verbose;
		return;
	}

	my $ses_data_dir = $self->get_session_data_dir;
	$ses_type .= "_cache";

	print STDERR "Looking for $ses_data_dir/$ses_type...\n" if $verbose;
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
	print STDERR "Session::delete_all_cached_results: Starting!\n" if $verbose;
	my $self = shift;
	my $ses_data_dir = $self->get_session_data_dir;
	eval {
			`rm -rf $ses_data_dir/*_cache`;
	};
}

sub save_graphviz_image {
	print STDERR "Session::save_graphviz_image: Starting!\n" if $verbose;
	my $self = shift;
	my $graphviz = shift;
	my $ses_id = $self->id;

	if (!$ses_id)
	{	print STDERR "Session ID not set!\n" if $verbose;
		$self->suicide_message("AmiGO configuration error: no session ID!");
	}

	my $tmp_img_dir = $self->get_tmp_images_dir . '/' . $ses_id;

	if (!new DirHandle($tmp_img_dir)) {
		print STDERR "Session::save_graphviz_image: Eval-ing the create dir command\n" if $verbose;
		eval {
			mkdir($tmp_img_dir, 0755);
			`chmod a+rw $tmp_img_dir`;
		};
		if ($@)
		{	print STDERR "Session::save_graphviz_image: Error: $@" if $verbose;
			$self->suicide_message("AmiGO configuration error: $@");
		}
	}

	my $num = int(rand(1000));
	my $file_name = $num . time() . ".png";
	my $img_url;
	my $html;

#	print STDERR "Session::save_graphviz_image: tmp_img_dir: $tmp_img_dir; file name: $file_name\n";
	my $fh = new FileHandle;
	if ($fh->open( "> $tmp_img_dir/$file_name" ))
	{	print $fh $graphviz->as_png;
		$fh->close;
	}
	else
	{	print STDERR "Session::save_graphviz_image: Error! Could not write the image file.\n";
		$self->suicide_message("AmiGO configuration error: could not write the image file.\n");
	}

	chmod (0666, "$tmp_img_dir/$file_name");
	$img_url = "tmp_images/$ses_id/$file_name";
#	print STDERR "img_url: $img_url\n" if $verbose;

	$html = $graphviz->as_cmapx;
	#	remove the \ns from the title attributes
	$html =~ s/\\n/ /g;
	
	return { img_url => $img_url, html => $html };
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

#	get the species/database mapping
#	this is a temporary hack until the db is sorted out
sub get_spp_db_mapping {
	print STDERR "Session::get_spp_db_mapping: Starting!\n" if $verbose;
	my $self = shift;
	if ($self->{'spp_db_hash'}) {
		return $self->{'spp_db_hash'};
	} else {
		if (-f "spp_db_map.pl") {
			open (FILE, 'spp_db_map.pl');
			my $hash;
			while (<FILE>) {
				$hash .= $_;
			}
			my ($a) = thaw $hash;
			$self->{'spp_db_hash'} = $a;
			return $a;
		} else {
			$self->{'spp_db_hash'} = {};
			return {};
		}
	}
}


##### Synchronizing sessions #####

sub __synchronize_session {
	print STDERR "Session::__synchronize_session: Starting!\n" if $verbose;
	my $self = shift;
	my $cgi = shift;

	$self->{params}{saved} = {} unless $self->{params}{saved};
	$self->{params}{current} = {};
#	for ('current', 'saved')
#	{	$self->{params}{$_} = {} if !$self->{params}{$_};
#	}
	my %params = $cgi->Vars;
	my $ses_type = $self->ses_type;
	my $ses_params = $self->get_ses_type_params;
	
	#	set the persisting parameters in the 'saved' param hash
	foreach my $p (keys %params) {
	#	my @values = split ("\0", $params{$p});
		if (grep { $p eq $_ } &_persisting_params)
		{	$self->set_saved_param($p, $params{$p});
			#$self->set_saved_param($p, \@values);
		}
	#	elsif (grep { $p eq $_ } &_current_params)
		elsif (grep { $p eq $_ } @$ses_params)
		{	$self->set_current_param($p, $params{$p});
		}
	}

	#	check and set the apph filters
	$self->__trim_and_set_filters($cgi);

#	print STDERR "Params: ".Dumper($self->{'params'})."\n\n" if $verbose;

	#if ($self->get_param('format'))
	#{	$self->set_current_param('page_size', 'all');
	#}
#	print STDERR "session after: ".Dumper($self->{params})."\n" if $verbose;
}

sub search_sync_UNUSED {
	my $self = shift;
	my $cgi = $self->get_cgi;
	my %params = $cgi->Vars;
	
	my $action = $cgi->param('action');
	if ($action && $action eq 'new-search' || $action eq 'advanced_query')
	{	#	delete cached results
		$self->delete_cached_results;
	#	$self->delete_param($_) foreach &_search_params;
	}

	my $sc = $cgi->param('search_constraint') || $self->get_default_param('search_constraint');
	$self->set_current_param('search_constraint', $sc);

	my $exact = $cgi->param('exact_match');
	if ($exact && $exact eq 'yes')
	{	$self->set_current_param('exact_match', 1);
	}
	
	foreach my $p ('termfields', 'gpfields', 'sppfields')
	{	my @list = split( /\0|,/, $params{$p}) if $params{$p};
		$self->set_current_param($p, [@list]) if @list;
	}
	
#	my %q_hash;
#	if ($params{query})
#	{	my @queries = split /(\n|\0)/, $params{query};
#		foreach my $q (@queries)
#		{	foreach ( split /(\n|\0)/, $q )
#			{	#	get rid of any tracts of whitespace
#				$q =~ s/(\t|\s{2,})/ /g;
#				$q =~ s/^\s*(\S+.*?)\s*$/$1/;
#				$q_hash{$q}++ if $q =~ /\w/;
#			}
#		#}
#	}

#	my $file = $q->param('idfile') || undef;
#	if ($file)
#	{	print STDERR "Found an idfile!\n" if $verbose;
#		while (<$file>) {
#			#	get rid of any tracts of whitespace
#			s/(\t|\s{2,})/ /g;
#			s/^\s*(\S+.*?)\s*$/$1/;
#			$q_hash{$_}++ if /\w/;
#		}
#	}
#	
#	if (keys %q_hash)
#	{	$self->set_current_param('query', \%q_hash);
#	}
}

sub graph_sync {
	my $self = shift;
	my $params = shift;

	print STDERR "Session::graph_sync: Starting!\nSession::graph_sync params: ".Dumper($params)."\n" if $verbose;
	my $last_action;
	my $action_node_list;

#	if we just performed some sort of action, see what the target of
#	the action was and put it in the action node list

	#	the params that we are interested in are
	#	action, format, term, open_0, open_1, closed

	my $action = $params->{action};
	my $tree = $params->{tree};
	
	if (!$action && ($params->{'format'} || $self->ses_type eq 'graphviz'))
	{	$action = 'permalink';
	}
	print STDERR "Session::graph_sync action: ". ( $action || 'undefined' )."\n" if $verbose;

	my @tree_params = &_tree_params;

	if (!$action)
	{	#	dump the tree
		print STDERR "Session::graph_sync no action: dumping the tree\n" if $verbose;
		undef $tree;
	}
	elsif ($action eq 'reset-tree') {
		# this happens in the term-select cgi
		undef $tree;
		$tree->{open_0} = get_valid_list($tree->{term}) if $tree->{term};
		delete $tree->{term};
#		$tree->{open_0} = get_valid_list($params->{term}) if $params->{term};
	}
	elsif ($action eq 'set-tree') {
		# this only happens in the term-details and term-select cgi
		$tree->{open_0} = get_valid_list($tree->{term}) if $tree->{term};
		delete $tree->{term};
	}
	elsif ($action eq 'minus_node') {
		#	the term in 'target' has been closed
		my $target = $params->{target};
		print STDERR "Session::graph_sync closing node target, $target\n" if $verbose;
		$last_action = 'Closed '.$target;
		$action_node_list = [ split "\0", $params->{'target'} ];
		#	add it to the list of closed nodes
		$tree->{closed} = add_value_to_list($tree->{closed} || undef, $target);
		#	remove it from the open_1 node list (if it is present)
		$tree->{open_1} = remove_value_from_list($tree->{open_1} || undef, $target);
	}
	elsif ($action eq 'plus_node') {
		#	the term in 'target' has been opened
		my $target = $params->{target};
		print STDERR "Session::graph_sync opening node target, $target\n" if $verbose;
		$last_action = 'Opened '.$target;
		$action_node_list = [ split "\0", $params->{'target'} ];
		#	add it to the list of open nodes
		$tree->{closed} = remove_value_from_list($tree->{closed} || undef, $target);
		#	remove it from the closed node list (if it is present)
		$tree->{open_1} = add_value_to_list($tree->{open_1} || undef, $target);
	}
	else #	other actions, e.g. filtering, permalink, set tree
	{	#	get the params from the cgi
		foreach my $p (@tree_params)
		{	#	move the params from the cgi in
			$tree->{$p} = get_valid_list($tree->{$p}) if $tree->{$p};
		}
		if ($action eq 'reset-filters')
		{	$last_action = 'Reset filters';
		}
		elsif ($action eq 'filter')
		{	$last_action = 'Set filters';
		}
		elsif ($action eq 'permalink')
		{	$last_action = 'Created permalink';
		}
		elsif ($action eq 'set-tree')
		{	$last_action = 'Set the tree';
		}
	}

	foreach (keys %$tree)
	{	delete $tree->{$_} unless defined $tree->{$_};
	}
	
	print STDERR "tree post: ".Dumper($tree)."\n" if $verbose;

#	END BIG INSERT!

	return { tree => $tree, last_action => $last_action || 'Reset the tree', action_node_list => $action_node_list };
#	$self->set_current_param($_, $tree->{$_}) foreach @tree_params;
#	$self->save_cached_results($tree, 'tree');
}

sub graph_sync_old {
	my $self = shift;

	#	the params that we are interested in are
	#	action, format, term, open_0, open_1, closed

	#print STDERR "session: ".Dumper($self)."\n" if $verbose;
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

	print STDERR "tree pre: ".Dumper($tree) if $verbose;


	print STDERR "action: ". ( $action || 'undefined' )."\n" if $verbose;

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
			{	$tree->{open_0} = get_valid_list($cgi_params{term});
			}
		}
		elsif ($action eq 'set-tree') {
			#	delete all current tree settings and
			#	replace them with those from the cgi
			foreach my $p (@tree_params)
			{	#	move the params from the cgi in
				if ($cgi_params{$p})
				{	$tree->{$p} = get_valid_list($cgi_params{$p});
				}
			}

			if ($cgi_params{term})
			{	$tree->{open_0} = get_valid_list($cgi_params{term});
			}

		}
		elsif ($action eq 'minus_node') {
			#	the term in 'target' has been closed
			my $target = $cgi->param('target');
			print STDERR "closing node target, $target\n" if $verbose;
			#	add it to the list of closed nodes
			$tree->{closed} = add_value_to_list($tree->{closed} || undef, $target);
			#	remove it from the open_1 node list (if it is present)
			$tree->{open_1} = remove_value_from_list($tree->{open_1} || undef, $target);
		}
		elsif ($action eq 'plus_node') {
			#	the term in 'target' has been opened
			my $target = $cgi->param('target');
			print STDERR "opening node target, $target\n" if $verbose;
			#	add it to the list of open nodes
			$tree->{closed} = remove_value_from_list($tree->{closed} || undef, $target);
			#	remove it from the closed node list (if it is present)
			$tree->{open_1} = add_value_to_list($tree->{open_1} || undef, $target);
		}
		#	other actions - e.g. filtering, permalink: do nothing
	}
	
	print STDERR "tree post: ".Dumper($tree)."\n" if $verbose;
	
	$self->set_current_param($_, $tree->{$_}) foreach @tree_params;
	$self->save_cached_results($tree, 'tree');
	
#			print STDERR "session: ".Dumper($self)."\n" if $verbose;
}

sub term_sync_old {
	my $self = shift;
#	print STDERR "session before sync:\n".Dumper($self)."\n" if $verbose;
#	my $cgi = $self->get_cgi;
	my $cgi = shift;
	my %cgi_params = $cgi->Vars;
	my $action = $cgi->param('action'); #$self->get_current_param('action');
	if ($self->ses_type eq 'graphviz' || $cgi->param('format'))
	{	$action = 'permalink';
	}
	my $tree;
	my @tree_params = &_tree_params;

	print STDERR "tree pre: ".Dumper($tree) if $verbose;


	print STDERR "action: ". ( $action || 'undefined' )."\n" if $verbose;

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
			print STDERR "closing node target, $target\n" if $verbose;
			#	add it to the list of closed nodes
			$tree->{closed} = add_value_to_list($tree->{closed} || undef, $target);
			#	remove it from the open_1 node list (if it is present)
			$tree->{open_1} = remove_value_from_list($tree->{open_1} || undef, $target);
		}
		elsif ($action eq 'plus_node') {
			#	the term in 'target' has been opened
			my $target = $cgi->param('target');
			print STDERR "opening node target, $target\n" if $verbose;
			#	add it to the list of open nodes
			$tree->{closed} = remove_value_from_list($tree->{closed} || undef, $target);
			#	remove it from the closed node list (if it is present)
			$tree->{open_1} = add_value_to_list($tree->{open_1} || undef, $target);
		}
		#	other actions - e.g. set / reset filters: do nothing
	} else {
		#hmm when to clear up, change filter should not!! how to check that?
#		unless ($self->get_cgi->param('ch_filter')) {
			undef $tree;
#		}
	}
	
	
#	$self->__delete_fields(-query=>'current', -fields=>['open_0']);
#	print STDERR "\nsession after sync:\n".Dumper($self)."\n" if $verbose;
	
	print STDERR "tree post: ".Dumper($tree)."\n" if $verbose;
	
	$self->set_current_param($_, $tree->{$_}) foreach @tree_params;
	$self->save_cached_results($tree, 'tree');
	
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
	my $cgi = shift;
	foreach (&_search_params, &_tree_params, 'term', 'gp')
	{	$self->delete_current_param($_);
	}
	$self->set_current_param($cgi->param('use_filters'));

#	$self->__delete_fields(-query=>'1', -fields=>[&_search_params, &_tree_params]);
#	$self->__copy_or_delete(-fields=>['use_filters']);
#	$self->__transfer_fields(-fields=>[&_blast_params]);
}

sub prefs_sync {
	my $self = shift;
	my $cgi = shift;
	foreach my $f (&_filter_fields)
	{	if ($cgi->param('disable_'.$f.'_filter'))
		{	my $v = $cgi->param('disable_'.$f.'_filter');
		if ($v)
			{	$self->set_saved_param('disable_'.$f.'_filter', 1);
				$cgi->delete($f);
			}
		}
	}
	if ($cgi->param('tree_view') && $cgi->param('tree_view') eq 'compact')
	{	$self->set_saved_param('tree_view', 'compact');
	}
}

=head2 __trim_and_set_filters

Usage: $session->__trim_and_set_filters;

Sets the apph filters, moves the filter params from 'current' to
'1'. Checks that each field is enabled globally and that the user
hasn't disabled it; also ensures that if 'all' is selected as an
option, the filter isn't set. The evcode filter has an additional
check for 'aca', a deprecated code meaning 'all curator approved'.

=cut

sub __trim_and_set_filters {

  print STDERR "Session::__trim_and_set_filters: Starting!\n"
    if $verbose;

  my $self = shift;
  my $cgi = shift;
  # my $save_values_to_session = shift || undef;
  my %params = $cgi->Vars;
  my $apph = $self->apph;

  print STDERR "saved before: ".
    Dumper($self->{params}{saved})
      if $verbose;

  my %filters;
  my $action = $cgi->param('action');
  my $reset = $cgi->param('reset-filters') || undef;

  if (($action && $action eq 'reset-filters') || $reset){
    $apph->filters(\%filters);
    $self->delete_param($_) foreach &_filter_fields;
    # delete any cached results
    $self->delete_cached_results;
    undef %params;
  }elsif ($action && $action eq 'filter'){
    #	delete any cached results
    $self->delete_cached_results;
  }

  #	get the active filter fields
  my $active_filters = $self->get_active_filter_fields;

## This section never executed ('ieas_loaded'). Trying removal.
#   ## COMMENTARY: This section is mostly doing something id the
#   ## 'ieas_loaded' variable is true; however, this variable seems to
#   ## be otherwise gone in the code base. From now on, IEAs should be
#   ## loaded and ubiquitous, so we'll hardcode to true for now.
#   my $evcode = 0;
#   #if (get_environment_param('ieas_loaded') &&
#   if ( 1 &&
#       grep { 'evcode' } @$active_filters &&
#       $params{'evcode'}){
#     # evcode has to be handled slightly differently if IEAs
#     # are present, so remove it from the list of filters
#     $evcode = 1;
#     my @other_filters = grep { $_ ne 'evcode' } @$active_filters;
#     $active_filters = [ @other_filters ];
#   }
  print STDERR "active filters (possibly minus evcode): ".
    Dumper($active_filters)."\n"
      if $verbose;

  my $to_delete;
  foreach my $f (@$active_filters){
    if (!$params{$f}){
      #	set the filter using the values in the saved params
      $filters{$f} = $self->get_saved_param($f) if $self->get_saved_param($f);
      next;
    }
    my $p_list = get_valid_list( [split("\0", $params{$f})] );
    print STDERR "f: $f; list: ".Dumper($p_list)."\n" if $verbose;
    if (!@$p_list ||  # no values present
	grep { $_ eq 'all' } @$p_list){  # the list contains 'all'
      #	delete the param
      push @$to_delete, $f;
    }elsif ($self->get_param_list_size($f) == scalar @$p_list){
      #	slight cheat here: param list size is the same as
      #	number of selected params
      push @$to_delete, $f;
    }else{
      $filters{$f} = \@$p_list;
      $self->set_saved_param($f, \@$p_list);
      print STDERR "set $f filters.\n" if $verbose;
    }
  }

## This section never executed ('ieas_loaded'). Trying removal.
#   ## COMMENTARY: This next bit seems to be a bit of evcode filter
#   ## optimization hinging on the flag above and the relationships
#   ## between 'all', 'aca', and 'iea'. Probably useful in preventing
#   ## needless joins.
#   # evcode
#   if ($evcode){  # this will only be on if IEAs are loaded
#     my $p_list = get_valid_list( split("\0", $params{'evcode'}) );
#     #print STDERR "f: $f; list: ".join(", ", @{$p_list || []})."\n" if $verbose;
#     if (!@$p_list ||  # no values present
# 	grep { $_ eq 'all' } @$p_list){  # the list contains 'all'
#       # delete the param
#       push @$to_delete, 'evcode';
#     }elsif (grep { $_ eq 'aca' } @$p_list){
#       #	parameter setting 'all curator approved' is on
#       if (!grep { $_ eq 'iea' } @$p_list) {
# 	# IEAs are loaded, ACA is the search set, IEA not in the search set
# 	print STDERR "Param IEAS LOADED is ON\n" if $verbose;
# 	$filters{'evcode'} = ['!IEA'];
#       }else{
# 	# IEA *and* ACA are on ==> redundant parameter
# 	push @$to_delete, 'evcode';
#       }
#     }elsif ($self->get_param_list_size('evcode') == scalar @$p_list){
#       #	slight cheat here: param list size is the same as
#       #	number of selected params
#       push @$to_delete, 'evcode';
#     }else{
#       $filters{'evcode'} = \@$p_list;
#       $self->set_saved_param('evcode', \@$p_list);
#       print STDERR "set evcode filters.\n" if $verbose;
#     }
#   }

  ## Commit deletables from above to the apphandle filters.
  foreach (@$to_delete){
    $self->delete_saved_param($_);
    $cgi->delete($_);
  }
  $apph->filters(\%filters);

  print STDERR "apph filters: ".Dumper(\%filters) if $verbose;
  print STDERR "Set apph filters!\n" if $verbose;

  # check whether it's going to be ok to get gp counts
  if (!keys %filters){
    $self->gp_count_ok(1);
    #	$self->show_counts('gp', 1);	#	done by gp_count_ok
    $self->show_counts('term', 1);
  }else{
    my @gp_count_correct_fields = &_gp_count_correct_fields;

    print STDERR "gp count correct fields: ".
      Dumper(\@gp_count_correct_fields)."\n"
	if $verbose;

    # check if there are filters which aren't in gp_count_correct_fields
    my $set;
    foreach my $f (keys %filters){
      print STDERR "f = $f\n" if $verbose;
      if (!grep { $f eq $_ } @gp_count_correct_fields){
	print STDERR "$f is not in gp_count_correct\n" if $verbose;
	$self->gp_count_ok(0);
	$set = 1;
	last;
      }
    }

    ### HACK ALERT! ###
    # if we have both spp and db filters, check that they overlap
    # this is for getting gp counts from the db using the apph method
    # as long as there's an overlap, we're OK to use the apph method
    if( $ENV{GO_HAS_COUNT_BY_SPECIES} &&
	$ENV{GO_HAS_COUNT_BY_SPECIES} == 1 &&
	$filters{speciesdb} &&
	$filters{taxid} ){

      print STDERR "Checking whether our species/db combo are OK\n"
	if $verbose;

      my $map = $self->get_spp_db_mapping;
      if (scalar keys %$map > 0){
	# check that the taxids we are looking for correspond with the dbs
	my $db_hash;
	$db_hash->{$_} = 1 foreach @{$filters{speciesdb}};

	foreach my $spp (@{$filters{taxid}}){
	  if ($map->{$spp}){
	    foreach (@{$map->{$spp}}){
	      if (!$db_hash->{ $_ }){
		# the db for that spp is not in the db filter list
		$self->gp_count_ok(0);
		$set = 1;
		last;
	      }
	    }
	  }else{ # spp not in the hash (erk! this shouldn't happen)
	    $self->gp_count_ok(0);
	    $set = 1;
	    last;
	  }
	  last if $set;
	}

	# if we got here, our spp correspond with our db (hurrah!)
	# we can ignore the database filter when getting gp counts
	$self->gp_count_ok('ignore_db_filter') if !$set;
	$set = 1;
      }else{
	$self->gp_count_ok(0);
	$set = 1;
      }
    }
    ### END HACK ###

    $self->gp_count_ok(1) if !$set;
  }

  print STDERR "gp_count_ok: ".$self->gp_count_ok."\n" if $verbose;
  print STDERR "saved after: ".Dumper($self->{params}{saved}) if $verbose;
}

##### Useful parameter lists #####

sub _global_current_params {
	return (qw(page page_size format action term gp), &_filter_fields);
}

sub get_ses_type_params {
	my $self = shift;
	my $ses_type = shift || $self->ses_type;
	my $current_params_by_ses_type = {
			term_assoc => [ 'term_assocs' ],
			gp_assoc => [ 'show_blast_scores' ],
			term_details => [ 'term_context' ],
			search => [ &_search_params ],
			blast => [ &_blast_params ],
	};
	
	return $current_params_by_ses_type->{$ses_type} || undef;
}

sub _current_params {
	return qw(term_assocs term_context show_blast_scores);
}

# these are all single scalar values
sub _persisting_params {
#	return qw(reltype show_associations term_context termsort gpsort sppsort session_id);
	my @list = qw(termsort gpsort sppsort tree_view term_assoc_sort gp_assoc_sort);
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

=head2 _gp_count_correct_fields

The fields for which the gp count is correctly calculated

=cut

sub _gp_count_correct_fields {
	my @fields = qw(ont speciesdb);
	if ($ENV{GO_HAS_COUNT_BY_SPECIES} && $ENV{GO_HAS_COUNT_BY_SPECIES} == 1)
	{	return (@fields, 'taxid');
	}
	return @fields;
}

=head2 _filter_fields_by_type

Filterable fields organized by the aspect that they filter

=cut

sub _filter_fields_by_type {

	return 
	{	gp => ['speciesdb', 'taxid', 'gptype'],
		assoc => ['evcode', 'assby', 'qual'],
		ont => ['ont'],
	};
}

=head2 _filter_fields

All filterable fields

=cut

sub _filter_fields {
	my $filters = &_filter_fields_by_type;
	return (map { @$_ } values %$filters);
}

=head2 get_filters_by_type

Gets all the filter fields of a certain type
If no argument is passed, it returns a list of all the filterable fields

=cut

sub get_filters_by_type {
	my $f_type = shift;
	my $filters = _filter_fields_by_type;
	
	if ($f_type && $filters->{$f_type})
	{	return $filters->{$f_type};
	}
	
	return [ map { @$_ } values %$filters ];
	
}

=head2 get_type_of_filter

Gets the aspect that the filter applies to

=cut

sub get_type_of_filter {
	my $filter = shift || return;
	my $filters = _filter_fields_by_type;
	my $filter_h;
	foreach my $f (keys %$filters)
	{	$filter_h->{$_} = $f foreach @{$filters->{$f}};
	}
	
	return $filter_h->{$filter} || undef;
}


=head2 _gp_filters

Filterable gene product fields


sub _gp_filters {
	return qw(speciesdb taxid gptype);
}

=cut

=head2 _assoc_filters

Filterable association fields

sub _assoc_filters {
	return qw(evcode qual assby); #assocdate);
}

=cut

=head2 _ont_filters

Filterable ontology fields

sub _ont_filters {
	return ('ont');
}

=cut


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
	
	print STDERR "getting active filter fields...\n" if $verbose;
	my @active;

	my @fields = &_filter_fields;
	print STDERR "All filter fields: ".Dumper(\@fields) if $verbose;

	foreach (&_filter_fields)
	{	print STDERR "field: $_\n" if $verbose;
	
		push @active, $_ if (get_environment_param('show_'.$_.'_filter') && !$self->get_saved_param('disable_'.$_.'_filter'));
	}
	$self->{active_filter_fields} = [@active];
	print STDERR "active filter fields: ".join(", ", @active)."\n" if $verbose;
	return $self->{active_filter_fields};
}

sub get_search_field_list {
	my $self = shift;
	my $sc = shift;
	my $field = shift || 'srch_options';

	require GO::CGI::Search;

	$sc =~ s/fields//;
	return GO::CGI::Search::__search_field_list($sc, $field) || undef;
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

sub get_list {
	my $self = shift;
	my $param = shift;
#	print STDERR "get_list: param: $param\n" if $verbose;
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
		graph_bgcolor => [ 'beige', 'black', 'blue', 'forestgreen', 'maroon', 'navy', 'pink', 'purple', 'red', 'skyblue', 'white', 'yellow' ],
		graph_textcolor => [ 'beige', 'black', 'blue', 'forestgreen', 'maroon', 'navy', 'pink', 'purple', 'red', 'skyblue', 'white', 'yellow' ],
		term_assoc_sort => [ 'term,gp', 'op,term,gp', 'term,op,gp' ],
		gp_assoc_sort => [ 'gp,term', 'op,gp,term', 'gp,op,term' ],
	);
	
#	print STDERR "lists{param} = ".Dumper($lists{$param})."\n" if $verbose;
	
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
	print STDERR "param = $param\n" if $verbose;
	
	my $sub = 'get_'.$param.'_list';
	my $list = [];
	eval {
		$list = $self->$sub;
	};
	return $list;
}

##### Preparing page templates #####

sub suicide_message {
	my $self = shift;
	my $error = set_message(undef, 'fatal', shift);
	
	process_page_template($error, $self, 'amigo_message');
	
#	die(shift);
	
	exit;
}

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

	$self = GO::CGI::Session->new(-temp=>1) if !$self;
	$ses_type = $self->ses_type if !$ses_type;

	foreach (keys %$vars)
	{	delete $vars->{$_} if !defined $vars->{$_};
	}

	$vars->{session_id} = $self->id;
	$vars->{session_id_for_url} = 'session_id='.$self->id;

	## Make sure that the instance data is there before we use
	## it. A lot of times in experimental DBs it seems to not get
	## populated.
	$vars->{release_date} = '1970-01-01';
	if ( $self->apph->instance_data &&
	     $self->apph->instance_data->{release_name} ) {
	  $vars->{release_date} = $self->apph->instance_data->{release_name};
	}

	unless ($ses_type eq 'amigo_message')
	{	$vars->{munger} = GO::CGI::NameMunger->new();
		$vars->{filterdata} = $self->get_data_for_filters;
		$vars->{'s'} = 4;

		if ($self->get_default_param('show_gp_options'))
		{	$vars->{show_gp_options} = 1;
			for ('gp', 'term')
			{	$vars->{"show_".$_."_counts"} = $self->show_counts($_);
			}
		}
	}

	output_template($vars, $ses_type);
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
	print STDERR "Session::get_data_for_filters: Starting!\n" if $verbose;
	my $self = shift;
#	my $filter_h = shift;
#	my $extra = shift;
	my $ses_type = $self->ses_type;
	my $filterdata;

#	print STDERR Dumper($filter_h) if $verbose;

#	these session types don't need to display any filter data
	if (grep { $ses_type eq $_ } qw(front amigo_message blast_waiting gp_details vocab_details spp_search graphviz))
	{	return $filterdata;
	}
	
#	active filters
	my @active = @{$self->get_active_filter_fields};

#	print STDERR "active filters: ".join(", ", @active)."\n" if $verbose;

#	not all pages need all the filter data
#	any page which shows the current filters needs the 'selected' filter data
#	for pages where a certain filter parameter can be set, you need all the
#	filter data

#	ses_types which allow you to filter on all fields:
#	gp_search, blast_query, prefs, advanced_query

	my @gpfields;
	if (get_environment_param('calculate_gp_counts'))
	{	@gpfields = @{get_filters_by_type('gp')};
	}
	else
	{	@gpfields = &_gp_count_correct_fields;
	}

	my $allow_filter_by = {
	#	gp_count_correct fields
		term_details => [@gpfields],
		term_chart => [@gpfields],

	#	gp_count_correct fields, ont
		subset_details => [@gpfields, 'ont'],
		browse => [@gpfields, 'ont'],

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
	{	print STDERR "No selected found. Getting all data...\n" if $verbose;
		#	no specific data required, so we'll just get everything
		foreach my $f (@active)
		{	$filterdata->{$f} = $self->set_option($f);
		}
	}
	else
	{	print STDERR "Found some selected. Getting data for filters "
		.join(", ", @{$allow_filter_by->{$ses_type}})
		."...\n" if $verbose;
		foreach my $f (@active)
		{	if (grep { $f eq $_ } @{$allow_filter_by->{$ses_type}})
			{	print STDERR "Found $f in filter_h!\n" if $verbose;
				$filterdata->{$f} = $self->set_option($f);
			}
			else
			{	print STDERR "Didn't find $f in filter_h!\n" if $verbose;
				$filterdata->{$f} = $self->set_option($f, 'selected_only');
			}
		}
	}

	return $filterdata;
}

sub set_option {
	print STDERR "Session::set_option: Starting!\n" if $verbose;
	my $self = shift;
	my $param = shift;
	my $selected_only = shift || undef;
	my $all = 0;

#	print STDERR "param: $param". ($selected_only ? " selected_only" : "") . "\n" if $verbose;

	#	get the current values of the parameter
	my $selected = $self->get_param($param) || $self->get_cgi_param($param);
#	print STDERR "selected: ".Dumper($selected)."\n" if $verbose;

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
		{	join("\0", GO::CGI::NameMunger::get_full_name_fn($_), $_);
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
			{	print STDERR "results:\n".Dumper($results)."\n" if $verbose;
				map { my %hash = @$_; $_ = \%hash } @$results;
			#	map { $_ = \%{@$_} } @$results;
				print STDERR "results:\n".Dumper($results)."\n" if $verbose;
				unshift @taxa, @$results;
			}
		}
=cut
#		print STDERR "taxa:\n".Dumper(\@taxa)."\n" if $verbose;
		
#		$option_l = [@taxa];
	}
	elsif ($param =~ /(term|gp|spp)fields/)
	{	my $fields = $self->get_search_field_list($param);
		push @$fields, 'all';
		$option_l = [ map { { value => $_, label => GO::CGI::NameMunger::get_field_name_fn($_) } } @$fields ];
	}
	else
	{	#print STDERR "param: $param\n" if $verbose;
		my $list = $self->get_list($param);
	#	print STDERR "list: ".Dumper($list)."\n" if $verbose;
		$option_l = [ map { { label => GO::CGI::NameMunger::get_human_name_fn($_), value => $_ } } @$list ];
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
	#		last;
		}
		foreach (@select)
		{	$_->{selected} = 1;
			#	get the URL
			if ($param eq 'ont' || $param eq 'evcode')
			{	$_->{url} = GO::CGI::NameMunger::get_GO_doc_url_fn($param, $_->{value});
			}
			elsif ($param eq 'taxid')
			{	$_->{url} = GO::CGI::NameMunger::get_url_fn('ncbi_taxid', $_->{value});
			}
			elsif ($param eq 'speciesdb')
			{	$_->{url} = GO::CGI::NameMunger::get_db_url_fn($_->{value});
			}
		}
	}


	my $param_data = { title => GO::CGI::NameMunger::get_human_name_fn($param) };
	$param_data->{selected} = \@select unless (!@select);

	#	Add an 'all' option if appropriate
	if (grep { $_ eq $param } (@{$self->get_active_filter_fields}))#, 'termfields', 'gpfields', 'sppfields'))
	{	if ($all == 1 && !$selected_only)
		{	unshift @$option_l, { value => 'all', label => 'All', selected => 1 };
		}
		else
		{	unshift @$option_l, { value => 'all', label => 'All'};
		}
		$param_data->{filtertype} = get_type_of_filter($param);
		$param_data->{gp_count_ok} = 1 if grep { $param eq $_ } &_gp_count_correct_fields;
	}

	if ($selected_only)
	{	return $param_data;
	}

	$param_data->{data} = $option_l;
	return $param_data;
}

sub set_filter_option {
	print STDERR "Session::set_filter_option: Starting!\n" if $verbose;
	my $self = shift;
	my $param = shift;
	my $selected_only = shift || undef;
	my $all = 0;

#	print STDERR "param: $param". ($selected_only ? " selected_only" : "") . "\n" if $verbose;

	#	get the current values of the parameter
#	my $selected = $self->get_param($param) || $self->get_cgi_param($param);
	my $selected = $self->apph->filters->{$param} || undef;
#	print STDERR "selected: ".Dumper($selected)."\n" if $verbose;


	#	if there are no selected values and we only want to see the selected...
	if ($selected_only && (!$selected || !@$selected))
	{	return;
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
		{	join("\0", GO::CGI::NameMunger::get_full_name_fn($_), $_);
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
			{	print STDERR "results:\n".Dumper($results)."\n" if $verbose;
				map { my %hash = @$_; $_ = \%hash } @$results;
			#	map { $_ = \%{@$_} } @$results;
				print STDERR "results:\n".Dumper($results)."\n" if $verbose;
				unshift @taxa, @$results;
			}
		}
=cut
#		print STDERR "taxa:\n".Dumper(\@taxa)."\n" if $verbose;
		
#		$option_l = [@taxa];
	}
	else
	{	#print STDERR "param: $param\n" if $verbose;
		my $list = $self->get_list($param);
	#	print STDERR "list: ".Dumper($list)."\n" if $verbose;
		$option_l = [ map { { label => GO::CGI::NameMunger::get_human_name_fn($_), value => $_ } } @$list ];
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
	#		last;
		}
		foreach (@select)
		{	$_->{selected} = 1;
			#	get the URL
			if ($param eq 'ont' || $param eq 'evcode')
			{	$_->{url} = GO::CGI::NameMunger::get_GO_doc_url_fn($param, $_->{value});
			}
			elsif ($param eq 'taxid')
			{	$_->{url} = GO::CGI::NameMunger::get_url_fn('ncbi_taxid', $_->{value});
			}
			elsif ($param eq 'speciesdb')
			{	$_->{url} = GO::CGI::NameMunger::get_db_url_fn($_->{value});
			}
		}
	}


	my $param_data = { title => GO::CGI::NameMunger::get_human_name_fn($param) };
	$param_data->{selected} = \@select unless (!@select);

	#	Add an 'all' option if appropriate
	if (grep { $_ eq $param } (@{$self->get_active_filter_fields}))#, 'termfields', 'gpfields', 'sppfields'))
	{	if ($all == 1 && !$selected_only)
		{	unshift @$option_l, { value => 'all', label => 'All', selected => 1 };
		}
		else
		{	unshift @$option_l, { value => 'all', label => 'All'};
		}
		$param_data->{filtertype} = get_type_of_filter($param);
		$param_data->{gp_count_ok} = 1 if grep { $param eq $_ } &_gp_count_correct_fields;
	}

	if ($selected_only)
	{	return $param_data;
	}

	$param_data->{data} = $option_l;
	return $param_data;
}

sub set_option_with_value {
	print STDERR "Session::set_option_with_value: Starting!\n" if $verbose;
	my $self = shift;
	my $param = shift;
	#	get the current values of the parameter
	my $selected = shift;

	#	if there are no selected values...
	if (!$selected)
	{	$self->set_option($param, shift);
	}

	if ($selected && !ref($selected))
	{	$selected = [ $selected ];
	}
	
	#	get the human names for the param values
	#	and the rest of the values, if appropriate (i.e. selected_only NOT on)
	my $option_l;

	if ($param =~ /(term|gp|spp)fields/)
	{	my $fields = $self->get_search_field_list($param);
		push @$fields, 'all';
		$option_l = [ map { { value => $_, label => GO::CGI::NameMunger::get_field_name_fn($_) } } @$fields ];
	}
	else
	{	#print STDERR "param: $param\n" if $verbose;
		my $list = $self->get_list($param);
	#	print STDERR "list: ".Dumper($list)."\n" if $verbose;
		$option_l = [ map { { label => GO::CGI::NameMunger::get_human_name_fn($_), value => $_ } } @$list ];
	}

	my @select;
	foreach my $s (@$selected)
	{	push @select, (grep { $_->{value} eq $s } @{$option_l});
	}
	foreach (@select)
	{	$_->{selected} = 1;
	}

	my $param_data = { title => GO::CGI::NameMunger::get_human_name_fn($param) };
	$param_data->{selected} = \@select unless !@select;
	$param_data->{data} = $option_l;
	return $param_data;
}


1;
