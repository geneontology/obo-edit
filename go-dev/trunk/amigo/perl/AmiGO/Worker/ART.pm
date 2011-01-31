=head1 AmiGO::Worker::ART

An attempt at combining the search of GOBO::DBIC::GODBModel::Query and
Utility::TSLParser.

WARNING: There are several bits that are SF.net specific at this
point. Both this server and the client need to have those kinds of
bits encapsulated when we start having other trackers.

=cut

use utf8;
use strict;

package AmiGO::Worker::ART;

use base ("AmiGO");

##
use LWP::UserAgent;
use HTTP::Cookies;
use HTTP::Request;
use HTML::TableExtract;
use WWW::Mechanize;
use Data::Dumper;
use Time::HiRes qw(gettimeofday tv_interval);

## Some global defaults:
my $allow_anonymous = 0;
my $global_category_id_default = '100';
my $global_artifact_group_id_default = '100';
my $global_login_default = 'orb-default';
my $global_password_default = '0R8d3fau17'; ## BUG: This is in the
                                            ## clear! This should be
                                            ## read from a secret
                                            ## place on the server.

=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  $self->{TRACKERS} = parse_trackers_dot_txt()
    die "couldn\'t parse trackers.txt") if ! $trackers;

  bless $self, $class;
  return $self;
}


## The cases are:
##   'trackers': general tracker info
##   'tracker_information': info on specific tracker
##   'items': look at what is already in the tracker
##   'add': add a term to a specific tracker
##   'jsapi': get JavaScript API
##   '': get XML API

=item get_trackers

=cut
sub get_trackers {

  my $self = shift;

  ## Give the vital information for the trackers.
  foreach my $tracker (keys %{$self->{TRACKERS}}){

    if( $tracker &&
	$$self->{TRACKERS}{$tracker}{'name'} &&
	$$self->{TRACKERS}{$tracker}{'description'} &&
	$$self->{TRACKERS}{$tracker}{'url'} &&
	$$self->{TRACKERS}{$tracker}{'tracker'} ){

      push @xml_output, '<tracker>';
      push @xml_output, '<ontology_id>';
      push @xml_output, $tracker;
      push @xml_output, '</ontology_id>';
      push @xml_output, '<name>';
      push @xml_output, $$self->{TRACKERS}{$tracker}{'name'};
      push @xml_output, '</name>';
      push @xml_output, '<description>';
      push @xml_output, $$self->{TRACKERS}{$tracker}{'description'};
      push @xml_output, '</description>';
      push @xml_output, '<url>';
      push @xml_output, escapeHTML($$self->{TRACKERS}{$tracker}{'url'});
      push @xml_output, '</url>';
      push @xml_output, '<type>';
      push @xml_output, $$self->{TRACKERS}{$tracker}{'tracker'};
      push @xml_output, '</type>';
      push @xml_output, '<group_id>';
      push @xml_output, $$self->{TRACKERS}{$tracker}{'group_id'};
      push @xml_output, '</group_id>';
      push @xml_output, '<atid>';
      push @xml_output, $$self->{TRACKERS}{$tracker}{'atid'};
      push @xml_output, '</atid>';
      push @xml_output, '</tracker>';

    }else{
      die_xml('all required information is not present in trackers file');
    }
  }

  ## Cap output.
  push @xml_output, '</response>';

} elsif ( $request eq 'tracker_information' ) {

  ## The ontology id must be defined.
  die_xml('ontology must be defined when requesting tracker_information')
    if ! $ontology_id;

  ## Goto tracker page.
  my $mech = WWW::Mechanize->new();
  $mech->get('http://sourceforge.net/tracker/?' .
	     'group_id=' . $$self->{TRACKERS}{$ontology_id}{'group_id'} .
	     '&atid=' . $$self->{TRACKERS}{$ontology_id}{'atid'});
  die_xml('falied to access tracker page') if ! $mech->res->is_success;


  ## Now that we got it, scrape the content and return a list of
  ## information. Starting with the group and category ids...
  $mech->content =~ /<select NAME="_category">(.*?)<\/select>/s;
  my $select_content_cat = $1;
  my %options_cat = $select_content_cat =~
    /<OPTION VALUE="(.*?)">(.*?)<\/OPTION>/gs;

  $mech->content =~ /<select NAME="_group">(.*?)<\/select>/s;
  my $select_content_grp = $1;
  my %options_grp = $select_content_grp =~
    /<OPTION VALUE="(.*?)">(.*?)<\/OPTION>/gs;

  ## Error check. If pass, produce the meta-info.
  if ( keys %options_cat && keys %options_grp ) {

    foreach my $key (keys %options_cat) {
      push @xml_output, '<category_id>';
      push @xml_output, '<name>';
      push @xml_output, $options_cat{$key};
      push @xml_output, '</name>';
      push @xml_output, '<value>';
      push @xml_output, $key;
      push @xml_output, '</value>';
      push @xml_output, '</category_id>';
    }
    foreach my $key (keys %options_grp) {
      push @xml_output, '<artifact_group_id>';
      push @xml_output, '<name>';
      push @xml_output, $options_grp{$key};
      push @xml_output, '</name>';
      push @xml_output, '<value>';
      push @xml_output, $key;
      push @xml_output, '</value>';
      push @xml_output, '</artifact_group_id>';
    }
  }

  ## Cap output.
  push @xml_output, '</response>';

} elsif ( $request eq 'items' ) {

  ## The ontology id must be defined.
  die_xml('ontology must be defined when requesting items') if ! $ontology_id;
  die_xml('somehow skipped defining detailed_information')
    if ! $detailed_information;

  ## Goto tracker page.
  my $mech = WWW::Mechanize->new();
  $mech->get('http://sourceforge.net/tracker/?' .
	     'group_id=' . $$self->{TRACKERS}{$ontology_id}{'group_id'} .
	     '&atid=' . $$self->{TRACKERS}{$ontology_id}{'atid'});
  die_xml('failed to access tracker page') if ! $mech->res->is_success;

  my @items = ();

  ## Scrape the tables and see if there are more results than what is
  ## displayed.
  my $stop_p = 0;
  do {

    ## Next, extract the important table using a perl module.
    my @column_headers = ("Request ID", "Summary", "Open Date",
			  "Priority", "Assigned To", "Submitted By");
    my $te = HTML::TableExtract->new( headers => [@column_headers] );
    $te->parse($mech->content);
    foreach my $ts ($te->tables) {
      foreach my $row ($ts->rows) {

	## Skip any undefined rows that may appear while dealing with
	## pages that have more than 50 rows.
	if( $$row[0] && $$row[1] && $$row[2] &&
	    $$row[3] && $$row[4] && $$row[5] ){

	  ## Optionally, only get the ones for their login if they
	  ## supplied one.
	  if( $username && $username eq $$row[5] ||
	      ! $username ){

	    ## Push what we want into a hash.
	    (my $foo = $$row[2]) =~ tr/0-9\:\- //cd; # Nuke strange char.
	    push @items, {
			  submitted_by => join(' ', split(' ', $$row[5])),
			  item_id => join(' ', split(' ', $$row[0])),
			  open_date => join(' ', split(' ', $foo)),
			  priority => join(' ', split(' ', $$row[3])),
			  assigned_to => join(' ', split(' ', $$row[4])),
			  summary =>
			  escapeHTML( join(' ', split(' ', $$row[1])))
			 };
	  }
	}
      }
    }

    ## Go to next page and see if there are more tracker items.
    #print STDERR ">Iteration $continue_p: " . $mech->uri() . "\n";
    if( $mech->content =~ /Next 50 --/gs ){
      my $followed = $mech->follow_link( text_regex => qr/Next 50 --/ );
      die_xml('failed to follow tracker link') if ! $followed;
      die_xml('failed to access tracker page') if ! $mech->res->is_success;
    }else{
      $stop_p = 1;
    }
  } until $stop_p;


  ## If we want added details, go through what we've collected to far
  ## and add them.
  if( $detailed_information eq 'true' ){

    ## For every item we collected...add more information fields.
    foreach my $item (@items){

      #print STDERR 'http://sourceforge.net/tracker/index.php?func=detail' .
      #	'&group_id=' . $$self->{TRACKERS}{$ontology_id}{'group_id'} .
      #	  '&atid=' . $$self->{TRACKERS}{$ontology_id}{'atid'} .
      #	    '&aid=' . $$item{'item_id'} . "\n";
      #      sleep 1;

      ## ...goto tracker page...
      my $mech = WWW::Mechanize->new();
      $mech->get('http://sourceforge.net/tracker/index.php?func=detail' .
		 '&group_id=' . $$self->{TRACKERS}{$ontology_id}{'group_id'} .
		 '&atid=' . $$self->{TRACKERS}{$ontology_id}{'atid'} .
		 '&aid=' . $$item{'item_id'});
      die_xml('failed to access item page on tracker page')
	if ! $mech->res->is_success;

      ## ...scrape the status...
      $mech->content =~ /<b>Status: .*<br>\s*(\w*)\s*<\/td>.*Resolution: /s;
      my $status_text = $1;
      die_xml('failed to get item status') if ! $status_text;
      $status_text =~ tr/A-Z/a-z/;
      $$item{'status'} =  $status_text;

      ## ...and the resolution status...
      $mech->content =~ /<b>Resolution: .*<br>\s*(\w*)\s*<\/td>.*Summary: /s;
      my $resolution_text = $1;
      die_xml('failed to get item resoluton') if ! $resolution_text;
      $resolution_text =~ tr/A-Z/a-z/;
      $$item{'resolution_status'} = $resolution_text;

      ## ...and the actual resolution ('none' if none) in the most
      ## recent [TERM]...END block.
      my @resolutions = $mech->content =~ /\[RESOLUTION\](.*?)END/gs;
      #my $resolution_actual = $resolutions[$#resolutions];
      my $resolution_actual = $resolutions[0] if $resolutions[0];
      $resolution_actual = 'none' if ! $resolutions[0];
      $resolution_text = remove_html_br($resolution_text);
      $$item{'resolution'} =  escapeHTML( $resolution_actual );

      ## ...and the definition ('' if none) in the most recent
      ## [DEFINITION]...END block.
      my @definitions = $mech->content =~ /\[DEFINITION\](.*?)END/gs;
      my $definition_text = $definitions[0] if $definitions[0];
      $definition_text = '' if ! $definitions[0];
      $definition_text = remove_html_br($definition_text);
      $$item{'definition'} =  escapeHTML( $definition_text );

      ## ...and the details ('' if none) in the most recent
      ## [DETAILS]...END block.
      my @details = $mech->content =~ /\[DETAILS\](.*?)END/gs;
      my $details_text = $details[0] if $details[0];
      $details_text = '' if ! $details[0];
      $details_text = remove_html_br($details_text);
      $$item{'details'} =  escapeHTML( $details_text );

      ## ...and the attribution ('' if none) in the most recent
      ## [ATTRIBUTION]...END block.
      my @attribution = $mech->content =~ /\[ATTRIBUTION\](.*?)END/gs;
      my $attribution_text = $attribution[0] if $attribution[0];
      $attribution_text = '' if ! $attribution[0];
      $attribution_text = remove_html_br($attribution_text);
      $$item{'attribution'} =  escapeHTML( $attribution_text );

      ## ...and the agent ('' if none) in the most recent
      ## [AGENT]...END block.
      my @agent = $mech->content =~ /\[AGENT\](.*?)END/gs;
      my $agent_text = $agent[0] if $agent[0];
      $agent_text = '' if ! $agent[0];
      $agent_text = remove_html_br($agent_text);
      $$item{'agent'} =  escapeHTML( $agent_text );

      ## ...and the modtype ('' if none) in the most recent
      ## [MODTYPE]...END block.
      my @modtype = $mech->content =~ /\[MODTYPE\](.*?)END/gs;
      my $modtype_text = $modtype[0] if $modtype[0];
      $modtype_text = '' if ! $modtype[0];
      $modtype_text = remove_html_br($modtype_text);
      $$item{'modtype'} =  escapeHTML( $modtype_text );

      #print STDERR @resolutions . "\n";
      #print STDERR $resolution_actual . "\n";
      #sleep 1;
    }
  }


  ## Emit everything that we have collected.
  #print STDERR "_" . @items . "\n";
  if ( $format eq 'obo' ) {


    my $day = (localtime)[3];
    if( length($day) == 1 ){ $day = '0' . $day; }
    my $month = (localtime)[4];
    if( length($month) == 1 ){ $month = '0' . $month; }
    my $year = (localtime)[5];
    if( length($year) == 1 ){ $year = '0' . $year; }
    ## BUG:
    $year = '2007';
    my $hour = (localtime)[2];
    if( length($hour) == 1 ){ $hour = '0' . $hour; }
    my $minute = (localtime)[1];
    if( length($minute) == 1 ){ $minute = '0' . $minute; }

    ## Header.
    push @obo_output, 'format-version: 1.2';
    push @obo_output, "\n";
    push @obo_output, 'date: ' .
      $day . ':' . $month . ':' . $year . ':' . $hour . ':' . $minute;
    push @obo_output, "\n";
    push @obo_output, 'auto-generated-by: AmiGO ORB Client 0.1b';
    push @obo_output, "\n";
    push @obo_output, "\n";

    ## Stanzas.
    foreach my $item (@items){

      push @obo_output, '[Term]';
      push @obo_output, "\n";
      push @obo_output, 'id: ' .
	$$item{'submitted_by'} . ':' .
	  $$item{'item_id'};
      push @obo_output, "\n";

      ##
      if ( $$item{'resolution'} ne 'none' ) {
	push @obo_output, 'is_obsolete: true';
	push @obo_output, "\n";
	$$item{'resolution'} =~ s/^\s+//;
	$$item{'resolution'} =~ s/\s+$//;
	$$item{'resolution'} =~ s/\s+/ /;
	push @obo_output, 'consider: ' . $$item{'resolution'};
	push @obo_output, "\n";
      }

      push @obo_output, 'name: ' . $$item{'summary'};
      push @obo_output, "\n";
      push @obo_output, 'comment: ' . $$item{'details'};
      push @obo_output, "\n";
      push @obo_output, 'creation_date: ' . $$item{'open_date'};
      push @obo_output, "\n";
      push @obo_output, 'definition: "' . $$item{'definition'} .
	'" [orb:' . $$item{'submitted_by'} . ']';
      push @obo_output, "\n";
      push @obo_output, 'attribution: ' . $$item{'attribution'};
      push @obo_output, "\n";
      push @obo_output, "\n";
    }

}else {

    foreach my $item (@items){
      push @xml_output, '<item>';
      foreach my $field (keys %$item){
	push @xml_output, '<';
	push @xml_output, $field;
	push @xml_output, '>';
	push @xml_output, $$item{$field};
	push @xml_output, '</';
	push @xml_output, $field;
	push @xml_output, '>';
      }
      push @xml_output, '</item>';
    }

    ## Cap output.
    push @xml_output, '</response>';
  }

} elsif ( $request eq 'add' ) {

  ## Common requirements.
  die_xml('must include required sub-parameter: agent')
    if ! $agent;
  die_xml('must include required sub-parameter: summary')
    if ! $summary;
  die_xml('must include required sub-parameter: definition')
    if ! $definition;
  die_xml('must include required sub-parameter: details')
    if ! $details;
  die_xml('must include required sub-parameter: ontology_id')
    if ! $ontology_id;

  ## Here we create a unified detail field for the curators sanity:
  my $sf_details = '';

  ## Add optional modtype.
  $modtype = 'none' if ! $modtype;
  $sf_details .= "\n[MODTYPE]\n" . $modtype . "\nEND\n\n";

  ## Add optional attribution.
  $attribution = 'none' if ! $ attribution;
  $sf_details .= "[ATTRIBUTION]\n" . $attribution . "\nEND\n\n";

  ## Add required definition.
  $definition = 'none' if ! $definition;
  $sf_details .= "[DEFINITION]\n" . $definition . "\nEND\n\n";

  ## Add required details.
  $details = 'none' if ! $details;
  $sf_details .= "[DETAILS]\n" . $details . "\nEND\n\n";

  ## Add required agent.
  $agent = 'none' if ! $agent;
  $sf_details .= "[AGENT]\n" . $agent . "\nEND\n\n";

  ## Mangle all things that look like they might be email addresses in
  ## the $sf_details. Not a perfect RFC822, but close-ish.
  $sf_details =~ s/([a-zA-Z0-9._%-]+)\@([a-zA-Z0-9.-]+\.[a-zA-Z]{2,8})/\{person\/handle $1 who is located in $2\}/g;

  ## Get the tracker form and add item.
  my $fields = {
		'func' => 'postadd',
		'group_id' => $$self->{TRACKERS}{$ontology_id}{'group_id'},
		'atid' => $$self->{TRACKERS}{$ontology_id}{'atid'},
		'category_id' => $category_id,
		'artifact_group_id' => $artifact_group_id,
		'summary' => $summary,
		'details' => $sf_details
	       };

  ## Default to default login and password if neither and we are
  ## disallowing anonymous logins.
  if ( ! $login && ! $password && ! $allow_anonymous ) {
    $login = $global_login_default;
    $password = $global_password_default;
  }

  ## Here is the user login subsection. If there is a login, there
  ## must also be a password.
  my $mech = WWW::Mechanize->new();
  $mech->cookie_jar(HTTP::Cookies->new());
  my $response;
  if ( $login ) {

    die_xml('password is required for login') if ! $password;

    ## Goto login page.
    $response = $mech->get('https://sourceforge.net/account/login.php');
    die_xml( 'failed to add find login page') if ! $response->is_success;

    ## Get the login form and login
    $mech->form_name('login');
    $mech->field(form_loginname => $login);
    $mech->field(form_pw => $password);
    $mech->click();

    ## Check login success.
    die_xml('bad login or password')
      if $mech->content =~ /Invalid Password or User Name/s;

    ## Goto tracker page.
    $mech->get('https://sourceforge.net/tracker/?' .
	       'func=add' .
	       '&group_id=' . $$self->{TRACKERS}{$ontology_id}{'group_id'} .
	       '&atid=' . $$self->{TRACKERS}{$ontology_id}{'atid'});

    $response = $mech->submit_form(form_number => 4,
				   fields => $fields);

  }else{

    if( ! $login && $password ){
      die_xml('missing login name');
    }

    ## Goto tracker page.
    $mech->get('http://sourceforge.net/tracker/?' .
	       'func=add' .
	       '&group_id=' . $$self->{TRACKERS}{$ontology_id}{'group_id'} .
	       '&atid=' . $$self->{TRACKERS}{$ontology_id}{'atid'});

    $response = $mech->submit_form(form_number => 3,
				   fields => $fields);
  }

  # ## DEBUG:
  #open(OUTFILE, ">out1.html");
  #print OUTFILE $mech->content();
  #close(OUTFILE);

  ## Final and check.
  die_xml( 'failed to add term (connection)') if ! $response->is_success;

  $response->content =~ /aid=(\d+)/s;
  my $return_id = $1;

  ## Check that we got the success page.
  die_xml('failed to add term (failure); possible duplicate') if ! $return_id;

  push @xml_output, '<ontology_id>';
  push @xml_output, $ontology_id;
  push @xml_output, '</ontology_id>';

  push @xml_output, '<item_id>';
  push @xml_output, $return_id;
  push @xml_output, '</item_id>';

  ## Cap output.
  push @xml_output, '</response>';

} elsif ( $request eq 'jsapi' ) {

  ## Flush output buffer.
  @xml_output = ();

  push @xml_output, "Cache-Control: no-cache\n";
  push @xml_output, "Content-Type: text/javascript; charset=utf-8\n\n";

  my $full_path = '../../htdocs/amigo2/orb.js';

  open(INFILE, "<$full_path")
    or die_xml('failed to find JSAPI');

  my $current_ontology = '';
  while (<INFILE>) {
    push @xml_output, $_;
  }

  ## Add the vital information for the trackers as an object at the
  ## end.
  push @xml_output, "\n\n";
  push @xml_output, "// Dynamically generated list.\n";
  push @xml_output, "var ORBTrackers = [\n";

  my @cache = ();
  foreach my $tracker (keys %$self->{TRACKERS}){

    ## TODO: Add some proper erroring.
    if( $tracker &&
	$$self->{TRACKERS}{$tracker}{'name'} &&
	$$self->{TRACKERS}{$tracker}{'description'} &&
	$$self->{TRACKERS}{$tracker}{'url'} &&
	$$self->{TRACKERS}{$tracker}{'tracker'} ){

      push @cache, '{ ' .
	'ontology_id: "' . $tracker . '", ' .
	  'name: "' . $$self->{TRACKERS}{$tracker}{'name'} . '", ' .
	    'description: "' . $$self->{TRACKERS}{$tracker}{'description'} . '", ' .
	      'url: "' . escapeHTML($$self->{TRACKERS}{$tracker}{'url'}) . '", ' .
		'tracker: "' . $$self->{TRACKERS}{$tracker}{'tracker'} . '", ' .
		  'group_id: "' . $$self->{TRACKERS}{$tracker}{'group_id'} . '", ' .
		    'atid: "' . $$self->{TRACKERS}{$tracker}{'atid'} . '" ' .
		      '}';
    }else{
      push @cache, '{ ' .
	'ontology_id: "unknown", ' .
	  'name: "unknown", ' .
	    'description: "unknown", ' .
	      'url: "unknown", ' .
		'tracker: "unknown", ' .
		  'group_id: "unknown", ' .
		    'atid: "unknown" ' .
		      '}';
    }
  }

  push @xml_output, join ", \n", @cache;
  push @xml_output, "\n];";

} else {

  push @xml_output, '<parameter name="request" type="required"';
  push @xml_output, '           value="defined">';

  push @xml_output, ' <value name="trackers">';
  push @xml_output, '  <description>';
  push @xml_output, '   Return information about defined trackers.';
  push @xml_output, '  </description>';
  push @xml_output, ' </value>';

  push @xml_output, ' <value name="tracker_information">';
  push @xml_output, '  <description>';
  push @xml_output, '   Return the different possible field name/value ';
  push @xml_output, '   pairs for a given ontology.';
  push @xml_output, '  </description>';

  push @xml_output, '  <parameter name="ontology_id" type="required"';
  push @xml_output, '             value="limited:runtime">';
  push @xml_output, '   <description>';
  push @xml_output, '    ID of single defined ontology in a tracker.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, ' </value>';

  push @xml_output, ' <value name="items">';
  push @xml_output, '  <description>';
  push @xml_output, '   Returns opened terms that are in the queue.';
  push @xml_output, '  </description>';

  push @xml_output, '  <parameter name="ontology_id" type="required"';
  push @xml_output, '             value="limited:runtime">';
  push @xml_output, '   <description>';
  push @xml_output, '    ID of single defined ontology in a tracker.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="username" type="opt"';
  push @xml_output, '             value="free">';
  push @xml_output, '   <description>';
  push @xml_output, '    A filter on the login name of the user.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="detailed_information" type="opt"';
  push @xml_output, '             value="true|false">';
  push @xml_output, '   <description>';
  push @xml_output, '    Whether or not we want deep details of the items. ';
  push @xml_output, '    WARNING: Could be expensive.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, ' </value>';

  push @xml_output, ' <value name="add">';
  push @xml_output, '  <description>';
  push @xml_output, '   Add term or ontology modification proposal ';
  push @xml_output, '   to the tracker.';
  push @xml_output, '  </description>';

  push @xml_output, '  <parameter name="agent" type="required"';
  push @xml_output, '             value="free">';
  push @xml_output, '   <description>';
  push @xml_output, '    A string that identifies the user agent.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="ontology_id" type="required"';
  push @xml_output, '             value="limited:runtime">';
  push @xml_output, '   <description>';
  push @xml_output, '    ID of single defined ontology in a tracker.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="category_id" type="optional"';
  push @xml_output, '             value="limited:runtime">';
  push @xml_output, '   <description>';
  push @xml_output, '    Category ID of term.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="artifact_group_id" type="optional"';
  push @xml_output, '             value="limited:runtime">';
  push @xml_output, '   <description>';
  push @xml_output, '    Group ID of term.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="summary" type="required"';
  push @xml_output, '             value="free">';
  push @xml_output, '   <description>';
  push @xml_output, '    Summary of the term to be added/modified.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="definition" type="required"';
  push @xml_output, '             value="free">';
  push @xml_output, '   <description>';
  push @xml_output, '    The definition in the ontology of the modification.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="details" type="required"';
  push @xml_output, '             value="free">';
  push @xml_output, '   <description>';
  push @xml_output, '    Details/reason of the term to be added/modified.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="modtype" type="optional"';
  push @xml_output, '             value="defined">';
  push @xml_output, '   <description>';
  push @xml_output, '    The type of proposed modification to make.';
  push @xml_output, '   </description>';
  push @xml_output, '   <value name="new">';
  push @xml_output, '    <description>';
  push @xml_output, '     Propose a new term.';
  push @xml_output, '    </description>';
  push @xml_output, '   </value>';
  push @xml_output, '   <value name="modify">';
  push @xml_output, '    <description>';
  push @xml_output, '     Propose a term or ontology modification.';
  push @xml_output, '    </description>';
  push @xml_output, '   </value>';
  push @xml_output, '   <value name="other">';
  push @xml_output, '    <description>';
  push @xml_output, '     Propose some other modification.';
  push @xml_output, '    </description>';
  push @xml_output, '   </value>';
  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="login" type="optional"';
  push @xml_output, '             value="free">';
  push @xml_output, '   <description>';
  push @xml_output, '    SF.net login name of user.';
  push @xml_output, '   </description>';

  push @xml_output, '   <parameter name="password" type="required"';
  push @xml_output, '              value="free">';
  push @xml_output, '    <description>';
  push @xml_output, '     Password for SF.net login.';
  push @xml_output, '    </description>';
  push @xml_output, '   </parameter>';

  push @xml_output, '  </parameter>';

  push @xml_output, '  <parameter name="attribution" type="optional"';
  push @xml_output, '             value="free">';
  push @xml_output, '   <description>';
  push @xml_output, '    Identity information for the submitting user.';
  push @xml_output, '   </description>';
  push @xml_output, '  </parameter>';

  push @xml_output, ' </value>';

  push @xml_output, ' <value name="jsapi">';
  push @xml_output, '  <description>';
  push @xml_output, '   Provides JavaScript API file with embeddable client.';
  push @xml_output, '  </description>';
  push @xml_output, ' </value>';

  push @xml_output, '</parameter>';

  ## Cap output.
  push @xml_output, '</response>';
}

## Join and dump the output buffer.
if ( $format eq 'obo') {
  print join '', @obo_output;
}else {
  print join '', @xml_output;
}



##########
##
## Subs.
##
##########


##
sub remove_html_br{

  my $line = shift;

  my $newline;
  ($newline = $line) =~ s/\<br\s*?\/?\s*?\>//g;
  $newline =~ s/^\s+//;
  $newline =~ s/\s+$//;

  return $newline;
}


## Fills a hash with the information in 'trackers.txt'. All incoming
## information should be checked against this hash.
sub parse_trackers_dot_txt{

  ## Parse trackers.txt and check to see if there is anything by that
  ## name that we can work with.
  open(INFILE, "<trackers.txt")
    or return ();

  ## Parse this:
  #! OBO Trackers
  #
  #name: geneontology-Curator-requests
  #ontology_idspace: GO
  #description: blah blah blah
  #url: http://sourceforge.net/tracker/?atid=440764&group_id=36855
  #tracker: sourceforge
  my %trackers = ();

  ## Slorp it in.
  my @mini_buff = ();
  while (<INFILE>) { push @mini_buff, $_; }
  close(INFILE);
  my $trackers_txt = join('', @mini_buff);

  ## Break it into double newline chunks.
  my @chunks = split /\n\n/s, $trackers_txt;

  foreach my $chunk (@chunks){

    ## If it looks like it has a key.
    if( $chunk =~ /ontology_idspace: (.*)/g ){

      my $current_ontology = $1;
      $self->{TRACKERS}{$current_ontology} = {};

      if( $chunk =~ /name: (.*)/ ){
	$trackers{$current_ontology}{'name'} = $1; }

      if( $chunk =~ /description: (.*)/ ){
	$trackers{$current_ontology}{'description'} = $1; }

      if( $chunk =~ /url: (.*)/ ){
	$trackers{$current_ontology}{'url'} = escapeHTML($1); }

      if( $chunk =~ /tracker: (.*)/ ){
	$trackers{$current_ontology}{'tracker'} = $1; }
    }
  }

  ## Chew %trackers to find out if it is a known tracker type and it
  ## the tracker type matches the information in the URL. Failure at any
  ## point should bomb.
  foreach my $oid (keys %trackers) {

    my $tracker =  $trackers{$oid}{'tracker'};
    my $url =  $trackers{$oid}{'url'};

    #die_xml("currently only supports SF.net") if $tracker ne 'sourceforge';
    if( $tracker ne 'sourceforge' ){
      delete $trackers{$oid};
      next;
    }

    $url =~ /group_id=(\d+)/s;
    return () if ! $1;
    my $group_id = $1;

    $url =~ /atid=(\d+)/s;
    return () if ! $1;
    my $atid = $1;

    $trackers{$oid}{'group_id'} = $group_id;
    $trackers{$oid}{'atid'} = $atid;
  }

  return \%trackers;
}


##
sub die_xml{

  my $error_message = shift;
  my @err_output = ();

  push @err_output, "Cache-Control: no-cache\n";
  push @err_output, "Content-Type: text/xml; charset=utf-8\n\n";
  push @err_output, '<response>';
  push @err_output, '<error>';
  push @err_output, $error_message;
  push @err_output, '</error>';
  push @err_output, '</response>';
  print join '', @err_output;

  die "fatal error: $error_message";
}



1;
