#!/usr/bin/perl -w
####
#### Emit config.pl and install AmiGO.
#### Optionally emit config.json.
####
####
#### TODO: There is a load of environmental variables that are set in
#### the original AmiGO that I don't set here. I should find out which
#### ones will be useful in the future and take care of them too.
####

use utf8;
use strict;
use vars qw(
	     $opt_h
	     $opt_v
	     $opt_r
	     $opt_i
	     $opt_t
	     $opt_s
	     $opt_c
	     $opt_f
	     $opt_e
	     $opt_g
	     $opt_G
	     $opt_d
	  );
use Getopt::Std;
use File::Basename;
use File::Find;
use Cwd;
use Data::Dumper;

##
my $amigo_base = getcwd();
my $go_base = substr($amigo_base, 0, 0 - length('/go-dev/amigo'));

## These are the user changable variables. They may be used to create
## synthetic variables.
my @env_conf_order = qw(
			 GO_SVN_ROOT

			 GO_DBNAME
			 GO_DBHOST
			 GO_DBUSER
			 GO_DBAUTH
			 GO_DBPORT
			 GO_DBSOCKET

			 GO_HAS_COUNT_BY_SPECIES

			 AMIGO_PROJECT_NAME
			 AMIGO_HTDOCS_PARTIAL_PATH
			 AMIGO_HTDOCS_PARTIAL_URL
			 AMIGO_CGI_PARTIAL_PATH
			 AMIGO_CGI_PARTIAL_URL

                         AMIGO_PUBLIC_CGI_PARTIAL_URL

			 AMIGO_SHOW_GP_OPTIONS
			 AMIGO_SHOW_GRAPHVIZ
			 AMIGO_DOT_PATH

			 AMIGO_SHOW_BLAST
			 AMIGO_FASTA_DB
			 AMIGO_BLASTP
			 AMIGO_BLASTX
			 AMIGO_BLAST_METHOD
			 AMIGO_QSUB
			 AMIGO_QUEUE
			 AMIGO_PBS_USER
			 AMIGO_MAX_SEQ_NUM
			 AMIGO_MAX_SEQ_LENGTH

			 AMIGO_SHOW_GOOSE_LINKS

			 AMIGO_USE_DEFAULT_AMIGO_FILTERS
			 AMIGO_SHOW_ONT_FILTER
			 AMIGO_SHOW_TAXID_FILTER
			 AMIGO_SHOW_SPECIESDB_FILTER
			 AMIGO_SHOW_EVCODE_FILTER
			 AMIGO_SHOW_GPTYPE_FILTER
			 AMIGO_SHOW_ASSBY_FILTER
			 AMIGO_SHOW_QUAL_FILTER
			 AMIGO_TEMPLATE_PATHS
			 AMIGO_SESSION_DIR
			 AMIGO_MAX_SESSIONS
			 AMIGO_SESSION_TIMEOUT
			 AMIGO_PAGE_SIZE
			 AMIGO_MAX_RESULTS_HTML
			 AMIGO_MAX_RESULTS_DOWNLOAD
			 AMIGO_CALCULATE_GP_COUNTS
			 AMIGO_CALCULATE_TERM_COUNTS
			 AMIGO_GET_RELEVANCE
			 AMIGO_CLEVER_MODE
			 AMIGO_OBSOLETE_BEHAVIOUR
			 AMIGO_TERM2TERM_METADATA_LOADED

			 AMIGO_TERM_REGEXP

		         AMIGO_GO_ONLY_GRAPHICS

		         AMIGO_VERSION
			 AMIGO_VERBOSE
			 AMIGO_TROUBLE_SWITCH
			 AMIGO_TROUBLE_MESSAGE
			 AMIGO_BETA
                         AMIGO_GOOGLE_ANALYTICS_ID
		      );

my %synth_vars = ();
my %env_conf = (
		GO_SVN_ROOT =>
		{
		 DEFAULT => $go_base,
		 MESSAGE => "Please enter the full path to the " .
		 "SVN geneontology directory ",
		 ERROR => 'not a directory',
		 PARSER => \&is_a_directory_p
		},

		AMIGO_SHOW_GP_OPTIONS =>
		{
		 DEFAULT => '1',
		 MESSAGE => "Have you loaded gene product associations " .
		 "into the database?\nUse 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p
		},

		AMIGO_SHOW_GRAPHVIZ =>
		{
		 DEFAULT => '1',
		 MESSAGE => "If you have GraphViz installed, AmiGO " .
		 "can use it to display\nontologies graphically. Enable " .
		 "GraphViz visualizations?\nUse 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p
		},

		AMIGO_DOT_PATH =>
		{
		 DEFAULT => '/usr/bin/dot',
		 MESSAGE => "Please enter the full path for the \"dot\"" .
		 " command ",
		 ERROR => 'not a binary file',
		 PARSER => \&is_a_binary_p,
		 DEPENDS => \&depends_is_graphviz_true_p,
		},

		GO_DBNAME =>
		{
		 DEFAULT => 'go_latest',
		 MESSAGE => "Please enter the name of the GO database " .
		 "on the server ",
		 ERROR => 'not a string',
		 PARSER => \&is_a_string_p
		},

		GO_DBHOST =>
		{
		 DEFAULT => 'localhost',
		 MESSAGE => "Please enter the hostname of the GO database ",
		 ERROR => 'not a string',
		 PARSER => \&is_a_url_p
		},

		GO_DBUSER =>
		{
		 DEFAULT => '',
		 MESSAGE => "Please enter the database user login ",
		 ERROR => 'not a string',
		 PARSER => \&is_a_string_or_empty_p
		},

		GO_DBAUTH =>
		{
		 DEFAULT => '',
		 MESSAGE => "Please enter the database user password ",
		 ERROR => 'not a string',
		 PARSER => \&is_a_string_or_empty_p
		},

		GO_DBSOCKET =>
		{
		 DEFAULT => '',
		 MESSAGE => "Please enter the database socket ",
		 ERROR => 'not a string',
		 PARSER => \&is_a_string_or_empty_p
		},

		GO_DBPORT =>
		{
		 DEFAULT => '',
		 MESSAGE => "Please enter the database port (the default nothing is a sane default) ",
		 ERROR => 'not a string',
		 PARSER => \&is_a_string_or_empty_p
		},

		GO_HAS_COUNT_BY_SPECIES =>
		{
		 DEFAULT => '1',
		 MESSAGE => "Please enter whether there is a count by species ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_PROJECT_NAME =>
		{
		 DEFAULT => 'amigo',
		 MESSAGE => "Please enter the name of this AmiGO installation ",
		 ERROR => 'not a string',
		 PARSER => \&is_a_string_p
		},

		AMIGO_HTDOCS_PARTIAL_PATH =>
		{
		 DEFAULT => '/srv/www/htdocs',
		 MESSAGE => "Please enter the full path to a usable " .
		 "static document directory ",
		 ERROR => 'not a writeable directory',
		 PARSER => \&is_a_writeable_directory_p
		},

		AMIGO_HTDOCS_PARTIAL_URL =>
		{
		 DEFAULT => 'http://localhost',
		 MESSAGE => "Please enter the URL that this path maps to ",
		 ERROR => 'not a URL',
		 PARSER => \&is_a_url_p
		},

		AMIGO_CGI_PARTIAL_PATH =>
		{
		 DEFAULT => '/srv/www/cgi-bin',
		 MESSAGE => "Please enter the full path to a usable CGI " .
		 "enabled directory ",
		 ERROR => 'not a writeable directory',
		 PARSER => \&is_a_writeable_directory_p
		},

		AMIGO_CGI_PARTIAL_URL =>
		{
		 DEFAULT => 'http://localhost/cgi-bin',
		 MESSAGE => "Please enter the URL that this path maps to ",
		 ERROR => 'not a URL',
		 PARSER => \&is_a_url_p
		},

		AMIGO_PUBLIC_CGI_PARTIAL_URL =>
		{
		 DEFAULT => 'http://amigo.geneontology.org/cgi-bin',
		 MESSAGE => "Please enter the partial public URL ",
		 ERROR => 'you should not play with this',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		#		AMIGO_DATA_PATH	=>
		#		{
		#		 DEFAULT => '/www/toy_9002/cgi-bin',
		#		 MESSAGE => "Please enter the full path to a usable (+rw) " .
		#		 "data directory ",
		#		 ERROR => 'not a writeable directory',
		#		 PARSER => \&is_a_writeable_directory_p
		#		},

		AMIGO_SHOW_BLAST =>
		{
		 DEFAULT => '1',
		 MESSAGE => "If you have installed the BLAST package from " .
		 "http://blast.wustl.edu\nyou can activate the AmiGO BLAST " .
		 "search. Enable BLAST searches?\nUse 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p
		},

		AMIGO_FASTA_DB =>
		{
		 DEFAULT => '/srv/www/data/go.fasta',
		 MESSAGE => "Please enter the location of the FASTA db ",
		 ERROR => 'not a readable file',
		 PARSER => \&is_a_file_p,
		 DEPENDS => \&depends_is_blast_true_p,
		},

		AMIGO_BLASTP =>
		{
		 DEFAULT => '/usr/bin/blastp',
		 MESSAGE => "Please enter the full path for the blastp " .
		 "command ",
		 ERROR => 'not a binary file',
		 PARSER => \&is_a_binary_p,
		 DEPENDS => \&depends_is_blast_true_p,
		},

		AMIGO_BLASTX =>
		{
		 DEFAULT => '/usr/bin/blastx',
		 MESSAGE => "Please enter the full path for the blastx " .
		 "command ",
		 ERROR => 'not a binary file',
		 PARSER => \&is_a_binary_p,
		 DEPENDS => \&depends_is_blast_true_p,
		},

		#		AMIGO_BLASTN =>
		#		{
		#		 DEFAULT => '/share/bdgp64/wublast/blastn',
		#		 MESSAGE => "Please enter the full path for the blastn " .
		#		 "command ",
		#		 ERROR => 'not a binary file',
		#		 PARSER => \&is_a_binary_p,
		#		 DEPENDS => \&depends_is_blast_true_p,
		#		},

		AMIGO_MAX_SEQ_NUM =>
		{
		 DEFAULT => '100',
		 MESSAGE => "Please enter the maximum number of " .
		 "sequences allowed for BLAST ",
		 ERROR => 'not a number',
		 PARSER => \&is_a_number_p,
		 DEPENDS => \&depends_is_blast_true_p,
		},

		AMIGO_MAX_SEQ_LENGTH =>
		{
		 DEFAULT => '3000000',
		 MESSAGE => "Please enter the maximum sequence length " .
		 "allowed for BLAST ",
		 ERROR => 'not a number',
		 PARSER => \&is_a_number_p,
		 DEPENDS => \&depends_is_blast_true_p,
		},

		AMIGO_BLAST_METHOD =>
		{
		 DEFAULT => 'cgi',
		 MESSAGE => "Please enter the BLAST submission method " .
		 "(\"cgi\" or \"pbs\") ",
		 ERROR => 'not "cgi" or "pbs"',
		 PARSER => \&is_a_blast_method_p,
		 DEPENDS => \&depends_is_blast_true_p,
		},

		AMIGO_QSUB =>
		{
		 DEFAULT => '/usr/local/command',
		 MESSAGE => "Please enter full path for pbs qsub command ",
		 ERROR => 'not a URL',
		 PARSER => \&is_a_binary_p,
		 DEPENDS => \&depends_is_blast_pbs_true_p,
		},

		AMIGO_QUEUE =>
		{
		 DEFAULT => '/usr/local/queue',
		 MESSAGE => "Please enter pbs queue ",
		 ERROR => 'not a URL',
		 PARSER => \&is_a_directory_p,
		 DEPENDS => \&depends_is_blast_pbs_true_p,
		},

		AMIGO_PBS_USER =>
		{
		 DEFAULT => 'nobody',
		 MESSAGE => "Please enter pbs user ",
		 ERROR => 'not a URL',
		 PARSER => \&is_a_string_p,
		 DEPENDS => \&depends_is_blast_pbs_true_p,
		},

		AMIGO_SHOW_GOOSE_LINKS =>
		{
		 DEFAULT => '1',
		 MESSAGE => "Do you wish to show links to GOOSE, the GO Online " .
		 "SQL Environment?\nNote that this is only suitable for " .
		 "if you are installing the GO database.\nUse 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p
		},

		AMIGO_USE_DEFAULT_AMIGO_FILTERS =>
		{
		 DEFAULT => '1',
		 MESSAGE => "By default, AmiGO allows users to filter by " .
		 "ontology,\nspecies, database, evidence code and gene " .
		 "product type.\nThe qualifier and assigned by fields can " .
		 "also be filtered.\nUse the default filter set? " .
		 "Use 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p
		},

		AMIGO_SHOW_ONT_FILTER =>
		{
		 DEFAULT => '1',
		 MESSAGE => "Enable the ontology filter? " .
		 "Use 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p,
		 DEPENDS => \&depends_is_filters_false_p,
		},

		AMIGO_SHOW_TAXID_FILTER =>
		{
		 DEFAULT => '1',
		 MESSAGE => "Enable the taxonomy id filter? " .
		 "Use 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p,
		 DEPENDS => \&depends_is_filters_false_p,
		},

		AMIGO_SHOW_SPECIESDB_FILTER =>
		{
		 DEFAULT => '1',
		 MESSAGE => "Enable the species database filter? " .
		 "Use 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p,
		 DEPENDS => \&depends_is_filters_false_p,
		},

		AMIGO_SHOW_EVCODE_FILTER =>
		{
		 DEFAULT => '1',
		 MESSAGE => "Enable the evidence filter? " .
		 "Use 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p,
		 DEPENDS => \&depends_is_filters_false_p,
		},

		AMIGO_SHOW_GPTYPE_FILTER =>
		{
		 DEFAULT => '1',
		 MESSAGE => "Enable the gene product type filter? " .
		 "Use 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p,
		 DEPENDS => \&depends_is_filters_false_p,
		},

		AMIGO_SHOW_ASSBY_FILTER =>
		{
		 DEFAULT => '0',
		 MESSAGE => "Enable the 'assigned by' filter? " .
		 "Use 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p,
		 DEPENDS => \&depends_is_filters_false_p,
		},

		AMIGO_SHOW_QUAL_FILTER =>
		{
		 DEFAULT => '0',
		 MESSAGE => "Enable the qualifier filter? " .
		 "Use 1 for yes and 0 for no ",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p,
		 DEPENDS => \&depends_is_filters_false_p,
		},

		AMIGO_TERM_REGEXP =>
		{
		 DEFAULT => 'all|GO\:[0-9]{7}',
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_TEMPLATE_PATHS =>
		{
		 DEFAULT => 'templates/pages:templates/includes',
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_SESSION_DIR =>
		{
		 DEFAULT => 'sessions',
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_MAX_SESSIONS =>
		{
		 DEFAULT => '200',
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_SESSION_TIMEOUT =>
		{
		 DEFAULT => '7200',
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_PAGE_SIZE =>
		{
		 DEFAULT => '50',
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_MAX_RESULTS_HTML =>
		{
		 #DEFAULT => '2000',
		 DEFAULT => '200', # 4 pages worth
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_MAX_RESULTS_DOWNLOAD =>
		{
		 #DEFAULT => '20000',
		 #DEFAULT => '200', # 4 pages worth
		 DEFAULT => '1000', # 20 pages worth
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_CALCULATE_GP_COUNTS =>
		{
		 DEFAULT => '0',
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_CALCULATE_TERM_COUNTS =>
		{
		 DEFAULT => '0',
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_GET_RELEVANCE =>
		{
		 DEFAULT => '1',
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_CLEVER_MODE =>
		{
		 DEFAULT => '1',
		 MESSAGE => 'hidden feature',
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_OBSOLETE_BEHAVIOUR =>
		{
		 DEFAULT => 'include_all',
		 MESSAGE => "AmiGO term searches may return obsolete terms.\n" . 
		 "Please choose how AmiGO should handle these terms:\n" .
		 "\"ignore\":all obsolete terms will be excluded from the results \n" .
		 "\"include_commented\": obsolete terms which have a GO id tucked away in a nearby field will be included; non-GO sites should never use this value as the GO id part is currently hard-coded\n" .
		 "\"include_all\": no obsolete terms will be excluded from the results",
		 ERROR => 'not "ignore", "include_commented", or "include_all"',
		 PARSER => \&is_an_obsolete_behaviour_p,
		 #DEPENDS => \&is_always_false,
		},

		AMIGO_TERM2TERM_METADATA_LOADED =>
		{
		 DEFAULT => '0',
		 MESSAGE => 
		 "Do you have the term2term_metadata table in the database populated?\n".
		 "If you have generated your database from an OBO v1.2 (or greater)\n".
		 "format file, you should answer yes to this.\n".
		 "Use 1 for yes and 0 for no.",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p,
		},

		AMIGO_GO_ONLY_GRAPHICS =>
		{
		 DEFAULT => '1',
		 MESSAGE => "If you want to use graphics that cannot possibly work outside of GO.",
		 ERROR => 'not a 0 or a 1',
		 PARSER => \&is_a_boolean_p
		},

		AMIGO_VERBOSE =>
		{
		 DEFAULT => '0',
		 MESSAGE => "Use verbose AmiGO debugging messages.",
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_TROUBLE_SWITCH =>
		{
		 DEFAULT => '0',
		 MESSAGE =>
		 "Mark the AmiGO site as TROUBLED (triggers some changes).",
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_TROUBLE_MESSAGE =>
		{
		 DEFAULT => '<h2 style="text-align: center;">AmiGO is currently experiencing some difficulty.<br />You may want to temporarily try <a href="http://amigo.berkeleybop.org/cgi-bin/amigo/go.cgi" title="Go to AmiGO Labs">AmiGO Labs</a> instead.</h2>',
		 MESSAGE =>
		 "Message to present when AmiGO is TROUBLED.",
		 ERROR => 'not a string or empty',
		 PARSER => \&is_a_string_or_empty_p,
		},

		AMIGO_BETA =>
		{
		 DEFAULT => '0',
		 MESSAGE =>
		 "Mark the AmiGO site as BETA (triggers some changes in variables and installed features).",
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_VERSION =>
		{
		 DEFAULT => '1.8',
		 MESSAGE => "Displayed AmiGO version string.",
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},

		AMIGO_GOOGLE_ANALYTICS_ID =>
		{
		 DEFAULT => '',
		 MESSAGE => "Google analytics id",
		 ERROR => 'generic error',
		 PARSER => \&is_always_true,
		 DEPENDS => \&is_always_false,
		},
	       );

## First things first, check that the env_conf and env_conf_order
## variable have the same contents.
## Check one-to-one.
foreach ( @env_conf_order ) {
  die "Error: Order and contents don't match ($_) $!"
    if ! $env_conf{$_};
}
## Check onto.
foreach my $env_conf_key ( keys %env_conf ) {
  my $found_it_p = 0;
  foreach my $env_conf_order_entry ( @env_conf_order ) {
    $found_it_p = 1
      if $env_conf_key eq $env_conf_order_entry;
  }
  die "Error: Contents and order don't match ($env_conf_key) $!"
    if ! $found_it_p;
}


## Since internal checks are done, get ready for user input.
getopts('hvriscmtegGdf:');

## Embedded help through perldoc.
if( $opt_h ){
  system('perldoc', __FILE__);
  exit 0;
}

## Empty init (helps later scoping fun).
my @args = ();

## Check our options.
ll("Will print verbose messages.");
if( $opt_s ){
  ll("Will NOT create caches.");
}
if ( $opt_t ) {
  ll("Will do test run.");
}
if ( $opt_e ) {
  ll("Will also install experimental binaries and executables.)");
}
if ( $opt_g ) {
  ll("Will also GOOSE.");
}
if ( $opt_G ) {
  ll("Will try to install *only* GOOSE.");
}

## First check that we are in the proper directory. Installation
## should only occur in the top directory.
if ( ! -f "./version.pl" ||
     ! -f "./simulate.pl" ||
     ! -f "./refresh.pl" ||
     ! -f "./install.pl" ) {
  ll("This does not seem to be the base AmiGO directory!");
  ll("Please install from the base AmiGO directory.");
  exit 0;
}

## Check to see if there is a config file or an
## config.pl file.
if ( $opt_f && -f $opt_f ) {
  ll("Configuration file at \"$opt_f\" found.");
  get_amigo_installation_variables_from_file($opt_f);
} elsif ( -f 'config.pl' ) {
  if ( $opt_r ) {
    ll("Previous \"config.pl\" file found: " .
       "will use as basis for reconfiguration.");
    get_amigo_installation_variables_from_file('config.pl');
    get_amigo_installation_variables_from_user();
  } elsif ( $opt_i ) {
    ll("Ignoring previous \"config.pl\" file...");
    get_amigo_installation_variables_from_user();
  } else {
    ll("Previous \"config.pl\" file found: " .
       "will use as this instance's configuration file.");
    get_amigo_installation_variables_from_file('config.pl');
  }
} else {
  ll("No \"config.pl\" file found: " .
     "will interactively create a new one.");
  get_amigo_installation_variables_from_user();
}


## We now have as much information as we're going to get about the
## wanted environment, so let's make new variables and write
## everything out to the config file.

## As a convenience, let's switch the amigo_beta new_value on the "-e"
## flag. This will tie installation and experimental.
if( $opt_e ){
  $env_conf{AMIGO_BETA}{NEW_VALUE} = 1;
}

$synth_vars{AMIGO_HTDOCS_ROOT_DIR} =
  eval {
    my $foo = '';
    ( $foo = $env_conf{AMIGO_HTDOCS_PARTIAL_PATH}{NEW_VALUE} ) =~ s/\/$//;
    $foo; }
  . '/' . $env_conf{AMIGO_PROJECT_NAME}{NEW_VALUE};

$synth_vars{AMIGO_CGI_ROOT_DIR} =
  eval {
    my $foo = '';
    ( $foo = $env_conf{AMIGO_CGI_PARTIAL_PATH}{NEW_VALUE} ) =~ s/\/$//;
    $foo; }
  . '/' . $env_conf{AMIGO_PROJECT_NAME}{NEW_VALUE};

$synth_vars{AMIGO_CGI_URL} =
  eval {
    my $foo = '';
    ( $foo = $env_conf{AMIGO_CGI_PARTIAL_URL}{NEW_VALUE} ) =~ s/\/$//;
    $foo; }
  . '/' . $env_conf{AMIGO_PROJECT_NAME}{NEW_VALUE};

$synth_vars{AMIGO_SERVICE_URL} = $synth_vars{AMIGO_CGI_URL} . '/aserve';

$synth_vars{AMIGO_PUBLIC_CGI_URL} =
  eval {
    my $foo = '';
    ( $foo = $env_conf{AMIGO_PUBLIC_CGI_PARTIAL_URL}{NEW_VALUE} ) =~ s/\/$//;
    $foo; }
  . '/amigo';

$synth_vars{AMIGO_HTML_URL} =
  eval {
    my $foo = '';
    ( $foo = $env_conf{AMIGO_HTDOCS_PARTIAL_URL}{NEW_VALUE} ) =~ s/\/$//;
    $foo; }
  . '/' . $env_conf{AMIGO_PROJECT_NAME}{NEW_VALUE};

## Session top-level location.
$synth_vars{AMIGO_SESSIONS_ROOT_DIR} =
  $synth_vars{AMIGO_CGI_ROOT_DIR} .'/'. $env_conf{AMIGO_SESSION_DIR}{NEW_VALUE};

## Temp image URL and dir.
$synth_vars{AMIGO_TEMP_IMAGE_DIR} =
  $synth_vars{AMIGO_HTDOCS_ROOT_DIR} . '/' . 'tmp_images';
$synth_vars{AMIGO_TEMP_IMAGE_URL} =
  $synth_vars{AMIGO_HTML_URL} . '/' . 'tmp_images';

## Image URL and dir.
$synth_vars{AMIGO_IMAGE_DIR} =
  $synth_vars{AMIGO_HTDOCS_ROOT_DIR} . '/' . 'images';
$synth_vars{AMIGO_IMAGE_URL} =
  $synth_vars{AMIGO_HTML_URL} . '/' . 'images';

## Tests URL and dir.
$synth_vars{AMIGO_TESTS_DIR} =
  $synth_vars{AMIGO_HTDOCS_ROOT_DIR} . '/' . 'tests';
$synth_vars{AMIGO_TESTS_URL} =
  $synth_vars{AMIGO_HTML_URL} . '/' . 'tests';

## For page pre-rendering.
$synth_vars{AMIGO_PRE_RENDER_DIR} =
  $synth_vars{AMIGO_HTDOCS_ROOT_DIR} .
  '/' . 'pre_render';
$synth_vars{AMIGO_PRE_RENDER_URL} =
  $synth_vars{AMIGO_HTML_URL} .
  '/' . 'pre_render';

## A place for our various search indexes.
$synth_vars{AMIGO_INDEX_DIR} =
  $synth_vars{AMIGO_CGI_ROOT_DIR} .
  '/' . 'index';

## Specific paths to be used other places.
$synth_vars{AMIGO_INDEX_DIR_GENERAL} =
  $synth_vars{AMIGO_INDEX_DIR} .
  '/' . 'lucene/general';
$synth_vars{AMIGO_INDEX_DIR_TERM} =
  $synth_vars{AMIGO_INDEX_DIR} .
  '/' . 'lucene/term';
$synth_vars{AMIGO_INDEX_DIR_GENE_PRODUCT} =
  $synth_vars{AMIGO_INDEX_DIR} .
  '/' . 'lucene/gene_product';
# $synth_vars{AMIGO_INDEX_DIR_GENE_ASSOCIATION} =
#   $synth_vars{AMIGO_INDEX_DIR} .
#   '/' . 'lucene/association';
# $synth_vars{AMIGO_INDEX_DIR_GENE_XREF} =
#   $synth_vars{AMIGO_INDEX_DIR} .
#   '/' . 'lucene/xref';

## A place for caching databases.
$synth_vars{AMIGO_CACHE_DIR} =
  $synth_vars{AMIGO_CGI_ROOT_DIR} .
  '/' . 'cache';

## A place for log files.
$synth_vars{AMIGO_LOG_DIR} =
  $synth_vars{AMIGO_CGI_ROOT_DIR} .
  '/' . 'log';

## A place for scratch files.
$synth_vars{AMIGO_SCRATCH_DIR} =
  $synth_vars{AMIGO_CGI_ROOT_DIR} .
  '/' . 'sessions/scratch';

## TODO/BUG: Temporary hack to bridge the CVS to SVN conversion.
$synth_vars{GO_ROOT} = $env_conf{GO_SVN_ROOT}{NEW_VALUE} . '/go-dev/trunk';

create_synthetic_amigo_environmental_variables();
ll("Finish setting the installation environment.");

ll("Starting installation...");

## Make the new htdoc and cgi directories if necessary.
safe_make_dir( $synth_vars{AMIGO_CGI_ROOT_DIR} );
safe_make_dir( $synth_vars{AMIGO_CACHE_DIR} );
make_permissive( $synth_vars{AMIGO_CACHE_DIR} );
safe_make_dir( $synth_vars{AMIGO_LOG_DIR} );
make_permissive( $synth_vars{AMIGO_LOG_DIR} );
safe_make_dir( $synth_vars{AMIGO_INDEX_DIR} );
safe_make_dir( $synth_vars{AMIGO_INDEX_DIR} . '/xapian' );
safe_make_dir( $synth_vars{AMIGO_INDEX_DIR} . '/lucene' );
safe_make_dir( $synth_vars{AMIGO_INDEX_DIR} . '/lucene/general' );
safe_make_dir( $synth_vars{AMIGO_INDEX_DIR} . '/lucene/term' );
safe_make_dir( $synth_vars{AMIGO_INDEX_DIR} . '/lucene/gene_product' );
safe_make_dir( $synth_vars{AMIGO_CGI_ROOT_DIR} . '/sessions' );
safe_make_dir( $synth_vars{AMIGO_SCRATCH_DIR} );
make_permissive( $synth_vars{AMIGO_CGI_ROOT_DIR} . '/sessions' );
make_permissive( $synth_vars{AMIGO_SCRATCH_DIR} );
safe_make_dir( $synth_vars{AMIGO_HTDOCS_ROOT_DIR} );
safe_make_dir( $synth_vars{AMIGO_HTDOCS_ROOT_DIR} . '/opensearch');
safe_make_dir( $synth_vars{AMIGO_TEMP_IMAGE_DIR} );
make_permissive( $synth_vars{AMIGO_TEMP_IMAGE_DIR} );
safe_make_dir( $synth_vars{AMIGO_PRE_RENDER_DIR} );
safe_make_dir( $synth_vars{AMIGO_TESTS_DIR} );



##
if ( $opt_c ) {
  ll("Will generate cache files and perform no other operations.");
} else {

  ## Copy refresh script over to cgi-bin.
  #force_copy(Cwd::cwd() . '/amigo/cgi-bin/' . 'refresh',
  #       $synth_vars{AMIGO_CGI_ROOT_DIR} );
  #make_executable( $synth_vars{AMIGO_CGI_ROOT_DIR} . '/' . 'refresh');
  #make_non_cgi_link( $synth_vars{AMIGO_CGI_ROOT_DIR} . '/' . 'refresh');

  ## Standard modules
  my @std_mods = ();

  ## Copy bulk things over to the new directory, but only if we aren't
  ## doing a GOOSE-only install.
  if( ! $opt_G ){
    push @std_mods,
      qw(
	  amigo
	  aserve
	  visualize
	  term_details

	  browse.cgi
	  blast.cgi
	  go.cgi
	  gp-assoc.cgi
	  gp-assoc-view.cgi
	  gp-details.cgi
	  gp-select.cgi
	  term-assoc.cgi
	  term-chart.cgi
	  term-details.cgi
	  term-select.cgi
	  search.cgi

	  slimmer
	  term_enrichment
       );
  }

  ## Additional/non-standard modules
  my @additional_mods = ();

  ## Copy bulk experimental things over to the new directory.
  if ( $opt_e && ! $opt_G ) {
    push @additional_mods,
      qw(
	  amigo_exp
	  aserve_exp

	  completion
	  fcompletion

	  phylotree

	  xp_term_request

	  livesearch
	  report_1
	  report_2
	  report_3
	  report_4
	  page_test
	  test
	  ont
	  orb
	  tick
	  tock
	  gander
	  cont-test
	  wms
	  bench-cache-sqlite3.cgi
	  bench-cache-freezethaw.cgi
       );
  }

  ## Copy new GOOSE over on -g or -G.
  if ( $opt_g || $opt_G ) {
    push @additional_mods,
      qw(
	  goose
       );
  }

  ## Copy diagnose over on -d.
  if ( $opt_d ) {

    ## Copy the script over anyways (it has other useful stuff).
    push @std_mods,
      qw(
	  diagnose
       );

    ## Used perl lib cache. Don't want output, just the side effects.
    @args = ("perl", "./simulate.pl", "-j", ">", "/dev/null", "2>&1");
    ll("System: \"@args\"");
    eval {
      system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
    };
    if(@!){
      ww("WARNING: Apparently, simulate.pl isn\'t happy here.");
    }

    ## Copy over if we got it.
    if( -f Cwd::cwd() . '/perl_libs.json' ){
      force_copy(Cwd::cwd() . '/perl_libs.json',
		 $synth_vars{AMIGO_CGI_ROOT_DIR} . '/');
    }
  }

  ## Install all standard modules (drawn from the main directory).
  foreach ( @std_mods ) {
    force_copy(Cwd::cwd() . '/amigo/cgi-bin/' . $_,
	       $synth_vars{AMIGO_CGI_ROOT_DIR} . '/');
    make_executable( $synth_vars{AMIGO_CGI_ROOT_DIR} . '/' . $_);
    make_non_cgi_link( $synth_vars{AMIGO_CGI_ROOT_DIR} . '/' . $_);
  }

  ## Install all exp modules (drawn from the exp directory).
  foreach ( @additional_mods ) {
    force_copy(Cwd::cwd() . '/amigo/cgi-bin/experimental/' . $_,
	       $synth_vars{AMIGO_CGI_ROOT_DIR} . '/');
    make_executable( $synth_vars{AMIGO_CGI_ROOT_DIR} . '/' . $_);
  }

  ## The old term-details.cgi needs to be kept around to catch all of
  ## the things that fall through to the term subset routines (which
  ## is not covered by the new term_details).
  # ## Now that things are copied over, make term-details.cgi link to
  # ## term_details and copy term-.
  # {
  #   my $hst = $synth_vars{AMIGO_CGI_ROOT_DIR} . '/term-details-old.cgi';
  #   my $old = $synth_vars{AMIGO_CGI_ROOT_DIR} . '/term-details.cgi';
  #   my $new = $synth_vars{AMIGO_CGI_ROOT_DIR} . '/term_details';

  #   ## Make sure the old ones are gone.
  #   unlink $old if -f $old;
  #   unlink $hst if -f $hst;

  #   ## Make the link.
  #   my @args = ("ln", "-f", "-s", $new, $old);
  #   ll("Old to new term details link: link $new to $old");
  #   system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;

  #   # ## Put old one over for to cover for the old.
  #   # force_copy(Cwd::cwd() . '/amigo/cgi-bin/term-details.cgi', $old);
  #   # make_executable($old);
  # }

  ## Copy perl config over.
  force_copy(Cwd::cwd() . '/config.pl', $synth_vars{AMIGO_CGI_ROOT_DIR} . '/');
  make_readable( $synth_vars{AMIGO_CGI_ROOT_DIR} . '/config.pl' );

  ## Copy JSON config over.
  force_copy(Cwd::cwd() . '/config.json',
	     $synth_vars{AMIGO_CGI_ROOT_DIR} . '/');
  make_readable( $synth_vars{AMIGO_CGI_ROOT_DIR} . '/config.json' );

  ## Copy statics.
  force_copy(Cwd::cwd() . '/amigo/templates',
	     $synth_vars{AMIGO_CGI_ROOT_DIR} );
  force_copy(Cwd::cwd() . '/amigo/css',
	     $synth_vars{AMIGO_HTDOCS_ROOT_DIR} );
  force_copy(Cwd::cwd() . '/amigo/images',
	     $synth_vars{AMIGO_HTDOCS_ROOT_DIR} );
  force_copy(Cwd::cwd() . '/amigo/js',
	     $synth_vars{AMIGO_HTDOCS_ROOT_DIR} );
  ## TODO/BUG: Temporarily get the Newick tree stuff over until we can
  ## fix it more permanently.
  if ( $opt_e ) {
    force_copy($env_conf{GO_SVN_ROOT}{NEW_VALUE} . '/javascript/newick_tree',
	       $synth_vars{AMIGO_HTDOCS_ROOT_DIR} . '/js');
  }

  ## Copy OpenSearch documents over.
  force_copy(Cwd::cwd() . '/external/extensions/opensearch/amigo_general.xml',
	     $synth_vars{AMIGO_HTDOCS_ROOT_DIR} . '/opensearch');
  force_copy(Cwd::cwd() . '/external/extensions/opensearch/amigo_term.xml',
	     $synth_vars{AMIGO_HTDOCS_ROOT_DIR} . '/opensearch');
  force_copy(Cwd::cwd() . '/external/extensions/opensearch/amigo_gene_product.xml',
	     $synth_vars{AMIGO_HTDOCS_ROOT_DIR} . '/opensearch');
}

## Make cache files.
if ( $opt_s ) {
  ll("Skipping generation of cache files.");
} else {

  ## Farm the cache file work out to make_cache.pl and
  ## make_exp_cache.pl (where applicable).
  ll("Working on required cache files, please wait...");
  @args = ("perl", "./refresh.pl", "-c");
  ll("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
  ll("Finished working on cache files.");

  ## Things to do only under experimental.
  ## AmiGO.pm should be okay here as we've already copied config.pl.
  if ( $opt_e ) {
    ll("\n!!! Experimental file generation: these may take a (very great) while!\n!!! Using ctrl-c on these jobs should not be problematic\nin the main parts of AmiGO...\n");

    ## Heavy-duty caching files (including expensive RG).
    @args = ("perl", "./scripts/make_exp_caches.pl");
    ll("System: \"@args\"");
    system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;

    ll("Finished making experimental files.");
  }
}

## Done.
print "Done.\n\n";
print "Your new installation is at: ". $synth_vars{AMIGO_CGI_URL} ."/go.cgi\n";

print <<EOC;
You may use the refresh script (refresh.pl) with the "-r" flag to regularly 
purge unneeded and old files generated while AmiGO is running.

The refresh.pl script has many other uses, please see the
documentation for flags that apply to you. You will need to run
refresh.pl separately to generate the Lucene search indexes (necessary
for most external installations) and Reference Genome cache
(unnecessary for non-GO sites).

EOC


##
sub create_synthetic_amigo_environmental_variables{

  ## Open file.
  open(FILE, ">config.pl");

  print FILE <<EOC;
############################
##
## User Changeable Variables
##
## You may alter the install template by changing the
## variable below.
##
############################

EOC

  ## Write user changable variables.
  #foreach( keys %env_conf ){
  foreach ( @env_conf_order ) {

    ## Squeeze stuff out of the hash.
    my $default = $env_conf{$_}->{DEFAULT};
    my $nv = $env_conf{$_}->{NEW_VALUE};
    if ( $default && ! defined($nv) ) {
      die "$_ doesn't have a NEW_VALUE: please fix the installer: $!";
    }

    print FILE "\$ENV{$_}=\'$nv\';\n";
  }

  print FILE <<EOC;

############################
##
## Synthetic Variables
##
## Changing these accomplishes nothing,
## they are here for debugging purposes.
##
############################

EOC

  ##
  foreach ( keys %synth_vars ) {

    my $syn = "\$ENV{" . $_. "}=";
    #if( $_ eq 'PATH' ){
    #  $syn .= $synth_vars{$_};
    #}else{
    $syn .= "\'" . $synth_vars{$_} . "\'";
    #}
    print FILE $syn . ";\n";
    ll("Created: " . $syn);
  }

  ## End.
  print FILE <<EOC;

1;
EOC

  ## Close file.
  close(FILE);
  ll("Created synthetic variables and configuration file \"config.pl\".");

  ## Emit JSON version for experimental. Already did this once, so we
  ## can ignore the checks.
  if( $opt_e ){

    use JSON;

    ## Glop everything together.
    my $struct = {};
    foreach ( @env_conf_order ) {
      $struct->{$_} = $env_conf{$_}->{NEW_VALUE};
    }
    foreach ( keys %synth_vars ) {
      $struct->{$_} = $synth_vars{$_};
    }

    ## Dump it out to file.
    my $json = JSON->new();
    $json->pretty(1);
    $json->indent(1);
    $json->space_after(1);
    $json->utf8(1);
    my $jtext = $json->encode($struct);
    open(JFILE, ">config.json");
    print JFILE $jtext;
    close(JFILE);

    ll("Created configuration file \"config.json\".");
  }

}


##
sub get_amigo_installation_variables_from_file{

  my $file = shift;
  do $file;

  ## Get user changable variables where necessary.
  #foreach( keys %env_conf ){
  foreach ( @env_conf_order ) {

    $env_conf{$_}->{DEFAULT} = $ENV{$_} if $ENV{$_};
    # ## DEBUG
    # ww("\n0:$_\n";
    # ww("1: " . $env_conf{$_}->{DEFAULT});
    # ww("2: " . $env_conf{$_}->{NEW_VALUE});
    # ww("3: " . $ENV{$_});

    ## Squeeze stuff out of the hash.
    my $default = $env_conf{$_}->{DEFAULT};
    my $error = $env_conf{$_}{ERROR}
      || die "$_ doesn't have an ERROR: please fix the installer: $!";
    my $parser = $env_conf{$_}{PARSER}
      || die "$_ doesn't have a PARSER: please fix the installer: $!";

    ##
    my $depends_met = $env_conf{$_}{DEPENDS};
    if (  ! defined( $depends_met ) ||
	  ( defined( $depends_met ) &&
	    &$depends_met() ) ) {

      my $answer = $ENV{$_};
      $answer = '' if ! defined($answer);
      if ( ! defined($answer) || $answer eq '' ) {
	## Go with the default.
	$env_conf{$_}{NEW_VALUE} = $default;
      } elsif ( &$parser($answer) ) {
	$env_conf{$_}{NEW_VALUE} = $answer;
      } else {
	print "\n";
	die "Oops: $_ $error: \'$answer\'. Please correct template file.\n";
      }
      print "$_ will be: \'$env_conf{$_}{NEW_VALUE}\'\n";

    } else {
      ## The new value is the default.
      $env_conf{$_}{NEW_VALUE} = $env_conf{$_}{DEFAULT};
    }
  }

  ll("Read variables from \"$file\".");
}


##
sub get_amigo_installation_variables_from_user{

  ## Get user changable variables where necessary.
  #foreach( keys %env_conf ){
  foreach ( @env_conf_order ) {

    ## Squeeze stuff out of the hash.
    my $default = $env_conf{$_}->{DEFAULT};
    my $message = $env_conf{$_}{MESSAGE}
      || die "$_ doesn't have a MESSAGE: please fix the installer: $!";
    my $error = $env_conf{$_}{ERROR}
      || die "$_ doesn't have an ERROR: please fix the installer: $!";
    my $parser = $env_conf{$_}{PARSER}
      || die "$_ doesn't have a PARSER: please fix the installer: $!";

    ##
    my $depends_met = $env_conf{$_}{DEPENDS};
    if (  ! defined( $depends_met ) ||
	  ( defined( $depends_met ) &&
	    &$depends_met() ) ) {

      ## Continue loop.
      my $loop_p = 1;
      while ( $loop_p ) {

	print "\n";
	print $message;
	print "\n";
	print "[$default]: ";

	my $answer = <STDIN>;
	chomp $answer;
	if ( ! $answer && $answer ne '0' ) {

	  #ww("===DEF:" . $default);
	  #sleep 1;

	  ## Go with the default.
	  if ( &$parser($default) ) {
	    $env_conf{$_}{NEW_VALUE} = $default;
	    $loop_p = 0;
	  } else {
	    print "\n";
	    print "Oops: $error. Please try again.\n";
	  }

	} elsif ( &$parser($answer) ) {

	  #ww("===OK:" . $answer);
	  #sleep 1;

	  $env_conf{$_}{NEW_VALUE} = $answer;
	  $loop_p = 0;
	} else {
	  print "\n";
	  print "Oops: $error. Please try again.\n";
	}
      }
    } else {
      ## The new value is the default.
      $env_conf{$_}{NEW_VALUE} = $env_conf{$_}{DEFAULT};
    }
  }

  ##
  ## From here, the fiddly GOst variables.
  ##

  ll("Finished getting user input.");
}


##
sub is_a_boolean_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( $string =~ /^[01]$/ ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_a_string_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( length($string) &&
       $string =~ /^[a-zA-Z0-9\.\-\_\/\\\:]+$/ ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_a_string_or_empty_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( length($string) &&
       $string =~ /^[a-zA-Z0-9\.\-\_\/\\:\=\<\>\"\'\/\;\!\?\\\ ]+$/ ) {
    $return_val = 1;
  } elsif ( $string eq '' ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_a_number_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( length($string) &&
       $string =~ /^[0-9]+$/ ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_a_blast_method_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( length($string) &&
       ( $string eq 'cgi' ||
         $string eq 'pbs' ) ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_a_url_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( length($string) &&
       $string =~ /^[a-zA-Z0-9\-\_\:\_\/\.]+$/ ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_a_file_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( length($string) &&
       -e $string &&
       -f $string &&
       -R $string ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_a_binary_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( length($string) &&
       -e $string ) {
    #-e $string &&
    #-f $string &&
    #-X $string ){
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_a_directory_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( length($string) &&
       -e $string &&
       -d $string &&
       -R $string ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_an_obsolete_behaviour_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( length($string) && (
			   $string eq 'ignore' ||
			   $string eq 'include_commented' ||
			   $string eq 'include_all'
			  ) ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_a_writeable_directory_p {

  my $string = shift;
  die "this function requires an argument: $!" if ! defined $string;

  my $return_val = 0;
  if ( length($string) &&
       -e $string &&
       -d $string &&
       -W $string ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub depends_is_filters_false_p {

  my $return_val = 0;
  if ( $env_conf{AMIGO_USE_DEFAULT_AMIGO_FILTERS}{NEW_VALUE} eq '0' ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub depends_is_graphviz_true_p {

  #ww("+++" . $env_conf{AMIGO_SHOW_BLAST}{DEFAULT});
  #ww("+++" . $env_conf{AMIGO_SHOW_BLAST}{NEW_VALUE});
  #sleep 1;

  my $return_val = 0;
  if ( $env_conf{AMIGO_SHOW_GRAPHVIZ}{NEW_VALUE} eq '1' ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub depends_is_blast_true_p {

  #ww("+++" . $env_conf{AMIGO_SHOW_BLAST}{DEFAULT});
  #ww("+++" . $env_conf{AMIGO_SHOW_BLAST}{NEW_VALUE});
  #sleep 1;

  my $return_val = 0;
  if ( $env_conf{AMIGO_SHOW_BLAST}{NEW_VALUE} eq '1' ) {
    $return_val = 1;
  }

  return $return_val;
}


##
sub is_always_true {
  return 1;
}


##
sub is_always_false {
  return 0;
}


##
sub depends_is_blast_pbs_true_p {

  my $return_val = 0;
  if ( $env_conf{AMIGO_SHOW_BLAST}{NEW_VALUE} eq '1' ) {
    if ( $env_conf{AMIGO_BLAST_METHOD}{NEW_VALUE} eq 'pbs' ) {
      $return_val = 1;
    }
  }

  return $return_val;
}


##
sub safe_make_dir {

  my $dir_to_make = shift || die;

  ## Make the new session directory if necessary.
  if ( ! -e  $dir_to_make ) {
    my @args = ("mkdir", $dir_to_make);
    ll("System: \"@args\"");
    system(@args) == 0 || die "system @args failed: $?" if ! $opt_t;
  }
}


##
sub force_copy {

  my $from = shift || die;
  my $to = shift || die;

  my @args = ("cp", "-r", "-f", $from, $to);
  ll("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
}


##
sub make_executable {

  my $file = shift || die;

  my @args = ("chmod", "a+x", $file);
  ll("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
}


##
sub make_permissive {

  my $file = shift || die;

  my @args = ("chmod", "777", $file);
  ll("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
}


##
sub make_readable {

  my $file = shift || die;

  my @args = ("chmod", "644", $file);
  ll("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
}


## Make all of the old .cgi filenames accessible through no-cgi names,
## if applicable.
sub make_non_cgi_link {

  my $file = shift || die;

  if ( $file =~ /(.*)\.cgi$/ ) {
    my @args = ("ln", "-f", "-s", $file, $1);
    ll("Old CGI link: new link from $1 to $file");
    system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
  }
}


## Just a little printin' when feeling verbose.
sub ll {
  my $str = shift || '';
  print $str . "\n" if $opt_v;
}


## Just a little printin' when feeling afraid.
sub ww {
  my $str = shift || '';
  print STDERR $str . "\n";
}



=head1 NAME

install.pl

=head1 SYNOPSIS

install.pl [-v] [-r] [-i] [-s] [-c] [-t] [-e] [-j] [-d] [-f <template_name>]

=head1 DESCRIPTION

This is the main AmiGO installation script--it moves files to the
proper location with the proper permissions as well as creates the
very important config.pl file.

For first-time users (i.e. the config.pl file doesn't exist yet), it
asks questions about the user's environment and tries to catch the
biggest errors that one can make during installation. For people who
have successfully run the script before, there are more options (using
alternate config.pl files, installing experimental modules, etc.).

=head1 OPTIONS

=over

=item -v

Enable more verbose messages. This is useful for checking installation errors.

=item -r

Overwrite an already extant "config.pl" and use its contents as
defaults when asking the questions again. Useful for retrying a failed
install.

=item -i

Ignore and overwrite "config.pl" if it exists. This should start you
fresh.

=item -s

Skip cache file generation; only for reinstalls.

=item -c

Do *only* cache generation and skip all other operations.
Note that this still requires some kind of config.pl file
to be defined either by default or other options.

=item -t

Do a test run. Only configure files will be read and written.

=item -d

Additionally add the diagnose script and run the simulate.pl script
for external consumption.

=item -e

Also install experimental CGIs, binaries, and config.json as well as
config.pl (same information, different format).

=item -g

Also install GOOSE.

=item -G

Install GOOSE and exclude most other parts.

=item -f <file>

Use the file <file> as the configuration (config.pl).

=back

=head1 SEE ALSO

http://wiki.geneontology.org/index.php/AmiGO_Manual:_Installation


=cut
