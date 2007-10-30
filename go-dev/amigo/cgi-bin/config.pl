$ENV{PATH} .= ':/usr/local/graphviz/bin';


##########################################
# general                                #
##########################################

## go-dev location
$ENV{GO_ROOT}='/Library/WebServer/svn/geneontology/go-dev';
#$ENV{GO_ROOT}='/Library/WebServer/cvs/go-dev';

## database.
#$ENV{GO_DBNAME}='go';
#$ENV{GO_DBHOST}='localhost';
#$ENV{GO_DBUSER}='root';
$ENV{GO_DBNAME}='go_latest_lite_sc';
$ENV{GO_DBHOST}='spitz.lbl.gov';

# comment this line out if your db doesn't have the species count
$ENV{POPULATE_COUNT_BY_SPECIES} = 1;

##########################################
# Locations of images/sessions/etc       #
##########################################

$ENV{GO_HTML_DIR}='/Library/WebServer/amigo';
$ENV{GO_HTML_URL}='http://127.0.0.1/amigo';
$ENV{GO_CGI_ROOT}='/Library/WebServer/cgi-bin/amigo'; # used in installation
#$ENV{GO_CGI_URL}='http://127.0.0.1/cgi-bin/amigo';
$ENV{GO_CGI_URL}='http://127.0.0.1/amigo-cgi-bin';
## Directories where templates live.  Must
## be a relative file path to cgi-bin.
$ENV{GO_TEMPLATE_PATHS}='../templates/pages:../templates/includes';

## Directory where sessions live.  Must
## be a relative file path to cgi-bin.
## Default is 'sessions'
$ENV{GO_SESSION_DIR}='../sessions';

#$ENV{GO_SESSION_DIR} = '../sessions';
#$ENV{GO_SESSION_DIR} = '/Library/WebServer/svn/geneontology/go-dev/new-amigo/sessions';

## hard-coded temp image dir location
#$ENV{GO_TMP_IMG_DIR} = '/Library/WebServer/svn/geneontology/go-dev/new-amigo';


#######################################################
# Feature Toggles                                     #
#######################################################

# Switch GO_SHOW_BLAST to 1 to enable the GO BLAST search.
$ENV{GO_SHOW_BLAST} = 1;

# Switch GO_SHOW_GRAPHVIZ to 1 to enable the graph visualizations.
$ENV{GO_SHOW_GRAPHVIZ} = 1;

$ENV{GO_SHOW_ONT_FILTER} = 1;

# If you've loaded Gene Product Associations, SHOW_GP_OPTIONS
# should be set to 1.
$ENV{GO_SHOW_GP_OPTIONS} = 1;

# Set these to zero if you want to turn off the gene product filters.
$ENV{GO_SHOW_GP_FILTERS} = 1;
$ENV{GO_SHOW_TAXID_FILTER} = 1;
$ENV{GO_SHOW_SPECIESDB_FILTER} = 1;
$ENV{GO_SHOW_EVCODE_FILTER} = 1;
$ENV{GO_SHOW_GPTYPE_FILTER} = 1;
$ENV{GO_SHOW_ASSBY_FILTER} = 1;
$ENV{GO_SHOW_QUAL_FILTER} = 0;

# If you want to allow user to calculate the GP counts no matter what
# filters are being used, set this to 1. It may slow down AmiGO's
# performance. NOT YET IMPLEMENTED!!
$ENV{GO_CALCULATE_GP_COUNTS} = 0;
$ENV{GO_CALCULATE_TERM_COUNTS} = 0;

$ENV{GO_GET_RELEVANCE} = 1; # search result relevance
$ENV{GO_CLEVER_MODE} = 1; # 'clever' AmiGO search mode

#	possible obsolete behaviours:
#	ignore: do not include any obsoletes in the results
#	include_commented : include terms with comments that
#	refer to terms not in the results list
#	include_all: keep all obsolete terms in the results
$ENV{GO_OBSOLETE_BEHAVIOUR} = 'include_commented';

#######################################################
# Look and Feel                                       #
#######################################################

$ENV{GO_MAX_SESSIONS} = 300;
$ENV{GO_SESSION_TIMEOUT} = 7200;
$ENV{GO_PAGE_SIZE} = 50;
$ENV{GO_MAX_RESULTS_PAGES} = 40;

##########################################
# GO blast stuff                         #
##########################################
## Directory where blast sessions live.  Must
## be a relative file path to AmiGO cgi dir.
## Default is 'blast_sessions'
#$ENV{GO_BLAST_SESSION_DIR}='blast_sessions';

#$ENV{GO_DATA_DIR}='/Library/WebServer/blast/data';
#$ENV{GO_DATA_DIR}='blast_sessions';

$ENV{GO_FASTA_DB}='/Library/WebServer/blast/data/go_latest-seqdblite.fasta';

$ENV{GO_BLASTP}='/Library/WebServer/blast/blastp';

$ENV{GO_BLASTX}='/Library/WebServer/blast/blastx';

$ENV{GO_BLASTN}='/Library/WebServer/blast/blastn';

$ENV{GO_BLAST_METHOD}='cgi';

#$ENV{GO_BLAST_URL}='http://127.0.0.1/cgi-bin/amigo/blast-sgd-update.pl';
$ENV{GO_BLAST_URL}='http://127.0.0.1/amigo-cgi-bin/blast-sgd-update.pl';

$ENV{GO_MAX_SEQ_NUM} = 100;

$ENV{GO_MAX_SEQ_LENGTH} = 3000000;

1;
