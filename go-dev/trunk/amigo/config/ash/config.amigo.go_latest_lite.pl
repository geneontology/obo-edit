############################
##
## User Changeable Variables
##
## You may alter the install template by changing the
## variable below.
##
############################

$ENV{GO_ROOT}='/home/sjcarbon/local/src/cvs/go-dev';
$ENV{GO_DBNAME}='go_latest_lite';
$ENV{GO_DBHOST}='spitz';
$ENV{GO_DBUSER}='';
$ENV{GO_DBAUTH}='';
$ENV{GO_DBPORT}='';
$ENV{GO_DBSOCKET}='';
$ENV{GO_HAS_COUNT_BY_SPECIES}='1';
$ENV{AMIGO_PROJECT_NAME}='amigo';
$ENV{AMIGO_HTDOCS_PARTIAL_PATH}='/local/www/htdocs';
$ENV{AMIGO_HTDOCS_PARTIAL_URL}='http://amigo.berkeleybop.org';
$ENV{AMIGO_CGI_PARTIAL_PATH}='/local/www/cgi-bin';
$ENV{AMIGO_CGI_PARTIAL_URL}='http://amigo.berkeleybop.org/cgi-bin';
$ENV{AMIGO_SHOW_GP_OPTIONS}='1';
$ENV{AMIGO_SHOW_GRAPHVIZ}='1';
$ENV{AMIGO_DOT_PATH}='/usr/bin/dot';
$ENV{AMIGO_SHOW_BLAST}='1';
$ENV{AMIGO_FASTA_DB}='/www/toy_9012/cgi-bin/data/go_20071106-seqdblite.fasta';
$ENV{AMIGO_BLASTP}='/share/bdgp64/wublast/blastp';
$ENV{AMIGO_BLASTX}='/share/bdgp64/wublast/blastx';
$ENV{AMIGO_BLAST_METHOD}='cgi';
$ENV{AMIGO_QSUB}='/usr/local/command';
$ENV{AMIGO_QUEUE}='/usr/local/queue';
$ENV{AMIGO_PBS_USER}='nobody';
$ENV{AMIGO_MAX_SEQ_NUM}='100';
$ENV{AMIGO_MAX_SEQ_LENGTH}='3000000';
$ENV{AMIGO_SHOW_GOOSE_LINKS}='1';
$ENV{AMIGO_USE_DEFAULT_AMIGO_FILTERS}='1';
$ENV{AMIGO_SHOW_ONT_FILTER}='1';
$ENV{AMIGO_SHOW_TAXID_FILTER}='1';
$ENV{AMIGO_SHOW_SPECIESDB_FILTER}='1';
$ENV{AMIGO_SHOW_EVCODE_FILTER}='1';
$ENV{AMIGO_SHOW_GPTYPE_FILTER}='1';
$ENV{AMIGO_SHOW_ASSBY_FILTER}='0';
$ENV{AMIGO_SHOW_QUAL_FILTER}='0';
$ENV{AMIGO_TEMPLATE_PATHS}='templates/pages:templates/includes';
$ENV{AMIGO_SESSION_DIR}='sessions';
$ENV{AMIGO_MAX_SESSIONS}='200';
$ENV{AMIGO_SESSION_TIMEOUT}='7200';
$ENV{AMIGO_PAGE_SIZE}='50';
$ENV{AMIGO_MAX_RESULTS_HTML}='200';
$ENV{AMIGO_MAX_RESULTS_DOWNLOAD}='1000';
$ENV{AMIGO_CALCULATE_GP_COUNTS}='0';
$ENV{AMIGO_CALCULATE_TERM_COUNTS}='0';
$ENV{AMIGO_GET_RELEVANCE}='1';
$ENV{AMIGO_CLEVER_MODE}='1';
$ENV{AMIGO_OBSOLETE_BEHAVIOUR}='include_commented';
$ENV{AMIGO_TERM2TERM_METADATA_LOADED}='0';
$ENV{AMIGO_TERM_REGEXP}='GO\:[0-9]{7}';
$ENV{AMIGO_VERBOSE}='0';
$ENV{AMIGO_BETA}='1';

############################
##
## Synthetic Variables
##
## Changing these accomplishes nothing,
## they are here for debugging purposes.
##
############################

$ENV{AMIGO_INDEX_DIR}='/local/www/cgi-bin/amigo/index';
$ENV{AMIGO_PRE_RENDER_URL}='http://amigo.berkeleybop.org/amigo/pre_render';
$ENV{AMIGO_HTDOCS_ROOT_DIR}='/local/www/htdocs/amigo';
$ENV{AMIGO_HTML_URL}='http://amigo.berkeleybop.org/amigo';
$ENV{AMIGO_CGI_ROOT_DIR}='/local/www/cgi-bin/amigo';
$ENV{AMIGO_CGI_URL}='http://amigo.berkeleybop.org/cgi-bin/amigo';
$ENV{AMIGO_PRE_RENDER_DIR}='/local/www/htdocs/amigo/pre_render';
$ENV{AMIGO_TEMP_IMAGE_DIR}='/local/www/htdocs/amigo/tmp_images';
$ENV{AMIGO_TEMP_IMAGE_URL}='http://amigo.berkeleybop.org/amigo/tmp_images';

1;
