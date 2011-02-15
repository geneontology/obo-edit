############################
##
## User Changeable Variables
##
## You may alter the install template by changing the
## variable below.
##
############################

$ENV{GO_ROOT}='/users/sjcarbon/local/src/cvs/go-dev_goose';
$ENV{GO_DBNAME}='go_latest_lite';
$ENV{GO_DBHOST}='spitz';
$ENV{GO_DBUSER}='';
$ENV{GO_DBAUTH}='';
$ENV{GO_DBSOCKET}='';
$ENV{GO_HAS_COUNT_BY_SPECIES}='1';
$ENV{AMIGO_PROJECT_NAME}='goose';
$ENV{AMIGO_HTDOCS_PARTIAL_PATH}='/www/toy_9012/htdocs';
$ENV{AMIGO_HTDOCS_PARTIAL_URL}='http://toy.lbl.gov:9012';
$ENV{AMIGO_CGI_PARTIAL_PATH}='/www/toy_9012/cgi-bin';
$ENV{AMIGO_CGI_PARTIAL_URL}='http://toy.lbl.gov:9012/cgi-bin';
$ENV{AMIGO_DATA_PATH}='/www/toy_9012/cgi-bin';
$ENV{AMIGO_SHOW_GP_OPTIONS}='1';
$ENV{AMIGO_SHOW_GRAPHVIZ}='1';
$ENV{AMIGO_DOT_PATH}='/usr/bin/dot';
$ENV{AMIGO_SHOW_BLAST}='1';
$ENV{AMIGO_FASTA_DB}='/www/toy_9012/cgi-bin/data/go_20071106-seqdblite.fasta';
$ENV{AMIGO_BLASTP}='/share/bdgp64/wublast/blastp';
$ENV{AMIGO_BLASTX}='/share/bdgp64/wublast/blastx';
$ENV{AMIGO_BLASTN}='/share/bdgp64/wublast/blastn';
$ENV{AMIGO_BLAST_METHOD}='cgi';
$ENV{AMIGO_QSUB}='/usr/local/command';
$ENV{AMIGO_QUEUE}='/usr/local/queue';
$ENV{AMIGO_PBS_USER}='nobody';
$ENV{AMIGO_MAX_SEQ_NUM}='100';
$ENV{AMIGO_MAX_SEQ_LENGTH}='3000000';
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
$ENV{AMIGO_MAX_RESULTS_PAGES}='40';
$ENV{AMIGO_CALCULATE_GP_COUNTS}='0';
$ENV{AMIGO_CALCULATE_TERM_COUNTS}='0';
$ENV{AMIGO_GET_RELEVANCE}='1';
$ENV{AMIGO_CLEVER_MODE}='1';
$ENV{AMIGO_OBSOLETE_BEHAVIOUR}='include_commented';

############################
##
## Synthetic Variables
##
## Changing these accomplishes nothing,
## they are here for debugging purposes.
##
############################

$ENV{AMIGO_CGI_URL}='http://toy.lbl.gov:9012/cgi-bin/amigo';
$ENV{AMIGO_TEMP_IMAGE_DIR}='/www/toy_9012/htdocs/amigo/tmp_images';
$ENV{AMIGO_HTDOCS_ROOT_DIR}='/www/toy_9012/htdocs/amigo';
$ENV{AMIGO_HTML_URL}='http://toy.lbl.gov:9012/amigo';
$ENV{AMIGO_CGI_ROOT_DIR}='/www/toy_9012/cgi-bin/amigo';

1;
