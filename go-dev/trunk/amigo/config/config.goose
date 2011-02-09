############################
##
## User Changeable Variables
##
## You may alter the install template by changing the
## variable below.
##
############################

$ENV{GO_DEV_ROOT}='/home/swdev/local/src/svn/geneontology/go-dev/trunk';
$ENV{GO_JS_ROOT}='/home/swdev/local/src/svn/geneontology/javascript';
$ENV{GOBO_ROOT}='/home/swdev/local/src/svn/geneontology/gobo-dbic';
$ENV{GO_DBNAME}='go_latest_lite';
$ENV{GO_DBHOST}='spitz';
$ENV{GO_DBUSER}='';
$ENV{GO_DBAUTH}='';
$ENV{GO_DBPORT}='';
$ENV{GO_DBSOCKET}='';
$ENV{GO_HAS_COUNT_BY_SPECIES}='1';
$ENV{AMIGO_PROJECT_NAME}='goose';
$ENV{AMIGO_HTDOCS_PARTIAL_PATH}='/srv/www-goose/htdocs';
$ENV{AMIGO_HTDOCS_PARTIAL_URL}='http://go.berkeleybop.org';
$ENV{AMIGO_CGI_PARTIAL_PATH}='/srv/www-goose/cgi-bin';
$ENV{AMIGO_CGI_PARTIAL_URL}='http://go.berkeleybop.org/cgi-bin';
$ENV{AMIGO_PUBLIC_CGI_PARTIAL_URL}='http://amigo.geneontology.org/cgi-bin';
$ENV{AMIGO_SHOW_GP_OPTIONS}='1';
$ENV{AMIGO_SHOW_GRAPHVIZ}='1';
$ENV{AMIGO_DOT_PATH}='/usr/bin/dot';
$ENV{AMIGO_SHOW_BLAST}='1';
$ENV{AMIGO_FASTA_DB}='/srv/www-goose/fake/nil';
$ENV{AMIGO_BLASTP}='/srv/www-goose/fake/nil';
$ENV{AMIGO_BLASTX}='/srv/www-goose/fake/nil';
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
$ENV{AMIGO_GO_ONLY_GRAPHICS}='1';
$ENV{AMIGO_VERSION}='1.8';
$ENV{AMIGO_VERBOSE}='0';
$ENV{AMIGO_TROUBLE_SWITCH}='0';
$ENV{AMIGO_TROUBLE_MESSAGE}='<h2 style="text-align: center;">AmiGO is currently experiencing some difficulty.<br />You may want to temporarily try <a href="http://amigo.berkeleybop.org/cgi-bin/amigo/go.cgi" title="Go to AmiGO Labs">AmiGO Labs</a> instead.</h2>';
$ENV{AMIGO_BETA}='0';
$ENV{AMIGO_GOOGLE_ANALYTICS_ID}='UA-6180701-5';

############################
##
## Synthetic Variables
##
## Changing these accomplishes nothing,
## they are here for debugging purposes.
##
############################

$ENV{AMIGO_TESTS_DIR}='/srv/www-goose/htdocs/goose/tests';
$ENV{AMIGO_SCRATCH_DIR}='/srv/www-goose/cgi-bin/goose/sessions/scratch';
$ENV{AMIGO_INDEX_DIR}='/srv/www-goose/cgi-bin/goose/index';
$ENV{AMIGO_PRE_RENDER_URL}='http://go.berkeleybop.org/goose/pre_render';
$ENV{AMIGO_IMAGE_DIR}='/srv/www-goose/htdocs/goose/images';
$ENV{AMIGO_IMAGE_URL}='http://go.berkeleybop.org/goose/images';
$ENV{AMIGO_LOG_DIR}='/srv/www-goose/cgi-bin/goose/log';
$ENV{AMIGO_TESTS_URL}='http://go.berkeleybop.org/goose/tests';
$ENV{AMIGO_TEMP_IMAGE_DIR}='/srv/www-goose/htdocs/goose/tmp_images';
$ENV{AMIGO_INDEX_DIR_GENE_PRODUCT}='/srv/www-goose/cgi-bin/goose/index/lucene/gene_product';
$ENV{AMIGO_SERVICE_URL}='http://go.berkeleybop.org/cgi-bin/goose/aserve';
$ENV{AMIGO_CACHE_DIR}='/srv/www-goose/cgi-bin/goose/cache';
$ENV{AMIGO_PUBLIC_CGI_URL}='http://amigo.geneontology.org/cgi-bin/amigo';
$ENV{AMIGO_INDEX_DIR_GENERAL}='/srv/www-goose/cgi-bin/goose/index/lucene/general';
$ENV{AMIGO_HTDOCS_ROOT_DIR}='/srv/www-goose/htdocs/goose';
$ENV{AMIGO_INDEX_DIR_TERM}='/srv/www-goose/cgi-bin/goose/index/lucene/term';
$ENV{AMIGO_CGI_ROOT_DIR}='/srv/www-goose/cgi-bin/goose';
$ENV{AMIGO_HTML_URL}='http://go.berkeleybop.org/goose';
$ENV{AMIGO_CGI_URL}='http://go.berkeleybop.org/cgi-bin/goose';
$ENV{AMIGO_PRE_RENDER_DIR}='/srv/www-goose/htdocs/goose/pre_render';
$ENV{AMIGO_SESSIONS_ROOT_DIR}='/srv/www-goose/cgi-bin/goose/sessions';
$ENV{AMIGO_TEMP_IMAGE_URL}='http://go.berkeleybop.org/goose/tmp_images';

1;
