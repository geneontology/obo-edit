#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w

use strict;
no strict qw(subs);
use vars qw($AUTOLOAD);
use Getopt::Long;

our %conf;
our %defaults;
our %desc;
our $pwd = $ENV{PWD};
our $tmpl = (shift @ARGV) || "tmpl-amigo-install";
our ($cvsroot,$godbname, $goservername, $godbuser, $godbauth, $gosocket);
our $amigo = 'amigo';

$conf{trace}=0;
%defaults =
  (GO_DBNAME => 'go',
   GO_DBHOST => 'localhost',
   GO_ROOT   => $ENV{GO_ROOT},
  );

&intro;
&init;
&install_all;
&writeconf;
exit;

sub init {
	map { $conf{$_} = $defaults{$_} } keys %defaults;
	if (-f $tmpl) {
		#require $tmpl; #reading config values
		print "Using $tmpl as template file\n";
		do $tmpl;
	}
}

sub get_ans {
	my $k = shift;

	my $ans = <STDIN>;
	chomp $ans;
	undef $ans unless (length($ans));
	defined($ans) && ($conf{$k}=$ans);
}

sub install_all {

	my $k = 'GO_ROOT';
	print "\n\n";
	print "Please enter the full path to the go-dev root directory,\n";
	printf "e.g. /www/port_80/cvs/go-dev [%s]\n", $conf{$k} || '';
	&get_ans($k);
	$cvsroot = $conf{$k};
	$ENV{GO_ROOT} = $cvsroot;

#	$k = 'GO_AMIGO_ROOT';
#	print "\n\n";
#	print "Please enter the full path to the amigo root directory,\n";
#	printf "e.g. /www/port_80/cvs/go-dev/amigo [%s]\n", $conf{$k} || '';
#	&get_ans($k);
#	$cvsroot = $conf{$k};
#	$ENV{GO_AMIGO_ROOT} = $cvsroot;

	$k = 'GO_DBNAME';
	my $default = $conf{$k};
	print "Please enter the name of the GO database\n";
	printf "to which you'd like to connect [%s]\n", $conf{$k} || '';
	&get_ans($k);

	$k = 'GO_DBHOST';
	$default = $conf{$k};
	print "\n\n";
	print "Please enter the hostname of the mySQL server\n";
	printf "where this database resides [%s]\n", $conf{$k} || '';
	&get_ans($k);

	$k = 'GO_DBUSER';
	print "\n\n";
	printf "Please enter the database user login [%s]\n", $conf{$k} || '';
	&get_ans($k);

	$k = 'GO_DBAUTH';
	print "\n\n";
	printf "Please enter the database user password [%s]\n",$conf{$k} || '';
	&get_ans($k);

	$k = 'GO_DBSOCKET';
	print "\n\n";
	printf "Please enter the database socket [%s]\n", $conf{$k} || '';
	&get_ans($k);

	$k = 'GO_CGI_ROOT';
	print "\n\n";
	print "Please enter the full path to the cgi\n";
	print "directory in which you'd like to install AmiGO.\n";
	printf "e.g. /www/port_80/cgi-bin/amigo [%s]\n", $conf{$k} || '';
	&get_ans($k);

	my $cgiroot = $conf{$k};
	unless (-e $cgiroot) {
		mkdir $cgiroot or grace_exit('mkdir $cgiroot', $!);
	}

	$k = 'GO_CGI_URL';
	print "\n\n";
	print "Please enter the URL that the AmiGO cgi directory\n";
	print "($conf{GO_CGI_ROOT}/amigo)\n";
	printf "maps to, e.g. http://myserver/cgi-bin/amigo [%s]\n", $conf{$k};
	&get_ans($k);

=old
	$k = 'GO_AMIGO_URL';
	print "\n\n";
	print "Please enter the URL that the CGI_ROOT ($conf{GO_CGI_ROOT}) with go.cgi script\n";
	printf "maps to, ie http://myserver/cgi-bin/amigo/go.cgi [%s]\n", $conf{$k};
	&get_ans($k);
=cut
	$k = 'DOC_ROOT_DIR';
	print "\n\n";
	print "Please enter the full path of web server document root.\n";
	print "This must be writeable as the 'amigo' directory will be created under it.\n";
	printf "e.g. /www/port_80/docs [%s]\n",$conf{$k};
	&get_ans($k);

	$k = 'GO_HTML_URL';
	print "\n\n";
	print "Please enter the URL that the AmiGO directory\n";
	print "($conf{DOC_ROOT_DIR}/amigo)\n";
	printf "maps to, e.g. http://myserver/amigo [%s]\n",$conf{$k};
	&get_ans($k);

	my $amigo_docroot = $conf{DOC_ROOT_DIR}."/".$amigo;
	unless (-e $amigo_docroot) {
		mkdir $amigo_docroot or grace_exit("mkdir $amigo_docroot", $!);
	}

	print "Installing web accoutrements into $amigo_docroot...\n";
	mkdir "$amigo_docroot/tmp_images" or grace_exit("mkdir $amigo_docroot/tmp_images", $!) unless (-e "$amigo_docroot/tmp_images");
	`chmod a+rw $amigo_docroot/tmp_images`;
	mkdir "$amigo_docroot/images" or grace_exit("mkdir $amigo_docroot/images", $!) unless (-e "$amigo_docroot/images");
	mkdir "$amigo_docroot/css" or grace_exit("mkdir $amigo_docroot/css", $!) unless (-e "$amigo_docroot/css");
	mkdir "$amigo_docroot/js" or grace_exit("mkdir $amigo_docroot/js", $!) unless (-e "$amigo_docroot/js");
	cpr("$cvsroot/$amigo/images/* $amigo_docroot/images");
	cpr("$cvsroot/$amigo/css/* $amigo_docroot/css");
	cpr("$cvsroot/$amigo/js/* $amigo_docroot/js");
	
	print "\nInstalling cgis and templates into $cgiroot...\n";
	cp("$cvsroot/$amigo/cgi-bin/*.cgi $cgiroot/");
	`chmod a+x $cgiroot/*.cgi`;
	cp("$cvsroot/$amigo/cgi-bin/*.pl $cgiroot/");

	## Copy the CGI scripts to the target directory.
	cp("$cvsroot/amigo/cgi-bin/goose $cgiroot/");
	`chmod a+x $cgiroot/goose`;
	cp("$cvsroot/amigo/cgi-bin/term_enrichment $cgiroot/");
	`chmod a+x $cgiroot/term_enrichment`;
	cp("$cvsroot/amigo/cgi-bin/slimmer $cgiroot/");
	`chmod a+x $cgiroot/slimmer`;
	cp("$cvsroot/amigo/cgi-bin/visualize $cgiroot/");
	`chmod a+x $cgiroot/visualize`;
=cut for now
	cp("$cvsroot/amigo/cgi-bin/orb_client.cgi $cgiroot/");
	`chmod a+x $cgiroot/orb_client.cgi`;
	cp("$cvsroot/amigo/cgi-bin/go.cgi $cgiroot/");
	`chmod a+x $cgiroot/go.cgi`;
	cp("$cvsroot/amigo/cgi-bin/*.pl $cgiroot/");
=cut
	## Copy the library skeleton example to the right directory.
	my $library_dir = "$amigo_docroot/library";
	mkdir $library_dir
	or grace_exit("mkdir $library_dir", $!)
	unless (-e $library_dir);
	`chmod a+r $library_dir`;
#	cpr("$cvsroot/amigo/amigo/library/* $amigo_docroot/library/");
	cpr("$cvsroot/$amigo/library/* $amigo_docroot/library/");
	$conf{GO_LIBRARY} = $library_dir;
	$conf{GO_LIBRARY_LINK} = "/amigo/library";

	## Make sure that we have a writable sessions directory.
	my $sess_dir = "$cgiroot/sessions";
	mkdir $sess_dir
	or grace_exit("mkdir $sess_dir", $!)
	unless (-e $sess_dir);
	`chmod a+w $sess_dir`;

	## Make sure that we have a writable working directory.
	my $working_dir = "$conf{DOC_ROOT_DIR}/amigo/workbench";
	mkdir $working_dir
	or grace_exit("mkdir $working_dir", $!)
	unless (-e $working_dir);
	`chmod a+w $working_dir`;
	$conf{GO_WORKBENCH} = $working_dir;
	$conf{GO_WORKBENCH_LINK} = "/amigo/workbench";

	## Make sure that the templates are in the right place.
	my $tmpl_dir = "$cgiroot/templates";
	mkdir $tmpl_dir
	or grace_exit("mkdir $tmpl_dir", $!)
	unless (-e $tmpl_dir);
	`chmod a+r $tmpl_dir`;
	cpr("$cvsroot/$amigo/templates/* $cgiroot/templates/");
	
	print "All done!\n";

#	 cpr("$cvsroot/amigo/install/js/*.js $amigo_docroot/js");
#	 cpr("$cvsroot/amigo/js/*.js $amigo_docroot/js");

#	 mkdir "$amigo_docroot/docs" or grace_exit("mkdir $amigo_docroot/docs", $!) unless (-e "$amigo_docroot/docs");
#	 mkdir "$amigo_docroot/docs/user_guide" or grace_exit("mkdir $amigo_docroot/docs/user_guide", $!) unless (-e "$amigo_docroot/docs/user_guide");
#	 cpr("$cvsroot/amigo/install/docs/user_guide/* $amigo_docroot/docs/user_guide");

	$k = 'GO_SHOW_GP_OPTIONS';
	print "\n\n";
	print "Have you loaded gene product associations into the database?\n";
	printf "Use 1 for yes and 0 for no. [%d]\n",$conf{$k} || '1';
	&get_ans($k);

	$k = 'GO_SHOW_GRAPHVIZ';
	print "\n\n";
	print "If you have GraphViz installed, AmiGO can use it to display\n";
	print "ontologies graphically. Enable GraphViz visualizations?\n";
	printf "Use 1 for yes and 0 for no. [%d]\n",$conf{$k} || '1';
	&get_ans($k);

	$k = 'GO_SHOW_BLAST';
	print "\n\n";
	print "If you have installed the BLAST package from http://blast.wustl.edu\n";
	print "you can activate the AmiGO BLAST search. Enable BLAST searches?\n";
	printf "Use 1 for yes and 0 for no. [%d]\n",$conf{$k} || '1';
	&get_ans($k);

	my $blast_value = $conf{$k};

	if ($blast_value =~ /^y|1/i) {
		$conf{GO_SHOW_BLAST} = 1;
		&install_blast($cgiroot);
=none of the following is required
		$k = 'INSTALL_GOST';
		print "\n\n";
		print "If gost has not been setup or another instance of gost to be setup,\n";
		print "gost needs to be installed/configured\n";
		printf "Install gost? [%s]\n", $conf{$k};
		&get_ans($k);
		&install_blast if ($conf{$k} =~ /^y/i);

		$k = 'GO_GOST_URL';
		print "\n\n";
		print "Please enter the URL of the gost cgi\n";
		printf "For instance: http://myserver/cgi-bin/gost/gost.cgi [%s]\n",$conf{$k};
		&get_ans($k);
		$k = 'NEED_SET_BDGP_MODULE_PATH';
		printf "\n\n";
		printf "Do you need to set BDGP module path env? [%s]\n",$conf{$k};
		&get_ans($k);
		if ($conf{$k} =~ /^y/i) {
			$k = 'BDGP_MODULE_PATH';
			printf "\n\n";
			printf "Please enter BDGP module path [%s]\n", $conf{$k};
			&get_ans($k);
		}
=cut
	}

	&write_amigo_config;

	printf "Make species cache file, please wait...\n";
	`perl ./scripts/make_spec_key.pl $conf{GO_CGI_ROOT} 50`;
	printf "Make cache file for other items, please wait...\n";
	`perl ./scripts/make_misc_key.pl $conf{GO_CGI_ROOT}`;
	my $amigo_url = $conf{GO_CGI_URL} . "/amigo/search.cgi";

#	$k = 'INSTALL_GO_DEV';
#	printf "Would you like to install go-dev documentation? [%s]\n", $conf{$k} || 'n';
#	&get_ans($k);
#
#	if ($conf{$k} =~ /^y/i) {
#		&install_go_dev;
#	}

	print "Configuration is completed. The URL of your AmiGO installation is \n$amigo_url\n\n";
}

sub write_amigo_config {

	my $cgiroot = $conf{GO_CGI_ROOT};
	#finally write amigo config
	open(F, ">$cgiroot/config.pl");

	print F "##########################################\n";
	print F "# general                                #\n";
	print F "##########################################\n\n";

#	printf F "\$ENV{BDGP_MODULE_PATH}='%s';\n\n",$conf{BDGP_MODULE_PATH} if ($conf{BDGP_MODULE_PATH});

	print F "## go-dev location\n";
	printf F "\$ENV{GO_ROOT}='%s';\n\n", $conf{GO_ROOT};
#	printf F "\$ENV{GO_AMIGO_ROOT}='%s';\n\n", $conf{GO_AMIGO_ROOT};

	my @keys = qw(GO_DBNAME GO_DBHOST GO_DBUSER GO_DBAUTH GO_DBSOCKET);
	printf F "## database.\n";
	map{printf F "\$ENV{$_}='%s';\n", $conf{$_} if ($conf{$_})}@keys;

	print F "##########################################\n";
	print F "# Locations of images/sessions/etc       #\n";
	print F "##########################################\n\n";
	print F "## These settings should not be altered after setting up AmiGO.\n\n";

	$conf{GO_HTML_DIR} = "$conf{DOC_ROOT_DIR}/$amigo";
	@keys = qw(GO_HTML_DIR GO_HTML_URL GO_CGI_ROOT GO_CGI_URL);
	map{printf F "\$ENV{$_}='%s';\n",$conf{$_}}@keys;

	print F "## Directories where templates live. Must\n";
	print F "## be a relative file path to cgi-bin.\n";
	printf F "\$ENV{GO_TEMPLATE_PATHS}='templates/pages:templates/includes';\n\n";

	print F "## Directory where sessions live. Must\n";
	print F "## be a relative file path to cgi-bin.\n";
	printf F "\$ENV{GO_SESSION_DIR}='sessions';\n\n";

	print F "## Directory where static library files live.\n";
	printf F "\$ENV{GO_LIBRARY}='%s';\n\n", $conf{GO_LIBRARY};
	printf F "\$ENV{GO_LIBRARY_LINK}='%s';\n\n", $conf{GO_LIBRARY_LINK};

	print F "## Directory where generated static files live.\n";
	printf F "\$ENV{GO_WORKBENCH}='%s';\n\n", $conf{GO_WORKBENCH};
	printf F "\$ENV{GO_WORKBENCH_LINK}='%s';\n\n", $conf{GO_WORKBENCH_LINK};

	print F "#######################################################\n";
	print F "# General Settings                                    #\n";
	print F "#######################################################\n\n";

	printf F "\$ENV{GO_MAX_SESSIONS} = 200;\n";
	printf F "\$ENV{GO_SESSION_TIMEOUT} = 7200;\n";
	printf F "\$ENV{GO_PAGE_SIZE} = 50;\n";
	printf F "\$ENV{GO_MAX_RESULTS_PAGES} = 40;\n\n";

	print F "#######################################################\n";
	print F "# Feature Toggles                                     #\n";
	print F "#######################################################\n\n";

	printf F "# Set to 1 to enable the GO BLAST search.\n";
	printf F "\$ENV{GO_SHOW_BLAST} = '%s';\n\n", $conf{GO_SHOW_BLAST};

	printf F "# Set to 1 to enable graphical views of ontologies\n";
	printf F "\$ENV{GO_SHOW_GRAPHVIZ} = %d;\n\n", $conf{GO_SHOW_GRAPHVIZ} || 0;

	printf F "# If you've loaded gene product associations, SHOW_GP_OPTIONS\n";
	printf F "# should be set to 1.\n";
	printf F "\$ENV{GO_SHOW_GP_OPTIONS} = %d;\n\n", $conf{GO_SHOW_GP_OPTIONS} || 0;

	printf F "# Set specific filters to 0 to turn them off.\n";
#	printf F "\$ENV{GO_SHOW_GP_FILTERS} = 1;\n";
	foreach my $f qw(ONT TAXID SPECIESDB EVCODE GPTYPE)# ASSBY QUAL)
	{	print F "\$ENV{GO_SHOW_".$f."_FILTER} = 1;\n";
	}
	foreach my $f qw(ASSBY QUAL)
	{	print F "\$ENV{GO_SHOW_".$f."_FILTER} = 0;\n";
	}

#	 printf F "# Setting this to 0 turns off gene product counts. \n";
#	 printf F "\$ENV{GO_SHOW_GP_COUNTS} = 1;\n\n";

#	 printf F "#Control graph construction/navigation: tree mean use new API\n";
#	 printf F "#otherwise, use old API, which has problem of overexpanding tree\n";
#	 printf F "#because multiple parent lineage and node (expanded) disappearing after closing one parent\n";
#	 printf F "\$ENV{GO_GRAPH_VIEW} = '%s';\n\n",$conf{GO_GRAPH_VIEW};

print F "# If you want to allow user to calculate the GP counts no matter what\n";
print F "# filters are being used, set this to 1. It may slow down AmiGO's\n";
print F "# performance. NOT YET IMPLEMENTED!!\n";
print F "\$ENV{GO_CALCULATE_GP_COUNTS} = 0;\n";
print F "\$ENV{GO_CALCULATE_TERM_COUNTS} = 0;\n\n";

print F "\$ENV{GO_GET_RELEVANCE} = 1; # search result relevance\n";
print F "\$ENV{GO_CLEVER_MODE} = 1;   # 'clever' AmiGO search mode\n\n";

print F "#	possible obsolete behaviours:\n";
print F "#	ignore: do not include any obsoletes in the results\n";
print F "#	include_commented : include terms with comments that\n";
print F "#	refer to terms not in the results list\n";
print F "#	include_all: keep all obsolete terms in the results\n";
print F "\$ENV{GO_OBSOLETE_BEHAVIOUR} = 'include_commented';\n\n";

if ($conf{GO_SHOW_BLAST} =~ /(y|1)/i)
{	
	printf F "##########################################\n";
	printf F "# GO BLAST server                        #\n";
	printf F "##########################################\n\n";

#	 print F "## Directory where blast sessions live.  Must\n";
#	 print F "## be a relative file path to AmiGO cgi dir.\n";
#	 print F "## Default is 'blast_sessions'\n";
#	 printf F "\$ENV{GO_SESSION_DIR}='blast_sessions';\n\n";

#	 @keys = qw(GO_DATA_DIR GO_FASTA_DB GO_BLASTP GO_BLASTX GO_BLASTN GO_BLAST_METHOD GO_MAX_SEQ_NUM);
	@keys = qw(GO_FASTA_DB GO_BLASTP GO_BLASTX GO_BLASTN GO_BLAST_METHOD);
	if ($conf{GO_BLAST_METHOD} eq 'cgi') {
		push @keys, 'GO_BLAST_URL';
	} else {
		push @keys, qw(GO_QSUB GO_QUEUE GO_PBS_USER);
	}
	push @keys, 'GO_MAX_SEQ_NUM', 'GO_MAX_SEQ_LENGTH';
	map{printf F "\$ENV{$_}='%s';\n\n",$conf{$_}}@keys;
#	 map{printf F "\$ENV{$_}='%s';\n\n",$conf{$_}}@keys;

}
	printf F "1;\n";

	close(F);
}

sub install_blast {
	my $root = shift;
	my $k;
=remove all this
	my $k = 'GOST_CGI_ROOT';
	print "\n\n";
	print "Please enter the full path to the cgi\n";
	printf "directory in which you'd like to install gost\n";
	printf "ie /www/port_80/cgi-bin/gost [%s]\n",$conf{$k};
	&get_ans($k);
	my $gost_cgi_root = $conf{$k};

	unless (-e $gost_cgi_root) {
		mkdir $gost_cgi_root or grace_exit("mkdir $gost_cgi_root", $!);
	}

	my $docroot = "$conf{DOC_ROOT_DIR}/gost";
	unless (-e $docroot) {
		mkdir $docroot or grace_exit("mkdir $docroot", $!);
	}

	$k = 'GOST_HTML_URL';
	print "\n\n";
	print "Please enter the URL of the gost document directory ($conf{DOC_ROOT_DIR}/gost)\n";
	printf "maps to, ie http://myserver/gost [%s]\n", $conf{$k};
	&get_ans($k);

	mkdir "$docroot/tmp_images" or grace_exit("mkdir $docroot/tmp_images", $!) unless (-e "$docroot/tmp_images");
	`chmod a+rw $docroot/tmp_images`;
	mkdir "$docroot/images" or grace_exit("mkdir $docroot/images", $!) unless (-e "$docroot/images");
	cpr("$cvsroot/amigo/gost/images/* $docroot/images/");
	mkdir "$docroot/css" or grace_exit("mkdir $docroot/css", $!) unless (-e "$docroot/css");
	cpr("$cvsroot/amigo/gost/css/* $docroot/css/");
	mkdir "$docroot/docs" or grace_exit("mkdir $docroot/docs", $!) unless (-e "$docroot/docs");

	$docroot = "$conf{DOC_ROOT_DIR}/amigo";
	mkdir "$docroot/docs/gost_user_guide" or grace_exit("mkdir $docroot/docs/gost_user_guide", $!) unless (-e "$docroot/docs/gost_user_guide");
	cpr("$cvsroot/amigo/gost/docs/user_guide/* $docroot/docs/gost_user_guide");

	cp("$cvsroot/amigo/gost/cgi-bin/gost.cgi $gost_cgi_root/");
	`chmod a+x $gost_cgi_root/gost.cgi`;

	my $sess_dir = "$root/blast_sessions";
	mkdir $sess_dir or grace_exit("mkdir $sess_dir", $!) unless (-e $sess_dir);
	`chmod a+w $sess_dir`;
	my $tmpl_dir = "$root/templates/blast_includes";
	mkdir $tmpl_dir or grace_exit("mkdir $tmpl_dir", $!) unless (-e $tmpl_dir);
	`chmod a+r $tmpl_dir`;
	cpr("$cvsroot/amigo/install/templates/blast_includes/* $tmpl_dir/");

	#blast stuff
	$k = 'GO_DATA_DIR';
	printf "\n\n";
	printf "Please enter data dir [%s]\n", $conf{$k};
	&get_ans($k);
=cut

	$k = 'GO_FASTA_DB';
	printf "\n\n";
	printf "Please enter fasta db [%s]\n", $conf{$k};
	&get_ans($k);

	$k = 'GO_BLASTP';
	printf "\n\n";
	printf "Please enter full path for blastp cmd [%s]\n", $conf{$k};
	&get_ans($k);

	$k = 'GO_BLASTX';
	printf "\n\n";
	printf "Please enter full path for blastx cmd [%s]\n", $conf{$k};
	&get_ans($k);

	$k = 'GO_BLASTN';
	printf "\n\n";
	printf "Please enter full path for blastn cmd [%s]\n", $conf{$k};
	&get_ans($k);

	$k = 'GO_BLAST_METHOD';
	printf "\n\n";
	printf "Please enter the BLAST submission method, cgi or pbs [%s]\n", $conf{$k};
	&get_ans($k);

	if ($conf{$k} eq 'cgi') {
		$k = 'GO_BLAST_URL';
		printf "\n\n";
		printf "Please enter blast url [%s]\n", $conf{$k};
		&get_ans($k);
	} else { #pbs
		$k = 'GO_QSUB';
		printf "\n\n";
		printf "Please enter full path for pbs qsub cmd [%s]\n", $conf{$k};
		&get_ans($k);
		$k = 'GO_QUEUE';
		printf "\n\n";
		printf "Please enter pbs queue [%s]\n", $conf{$k};
		&get_ans($k);
		$k = 'GO_PBS_USER';
		printf "\n\n";
		printf "Please enter pbs user [%s]\n", $conf{$k};
		&get_ans($k);
	}

	$k = 'GO_MAX_SEQ_NUM';
		$conf{$k} ||= 100;
	printf "\n\n";
	printf "Please enter maximum number of sequences allowed for BLAST [%s]\n", $conf{$k};
	&get_ans($k);

	$k = 'GO_MAX_SEQ_LENGTH';
		$conf{$k} ||= 3000000;
	printf "\n\n";
	printf "Please enter maximum sequence length allowed for BLAST [%s]\n", $conf{$k};
	&get_ans($k);

#	 $k = 'GO_AMIGO_URL';
#	 printf "\n\n";
#	 printf "Please enter AmiGO url for linking GOst result [%s]\n", $conf{$k};
#	 &get_ans($k);

#	 open(F, ">>$root/config.pl");

=repeated
	print F "##########################################\n";
	print F "# general								  #\n";
	print F "##########################################\n\n";

	printf F "## go-dev location\n";
	printf F "\$ENV{GO_ROOT}='%s';\n\n", $conf{GO_ROOT};

	my @keys = qw(GO_DBNAME GO_DBHOST GO_DBUSER GO_DBAUTH GO_DBSOCKET);
	printf F "## database\n";
	map{printf F "\$ENV{$_}='%s';\n", $conf{$_} if ($conf{$_})}@keys;

	print F "##########################################\n";
	print F "# Locations of sessions/etc	   #\n";
	print F "##########################################\n\n";

	print F "# Docroot\n";
	printf F "\$ENV{GO_HTML_DIR}='%s';\n", $conf{GO_HTML_DIR};
	printf F "\$ENV{GO_HTML_URL}='%s';\n", $conf{GO_HTML_URL};
	printf F "\$ENV{GO_AMIGO_URL}='%s';\n", $conf{GO_AMIGO_URL};
	printf F "\$ENV{GO_CGI_ROOT}='%s';\n\n",$conf{GOST_CGI_ROOT};

	printf F "## Directories where templates live.	Must\n";
	printf F "## be a relative file path to the cgi dir.\n";
	printf F "\$ENV{GO_TEMPLATE_PATHS}='blast_templates/pages:blast_templates/includes';\n\n";

=cut

	print "\n\nThe AmiGO BLAST search is installed and configured.\n";
}

sub install_go_dev_DEPRECATED {

	my $k = 'WEB_ROOT_DIR';
   $conf{$k} ||= $conf{DOC_ROOT_DIR};
		printf "Please enter web document root where you will serve go-dev documentation\n";
	printf "ie /www/port_80/htdocs [%s]\n",$conf{$k};
	&get_ans($k);
	my $docroot = $conf{$k};

	my $go_dev_docroot = "$docroot/dev";
	mkdir "$go_dev_docroot" or grace_exit("mkdir $go_dev_docroot", $!) unless (-e "$go_dev_docroot");
	`chmod a+r $go_dev_docroot`;

	printf "Remove all files under $go_dev_docroot?";
	my $ans = <STDIN>; chomp $ans;
	if ($ans =~ /y/i) {
		printf "About to remove stuff under $docroot/dev/\n";
		print `rm -rf $docroot/dev/*`;
	}
	$k = 'GO_DEV_SPEC_FILE';
	$conf{$k} ||= "dev_web_spec.pl";
	printf "Please enter go-dev doc spec [%s]\n", $conf{$k};
	&get_ans($k);
	my $spec_file = $conf{$k};
	open(F, "< $spec_file") or grace_exit('open', "can not open file, $spec_file, $!");
	my $perlstr = join("", <F>);
	close(F);
	my $inh = eval $perlstr;
	if ($@) {
		grace_exit('turn spec string into perl hash',$@);
	}
	foreach my $d (keys %{$inh || {}}) {
		my $o_dir = "$conf{GO_ROOT}/$d";
		my @a = split/\//,$d;
		shift @a if ($a[0] =~ /amigo/);
		my $destdir = "$go_dev_docroot/".join("/",@a);
		my @dirs = split/\//, $d;
		my $dir = $go_dev_docroot;
		map {
			$dir .= "/$_";
			mkdir "$dir" or grace_exit("mkdir $dir", $!) unless (-e "$dir");
		}@a;
		my $f_patterns = $inh->{$d};
		map{ $_ =~ s/\./\\./g}@{$f_patterns || []};
		map{ $_ =~ s/\*/.*/g}@{$f_patterns || []};
		opendir(DIR, $o_dir) or grace_exit("opendir $o_dir", $!);
		my @files = grep{ not /^\.{1,2}\z/ }readdir(DIR);
		closedir(DIR);
		foreach my $f (@files) {
			if (grep{$f =~ /$_/}@{$f_patterns || []}) {
				cp("$o_dir/$f $destdir/$f");
			}
		}
	}
}

sub grace_exit {
	my $cmd = shift;
	my $msg = shift;
	printf "$cmd failed: $msg\n";
	printf "Configured value will be in $tmpl file for next round default value\n";
	printf "Make necessary correction before redo config/install\n";
	&writeconf;
	exit 1;
}

sub trace {
	return unless $conf{trace};
	print STDERR "@_\n";
}

sub msg {
	my $msg = shift;
	print "$msg\n";
}

sub warning {
	msg(@_);
}

sub cp {
	trace(`cp @_`);
}

sub ln {
	my ($tdir, $f) = @_;
	trace(`cd $tdir;ln -fs $pwd/$f`);
}

sub cpr {
	trace(`cp -R @_`);
}

sub writeconf {
	unlink $tmpl if (-f $tmpl);
	open(F, ">$tmpl") || warning("Can't write tmpl file, $tmpl\n");
	print F "\%conf = (\n";
	foreach my $k (keys %conf) {
		next if ($k eq 'trace');
		printf F "	$k => '%s',\n", $conf{$k};
	}
	print F ");\n";
}

sub setup {
	&godbname;
	&godbhost;
	&cgiroot;
	&docroot;
	&amigo_image_dir_relative_to_cgiroot;
	&amigo_image_dir_relative_to_docroot;
	&amigo_session_relative_dir;
	&html_dir_relative_to_cgiroot;
}

sub intro {
	print <<EOM;

AmiGO Install Helper
========================

This installer is *ALPHA* software.
This script sets up AmiGO with optional BLAST to run in a web server.
It need to get information on database AmiGO will access, cgi directory
where AmiGO script lives, etc. Similarly for blast if to install blast.
All input values will be saved to '$tmpl' file so you can redo install.

Let's begin ....

EOM
}

sub cpanmodlist {
	my @mods = ();
	if (-d "cpan") {
		open(F, "cpan/modlist.txt");
		@mods = map{chomp;$_}<F>;
		close(F);
	}
	return @mods;
}

sub install_cpan {
	my @mods = &cpanmodlist;
	
}

sub AUTOLOAD {
	
	my $self = shift;
 
	my $name = $AUTOLOAD;

	if ($name eq "DESTROY") {
	# we dont want to propagate this!!
	return;
	}
	$name =~ s/.*://;	# strip fully-qualified portion


	$conf{$name} = shift if @_;
	if (!defined($conf{$name})) {
		my $def = $defaults{$name};
		print "Enter a value for $name [$def]:";
		my $val = <STDIN>;
		chomp $val;
		$val = $def unless $val;
		$conf{$name} = $val;
	}
	$conf{$name};
}


