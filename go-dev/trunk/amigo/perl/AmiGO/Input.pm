# =head1 AmiGO::Input

# Defines a few constants and helpers for the sub classes.

# =cut

# use utf8;
# use strict;
# use CGI qw/:standard/;
# ## No more carping--we'll do it from here on for style issues.
# #use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

# package AmiGO::Input;

# use base ("AmiGO");

# my $large_size  = 10000;
# my $medium_size = 256;
# my $small_size  = 4;

# my %known_formats =
#   (
#    'html' => 1,
#    'xml' => 1,
#    'json' => 1,
#    'tab' => 1,
#   );

# my %known_requests =
#   (
#    'client' => 1,
#    'results' => 1,
#    'jsapi' => 1,
#    'wsdl' => 1,
#   );


# ## An example of SUB_ARGS in the case of request might be:
# ## SUB_ARGS => { 'client' => {web_2_0 => {...},
# ##                            web_3_0 => {...}},
# ##               'results'=> {html_3_2 => {...},
# ##                            html_4_0 => {...}}
# my $DEFAULT_SET = {
# 		   request=>
# 		   {
# 		    CARDINALITY => 'one',
# 		    PARSER => [\&is_known_request_format_p],
# 		    SUB_ARGS => undef,
# 		   },
# 		  };
# my $TERM_SET = {};
# my $GP_SET = {};
# my $ASSOC_SET = {};


# =item inspect

# This function inspects the incoming input and returns a hash of all
# valid variables.

# Side effects: error accumulation.

# =cut
# sub inspect {

#   my $self = shift;
#   my $incoming = shift || "";

#   my %nice_params = ();

#   ## Run the defaults no matter what.
#   $self->_check($DEFAULT_SET);
#   ## Use mapcar to splice on.

#   ## Run the optional ones only if requested (explicitly or
#   ## implicitly).
#   $self->_check($TERM_SET) if $incoming eq 'TERM_SET' ||
#     $incoming eq 'ASSOC_SET';
#   $self->_check($GP_SET) if $incoming eq 'GP_SET' ||
#     $incoming eq 'ASSOC_SET';
#   $self->_check($ASSOC_SET) if $incoming eq 'ASSOC_SET';

#   return %nice_params;
# }


# ## TODO/BUG: This needs to be rewritten recursively so that SUB_ARGS
# ## can be explored.
# sub _check {

#   my $self = shift;
#   my $set = shift || {};
#   my %nice_params = ();

#   ## For each of the items defined in the set.
#   foreach my $p_name (keys %{$set}) {

#     ## We're supposed to be dealing with a single parameter.
#     if ( $set->{CARDINALITY} && ( $set->{CARDINALITY} eq '1' ||
# 				  $set->{CARDINALITY} == 1 ) ){

#       ## If the incoming is defined.
#       my $p_value = $self->{CGI}->param($p_name);
#       if ( $p_value ) {

# 	## ...do all of the validity checks.
# 	my $passed = 1;
# 	$set->{PARSER}->($p_value)
# 	  if ( ! defined( $set->{PARSER}->($p_value) ) {
# 	    #print STDERR "FAIL: $p_name:$p_value in $vfunc\n";
# 	    $passed = 0;
# 	    $self->_failure("$p_name failed a validity test");
# 	  }

# 	       ##
# 	       if ( $passed ) {

# 		 ## Add value to hash.
# 		 $nice_params{$p_name} = $p_value;
		 
# 		 ## Check if there are SUB_ARGS and recur.
# 		 ## TODO:
# 	       }
	       
# 	    }else {
	      
# 	      ## TODO
# 	      $set->{$p_name}->{GENERATOR});
	
#       }

#     }else {

#       ## OTherwise, we're dealing with an array of values.

#     }
#   }
# }



# # ## Optional but necessary numeric argument.
# # #
# # if( $min_gps &&
# #     ( length($min_gps) > $upper_arg_size_limit ||
# #       $min_gps =~ /[^0-9]+/ ) ){ # TODO: Make this a better check.
# #   die_template({MESSAGE => 'illegitimate min_gps value',
# # 		STAMP => $time_stamp, URL => $html_url});
# # }elsif( $min_gps ){
# #   $min_gps = $min_gps + 0;
# # }else{
# #   $min_gps = 2;
# # }


# #  }
# #
# #  return $foo;
# #}

# ## TODO/BUG: At some point I had started writing an argument
# ## meta-language, but I've abandoned this and am just going to use the
# ## ad-hoc code that has accumulated up until now.
# # 		  # version=>
# # 		  # 		  {
# # 		  # 		   DEFINED => undef,
# # 		  #      DEFAULT => '_TODO_', # TODO: could I get the from env?
# # 		  # 		   PARSER => undef,
# # 		  # 		   SUB_ARGS => undef,
# # 		  # 		  },
# # ## These are the default arguments that must be defined at the end of
# # ## the process.
# # 		  format=>
# # 		  {
# # 		   DESCRIPTION => 'action that we want to take',
# # 		   DEFINED => {'html', 'xml', 'tab'},
# # 		   DEFAULT => 'html',
# # 		   PARSER => undef,
# # 		   SUB_ARGS => undef,
# # 		  },
# # 		  force=>
# # 		  {
# # 		   DESCRIPTION => 'action that we want to take',
# # 		   DEFINED => {'yes', 'no'},
# # 		   DEFAULT => 'yes',
# # 		   PARSER => undef,
# # 		   SUB_ARGS => undef,
# # 		  },
# # 		  session_id=>
# # 		  {
# # 		   DESCRIPTION => 'action that we want to take',
# # 		   DEFINED => undef,
# # 		   DEFAULT => 'yes',
# # 		   PARSER => undef,
# # 		   SUB_ARGS => undef,
# # 		  },
# # 		 };

# # #    session_id =>
# # #    {
# # #     DEFAULT_GENERATOR => \&generate_new_session_id,
# # #     ERROR => 'not a legitimate "session_id" argument',
# # #     PARSER => \&is_a_session_id_p,
# # #     #DEPENDS => \&is_always_false,
# # #    },

# # #    version =>
# # #    {
# # #     DEFAULT_GENERATOR => \&generate_new_session_id,
# # #     ERROR => 'not a legitimate "session_id" argument',
# # #     PARSER => \&is_a_session_id_p,
# # #     #DEPENDS => \&is_always_false,
# # #    },

# # #    format =>
# # #    {
# # #     DEFAULT_GENERATOR => \&generate_new_session_id,
# # #     ERROR => 'not a legitimate "session_id" argument',
# # #     PARSER => \&is_a_session_id_p,
# # #     #DEPENDS => \&is_always_false,
# # #    },

# # #    force =>
# # #    {
# # #     DEFAULT_GENERATOR => \&generate_new_session_id,
# # #     ERROR => 'not a legitimate "session_id" argument',
# # #     PARSER => \&is_a_session_id_p,
# # #     #DEPENDS => \&is_always_false,
# # #    },
# # #   );


# =item new

# =cut
# sub new {

#   ##
#   my $class = shift;
#   my $self  = $class->SUPER::new();
#   #my $arg = shift || {};

#   ## We'll borrow SUCCES and ERROR_MESSAGE from AmiGO.

#   ## Set up CGI environment,
#   ## TODO: Consider using CGI::Simple instead.
#   $CGI::POST_MAX = 1024 * 100000; ## 100M uploads max.
#   $self->{CGI} = new CGI;
#   #$self->{ERROR} = AmiGO::Error->new(__PACKAGE__);

#   ## TODO: Replace these with is_too_long etc. functions.
#   #$self->{SMALL_LIMIT} = 256;
#   #$self->{LARGE_LIMIT} = 100000;

#   ## First, check the required default arguments.
#   #foreach my $req_arg (keys %amigo_args){
#   #}

#   bless $self, $class;
#   return $self;
# }


# =item is_small_p

# Arguments: arg
# Returns: 1 or 0

# =cut
# sub is_small_p{

#   my $self = shift;
#   my $in = shift || undef;
#   my $result = 0;

#   if ( $in && length($in) < $small_size ) {
#     $result = 1;
#   }

#   return $result;
# }


# =item is_medium_p

# Arguments: arg
# Returns: 1 or 0

# =cut
# sub is_medium_p{

#   my $self = shift;
#   my $in = shift || undef;
#   my $result = 0;

#   if ( $in && length($in) < $medium_size ) {
#     $result = 1;
#   }

#   return $result;
# }


# =item is_large_p

# Arguments: arg
# Returns: 1 or 0

# =cut
# sub is_large_p{

#   my $self = shift;
#   my $in = shift || undef;
#   my $result = 0;

#   if ( $in && length($in) < $large_size ) {
#     $result = 1;
#   }

#   return $result;
# }


# =item is_whole_p

# Arguments: arg
# Returns: 1 or 0

# =cut
# sub is_whole_p {

#   my $self = shift;
#   my $in = shift || undef;
#   my $result = 0;

#   if ( $in && $in =~ /^[0-9]+$/ ) {
#     $result = 1;
#   }

#   return $result;
# }


# =item is_integer_p

# Arguments: arg
# Returns: 1 or 0

# =cut
# sub is_integer_p {

#   my $self = shift;
#   my $in = shift || undef;
#   my $result = 0;

#   if ( $in && $in =~ /^[\+\-]?[0-9]+$/ ) {
#     $result = 1;
#   }

#   return $result;
# }


# =item is_float_p

# Arguments: arg
# Returns: 1 or 0

# =cut
# sub is_float_p {

#   my $self = shift;
#   my $in = shift || undef;
#   my $result = 0;

#   if ( $in && $in =~ /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/ ) {
#     $result = 1;
#   }

#   return $result;
# }


# =item is_known_response_format_p

# Arguments: arg
# Returns: 1 or 0

# =cut
# sub is_known_response_format_p {

#   my $arg = shift || undef;
#   my $return_val = 0;

#   if( $arg && $known_formats{ $arg } ){
#     $return_val = 1
#   }

#   return $return_val;
# }


# =item is_known_request_format_p

# Arguments: arg
# Returns: 1 or 0

# =cut
# sub is_known_request_format_p {

#   my $arg = shift || undef;
#   my $return_val = 0;

#   if( $arg && $known_requests{ $arg } ){
#     $return_val = 1
#   }

#   return $return_val;
# }


# =item is_yes_no_p

# Arguments: arg
# Returns: 1 or 0

# =cut
# sub is_yes_no_p {

#   my $arg = shift || undef;
#   my $return_val = 0;
#   if( $arg eq 'yes' ||
#       $arg eq 'no' ){
#     $return_val = 1
#   }

#   return $return_val;
# }




# # ## Gene labels in the input box for enrichment processing.
# # my $gp_list = $query->param('gp_list');
# # if( $gp_list &&
# #     ( length($gp_list) > $upper_list_arg_size_limit ||
# #       $gp_list =~ /[^0-9a-zA-Z\_\.\s\-\[\]\(\)\:]+/ ) ){
# #   die_template({MESSAGE => 'illegitimate gene list value',
# # 		STAMP => $time_stamp, URL => $html_url});
# # }
# # if( $gp_list && $gp_list !~ /[a-z0-9]/i ){ # text in there too
# #   $gp_list = '';
# # }

# # ## Stub for the background gene product list.
# # my $bggp_list = $query->param('bggp_list');
# # if( $bggp_list &&
# #     ( length($bggp_list) > $upper_list_arg_size_limit ||
# #       $bggp_list =~ /[^0-9a-zA-Z\_\.\s\-\[\]\(\)\:]+/ ) ){
# #   die_template({MESSAGE => 'illegitimate background gene list value',
# # 		STAMP => $time_stamp, URL => $html_url});
# # }
# # if( $bggp_list && $bggp_list !~ /[a-z0-9]/i ){ # text in there too
# #   $bggp_list = '';
# # }
# # #my $bggp_list = '';

# # ## The gene product list file.
# # my $gp_filehandle = $query->upload('gp_file');
# # if ( ! $gp_filehandle  && $query->cgi_error() ){
# #   my $error = $query->cgi_error();
# #   die_template({MESSAGE => "gp_file upload failed: $error ",
# # 		STAMP => $time_stamp, URL => $html_url});
# # }else{
# #   if ( $gp_filehandle && (
# # 			  $gp_filehandle =~ /\.gz/i ||
# # 			  $gp_filehandle =~ /\.bz/i ||
# # 			  $gp_filehandle =~ /\.bz2/i ||
# # 			  $gp_filehandle =~ /\.zip/i ||
# # 			  $gp_filehandle =~ /\.z/i ||
# # 			  $gp_filehandle =~ /\.tgz/i ) ){
# #     die_template({MESSAGE =>
# # 		  "AmiGO does not currently accept compressed files. " .
# # 		  "Please uncompress your file and try again.",
# # 		  STAMP => $time_stamp, URL => $html_url});
# #   }
# # }

# # ## The background gene product list file.
# # my $bggp_filehandle = $query->upload('bggp_file');
# # if ( ! $bggp_filehandle  && $query->cgi_error() ){
# #   my $error = $query->cgi_error();
# #   die_template({MESSAGE => "bggp_file upload failed: $error ",
# # 		STAMP => $time_stamp, URL => $html_url});
# # }else{
# #   if ( $bggp_filehandle && (
# # 			  $bggp_filehandle =~ /\.gz/i ||
# # 			  $bggp_filehandle =~ /\.bz/i ||
# # 			  $bggp_filehandle =~ /\.bz2/i ||
# # 			  $bggp_filehandle =~ /\.zip/i ||
# # 			  $bggp_filehandle =~ /\.z/i ||
# # 			  $bggp_filehandle =~ /\.tgz/i ) ){
# #     die_template({MESSAGE =>
# # 		  "AmiGO does not currently accept compressed files. " .
# # 		  "Please uncompress your file and try again.",
# # 		  STAMP => $time_stamp, URL => $html_url});
# #   }
# # }

# # ## What is the GP file type?
# # my $gp_file_type = $query->param('gp_file_type');
# # if( $gp_file_type && length($gp_file_type) > $upper_arg_size_limit ){
# #   die_template({MESSAGE => "illegitimate gp_file_type value",
# # 		STAMP => $time_stamp, URL => $html_url});
# # }elsif( $gp_file_type && ( $gp_file_type eq 'list' ||
# # 			   $gp_file_type eq 'ga' ) ){
# #   ## OK, let it go.
# # }elsif( $gp_file_type ){
# #   die_template({MESSAGE => "unknown gp_file_type value",
# # 		STAMP => $time_stamp, URL => $html_url});
# # }

# # ## What is the BGGP file type?
# # my $bggp_file_type = $query->param('bggp_file_type');
# # if( $bggp_file_type && length($bggp_file_type) > $upper_arg_size_limit ){
# #   die_template({MESSAGE => "illegitimate bggp_file_type value",
# # 		STAMP => $time_stamp, URL => $html_url});
# # }elsif( $bggp_file_type && ( $bggp_file_type eq 'list' ||
# # 			     $bggp_file_type eq 'ga' ) ){
# #   ## OK, let it go.
# # }elsif( $bggp_file_type ){
# #   die_template({MESSAGE => "unknown bggp_file_type value",
# # 		STAMP => $time_stamp, URL => $html_url});
# # }

# # ## Optional but necessary numeric argument.
# # #my $cutoff = $query->param('cutoff');
# # if( $cutoff &&
# #     ( length($cutoff) > $upper_arg_size_limit ||
# #       $cutoff =~ /[^0-9\.]+/ ) ){ # TODO: Make this a better check.
# #   die_template({MESSAGE => 'illegitimate cutoff value',
# # 		STAMP => $time_stamp, URL => $html_url});
# # }elsif( $cutoff ){
# #   $cutoff = $cutoff + 0.0;
# # }else{
# #   $cutoff = 0.1;
# # }


# # ## Optional but necessary numeric argument.
# # #my $min_gps = $query->param('min_gps');
# # if( $min_gps &&
# #     ( length($min_gps) > $upper_arg_size_limit ||
# #       $min_gps =~ /[^0-9]+/ ) ){ # TODO: Make this a better check.
# #   die_template({MESSAGE => 'illegitimate min_gps value',
# # 		STAMP => $time_stamp, URL => $html_url});
# # }elsif( $min_gps ){
# #   $min_gps = $min_gps + 0;
# # }else{
# #   $min_gps = 2;
# # }


# # ##
# # sub is_under_small_length_bounds_p {

# #   my $arg = shift || '';
# #   my $return_val = 0;

# #   if( $arg < $upper_small_arg_size_limit ){
# #     $return_val = 1
# #   }else{
# #     ## TODO: add real error here.
# #     print STDERR "not under small bound\n";
# #   }

# #   return $return_val;
# # }


# # ##
# # sub is_under_large_length_bounds_p {

# #   my $arg = shift || '';
# #   my $return_val = 0;

# #   if( $arg < $upper_large_arg_size_limit ){
# #     $return_val = 1
# #   }else{
# #     ## TODO: add real error here.
# #     print STDERR "not under large bound\n";
# #   }

# #   return $return_val;
# # }


# # ##
# # sub is_a_gp_list_p {

# #   my $arg = shift || '';
# #   my $return_val = 0;

# #       $gp_list
# #   die_template({MESSAGE => 'illegitimate gene list value',
# # 		STAMP => $time_stamp, URL => $html_url});
# # }
# # if( $gp_list !~ /[a-z0-9]/i ){ # text in there too


# #   if( $arg =~ /[^0-9a-zA-Z\_\.\s\-\[\]\(\)\:]+/ ){
# #     if( $gp_list !~ /[a-z0-9]/i ){ # text in there too

# #   }else{
# #   }
# #       $arg
# #  ){
# #     $return_val = 1
# #   }else{
# #     ## TODO: add real error here.
# #     print STDERR "not string boolean\n";
# #   }

# #   return $return_val;
# # }



# # ##
# # ##
# # ## 
# # ##

# # ## These are arguments that required by all amigo components.
# # ## request: if it is not defined, 

# # ##########
# # ##
# # ## Sanity check all possible incoming parameters:
# # ##
# # ## Flow arguments:
# # ## 'request' drop into data mode, build data structure
# # ## 'force' this will force continuation instead of dying for *small*
# # ## problems
# # ##
# # ## Results arguments:
# # ## 'output' what we output (e.g. map, gafile, count, etc.)
# # ## 'format' how we output
# # ##
# # ## Data arguments:
# # ## 'gp_list' list of gene ids
# # ## 'gp_file' content of a gene id file (post)
# # ## 'gp_file_type'
# # ## 'bggp_file' content of a gene id file (post) for the background set
# # ## 'bggp_file_type'
# # ##
# # ## Filter arguments:
# # ## 'cutoff'
# # ## 'min_gps'
# # ## 'speciesdb'
# # ## 'ontology'
# # ##### 'evcode' TODO
# # ##


# # ## TODO: output?
# # ## TODO: force?
# # ## These are arguments that may be required by all amigo components.
# # ## TODO: Lamers are below the gap.
# # my @amigo_optional_args = qw(
# # 			      term
# # 			      term_list
# # 			      gp
# # 			      gp_list
# # 			      homology

# # 			      chunk

# # 			      output
# # 			   );

# # ## These are the possible groups of filters for AmiGO components.
# # ## 
# # ## gene_product_group: tax_id, species_db, gp_type
# # ## association_group:  evcode, qualifier, assigned_by
# # ## term_group:         ont
# # my @amigo_filter_group_args = qw(
# # 				  gene_product_group
# # 				  association_group
# # 				  term_group
# # 			       );


# # ##
# # sub is_a_string_or_empty_p {

# #   my $string = shift;
# #   die "this function requires an argument: $!" if ! defined $string;

# #   my $return_val = 0;
# #   if ( length($string) &&
# #        $string =~ /^[a-zA-Z0-9\.\-\_\/\\:]+$/ ){
# #     $return_val = 1;
# #   }elsif( $string eq '' ){
# #     $return_val = 1;
# #   }

# #   return $return_val;
# # }


# # ##
# # sub is_a_url_p {

# #   my $string = shift;
# #   die "this function requires an argument: $!" if ! defined $string;

# #   my $return_val = 0;
# #   if ( length($string) &&
# #        $string =~ /^[a-zA-Z0-9\-\_\:\_\/\.]+$/ ){
# #     $return_val = 1;
# #   }

# #   return $return_val;
# # }


# # ##
# # sub is_a_binary_p {

# #   my $string = shift;
# #   die "this function requires an argument: $!" if ! defined $string;

# #   my $return_val = 0;
# #   if ( length($string) &&
# #        -e $string ){
# #        #-e $string &&
# #        #-f $string &&
# #        #-X $string ){
# #     $return_val = 1;
# #   }

# #   return $return_val;
# # }


# # ##
# # sub is_a_directory_p {

# #   my $string = shift;
# #   die "this function requires an argument: $!" if ! defined $string;

# #   my $return_val = 0;
# #   if ( length($string) &&
# #        -e $string &&
# #        -d $string &&
# #        -R $string ){
# #     $return_val = 1;
# #   }

# #   return $return_val;
# # }


# # ##
# # sub depends_is_filters_false_p {

# #   my $return_val = 0;
# #   if ( $env_conf{GO_USE_DEFAULT_AMIGO_FILTERS}{NEW_VALUE} eq '0' ){
# #     $return_val = 1;
# #   }

# #   return $return_val;
# # }


# # ##
# # sub is_always_true {
# #   return 1;
# # }


# # ##
# # sub depends_is_blast_pbs_true_p {

# #   my $return_val = 0;
# #   if ( $env_conf{GO_SHOW_BLAST}{NEW_VALUE} eq '1' ){
# #     if ( $env_conf{GO_BLAST_METHOD}{NEW_VALUE} eq 'pbs' ){
# #       $return_val = 1;
# #     }
# #   }

# #   return $return_val;
# # }




# 1;
