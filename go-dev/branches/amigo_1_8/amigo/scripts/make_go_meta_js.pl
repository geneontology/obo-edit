#!/usr/local/bin/perl
####
#### Produce the critical JS file that we need that contains the
#### location of our resources as well as useful metadata.
####
#### Usage:
####   ./scripts/make_go_meta_js.pl /srv/www/htdocs/amigo/js/org/bbop/amigo/go_meta.js
####

## Try to get the local environment sane. This is necessary for *any*
## operation, so we're really going to die.
BEGIN { require "config.pl" if -f "config.pl" ; }
use lib $ENV{GO_DEV_ROOT} . '/amigo/perl';

## Bring in necessaries.
use utf8;
use strict;
use AmiGO;
use JSON::PP;

my $core = AmiGO->new();

## If the db is already there, blow it away.
my $file_str = shift @ARGV;
unlink $file_str if -f $file_str;
open(FILE, ">$file_str") or die "can not open $file_str: $!";

## Get the basics. Should be fast as they are already cached in
## AmiGO.pm.
my $evcodes = $core->evidence_codes();
my $species = $core->species();
my $sources = $core->source();
my $gptypes = $core->gptype();
my $onts = $core->ontology();
my $relname = $core->release_name();
my $reltype = $core->release_type();

## Transform them into something a little more useful for the client
## (i.e. arrayified and ordered instead of the straight hash refs
## that we have.
my $fix = sub {

  my $thing = shift;

  ## Arrayify.
  $thing = [map { [$thing->{$_} , $_ ] } keys %$thing];

  ## Sort on key.
  $thing = [sort{
    #print STDERR $$a[0]. ' vs ' . $$b[0] . "\n";
    return $$a[0] cmp $$b[0];
  } @$thing];

  return $thing;
};

## Assemble all meta information.
my $ret =
  {
   ## Variables.
   app_base => $core->amigo_env('AMIGO_CGI_URL'),
   html_base => $core->amigo_env('AMIGO_HTML_URL'),
   image_base => $core->amigo_env('AMIGO_IMAGE_URL'),
   term_regexp => $core->amigo_env('AMIGO_TERM_REGEXP'),

   ## Data.
   evidence_codes => $evcodes,
   species_map => $species,
   species => &$fix($species),
   sources => &$fix($sources),
   gp_types => &$fix($gptypes),
   ontologies => &$fix($onts),
   release_name => $relname,
   release_type => $reltype,

   ## Resources (note naming convention).
   bbop_img_star => $core->get_image_resource('star'),
  };

##
my $js = JSON::PP->new();
$js->allow_bignum(1);
my $json_str = $js->encode($ret);

##
sub var_gen{
  my $key = shift || '';
  return "    var $key = meta_data.$key;";
}
sub getter_gen{
  my $key = shift || '';
  return "    this.$key = function(){ return $key; };";
}
my $acc_strings = ();
foreach my $key (keys %$ret){
  push @$acc_strings, var_gen($key);
  push @$acc_strings, getter_gen($key);
}
my $acc_string = join("\n", @$acc_strings);

##
print FILE <<EOJS;
////////////
////
//// org.bbop.amigo.go_meta
////
//// Purpose: Useful information about the GO and AmiGO.
////          Also serves as a repository and getter for web
////          resources such as images.
////
//// Requirements: amigo.js for org.bbop.amigo namespace.
////
//// NOTE: This file is generated dynamically at installation time.
////       Hard to work with unit tests--hope it's not too bad.
////       Want to keep this real simple.
////
//////////

//
org.bbop.amigo.go_meta = function(){

    // All of the server/instance-specific meta-data.
    var meta_data = $json_str;

    // Break out the data and various functions to access it...
$acc_string

    // Does it look like a term?
    var tre_str = meta_data.term_regexp;
    var tre = new RegExp(tre_str);
    this.term_id_p = function(term_id){
       var retval = false;
       if( tre.test(term_id) ){
          retval = true;
       }
       return retval;
    };

    // Get a named resource from the meta_data hash if possible.
    this.get_image_resource = function(resource){

       var retval = null;
       var mangled_res = 'bbop_img_' + resource;

       if( meta_data[mangled_res] ){
          retval = meta_data[mangled_res];
       }
       return retval;
    };
};
EOJS
