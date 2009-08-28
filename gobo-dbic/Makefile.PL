# $id$
use ExtUtils::MakeMaker;
require 5.6.0;


WriteMakefile(
	      'NAME' => 'GOBO::DBIC::GODBModel',
	      'DISTNAME' => 'GOBO-DBIC-GODBModel',
	      'ABSTRACT' =>
	      'DBIC wrapper and related for the Gene Ontology database.',
	      'AUTHOR' =>'Seth Carbon <sjcarbon@cpan.org>',
	      'VERSION_FROM' => 'GOBO/DBIC/GODBModel.pm',
	      'PREREQ_PM' =>
	      {
	       'Data::Dumper' => 0,
	       'DBIx::Class::Schema' => 0,
	      },
	      'EXE_FILES' => [],
	      'dist' =>
	      {
	       'SUFFIX'       => '.gz',
	       'DIST_DEFAULT' => 'all tardist',
	       'COMPRESS'     => 'gzip -9f',
	      },
	      clean => {
			FILES => 'GOBO-DBIC-$(VERSION).tar.gz',
		       },
	     );
