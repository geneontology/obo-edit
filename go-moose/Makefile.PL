# $id$

use ExtUtils::MakeMaker;
require 5.6.0;


WriteMakefile(
    'NAME'         => "OBO",
    'DISTNAME'         => "obo-perl",
    'ABSTRACT'     => 'OBO Perl',
    'AUTHOR'       => 'Chris Mungall <cjm@fruitfly.org>',
    'VERSION_FROM' => "OBO/Graph.pm",
    'PREREQ_PM'    => {
        'Moose'           => 0,
    },
    'EXE_FILES'    => [
        qw(
                                    bin/go-gaf-inference.pl
                                   )
    ],
    'dist'         => { 
        'SUFFIX'       => ".gz",
        'DIST_DEFAULT' => 'all tardist',
        'COMPRESS'     => "gzip -9f" 
    },
    clean => {
        FILES => 'go-perl-$(VERSION).tar.gz',
    },
    );
