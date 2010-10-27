#!/usr/bin/perl

use FileHandle;
$/= "\n\n";

my $n=0;
while (<>) {
    my $fh = new FileHandle;
    open(F,"|go2owl -p obo -|") || die;
    print F $_;
    my $skip_me = $n > 0;
    print "**N= $n ;; $skip_me\n";
    while(<F>) {
        if ($skip_me) {
            print "SKIP: $_\n";
            if (m@</owl:Ontology>@) {
                $skip_me = 0; # start writing from next
            }
            # skip
        }
        elsif (m@</rdf:RDF>@) {
            # skip
        }
        else {
            print "X:$_";
        }
    }
    close(F);
    $n++;
}
