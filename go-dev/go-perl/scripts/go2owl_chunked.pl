#!/usr/bin/perl -w
####
#### Whackiness with I/O: http://perldoc.perl.org/perlipc.html#Bidirectional-Communication-with-Another-Process
####

use FileHandle;
use IPC::Open2;
$/= "\n\n";

my $n=0;
while (<>) {
    my $fh = new FileHandle;
    #open(F,"|go2owl -p obo -|") || die;
    my $pid = open2(*READ, *WRITE, "go2owl -p obo" ) || die;
    print WRITE $_;
    my $skip_me = $n > 0;
    print "**N= $n ;; $skip_me\n";
    while(<READ>) {
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
    close(READ);
    close(WRITE);
    $n++;
}
