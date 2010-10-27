#!/usr/bin/perl -w
use FileHandle;
use strict;

my $chunksize = 10;
my $base = 'foo';
if (scalar(@ARGV) && $ARGV[0] =~ /^\-./) {
    my $opt = shift @ARGV;
    if ($opt eq '-c' || $opt eq '--chunk-size') {
        $chunksize = shift @ARGV;
    }
    elsif ($opt eq '-b' || $opt eq '--base') {
        $base = shift @ARGV;
    }
    else {
        die "unknown option $opt";
    }
}

$/= "\n\n";
my $tmpf = "tmp-$$.obo";

my $n=0;
my $i=0;

my $done = 0;
my $chunk = '';
while (!$done) {
    $_ = <>;
    $i++;

    if ($_) {
        $chunk .= $_;
    }
    else {
        $done = 1;
    }

    if (!$n || $done || $i > $chunksize) {
        # always pass through with the first (header) chunk
        # always pass through with the last chunk

        # reset within-chunk count
        $i=0;
    }
    else {
        $n++;
        next;
    }

    my $fh = new FileHandle;
    # go2fmt.pl -w xml $@ | go-apply-xslt oboxml_to_owl -
    open(F,"|go2fmt.pl -w xml -p obo - | go-apply-xslt oboxml_to_owl - --stringparam xmlbase_relative $base > $tmpf") || die "go2owl";
    #print "(((( $_ ))))\n";
    print F $chunk;
    close(F);
    $chunk = '';

    my $skip_me = $n > 0;
    #print "**N= $n ;; $skip_me\n";
    open(G,$tmpf) || die $tmpf;
    $/= "\n";
    while(<G>) {
        if ($skip_me) {
            #print "SKIP: $_\n";
            if (m@</owl:Ontology>@) {
                $skip_me = 0; # start writing from next
            }
            # skip
        }
        elsif (m@</rdf:RDF>@) {
            # skip
        }
        else {
            print "$_";
        }
    }
    close(G);
    $/= "\n\n";
    $n++;
}
print "</rdf:RDF>\n";
unlink($tmpf);
