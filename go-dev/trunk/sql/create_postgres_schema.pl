#!/usr/local/bin/perl
use strict;
use FileHandle;

my $outf;
my $target = "mysql";
while ($ARGV[0] =~ /^\-/) {
    my $arg = shift @ARGV;
    if ($arg =~ /^\-o$/) {
        $outf = shift @ARGV;
    }
    elsif ($arg =~ /^\-h/) {
        print usage();
        exit;
    }
    elsif ($arg =~ /^\-t/) {
        $target = shift @ARGV;
    }
    else {
        die ("switch $arg not regognized");
    }
}

my @path = ();
while (my $inf = shift @ARGV) {
    my $fho = FileHandle->new(">$outf") if $outf;
    readf($inf);
    $fho->close if $fho;
}

exit 0;

sub readf {
    my $f = shift @_;
    my $dirname = `dirname $f`;
    chomp $dirname;
    print STDERR "READING $f\n";
    my $fh = FileHandle->new($f);
    $fh || die("can't read $f");
    while(<$fh>) {
        chomp;
        if (/\#include\s+(.*)/) {
            my $schema_file = $1;
            my $schema = $schema_file;
            $schema =~ s/\.sql//;
            $schema =~ s/.*\///;
            $schema =~ s/\-/_/g;
            unshift(@path,$schema);
            my $path_str = join(',',@path);
            runcmd("./my2pg-comment-convert.pl -s $schema -p $path_str $schema_file");
        }   
        else {
            die;
        }
    }
    $fh->close;
}

sub runcmd {
    print STDERR "@_\n";
    system("@_");
}

sub usage {
    return <<EOM

compiledb [-t dbms] [-o outfile] sqlsrc

EOM
}
