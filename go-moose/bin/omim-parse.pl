#!/usr/bin/perl -w

use strict;
use Data::Dumper;
use GOBO::Writers::OBOWriter;

our %ignoreh = 
    (
     'ED' => 1,
     'CD' => 1,
     'CN' => 1,
     'SA' => 1,
    );

my %typemap =
    ('*' => 'gene',
     '#' => 'descriptive',
     '+' => 'gene_and_phenotype',
     '%' => 'unknown_basis',
     '' => 'not_established',
     '^' => 'obsolete');

$/ = '*RECORD*';


my $w = GOBO::Writers::OBOWriter->new();
$w->fh(\*STDOUT);

foreach my $k (keys %typemap) {
    $w->tagval(subsetdef => 
	       sprintf('%s "%s"',$typemap{$k}, $typemap{$k}));
}
$w->tagval(subsetdef => 'variant "variant"');

$w->nl;

my @records = ();
while(<>) {
    chomp;
    my %vh = parse($_);
    my %ph = process(%vh);
    write_obo(%ph);
    push(@records,\%ph);
    #export(%ph);
}
#print Dumper \@records;
exit 0;

sub parse {
    my $block = shift;
    my @lines = split(/\n/,$_);
    my $field;
    my %vh = ();
    for (my $i=0; $i<@lines; $i++) {
        my $line = $lines[$i];
        if ($line =~ /^\*FIELD\*\s+(\S+)/) {
            $field = $1;
            next;
        }
        if (!$field && $line) {
            die ";;$block;;";
        }
        if ($field) {
            $vh{$field} .= "$line\n";
        }
    }
    return %vh;
}

sub process {
    my %vh = @_;
    my %ph = ();
    foreach my $k (keys %vh) {
        process_field(\%ph,$k,$vh{$k});
    }
    return %ph;
}

sub process_field {
    my ($h,$k,$v) = @_;
    
    $v =~ s/\s+$//;
    $v =~ s/^\s+//;
    if ($k eq 'NO') {
        $h->{id} = $v;
    }
    elsif ($k eq 'TI') {
        if ($v =~ /^(\S+)\s+(.*)/) {
            my $id = $1;
            $v=$2;
            my $type = '';
            if ($id =~ /^([\#\*\$\%\^\+])(\d+)/) {
                $type = $1;
                $id = $2;
            }
            elsif ($id =~ /^(\d+)/) {
            }
            else {
                die $id;
            }
            $h->{type} = maptype($type);
            my @names = split(/\s*[\;\n]\s*/,$v);
            $h->{name} = shift @names;
            $h->{synonyms} = [grep {$_} @names];
        }
        else {
            die "v=$v";
        }
    }
    elsif ($k eq 'TX') {
        $h->{text} = $v;
        my @paras = split(/\n\n/,$v);
        if ($h->{type} && $h->{type} eq '#') {
            my @mims1 = ($paras[0] =~ /\(\S+;\s*([\d\.]+)\)/sg);
            my @mims2 = ($paras[0] =~ /\(([\d\.]+)\)/sg);
            # filter out years
            $h->{associated_with} = [grep {int($_) > 3000} @mims1, @mims2];
        }
    }
    elsif ($k eq 'AV') {
        # allelic variants
        my @lines = split(/\n/,$v);
        my %ah = ();
        push(@{$h->{variants}},\%ah);
        $ah{id} = shift @lines;
        my @anames = ();
        foreach (@lines) {
            if ($_ eq '') {
                my ($mgene,$mut) = split(/,\s+/,pop @anames);
                $ah{gene} = $mgene;
                $ah{seqvar} = $mut;
                last;
            }
            push(@anames,$_);
        }
        $ah{names} = \@anames;
        $ah{description} = join(" ",@lines);
    }
    elsif ($k eq 'CS') {
        # clinical symptoms
        # TODO
    }
    elsif ($k eq 'RF') {
        # refs
        # TODO
    }
    elsif ($ignoreh{$k}) {
    }
    else {
        print STDERR "? $k\n";
        $h->{$k} = $v;
    }
}

# * - gene
# # - disorder associated with gene
sub maptype {
    my $code = shift;
    return $typemap{$code};
}

sub export {
    my %vh = @_;
    print Dumper \%vh;
}

sub write_obo {
    my %ph = @_;
    return unless $ph{id};
    my $id = "MIM:$ph{id}";
    $w->open_stanza('Term');
    $w->tagval(id=>$id);
    $w->tagval(name=>$ph{name});
    if ($ph{name} =~ /MOVED TO (\d+)/) {
        $w->unary('is_obsolete');
        $w->tagval('replaced_by', "MIM:$1");
    }
    elsif ($ph{type} eq 'obsolete') {
        $w->unary('is_obsolete');
    }
    my $subset = $ph{type};
    $w->tagval('subset',$subset) if $subset;

    $w->ntagval('def', _quote($ph{text}), []);
    $w->ntagval('synonym',
                _quote($_),'RELATED','',[]) foreach @{$ph{synonyms}};
    foreach (@{$ph{associated_with} || []}) {
        $w->tagvals(relationship => ('associated_with', "MIM:$_"));
    }
    $w->nl;

    foreach my $v (@{$ph{variants} || []}) {
        $w->open_stanza('Term');
        $w->tagval(id=>$id."$v->{id}");
        $w->tagval(name=>"$v->{gene}$v->{id}");
        $w->ntagval('synonym',
                    _quote($_),'RELATED','',[]) foreach @{$v->{names}};
 	$w->tagval('subset','variant');
	$w->tagvals(relationship => ('variant_of', $id));
        $w->tagvals(relationship => ('sequence_modification', "MOD:$v->{seqvar}"));
        $w->tagvals(relationship => ('gene', "symbol:$v->{gene}"));
        $w->ntagval('def', _quote($v->{description}), []);
	$w->nl;

    }
}

sub _quote {
    GOBO::Writers::OBOWriter::_quote(@_);
}
