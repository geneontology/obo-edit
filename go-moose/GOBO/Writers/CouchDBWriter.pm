package GOBO::Writers::CouchDBWriter;
use Moose;
use strict;
extends 'GOBO::Writers::PerlHashWriter';
use JSON::PP;

has db_url => (is=>'rw', isa=>'Str');

sub write_stanza {
    my $self = shift;
    my $doc = $self->SUPER::write_stanza(@_);
    my $json = encode_json $doc;
    my $id = $doc->{id};
    my $db_url = $self->db_url;
    my @cmd = ("curl", "-X", "PUT", "$db_url/$id", "-d", $json);
    my $cmd = join ' ', @cmd;
    print STDERR "Command: $cmd\n";
    system(@cmd);
}

1;
