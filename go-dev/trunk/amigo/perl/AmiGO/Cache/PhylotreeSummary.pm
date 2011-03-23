package AmiGO::Cache::PhylotreeSummary;
use warnings;
use strict;
use Data::Dumper;

use base 'AmiGO::Cache';

our $code_name  = __PACKAGE__ . '::code';
our $insert_sth = __PACKAGE__ . '::sth';

sub new{
    my $c = shift;
    my $sig = __PACKAGE__;
    $sig =~ s/.*:://;
    my $s = $c->SUPER::new($sig);
    my @code = map {
	$_->code;
    } @_;
    #warn 'Caching:' . join(',', @code);
    $s->{$code_name} = \@code;

    return bless $s, $c;
}

sub code_names{
    my $s = shift;
    return @{ $s->{$code_name} };
}

sub build{
    my $s = shift;

    $s->initialize();
    $s->open();

    my $schema = <<SQL .
CREATE TABLE data(
 id INTEGER PRIMARY KEY AUTOINCREMENT,
 xref_dbname VARCHAR(55),
 xref_key VARCHAR(255),
 last_annotated DATE,
 members INTEGER,
 refg_members INTEGER,
 exp INTEGER,
SQL
      join(",\n", map {
	  ' ' . lc($_) . ' INTEGER';
      } $s->code_names()) . ')';

    $s->{CACHE_DBH}->do($schema) or die $s->{CACHE_DBH}->errstr;
    $s->close();
}


sub cache_data{
    my $s = shift;
    my @code = $s->code_names;

    if (! $s->{$insert_sth} ) {
	my $sql = 'INSERT INTO data(xref_dbname,xref_key,last_annotated,members,refg_members,exp,' .
	  join(',', map { lc } @code) . ')VALUES(?,?,?,?,?,?,' .
	    join(',', ('?') x scalar(@code)) . ')';
	warn $sql;
	$s->{$insert_sth} = $s->{CACHE_DBH}->prepare($sql);
    }

    return $s->{$insert_sth}->execute(map {
	$_ ? $_ : 0;
    } @_);
}

sub get_all_data{
    my $s = shift;

    $s->open();
    my $sth = $s->{CACHE_DBH};
    my $all = $sth->selectall_arrayref("SELECT * FROM DATA");
    $s->close();

    return @$all;
}

1;
