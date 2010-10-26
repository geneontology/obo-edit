package GOBO::Parsers::ChunkParser;
use Moose::Role;
use GOBO::Parsers::Parser;

has line_no => (is=>'rw', isa=>'Int', default=>sub{0});
has stalled => (is=>'rw', isa=>'Bool');
has max_chunk => (is=>'rw', isa=>'Int', init_arg => 'size', clearer=>'clear_max_chunk');

=head2 parse_chunk

$parser->parse_chunk(size => 50, options => $option_h)

input:  self
        size => 1000       # the number of lines to parse
        options => $opt_h  # a hash of options [optional]

Parse according to the options. Note that the file to be parsed should already
have been specified.

This method does not return anything; instead, the parser object can be queried
for the results.

=cut

sub parse_chunk {
	my $self = shift;
	if ($self->parsed_header && ! $self->stalled) {
		return 0;
	}
	confess "No file handle present!" unless $self->has_fh;
	my %args = (@_);
	$self->max_chunk($args{size}) if $args{size};
	$self->set_options($args{options}) if $args{options};
	return $self->_parse;
}

override 'next_line' => sub {
	my $self = shift;
	my $max_chunk = $self->max_chunk;
	my $line_no = $self->line_no + 1;
	$self->line_no($line_no);

	$self->stalled(0);
	if ($self->parsed_header && $max_chunk && $line_no > $max_chunk) {
		$self->line_no(0);
		$self->stalled(1);
		return undef;
	}
	my $lines = $self->lines;
	if (@$lines) {
		return shift @$lines;
	}

	if ($self->fh)
	{	my $fh = $self->fh;
		my $line = <$fh>;
		return $line;
	}
	return undef;
};

override 'unshift_line' => sub {
	my $self = shift;
	$self->line_no($self->line_no - scalar(@_));
	unshift(@{$self->lines},@_);
	return;
};

override 'reset_parser' => sub {
	my $self = shift;
	$self->clear_fh;
	$self->clear_all_options;
	$self->clear_data;
	$self->clear_max_chunk;
	$self->reset_temporary_variables;
};


override 'reset_temporary_variables' => sub {
	my $self = shift;
	$self->parsed_header(0);
	$self->stalled(0);
	$self->line_no(0);
	$self->checked_options(0);
};



1;
