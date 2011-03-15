package GOBO::Writers::ChunkWriter;

use warnings;
use strict;

# Other modules:
use Moose::Role;

# Module implementation
#

requires 'graph';
requires 'write_header';
requires 'write_body';

has 'header_written' => (
    is      => 'rw',
    isa     => 'Bool',
    default => 0
);

sub write_chunk {
    my ( $self, %arg ) = @_;
    if ( $arg{graph} ) {
        $self->graph( $arg{graph} );
    }

    if ( !$self->header_written ) {
        $self->write_header;
        $self->header_written(1);
    }
    $self->write_body;
}

1;    # Magic true value required at end of module

