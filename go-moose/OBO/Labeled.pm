package OBO::Labeled;
use Moose::Role;
use OBO::Synonym;

has label => (is => 'rw', isa => 'Str'); # TODO -- delegate to primary synonym?
has synonyms => ( is=>'rw', isa=>'ArrayRef[OBO::Synonym]');

sub add_synonyms {
    my $self = shift;
    if (!$self->synonyms) {
        $self->synonyms([]);
    }
    push(@{$self->synonyms},map { new OBO::Synonym(label=>$_) } @_);
    return;
}

sub add_synonym {
    my $self = shift;
    $self->add_synonyms(@_);
}

1;

