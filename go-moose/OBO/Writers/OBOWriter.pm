package OBO::Writers::OBOWriter;
use Moose;
use strict;
extends 'OBO::Writers::Writer';
use OBO::Node;
use OBO::LinkStatement;

sub write_header {
    my $self = shift;
    my $g = $self->graph;
    $self->tagval('format-version','1.2');
    return;
}

sub write_body {
    my $self = shift;
    my $g = $self->graph;

    foreach my $term (@{$g->terms}) {
        $self->write_stanza($term);
    }
    foreach my $relation (@{$g->relations}) {
        $self->write_stanza($relation);
    }
    # TODO: instances
    return;
}

sub write_stanza {
    my $self = shift;
    my $node = shift;

    $self->nl;
    my $stanzaclass = 'Instance';
    if ($node->isa('OBO::TermNode')) {
        $stanzaclass = 'Term';
    }
    elsif ($node->isa('OBO::RelationNode')) {
        $stanzaclass = 'Typedef';
    }
    elsif ($node->isa('OBO::Annotation')) {
        # TODO
    }
    
    $self->open_stanza($stanzaclass);
    $self->tagval('id',$node->id);
    $self->tagval('name',$node->label);
    $self->tagval('namespace',$node->namespace);
    $self->tagval('alt_id',$_) foreach @{$node->alt_ids || []};
    if ($node->definition) {
        $self->ntagval('def', _quote($node->definition), $node->definition_xrefs || [])
    }
    $self->tagval('comment',$node->comment);
    $self->tagval('subset',$_->id) foreach @{$node->in_subsets || []};
    $self->ntagval('synonym',
        _quote($_->label),$_->scope,$_->type,$_->xrefs || []) foreach @{$node->synonyms || []};
    if ($node->isa('OBO::RelationNode')) {
        foreach (OBO::RelationNode->unary_property_names) {
            $self->unary("is_$_") if $node->$_();
        }
    }
    return;
}

sub open_stanza {
    my $self = shift;
    my $c = shift;
    $self->println("[$c]");
    return;
}

sub unary {
    my $self = shift;
    $self->tagval(shift, 'true');
}

sub tagval {
    my $self = shift;
    my $tag = shift;
    my $val = shift;
    return unless defined $val;
    $self->printf("%s: %s\n",$tag,$val);
        
}

sub _quote {
    my $s = shift;
    $s =~ s/\"/\\\"/g;
    return sprintf('"%s"',$s);
}

# n-ary tags
sub ntagval {
    my $self = shift;
    my $tag = shift;
    my @vals = @_;
    $self->printf("%s:",$tag);
    foreach my $v (@vals) {
        next unless defined $v;
        $self->print(" ");
        if (ref($v)) {
            if (ref($v) eq 'ARRAY') {
                $self->print("[");
                $self->print(join(', ',
                                  @$v)); # TODO
                $self->print("]");
            }
            elsif (ref($v) eq 'HASH') {
                $self->print("{");
                $self->print(join(', ',
                                  map {
                                      sprintf('%s=%s',$_,_quote($v->{$_}))
                                  } keys %$v)); # TODO
                $self->print("}");
            }
            else {
            }
        }
        else {
            $self->print($v);
        }
    }
    $self->nl;
}


1;
