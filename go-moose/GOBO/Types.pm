package GOBO::Types;
use Moose;
use Moose::Util::TypeConstraints;

use DateTime::Format::ISO8601;
require DateTime;

use Data::Dumper;

subtype 'GOBO::Attributed::Date' => as class_type 'DateTime';
#    => where { $_->isa('DateTime') };

coerce 'GOBO::Attributed::Date'
    => from 'Str'
    => via {
        if (/(\d\d\d\d)(\d\d)(\d\d)/) {
            DateTime->new(year=>$1,month=>$2,day=>$3);
        }
        elsif (/(\d\d):(\d\d):(\d\d\d\d)\s+(\d\d):(\d\d+)/) {
            # date tags in obo headers follow this convention
            DateTime->new(year=>$3,month=>$2,day=>$1,hour=>$4,minute=>$5);
        }
        elsif (/(\d\d):(\d\d):(\d\d\d\d)/) {
            DateTime->new(year=>$3,month=>$2,day=>$1);
        }
        elsif (/\d\d\d\d\-/) {
            DateTime::Format::ISO8601->parse_datetime( $_ );
        }
        else {
            undef;
        }
};

class_type 'GOBO::Node';
coerce 'GOBO::Node'
	=> from 'Str'
	=> via { require GOBO::Node; GOBO::Node->new(id=>$_) };

class_type 'GOBO::ClassNode';
coerce 'GOBO::ClassNode'
      => from 'Str'
      => via { require GOBO::ClassNode; GOBO::ClassNode->new(id=>$_) };


class_type 'GOBO::Synonym';
coerce 'GOBO::Synonym'
      => from 'Str'
      => via { require GOBO::Synonym; GOBO::Synonym->new(label=>$_) };

class_type 'GOBO::AnnotationSubject';
coerce 'GOBO::AnnotationSubject'
 => from 'GOBO::Node'
 => via { bless $_, 'GOBO::AnnotationSubject' }
 => from 'Str'
 => via { require GOBO::AnnotationSubject; GOBO::AnnotationSubject->new(id=>$_) };

#class_type 'GOBO::NegatedStatement';
#coerce 'GOBO::NegatedStatement'
#    => from 'GOBO::Statement'
#    => via { require GOBO::Statement; require GOBO::NegatedStatement; GOBO::NegatedStatement->new(statement=>$_) };

class_type 'GOBO::Phylo::PhyloNode';
coerce 'GOBO::Phylo::PhyloNode'
    => from 'GOBO::Node'
    => via { require GOBO::Phylo::PhyloNode; GOBO::Phylo::PhyloNode->new(represents=>$_) };

class_type 'GOBO::RelationNode';
coerce 'GOBO::RelationNode'
    => from 'GOBO::Node'
    => via { require GOBO::RelationNode; bless $_, 'GOBO::RelationNode' }
    => from 'Str'
    => via { require GOBO::RelationNode; my $rel = GOBO::RelationNode->new(id=>$_); $rel->post_init;return $rel; };  # TODO -- is there a more elegant way of doing this?

subtype 'GOBO::Types::NodeArray'
	=> as 'ArrayRef[GOBO::Node]';

coerce 'GOBO::Types::NodeArray'
	=> from 'ArrayRef[Str]'
	=> via { require GOBO::Node; map { GOBO::Node->new($_) } @$_; }
	=> from 'GOBO::Node'
	=> via { [$_] };

subtype 'GOBO::Types::SingleNodeArray'
	=> as 'GOBO::Types::NodeArray'
	=> where { scalar @$_ <= 1 }
	=> message { "This array may only contain a single node" } ;

coerce 'GOBO::Types::SingleNodeArray'
	=> from 'ArrayRef[Str]'
	=> via { require GOBO::Node; map { GOBO::Node->new($_) } @$_; }
	=> from 'GOBO::Node'
	=> via { [$_] };

subtype 'GOBO::Types::TwoPlusNodeArray'
	=> as 'GOBO::Types::NodeArray'
	=> where { scalar @$_ == 0 || scalar @$_ >= 2 }
	=> message { "This array requires two or more nodes" } ;

coerce 'GOBO::Types::TwoPlusNodeArray'
	=> from 'ArrayRef[Str]'
	=> via { require GOBO::Node; map { GOBO::Node->new($_) } @$_; }
	=> from 'GOBO::Node'
	=> via { [$_] };

subtype 'GOBO::Indexes::StatementIx'
	=> as 'Object'
	=> where { $_->isa('GOBO::Indexes::StatementRefIndex') || $_->isa('GOBO::Indexes::StatementObjectIndex') };

subtype 'GOBO::Parsers::Parser::ProtoFileHandle'
	=> as class_type('FileHandle');

coerce 'GOBO::Parsers::Parser::ProtoFileHandle'
	=> from 'Str'
		=> via { FileHandle->new( $_ ) }
	=> from 'FileHandle'
		=> via { $_ };

subtype 'GOBO::Synonym::Scope'
	=> as 'Str'
	=> where { $_ eq 'EXACT' || $_ eq 'BROAD' || $_ eq 'NARROW' || $_ eq 'RELATED' }
	=> message { "Synonym scope must be one of EXACT, BROAD, NARROW or RELATED" };

# enum 'GOBO::Synonym::Scope' => qw( EXACT BROAD NARROW RELATED );

subtype 'GOBO::Parsers::ParserMode'
	=> as 'Str'
	=> where { lc($_) eq 'dispatch_hash' || lc($_) eq 'if_else' }
	=> message { "Parser mode can only be dispatch_hash or if_else" };

__PACKAGE__->meta->make_immutable;

1;
