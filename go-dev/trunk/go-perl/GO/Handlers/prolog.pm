# stag-handle.pl -p GO::Parsers::GoOntParser -m <THIS> function/function.ontology

package GO::Handlers::prolog;
use base qw(GO::Handlers::abstract_prolog_writer 
	    Data::Stag::Writer);
use strict;

sub s_obo {
    my $self = shift;
    $self->cmt("-- ********************************************************* --\n");
    $self->cmt("-- Autogenerated Prolog factfiles \n");
    $self->cmt("-- see http://www.blipkit.org for details \n");
    $self->cmt("-- ********************************************************* --\n");
    $self->nl;
}

sub e_header {
    my ($self, $hdr) = @_;
    my $idspace = $hdr->sget_idspace;
    if ($idspace && $idspace =~ /(\S+)\s+(\S+)/) {
        $self->factq('metadata_db:idspace'=>[$1]);
        $self->factq('metadata_db:idspace_uri'=>[$1,$2]);
    }
    if ($hdr->get_ontology) {
        $self->factq('ontology'=>[$hdr->sget_ontology]);
    }
    foreach ($hdr->get_subsetdef) {
        my $id = $_->sget_id;
        my $name = $_->sget_name;
        $self->factq('metadata_db:partition'=>[$id]);
        $self->factq('metadata_db:entity_label', [$id, $name]) if $name;
    }
    foreach ($hdr->get_synonymtypedef) {
        my $id = $_->sget_id;
        my $name = $_->sget_name;
        my $scope = $_->sget_scope || '';
        $self->factq('metadata_db:synonym_type_desc'=>[$id,$scope,$name]);
    }
    foreach ($hdr->get_import) {
        $self->factq('ontol_db:import_directive'=>[$_]);
    }
    foreach ($hdr->subnodes) {
        my $n = $_->name;
        if ($n =~ /^treat/) {
            $n =~ s/\-/_/g;
            my @vals = split(' ',$_->data);
            $self->factq("ontol_db:$n"=>[@vals]);
        }
    }
    $self->nl;
    return;
}


sub e_typedef {
    my ($self, $typedef) = @_;
    $self->cmt("-- Property/Slot --\n");
    my $ont = $typedef->get_namespace;
    my $id = $typedef->get_id || $self->throw($typedef->sxpr);
    my $proptype = 'property';
    my $domain = $typedef->get_domain;
    my $range = $typedef->get_range;
    if ($range && $range =~ /^xsd:/) {
        $proptype = 'slot';
    }
    #$self->fact($proptype, [$id, $typedef->sget_name]);
    $self->factq($proptype, [$id]);
    my $name = $typedef->sget_name;
    $self->factq('metadata_db:entity_label', [$id, $name]) if $name;
    my @is_as = $typedef->get_is_a;
    foreach my $is_a (@is_as) {
        if (ref($is_a)) {
            my $gci_rel = $is_a->sget('@/gci_relation');
            if ($gci_rel) {
                $self->rfactq($_,
                              'gci_subclass', 
                              [$id,$is_a->data, $gci_rel, $is_a->sget('@/gci_filler')]);
            }
            else {
                $self->rfactq($_, 'subclass', [$id, $is_a->data]);
            }
        }
        else {
                $self->rfactq($_, 'subclass', [$id, $is_a]);
        }
    }

    if ($ont) {
	$self->factq('metadata_db:entity_resource', [$id, $ont]);
    }
    if ($typedef->get_is_obsolete) {
	$self->factq( 'metadata_db:entity_obsolete', [$id, 'property']);
    }
    foreach (qw(is_reflexive 
                is_anti_symmetric 
                is_symmetric 
                is_transitive 
                is_functional 
                is_inverse_functional 
                is_proper 
                is_cyclic 
                is_metadata_tag
                is_class_level
                holds_for_all_times
                is_symmetric_on_instance_level 
                is_transitive_on_instance_level)) {
        if ($typedef->sget($_)) {
            $self->rfactq($typedef->sget($_), $_,[$id]);
        }
    }
    foreach (qw(all_some
                all_only
                all_some_all_times)) {
        my $val = $typedef->sget($_);
        if ($val) {
            $self->fact('property_relationship',[$id,$_,$val]);
        }
    }
#    if (!$typedef->sget_is_metadata_tag) {
#        $self->factq( 'all_some',[$id]);
#    }
    $self->export_tags($typedef);
    foreach (qw(domain range)) {
        my $val = $typedef->sget($_);
        if ($val) {
            $self->fact("property_$_",[$id,convert_to_ref($val)]);
        }
    }
    foreach (qw(transitive_over inverse_of class_level_inverse_of inverse_of_on_instance_level transitive_form_of cyclic_form_of cyclic_over 
complement_of directed_simple_path_over directed_path_over reflexive_over expand_expression_to expand_assertion_to)) {
        my $val = $typedef->sget($_);
        if ($val) {
            $self->factq( $_,[$id,$val]);
        }
    }
    my @relchains = $typedef->get_holds_over_chain;
    foreach my $relchain (@relchains) {
        my @rels = $relchain->get_relation;
        if (@rels) {
            $self->factq( holds_over_chain=>[$id,\@rels]);
        }
    }
    my @equivchains = $typedef->get_equivalent_to_chain;
    foreach my $equivchain (@equivchains) {
        my @rels = $equivchain->get_relation;
        if (@rels) {
            $self->factq( equivalent_to_chain=>[$id,\@rels]);
        }
    }
    $self->factq('disjoint_from', [$id, $_]) foreach $typedef->get_disjoint_from;
    $self->factq('disjoint_over', [$id, $_]) foreach $typedef->get_disjoint_over;
    my @xpelts = $typedef->get_intersection_of;
    foreach (@xpelts) {
        $self->factq( 'property_intersection_element',[$id,$_->get_to]);
    }
    @xpelts = $typedef->get_union_of;
    foreach (@xpelts) {
        $self->factq( 'property_union_element',[$id,$_->get_to]);
    }
    foreach (qw(holds_temporally_between holds_atemporally_between)) {
        my $holds = $typedef->sget($_);
        if ($holds) {
            $self->factq( $_,[$id,$holds->get_subject,$holds->get_object]);
        }
    }
    $self->nl;
    return;
}

sub e_term {
    my ($self, $term) = @_;
    my $id = $term->get_id || $self->throw($term->sxpr);
    my $name_h = $self->{name_h};
    my $name = $term->get_name;
    #$name =~ s/_/ /g;   # ontologies lack consistency; force use of spc
    my $ont = $term->get_namespace;

    if ($name) {
        # cache the name; useful for use in comment fields later
	if (!$name_h) {
	    $name_h = {};
	    $self->{name_h} = $name_h;
	}
	$name_h->{$id} = $name;
	#$self->cmt("-- $name --\n");
	$self->factq('metadata_db:entity_label', [$id, $name]) if $name;
    }
    if ($ont) {
	$self->factq('metadata_db:entity_resource', [$id, $ont]);
    }

    if ($term->get_is_obsolete) {
	$self->factq('metadata_db:entity_obsolete', [$id, 'class']);
    }
    else {
        # only declare this to be a class if not obsolete
        $self->factq('class', [$id]);
    }

    #my @is_as = $term->findval_is_a;
    my @is_as = $term->get_is_a;
    $self->rfactq($_,'subclass', [$id, ref($_) ? $_->get('.') : $_], $name_h->{$_}) foreach @is_as;

    my @equivs = $term->get_equivalent_to;
    $self->rfactq($_, 'equivalent_class', [$id, ref($_) ? $_->get('.') : $_]) foreach @equivs;

    my @xp = $term->get_intersection_of;
    if (scalar(@xp) == 1) {
        $self->warn("IGNORING single intersection_of tag for $id/$name");
        @xp=();
    }
    if (@xp) {
        my @genus_l = ();
        @xp = grep {
            # new style genus-differentia:
            #  we say intersection_of: ID rather than
            #         intersection_of: relation ID
            if (!$_->get_type || $_->get_type eq 'is_a') {
                if (@genus_l) {
                    $self->warn(">1 genus for $id/$name");
                }
                push(@genus_l, $_->get_to);
                0;
            }
            else {
                1;
            }
        } @xp;
        $self->factq('genus',[$id, $_])
          foreach @genus_l;

        foreach my $diff (@xp) {
            my $rel = $diff->get_type;
            my $min_card = $diff->sget('@/minCardinality');
            my $max_card = $diff->sget('@/maxCardinality');
            my $card = $diff->sget('@/cardinality');
            if ($card) {
                $min_card = $card;
                $max_card = $card;
            }
            if (defined $min_card &&
                !defined $max_card) {
                $rel = {card=>[$rel,$min_card || 0]};
            }
            elsif (defined $max_card) {
                $rel = {card=>[$rel,$min_card || 0,$max_card]};
            }
            else {
            }
            
            $self->factq('differentium', [$id, $rel, $diff->sget_to])
        }
    }
    # TODO - unify handling of cardinality
    my @rels = $term->get_relationship;
    foreach (@rels) {
        my @args =
          ($id, $_->get_type, convert_to_ref($_->get_to), map { convert_to_ref($_) } $_->get_additional_argument);
        my $gci_rel = $_->sget('@/gci_relation');
        if ($gci_rel) {
            $self->rfactq($_,
                          'gci_restriction', 
                          [@args, $gci_rel, $_->sget('@/gci_filler')], 
                          $name_h->{$_->get_to});
        }
        else {
            $self->rfactq($_,
                          'restriction', 
                          [@args], 
                          $name_h->{$_->get_to});
        }
        foreach my $cardp (qw(cardinality minCardinality maxCardinality)) {
            my $card = $_->sget('@/'.$cardp);
            if ($card) {
                my @cargs = @args;
                splice(@cargs,2,0,'');
                $cargs[2] = $card;
                my $pred = $cardp;
                if ($cardp =~ /min/) {
                    $pred = 'min_cardinality';
                }
                elsif ($cardp =~ /max/) {
                    $pred = 'max_cardinality';
                }
                $self->rfact($_,
                             $pred.'_restriction',
                             [@cargs], 
                             $name_h->{$_->get_to});
            }
        }
    }

    # subject to change:
    if ($term->get_all_direct_subclasses_disjoint) {
        $self->factq('all_direct_subclasses_disjoint', [$id]);
    }
    $self->factq('disjoint_from', [$id, $_]) foreach $term->get_disjoint_from;
    $self->factq('class_union_element', [$id, $_->sget_to]) foreach $term->get_union_of;

    foreach (qw(is_anonymous)) {
        if ($term->sget($_)) {
            $self->factq($_,[$id]);
        }
    }
    $self->export_tags($term);
    $self->nl;

    # metadata

    return;
}

sub _flatten_dbxref {
    my $x = shift;
    my $db = $x->sget_dbname;
    my $acc = $x->sget_acc;
    if ($db eq "URL" && $acc =~ /http/) {  # TODO - check for all URI forms (LSID,...)
        return $acc;
    }
    elsif ($acc eq 'NULL') {
        return $db;
    }
    else {
        return "$db:$acc";
    }
}

# stuff common to terms and typedefs and insts
sub export_tags {
    my ($self, $entity) = @_;
    my $def = $entity->get_def;
    my $id = $entity->sget_id;
    if ($def) {
        $self->factq('def',[$id, $def->sget_defstr]);
        foreach ($def->get_dbxref) {
            $self->factq('def_xref',[$id, _flatten_dbxref($_)]);
        }
    }
    foreach ($entity->get_alt_id) {
        $self->factq('metadata_db:entity_alternate_identifier',[$id, $_]);
    }
    foreach ($entity->get_consider) {
        $self->factq('metadata_db:entity_consider',[$id, $_]);
    }
    foreach ($entity->get_replaced_by) {
        $self->factq('metadata_db:entity_replaced_by',[$id, $_]);
    }
    foreach ($entity->get_comment) {
        $self->factq('metadata_db:entity_comment',[$id, $_]);
    }
    foreach ($entity->get_example) {
        $self->factq('metadata_db:entity_example',[$id, $_]);
    }
    foreach ($entity->get_subset) {
        $self->factq('metadata_db:entity_partition',[$id, $_]);
    }
    foreach ($entity->get_synonym) {
        my $syn = $_->sget_synonym_text;
        my $scope = $_->sget('@/scope');
        my $type = $_->sget('@/synonym_type');
        $self->factq('metadata_db:entity_synonym',[$id,$syn]);
        $self->factq('metadata_db:entity_synonym_scope',[$id,$syn,$scope]) if $scope;
        $self->factq('metadata_db:entity_synonym_type',[$id,$syn,$type]) if $type;
        $self->factq('metadata_db:entity_synonym_xref',[$id,$syn,_flatten_dbxref($_)]) foreach $_->get_dbxref;
    }
    foreach ($entity->get_xref_analog) {
        my $xref = _flatten_dbxref($_);
        #$self->factq('class_xref',[$id, sprintf("%s:%s",$_->sget_dbname,$_->sget_acc)]);
        $self->factq('metadata_db:entity_xref',[$id, $xref]);
        my $n = $_->sget('name');
        if ($n && !$self->{_written_name_for}->{$xref}) {
            $self->factq('metadata_db:entity_label',[$xref, $n]);
            $self->{_written_name_for}->{$id} = 1;
        }
    }
    foreach ($entity->get_formula) {
        $self->factq('logicalformula',[$id,$_->sget_formula_text,$_->sget('@/format')]);
    }
    #foreach ($entity->get_subset) {
    #    $self->fact('belongs_subset',$_->findval_scope || '',$_->sget_synonym_text);
    #}
    foreach (qw(lexical_category)) {
        my $val = $entity->sget($_);
        if ($val) {
            $self->factq($_,[$id,$val]);
        }
    }
    # property tag-val pairs
    foreach my $pv ($entity->get_property_value) {
        my $dt = $pv->sget_datatype;
        my @args = ($id,$pv->sget_type);
        my $link_id = $pv->get('@/id');
        if ($dt) {
            $self->factq('inst_sv',[@args,$pv->sget_value,$dt]);
        }
        else {
            $self->factq('inst_rel',[@args,$pv->sget_to]);
            if ($link_id) {
                $self->factq('reification',
                             [$link_id,{inst_rel=>[@args,$pv->sget_to]}]);
            }
        }
    }
    return;
}

sub nextid_by_prod {
    my $self = shift;
    $self->{_nextid_by_prod} = shift if @_;
    $self->{_nextid_by_prod} = {} 
      unless $self->{_nextid_by_prod};

    return $self->{_nextid_by_prod};
}

sub e_prod {
    my ($self, $gp) = @_;   
    my $proddb = $self->up(-1)->sget_proddb;
    my $prodacc = $gp->sget_prodacc;

    # all gene products go in seqfeature_db module
    my $id = "$proddb:$prodacc";
    $self->factq('seqfeature_db:feature',[$id]);
    $self->factq('seqfeature_db:feature_type',
                [$id,$gp->sget_prodtype]);
    $self->factq('metadata_db:entity_label',
                [$id,$gp->sget_prodsymbol]);
    $self->factq('metadata_db:entity_source',
                [$id,$gp->sget_proddb]);
    # duplicate?
    $self->factq('seqfeature_db:feature_organism',
                [$id,'NCBITaxon:'.$gp->sget("prodtaxa")]);
    #$self->factq('taxon_db:entity_taxon',
    #            [$id,'NCBITaxon:'.$gp->sget("prodtaxa")]);
    $self->factq('seqfeature_db:featureprop',
                [$id,'description',$gp->sget_prodname]);
    $self->factq('metadata_db:entity_synonym',
                [$id,$_])
      foreach $gp->get_prodsyn;

    # associations between gp and term'
    my @assocs = $gp->get_assoc;
    my $idh = $self->nextid_by_prod;
    foreach my $assoc (@assocs) {
        my $n = $idh->{$id}++;
	my $term_acc = $assoc->sget_termacc;
        my $is_not = $assoc->sget_is_not ? 1 : 0;
        my $aid = "$proddb:association-$id-$term_acc-$is_not";
        $self->fact('curation',[$aid]);
        my $pred = 'curation_statement';
        if ($is_not) {
            $pred = 'negative_'.$pred;
        }
        $self->fact($pred,
                    [$aid,$id,'has_role',$term_acc]);

        my $aspect = $assoc->sget_aspect;
        if ($aspect) {
            if (!$self->{_written_aspect}) {
                $self->{_written_aspect} = {};
            }
            if (!$self->{_written_aspect}->{$term_acc}) {
                $self->{_written_aspect}->{$term_acc} = 1;
                my $ont = '';
                if ($aspect eq 'F') {
                    $ont = 'molecular_function';
                }
                elsif ($aspect eq 'P') {
                    $ont = 'biological_process';
                }
                elsif ($aspect eq 'C') {
                    $ont = 'cellular_component';
                }
                if ($ont) {
                    $self->fact('metadata_db:entity_resource',[$term_acc,$ont]);
                }
            }
            
        }

        my @evs = $assoc->get_evidence;
        my $ne=0;
        foreach my $ev (@evs) {
            my $eid = "$aid-$ne";
            $ne++;
            # eg PMID
            $self->factq('metadata_db:entity_source',
                        [$aid,$ev->sget_ref]);
            $self->factq('evidence',[$eid]);
            $self->factq('curation_evidence',
                         [$aid,$eid]);
            $self->factq('evidence_type',
                        [$eid,$ev->sget_evcode]);
            $self->factq('evidence_with',
                         [$eid,$_])
              foreach $ev->get_with;
        }
        # note: we treat the source DB as the publisher (the source is the provenance)
        $self->factq('metadata_db:entity_publisher',
                     [$aid,$_])
          foreach $assoc->get_source_db;
        foreach ($assoc->get('properties/link')) {
            $self->factq('curation_subject_property_value',
                         [$aid,$term_acc,$_->get_type,$_->get_to])
        }
        foreach ($assoc->get_assocdate) {
            if (length($_) eq 8) {
                $_ = sprintf("%s-%s-%s",
                             substr($_,0,4),
                             substr($_,4,2),
                             substr($_,6,2));
            }
            $self->factq('metadata_db:entity_created',
                         [$aid,$_])
        }
        my @pvs = $assoc->get_property_value;
        foreach my $pv (@pvs) {
            $self->factq('curation_qualifier',
                        [$aid,$pv->sget_type,$pv->sget_to]);
        }
        my @quals = $assoc->get_qualifier;
        foreach my $q (@quals) {
	    next if $q eq 'not';
            $self->factq('curation_qualifier',
                        [$aid,$q,"true"]);
        }
    }
}

sub e_annotation {
    my ($self, $annotation) = @_;   

    my $idh = $self->nextid_by_prod;

    my $proddb = $annotation->sget_namespace || '_';
    my $subj = $annotation->sget_subject;
    my $rel = $annotation->sget_relation;
    my $obj = $annotation->sget_object;
    my $is_not = $annotation->sget_is_not ? 1 : 0;
    my $aid = "$proddb:$subj-$obj-$is_not";
    $self->fact('curation',[$aid]);
    my $pred = 'curation_statement';
    if ($is_not) {
        $pred = 'negative_'.$pred;
    }
    $self->fact($pred,
                    [$aid,$subj,$rel,$obj]);
#        my @evs = $assoc->get_evidence;
#        my $ne=0;
#        foreach my $ev (@evs) {
#            my $eid = "$aid-$ne";
#            $ne++;
#            # eg PMID
#            $self->factq('metadata_db:entity_source',
#                        [$aid,$ev->sget_ref]);
#            $self->factq('evidence',[$eid]);
#            $self->factq('curation_evidence',
#                         [$aid,$eid]);
#            $self->factq('evidence_type',
#                        [$eid,$ev->sget_evcode]);
#            $self->factq('evidence_with',
#                         [$eid,$_])
#              foreach $ev->get_with;
#        }
        # note: we treat the source DB as the publisher (the source is the provenance)
    $self->factq('metadata_db:entity_publisher',
                 [$aid,$_])
      foreach $annotation->get_provenance;
    $self->factq('metadata_db:entity_creator',
                 [$aid,$_])
      foreach $annotation->get_creator;
    $self->factq('metadata_db:entity_resource',
                 [$aid,$_])
      foreach $annotation->get_namespace;
    my @pvs = $annotation->get_property_value;
    foreach my $pv (@pvs) {
        $self->factq('curation_qualifier',
                     [$aid,$pv->sget_type,$pv->sget_to]);
    }
}


sub e_instance {
    my ($self, $inst) = @_; 
    my $id = $inst->get_id;
    $self->factq('inst', [$id]);
    $self->factq('inst_of',[$id,$_])
        foreach $inst->get_instance_of;
    my $name = $inst->sget_name;
    $self->factq('metadata_db:entity_label', [$id, $name]) if $name;
    $self->export_tags($inst);
    foreach my $pv ($inst->get_relationship) {
        my @args = ($id,$pv->sget_type);
        my $link_id = $pv->get('@/id');
        $self->factq('inst_rel',[@args,$pv->sget_to]);
        if ($link_id) {
            $self->factq('reification',
                         [$link_id,{inst_rel=>[@args,$pv->sget_to]}]);
        }
    }
    return;
}

# todo
sub convert_to_ref {
    my $to = shift;
    if (ref($to)) { # may be enum
        return '';
    }
    else {
        return $to;
    }
}

sub rfactq {
    my $self = shift;
    my $elt = shift;
    my $f = shift;
    my $args = shift;
    my $cmt = shift;
    $self->factq($f,$args,$cmt);
    if (ref($elt)) {
	my $reif_id = $elt->get('@/id');
	if ($reif_id) {
	    $self->factq('reification',
			 [$reif_id,{$f=>$args}]);
	}
    }
    return;
}

sub rfact {
    my $self = shift;
    my $elt = shift;
    my $f = shift;
    my $args = shift;
    my $cmt = shift;
    $self->fact($f,$args,$cmt);
    if (ref($elt)) {
	my $reif_id = $elt->get('@/id');
	if ($reif_id) {
	    $self->fact('reification',
                        [$reif_id,{$f=>$args}]);
	}
    }
    return;
}

1;
