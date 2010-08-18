# $Id: AppHandleSqlImpl.pm,v 1.97 2010/02/25 01:31:07 sjcarbon Exp $
#
# This GO module is maintained by Chris Mungall <cjm@fruitfly.org>
#
# see also - http://www.geneontology.org
#          - http://www.godatabase.org/dev
#
# You may distribute this module under the same terms as perl itself


package GO::AppHandles::AppHandleSqlImpl;

=head1 NAME

GO::AppHandles::AppHandleSqlImpl

=head1 SYNOPSIS

you should never use this class directly. Use GO::AppHandle
(All the public methods calls are documented there)

=head1 DESCRIPTION

implementation of AppHandle for a GO relational database

=head1 FEEDBACK

Email cjm@fruitfly.berkeley.edu

=cut

use strict;
use Carp;
use FileHandle;
use Carp;
use Data::Dumper;
use DBI;
use GO::Utils qw(rearrange pset2hash dd);
use GO::SqlWrapper qw(:all);
use GO::Model::Xref;
use GO::Model::Term;
use GO::Model::Association;
use GO::Model::GeneProduct;
use GO::Model::Relationship;
use GO::Model::Graph;
use GO::Reasoner;
use Exporter;
use base qw(GO::AppHandles::AppHandleAbstractSqlImpl);
use vars qw($AUTOLOAD $PATH $GPPROPERTY);

$PATH="graph_path";
$GPPROPERTY="gene_product_property";

our $GO_HAS_COUNT_BY_SPECIES = defined($ENV{GO_HAS_COUNT_BY_SPECIES}) ? $ENV{GO_HAS_COUNT_BY_SPECIES} : 1;

sub get_autoincrement {

    my $self = shift;
    my $table = shift;

    return get_autoincrement_val($self->dbh,$table);

}

sub refresh {
    my $self = shift;
    my $dbh = $self->dbh;

    # thanks to James Smith at Sanger for the optimisation tip..
    my $hl = select_hashlist($dbh,
			     "term2term",
                             [],
			     "distinct term2term.relationship_type_id as type_id"
			    );
    if (@$hl) {
	my $where = join ',', map $_->{'type_id'}, @$hl;
	$hl = select_hashlist( $dbh, "term", "id in ($where)", "id, name" );
        $self->{rtype_by_id} = {};
	foreach my $h (@$hl) {
	    $self->{rtype_by_id}->{$h->{id}} = $h->{name};
	}
    }
    else {
	# empty db
    }
}



# private accessor: boolean indicating if DB has transactions
# (Default: no; we assume mysql as default)
sub is_transactional {
    my $self = shift;
    $self->{_is_transactional} = shift if @_;
    return $self->{_is_transactional} || 
      ($self->dbms && (lc($self->dbms) ne "mysql"));
}


sub timestamp {
    my $self = shift;
    my $dbh = $self->dbh;

}


##
sub generate_goid {
  my $self = shift;
  my $dbh = $self->dbh;
  my ($user) =
    rearrange([qw(term user)], @_);

  my $acch = select_hash($dbh,
			 "term",
			 [],
			 "max(acc) AS m");
  return (int($acch->{'m'} || "0") +1);
}

sub add_term {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($termh, $user) =
      rearrange([qw(term user)], @_);
    if (!ref($termh)) {
	$termh = {name=>$termh};
    }
    my $term = $termh;
    if (ref($term) eq 'HASH') {
        $term = 
          $self->create_term_obj($termh);
    }
    my $h = select_hash($dbh, "term", "acc=".sql_quote($term->acc));
    my $id;
    my $update_h =
    {name=>$term->name,
     acc=>$term->acc,
     term_type=>$term->type,
     is_obsolete=>$term->is_obsolete,
     is_root=>$term->is_root};
    if ($h) {
	# we already have the term
	$id = $h->{id};
	update_h($dbh,
		 "term",
		 $update_h,
		 "id=$id");
    }
    else {
	$id =
	    insert_h($dbh,
		     "term",
		     $update_h);
    }
    $id or confess("id $id is false");
    $term->id($id);
    $self->update_term ($term, $user, "create");

    return $term;
}

sub update_term {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($term, $user, $update_type) =
	rearrange([qw(term user create)], @_);

    my $mod_flag = $update_type =~ /create/;
    
    if ($term->definition) {
	$mod_flag |= $self->add_definition(
				    {definition=>$term->definition,
				     term_id=>$term->id},
				     undef,
				    $user);
    }
    map {
	$mod_flag |= $self->add_synonym (
				  {term_id=>$term->id},
				  $_,
				  $user);

    } @{$term->synonym_list || []};
    return $term;
}

sub check_term {
    my $self = shift;
    my $dbh = $self->dbh;
    my $termh = shift;
    my $h = 
      select_hash($dbh,
		  "term",
		  $termh,
		  "count(*) AS c");
    if ($h->{c} < 1) {
	return 0;
    }
    elsif ($h->{c} > 1) {
	return 0;
    }
    else {
	return 1;
    }
}

sub add_synonym {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($termh, $synonym, $user) =
      rearrange([qw(term synonym user)], @_);

    if ($synonym) {
	my $term_id = $termh->{term_id};
	if (!$term_id) {
	    my $term = $self->get_term($termh);
	    if (!$term) {
		warn("Can't add synonym for a term that doesn't exist yet!\n");
		return 0;
	    }
	    $term_id = $term->id;
	}
	my @constr_arr;
	push (@constr_arr, "term_id = $term_id");
	push (@constr_arr, "term_synonym = ".sql_quote($synonym));
	my $h =
	    select_hash($dbh,
			"term_synonym",
			\@constr_arr);
	if (!$h) {
	    insert_h($dbh,
		     "term_synonym",
		     {term_id=>$term_id,
		      term_synonym=>$synonym});
	    return 1;
	}
    }
    return 0;
}

# adds dbxref if not present;
# fills in id if it is present
sub add_dbxref {
    my $self = shift;
    my $xref = shift;
    my $update = shift;
    my $dbh = $self->dbh;

    if (!ref($xref)) {
        if ($xref=~/(\w+):(.*)/) {
            $xref = $self->create_xref_obj({xref_dbname=>$1,
                                            xref_key=>$2});
        }
        else {
            $self->throw("Cannot parse xref: $xref");
        }
    }

    my $h = 
      select_hash($dbh,
                  "dbxref",
                  {xref_key=>$xref->xref_key,
                   xref_dbname=>$xref->xref_dbname});
    my $updateh =
      {xref_key=>$xref->xref_key || "",
       xref_keytype=>$xref->xref_keytype || "acc",
       xref_dbname=>$xref->xref_dbname,
       xref_desc=>$xref->xref_desc,
      }; 
    my $id;
    if ($h) {
        $id = $h->{id};
        if ($update) {
            update_h($dbh,
                     "dbxref",
                     $updateh,
                     "id=$id");
        }
    }
    else {
        $id =
          insert_h($dbh,
                   "dbxref",
                   $updateh,
                  );
    }
    $xref->id($id);
    $xref;
}

sub add_term_dbxref {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($term, $xrefh, $user) =
      rearrange([qw(term xref user)], @_);
    if (!$term->id) {
        confess("Can't add dbxref to a term that doesn't exist yet!");
    }
    my $xref = GO::Model::Xref->new($xrefh);
    $xref->xref_key || confess("must specify key for xref");

    $self->add_dbxref($xref, 1);

    my @constr_arr = ();
    push (@constr_arr, "term_id = ".$term->id);
    push (@constr_arr, "dbxref_id = ".$xref->id);
    my $t2x = select_hash($dbh,
			  "term_dbxref",
			  \@constr_arr);
    if (!$t2x) {
	insert_h($dbh,
		 "term_dbxref",
		 {term_id=>$term->id,
		  dbxref_id=>$xref->id});
    }
    return $xref;
}

#

sub add_relationship_type {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($type, $desc, $user) =
      rearrange([qw(type desc user)], @_);
    my $hl =
      select_hashlist($dbh,
                      "relationship_type",
                      "type_name=".sql_quote($type));
    my $id;
    if (@$hl) {
        if (@$hl>1) {
            confess("Assertion error - rel type");
        }
        $id = $hl->[0]->{id};
    }
    else {
        $desc = "" unless $desc;
        $id =
          insert_h($dbh,
                   "relationship_type",
                   {type_name=>$type,
                    type_desc=>$desc});
        $self->{rtype_by_id}->{$id} = $type;
    }
    return $id;
}

# synonym
sub add_relation {
    my $self = shift;
    $self->add_relationship(@_);
}

sub add_relationship {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($relh, $user) =
      rearrange([qw(relationship user)], @_);
    my $rel =
      GO::Model::Relationship->new($relh);

    # this is the parent
    my $t1 =
	select_hash($dbh,
		    "term",
		    "acc=$rel->{acc1}");
    # this is the child
    my $t2 =
	select_hash($dbh,
		    "term",
		    "acc=$rel->{acc2}");

    my $typeid =
      $self->add_relationship_type($rel->type);
    eval {
        insert_h($dbh,
                 "term2term",
                 {term1_id=>$t1->{id},
                  term2_id=>$t2->{id},
                  #	      is_inheritance=>($rel->is_inheritance ? 1:0),
                  #              relationship_type=>uc($rel->type),
                  relationship_type_id=>$typeid,
                  is_obsolete=>$rel->is_obsolete,
                 });
    };
    if ($@) {
        warn($@);
    }
    return $rel;
}

sub add_association {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($assoch, $user) =
      rearrange([qw(association user)], @_);

    my $assoc = $self->create_association_obj($assoch);
    $assoc->gene_product($assoch->{product});
    $assoc->role_group($assoch->{role_group});

    my $assoc_insert_h = 
      {gene_product_id=>$assoch->{product}->id,
       term_id=>$assoch->{term}->id};
#    $assoc_insert_h->{is_not} = $assoc->is_not if (defined($assoc->is_not));
    $assoc_insert_h->{is_not} = $assoc->is_not ? 1 : 0;
    $assoc_insert_h->{role_group} = $assoc->role_group if (defined($assoc->role_group));
    sql_delete($dbh,
	       "association",
	       ["gene_product_id=".$assoch->{product}->id,
		"term_id=".$assoch->{term}->id]);
    my $id =
      insert_h($dbh,
	       "association",
	       $assoc_insert_h);

    $assoc->id($id);
    $assoc;
}

sub add_evidence {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($evh, $user) =
      rearrange([qw(evidence user)], @_);
    my $evidence =
      GO::Model::Evidence->new({code=>$evh->{code},
				seq_acc=>$evh->{seq_acc},
				reference=>$evh->{reference},
			       });
    my $seq_acc = $evidence->seq_acc if ($evidence->seq_acc);
    my $xref_id;
    if ($evidence->xref) {
	my $h =
	  select_hash($dbh,
		      "dbxref",
		      ["xref_key=".
		       sql_quote($evidence->xref->xref_key || ""),
		       "xref_dbname=".
		       sql_quote($evidence->xref->xref_dbname)]);
	$xref_id = $h->{id};
	if (!$xref_id) {
	    $xref_id =
	      insert_h($dbh,
		       "dbxref",
		       {xref_key=>$evidence->xref->xref_key || "",
			xref_dbname=>$evidence->xref->xref_dbname});
	}
	$evidence->xref->id($xref_id);
    }
    my $id =
      insert_h($dbh, 
	       "evidence", 
	       {code=>$evidence->code,
		seq_acc=>($seq_acc),
		association_id=>$evh->{assoc}->id,
		dbxref_id=>$xref_id});
    $evidence->id($id);
    $evidence;
}

sub add_definition {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($defh, $xrefh, $user) =
      rearrange([qw(definition xref user)], @_);
    my $term = $self->create_term_obj();
    if (!$defh->{term_id}) {
	$term->public_acc($defh->{goid});
	$term = 
	  $self->get_term(
			  {acc=>$term->acc});
	if (!$term) {
	    confess("There is no term with go_id/acc $defh->{goid}\n");
	}
    }
    else {
	$term->id($defh->{term_id});
    }

    sql_delete($dbh,
	       "term_definition",
	       "term_id = ".$term->id);
    insert_h($dbh,
	     "term_definition",
	     {term_id=>$term->id,
	      term_definition=>$defh->{definition}
	     });

    if ($xrefh) {
	$self->add_term_dbxref($term, $xrefh, $user);
    }

    return 1;
}

sub add_product {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($producth, $user) =
      rearrange([qw(product user)], @_);

    my $product;
    $product = $self->get_product($producth);
    if ($product) {
	my $hl =
	  select_hashlist($dbh,
			  "association",
			  "gene_product_id=".$product->id,
			  ["id"]);
	map {
	    sql_delete($dbh,
		       "evidence",
		       "association_id=$_->{id}");
	} @$hl;
	sql_delete($dbh,
		   "gene_product",
		   "id=".$product->id);
	sql_delete($dbh,
		   "gene_product_synonym",
		   "gene_product_id=".$product->id);
	sql_delete($dbh,
		   "association",
		   "gene_product_id=".$product->id);
    }

    $product = $self->create_gene_product_obj($producth);
#    $product->speciesdb || confess("product $product ".$product->acc." has no speciesdb");
    my $xref_h = 
      select_hash($dbh,
		  "dbxref",
		  {"xref_key"=>$product->acc,
		   "xref_dbname"=>$product->speciesdb});
    
    my $xref_id;
    if ($xref_h) {
	$xref_id = $xref_h->{id};
    }
    else {
	$xref_id =
	  insert_h($dbh,
		   "dbxref",
		   {xref_key=>$product->acc,
		    xref_keytype=>"acc",
		    xref_dbname=>$product->speciesdb}
		  );
    }
    my $gh = {symbol=>$product->symbol,
	      dbxref_id=>$xref_id};
    $gh->{full_name} = $product->full_name if defined($product->full_name);
    
    my $id = insert_h($dbh, "gene_product", $gh);
    $product->id($id);

    my @syn_list = @{$product->synonym_list || []};
    foreach my $syn (@syn_list) {
	if ($syn) {
	    my $h =
	      select_hash($dbh,
			  "gene_product_synonym",
			  {product_synonym=>$syn,
			   gene_product_id=>$id});
	    if (!$h) {
		insert_h($dbh,
			 "gene_product_synonym",
			 {product_synonym=>$syn,
			  gene_product_id=>$id
			 });
	    }
	}
    }
    return $product;
}

sub remove_associations_by_speciesdb {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($speciesdb, $user, $max) =
      rearrange([qw(speciesdb user max)], @_);
    my $hl =
      select_hashlist($dbh,
                      ["association AS a",
                       "gene_product AS p",
                       "dbxref AS x"],
                      ["x.id = p.dbxref_id",
                       "a.gene_product_id = p.id",
                       "x.xref_dbname = ".sql_quote($speciesdb)],
                      "a.id AS id");
    my @aids = map {$_->{id}} @$hl;
    if ($max) {
	@aids = splice(@aids, 0, $max);
    }
    sql_delete($dbh,
               "evidence",
               "association_id in (".join(", ", @aids).")") if @aids;
    $hl =
      select_hashlist($dbh,
                      ["gene_product AS p",
                       "dbxref AS x"],
                      ["x.id = p.dbxref_id",
                       "x.xref_dbname = ".sql_quote($speciesdb)],
                      "p.id AS id");
    my @pids = map {$_->{id}} @$hl;
    $hl =
      select_hashlist($dbh,
                      "gene_product_seq",
                      "gene_product_id in (".join(", ", @pids).")",
                      "seq_id");
    my @seqids = map {$_->{seq_id}} @$hl;
    sql_delete($dbh,
               "gene_product",
               "id in (".join(", ", @pids).")");
    sql_delete($dbh,
               "gene_product_synonym",
               "gene_product_id in (".join(", ", @pids).")");
    sql_delete($dbh,
               "gene_product_seq",
               "gene_product_id in (".join(", ", @pids).")");
    sql_delete($dbh,
               "seq",
               "id in (".join(", ", @seqids).")") if @seqids;
    sql_delete($dbh,
               "gene_product_count");
}

sub remove_associations {
    my $self = shift;
    while ($self->remove_associations_partial(@_)) {
	print STDERR "removing assocs....\n";
    }
}

sub remove_associations_partial {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($evcodes, $speciesdb) =
      rearrange([qw(evcode speciesdb)], @_);
    my $LIMIT = 50000;
    my @q = ();
    my @t = ();
    if ($evcodes) {
# often $evcodes points to an empty ARRAY
# added extra check, this removed unneeded join on evidence table.
# jmc Oct2006
	if (defined(@$evcodes)) {
	    push(@q,
		 sqlin("e.code", $evcodes, 1),
		 "e.association_id = a.id");
	    push(@t, "evidence AS e");
	}
    }
    if ($speciesdb) {
	push(@q,
	     sqlin("x.xref_dbname", $speciesdb, 1),
	     "x.id = p.dbxref_id",
	     "a.gene_product_id = p.id",
	    );
	push(@t, 
	     "gene_product AS p",
	     "dbxref AS x");
    }
    my $asids =
      select_vallist($dbh,
		     -tables=>["association AS a",
		      @t],
		     -where=>\@q,
		     -columns=>"a.id",
		     -limit=>$LIMIT);
    #printf STDERR "GOT:%d\n", scalar(@$asids);
#    if ($max) {
#	@aids = splice(@aids, 0, $max);
#    }
    sql_delete($dbh,
               "evidence",
	       sqlin("association_id", $asids));
    my $pids =
      select_vallist($dbh,
                      ["association AS a",
		       "gene_product AS p",
		      ],
                      ["a.gene_product_id = p.id",
		       sqlin("a.id", $asids)],
                      "p.id");
    my $sids =
      select_vallist($dbh,
		     "gene_product_seq",
		     sqlin("gene_product_id", $pids),
		     "seq_id");
    sql_delete($dbh,
               "gene_product",
               sqlin("id", $pids));
    sql_delete($dbh,
               "association",
               sqlin("id", $asids));
    sql_delete($dbh,
               "gene_product_synonym",
               sqlin("gene_product_id", $pids));
    sql_delete($dbh,
               "gene_product_seq",
               sqlin("gene_product_id", $pids));
    sql_delete($dbh,
               "seq_dbxref",
               sqlin("seq_id", $sids));
    sql_delete($dbh,
               "seq_property",
               sqlin("seq_id", $sids));
    sql_delete($dbh,
               "seq",
               sqlin("id", $sids));

    ## COMMENTARY: Seems to be preventing counts when IEAs are
    ## around. No longer necessary.
    #     unless( ! $speciesdb &&
    # 	    $evcodes &&
    # 	    scalar(@$evcodes) == 1 &&
    # 	    $evcodes->[0] eq 'IEA' ){
    #       sql_delete($dbh, "gene_product_count");
    #     }

    return scalar(@$asids);
}

sub remove_iea {
    my $self = shift;
    my $dbh = $self->dbh;
    my $LIMIT = $ENV{REMOVE_IEA_CHUNK_SIZE} || 100000;

    sql_delete($dbh,
	       "evidence",
	       "code='IEA'");

    my %valid_asidh =
      map {$_=>1}
	@{select_vallist($dbh,
			 "evidence",
			 undef,
			 "distinct association_id")};
    my @togo =
      grep {
	  !$valid_asidh{$_}
      }
	@{select_vallist($dbh,
			 "association",
			 undef,
			 "id")};

    while (@togo) {
	my @ids = splice(@togo, 0, $LIMIT);
	sql_delete($dbh,
		   "association",
		   sqlin("id", \@ids));
    }
    
    my %valid_gpidh =
      map {$_=>1}
	@{select_vallist($dbh,
			 "association",
			 undef,
			 "distinct gene_product_id")};
    my @gptogo =
      grep {
	  !$valid_gpidh{$_}
      }
	@{select_vallist($dbh,
			 "gene_product",
			 undef,
			 "id")};
    while (@gptogo) {
	my $pids = [splice(@gptogo, 0, $LIMIT)];
	my $sids =
	  select_vallist($dbh,
			 "gene_product_seq",
			 sqlin("gene_product_id", $pids),
			 "seq_id");
	sql_delete($dbh,
		   "gene_product",
		   sqlin("id", $pids));
	sql_delete($dbh,
		   "gene_product_synonym",
		   sqlin("gene_product_id", $pids));
	sql_delete($dbh,
		   "gene_product_seq",
		   sqlin("gene_product_id", $pids));
#	sql_delete($dbh,
#		   "seq_dbxref",
#		   sqlin("seq_id", $sids));
#	sql_delete($dbh,
#		   "seq_property",
#		   sqlin("seq_id", $sids));
#	sql_delete($dbh,
#		   "seq",
#		   sqlin("id", $sids));
    }
    return;
}

sub set_product_seq {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($producth, $seqh, $user) =
      rearrange([qw(product seq user)], @_);
#    my $product = 
#	(ref($producth) eq "HASH") ? $self->create_gene_product_obj($producth) : $producth;
    my $seq = 
	(ref($seqh) eq "HASH") ? $self->create_seq_obj($seqh) : $seqh;
    my $product = $producth;
    if (!$product->id) {
	$self->get_product($producth);
    }
    if (!$seq->id) {
	$seq = $self->add_seq($seq);
    }
    my $gps_h = select_hash($dbh, 
                            "gene_product_seq", 
			    {gene_product_id=>$product->id,
			     seq_id=>$seq->id});                                                                      
    if (!$gps_h) { 
      insert_h($dbh,
	     "gene_product_seq",
	     {gene_product_id=>$product->id,
	      seq_id=>$seq->id});
    }   
}

sub add_seq {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($seqh, $user) =
      rearrange([qw(seq user)], @_);
    my $seq = $self->get_seq({display_id=>$seqh->display_id});
    my $is_insert = 0;
    if (!$seq) {
	$seq = 
	    (ref($seqh) eq "HASH") ? $self->create_seq_obj($seqh) : $seqh;
	$is_insert = 1;
    }
    else {

    }
    my $i_h =
    {
	display_id=>$seq->display_id,
	seq=>$seq->seq,
	md5checksum=>$seq->md5checksum,
	description=>$seq->desc,
	seq_len=>$seq->length
	};
    if ($is_insert) {
	my $id = 
	    insert_h($dbh, "seq", $i_h);
	$seq->id($id);
    }
    else {
	update_h($dbh,
		 "seq",
		 $i_h,
		 "id=".$seq->id);
    }
    sql_delete($dbh,
               "seq_dbxref",
               "seq_id=".$seq->id);
    my %done = ();
    foreach my $xref (@{$seq->xref_list || []}) {
        # mysql table is case insensitive;
        # check for dupes in case insensitive way
        next if $done{lc($xref->as_str)};
        $self->add_dbxref($xref);
        eval {
            insert_h($dbh,
                     "seq_dbxref",
                     {seq_id=>$seq->id,
                      dbxref_id=>$xref->id});
        };
        if ($@) {
            warn("problem inserting xrefs for ".$seq->id."; err: ".$@);
        }
        $done{lc($xref->as_str)} = 1;
    }
    $seq;
}

sub store_species {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($taxid, $binom, $common) =
      rearrange([qw(taxid binom common)], @_);
    my ($genus, @sp) = split(' ', $binom);
    my $species = join(' ', @sp);
    my $id = select_val($dbh, "species", "ncbi_taxa_id=$taxid");
    if ($id) {
	return
	update_h($dbh,
		   "species",
		   {
		    genus=>$genus,
		    species=>$species,
		    common_name=>$common,
		   },
		 "ncbi_taxa_id=$taxid");
    }
    else {
	return
	  insert_h($dbh,
		   "species",
		   {ncbi_taxa_id=>$taxid,
		    genus=>$genus,
		    species=>$species,
		    common_name=>$common,
		   });
    }
}

#
sub _delete_term {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($constr, $user) =
      rearrange([qw(constraints user)], @_);
    if (!$user->{authority} ||
	$user->{authority} < 10) {
	confess("Don't have authority! (".$user->{authority}.")");
    }
    my $term = 
      $self->get_term($constr);
    
    sql_delete($dbh,
	       "term_definition",
	       "term_id=".$term->{id});
    sql_delete($dbh,
	       "term_synonym",
	       "term_id=".$term->{id});
    sql_delete($dbh,
	       "term2term",
	       "term1_id=".$term->{id});
    sql_delete($dbh,
	       "term2term",
	       "term2_id=".$term->{id});
    sql_delete($dbh,
	       "term",
	       "id=".$term->{id});
}

# experimental
sub add_product_to_subset {
    my $self = shift;
    my $gp = shift;
    my $subset_name = shift;
    if (!ref($gp)) {
        $gp = $self->get_product({acc=>$gp});
    }
    if (!$gp) {
        warn("no such gp");
        return 0;
    }
    my $subset = $subset_name;
    if (!ref($subset)) {
        $subset = $self->get_term({acc=>$subset_name});
    }
    if (!$subset) {
        $subset = $self->create_term_obj({acc=>$subset_name,
                                          name=>$subset_name});
        $subset->type('subset');
        $self->add_term($subset);
    }
    my $gp_id = $gp->id;
    my $subset_id = $subset->id;
    my $dbh = $self->dbh;
    sql_delete($dbh,
               "gene_product_subset",
               "gene_product_id=$gp_id AND subset_id=$subset_id");
    insert_h($dbh,
             "gene_product_subset",
             {gene_product_id=>$gp_id,
              subset_id=>$subset_id});
    return 1;
}

# experimental
sub add_homolset {
    my $self = shift;
    my $acc = shift;
    my $symbol = shift;
    my $product_accs = shift;
    my $dbh = $self->dbh;

    my $xref = $self->add_dbxref($acc);

    my $xref_id=$xref->id;
    #sql_delete($dbh,"homolset","symbol='$acc'");
    sql_delete($dbh,"homolset","dbxref_id=$xref_id");
    my $homolset_id = 
      insert_h($dbh,
               "homolset",
               {symbol=>$symbol,
                dbxref_id=>$xref_id});
    my $ok = 1;
    foreach my $gp (@$product_accs) {
        my $gp_acc = $gp;
        # inconsistent styles...
        if (!ref($gp)) {
            $gp = $self->get_product({acc=>$gp_acc});
            if (!$gp && $gp_acc =~ /^\w+:(.*)/) {
                $gp = $self->get_product({acc=>$1});
            }
        }
        if (!$gp) {
            warn("no such gp: $gp_acc");
            $ok=0;
            next;
        }
        my $gp_id = $gp->id;
        my $dbh = $self->dbh;
        sql_delete($dbh,
                   "gene_product_homolset",
                   "gene_product_id=$gp_id AND homolset_id=$homolset_id");
        # TODO: evidence, etc
        insert_h($dbh,
                 "gene_product_homolset",
                 {gene_product_id=>$gp_id,
                  homolset_id=>$homolset_id});
    }
    return $ok;
}

sub new_add_root {
    my $self = shift;
    $self->get_root_terms();  # automatically caches if not pre-set
    return;
}

sub add_root {
    my $self = shift;
    if ($ENV{GO_NEW_ADD_ROOT}) {
        return $self->new_add_root(@_);
    }
    my $rname = shift || 'all';
    my $term_type = shift || 'universal';
    my $dbh = $self->dbh;

    # find existing roots
    my $current_root_ids = 
      select_vallist($dbh,
                     "term",
                     {is_root=>1},
                     "id");
    if (!@$current_root_ids) {
        confess("no current root ids!");
    }

    # make the new global root
    my $id = 
      select_val($dbh,
                 'term',
                 {acc=>$rname},
                 "id");
    if (!$id) {
        $id =
          insert_h($dbh,
                   "term",
                   {acc=>$rname,
                    name=>$rname,
                    term_type=>$term_type,
                    is_root=>1});
    }
    if (grep {$id == $_} @$current_root_ids) {
        #warn("already added root: $id");

        # remove real root from list of roots to be de-rooted
        @$current_root_ids = grep {$id != $_} @$current_root_ids;
    }
    return unless @$current_root_ids;

    # de-root old roots
    update_h($dbh,
             "term",
             {is_root=>0},
             sqlin("id",$current_root_ids));

    $self->refresh; # get rtypes
    my %h = reverse %{$self->{rtype_by_id}};
    
    my $isa_id = $h{'is_a'};
      
    # add is-a links from old to new root
    foreach (@$current_root_ids) {
        my $ins_h =
          {term1_id=>$id,
           term2_id=>$_,
           relationship_type_id=>$isa_id};
        if (select_val($dbh,
                       "term2term",
                       $ins_h)) {
            # already have link between this and root
        }
        else {
            insert_h($dbh,
                     "term2term",
                     $ins_h);
        }
    }
    return;
}


=item get_enriched_term_hash

Args: sample_prods (GP aref) and optional background_prods (GP aref)
Returns: enrichment hash array
   {
       term=>$term,
       n_gps_in_sample=>$n_gps_in_sample,
       n_gps_in_sample_annotated=>$x,
       gps_in_sample_annotated=>$sample_gps_by_term_id{$term_id},
       gps=>$gp_array,
       n_gps_in_background=>$n_gps_in_background,
       n_gps_in_background_annotated=>$M,
       p_value=>$pvalue
   }

=cut
## If an explicit background set is not given, we will use everything
## in the ontology. An offshoot of this is that the database filter
## will effectively become a way of creating a background set.
sub get_enriched_term_hash {

  require "GO/TermFinder/Native.pm";

  my $self = shift; # apph
  my $sample_prods = shift || []; # gene product sample set
  my $background_prods = shift || []; # gene product background set

  my $dbh = $self->dbh; # database handle

  ## If nothing is incoming, just use the whole ontology.
  my $use_all = 0;
  $use_all = 1 if ! $background_prods || ! @$background_prods;

  ##
  ## Process incoming background gene products (if any).
  ##

  my @background_prods = ();
  my %background_gp_by_id = (); # hash of GO gps indexed by id.
  my %background_gp_count_by_term_id = ();
  my %background_gps_by_term_id = ();
  my $n_gps_in_background = -1;
  if( $use_all ) {

    ## Get # gps in db. For error, either database has no gene
    ## products or a filter that filters everything has been
    ## used.
    #$n_gps_in_background = $self->get_product_count; # BUG?
    $n_gps_in_background = $self->get_deep_product_count;
    return {} if ! $n_gps_in_background;

  }else{

    ## Union the background and the sample.
    my @unioned_background_set = ();
    push @unioned_background_set, @$background_prods;
    push @unioned_background_set, @$sample_prods;

    ## Get a proper list of the incoming background products (which,
    ## frankly, we should already have, but I don't know who's using
    ## this so I'm not removing it).
    #foreach (@$background_prods){
    foreach (@unioned_background_set){
      my $ps;
      if( ref($_) && ref($_) =~ /GeneProduct/ ){
	$ps = [$_];
      }else{
	$ps = $self->get_products($_);
      }
      ## Only add it if we haven't seen it.
      ## TODO: if this section starts failing for users, see the fix
      ## for the symmetric code in the sample prods section.
      if( ! $background_gp_by_id{$_->id} ){
	push @background_prods, @$ps;
	$background_gp_by_id{$_->id} = $_ foreach @$ps;
      }
    }
    ## Get # gps in db. For error, either database has no gene
    ## products or a filter that filters everything has been
    ## used.
    $n_gps_in_background = scalar(@background_prods);

    return {} if ! $n_gps_in_background;

    ## For every incoming background product...
    foreach my $p (@background_prods) {

      my $terms = $self->get_terms({product=>$p,is_not=>0});
      my @term_ids = map {$_->id} @$terms;
      ## ...get the ancestors of the associated terms and...
      my $hl =
	select_hashlist($dbh,
			"graph_path",
			sqlin("term2_id",\@term_ids),
			"DISTINCT term1_id AS term_id");
      ## ...increment the hits to all ancestors and hash the product.
      foreach (@$hl) {
	my $term_id = $_->{term_id};
	$background_gp_count_by_term_id{$term_id}++;
	push(@{$background_gps_by_term_id{$term_id}},
	     $background_gp_by_id{$p->id});
      }
    }
  }

  ##
  ## Process sample gene products.
  ##

  ## Get a proper list of the incoming sample products (which, frankly, we
  ## should already have, but I don't know who's using this so I'm not
  ## removing it).
  my @sample_prods = (); # list of GO gps.
  my %sample_gp_by_id = (); # hash of GO gps indexed by id.
  my $n_gps_in_sample = -1; #
  foreach (@$sample_prods){
    my $ps;
    if( ref($_) && ref($_) =~ /GeneProduct/ ){
      $ps = [$_];
    }else{
      $ps = $self->get_products($_);
    }

    ## Only add if the gene product is in the background or we are
    ## using all of the ontology. Of course, this shouldn't be a
    ## problem since we've unioned the sample and background.
    foreach my $rgp (@$ps){
      if( $use_all || $background_gp_by_id{$rgp->id} ){
	## Only add it if we haven't seen it before.
	if( ! $sample_gp_by_id{$rgp->id} ){
	  push @sample_prods, $rgp;
	  $sample_gp_by_id{$rgp->id} = $rgp;
	}
      }
    }
  }
  $n_gps_in_sample = scalar(@sample_prods);

  ## For every (remaining, see above) incoming sample product...
  my %sample_gp_count_by_term_id = ();
  my %sample_gps_by_term_id = ();
  foreach my $p (@sample_prods) {

    my $terms = $self->get_terms({product=>$p,is_not=>0});
    my @term_ids = map {$_->id} @$terms;
    ## ...get the ancestors of the associated terms and...
    my $hl =
      select_hashlist($dbh,
		      "graph_path",
		      sqlin("term2_id",\@term_ids),
		      "DISTINCT term1_id AS term_id");
    ## ...increment the hits to all ancestors and hash the product.
    foreach (@$hl) {
      my $term_id = $_->{term_id};
      $sample_gp_count_by_term_id{$term_id}++;
      push(@{$sample_gps_by_term_id{$term_id}}, $sample_gp_by_id{$p->id});
    }
  }

  ##
  ## Calculate.
  ##

  ##
  my $distribution_calculator =
    GO::TermFinder::Native::Distributions->new($n_gps_in_background);
  my %stats_by_term_acc = ();
  my $correction_factor = 0;
  foreach my $term_id (keys %sample_gp_count_by_term_id) {

    my $total = -1;
    my $term = $self->get_term({id=>$term_id});
    if( $use_all ){
      #$total = $self->get_deep_product_count({term=>$term});
      $total = $self->get_deep_product_count({term=>$term, is_not=>0});
    }else{
      $total = $background_gp_count_by_term_id{$term_id};
    }

    # and increment the correction factor if that node has more
    # than 1 annotation in the background
    $correction_factor++ if $total > 1;

    ##
    my $x = $sample_gp_count_by_term_id{$term_id};
    my $n = $n_gps_in_sample;
    my $M = $total;
    my $N = $n_gps_in_background;

    #print STDERR "+++start:( $term_id ) [" . $term->acc . '] ' .
    #  "\$x:$x, " .
    #	"\$n:$n, " .
    #	  "\$M:$M, " .
    #	    "\$N:$N" .
    #	      " ... ";

    my $pvalue =
      $distribution_calculator->pValueByHypergeometric($x, $n, $M, $N);

    ## Assemble the names of the gene products for returning.
    my $gp_array = [];
    foreach my $gp ( @{$sample_gps_by_term_id{$term_id}} ){

      #print STDERR '_0_' . $sample_gps_by_term_id{$term_id} . "\n";
      #print STDERR '_1_' . $gp . "\n";
      #print STDERR '_2_' . $gp->acc . "\n";
      #sleep 1;

      if( $gp ){
	my $gp_hash = {
		       ACC => 'n/a',
		       SYMBOL => 'n/a',
		       FULL_NAME => 'n/a',
		       SPECIESDB => 'n/a',
		      };
	$gp_hash->{ACC} = sprintf("%s", $gp->acc) if $gp->acc;
	$gp_hash->{SYMBOL} = sprintf("%s", $gp->symbol) if $gp->symbol;
	$gp_hash->{FULL_NAME} = sprintf("%s", $gp->full_name) if $gp->full_name;
	$gp_hash->{SPECIESDB} = sprintf("%s", $gp->speciesdb) if $gp->speciesdb;

	push @$gp_array, $gp_hash;
      }
    }

    $stats_by_term_acc{$term->acc} =
      {
       term=>$term,
       n_gps_in_sample=>$n_gps_in_sample,
       n_gps_in_sample_annotated=>$x,
       gps_in_sample_annotated=>$sample_gps_by_term_id{$term_id},
       gps=>$gp_array,
       n_gps_in_background=>$n_gps_in_background,
       n_gps_in_background_annotated=>$M,
       p_value=>$pvalue
      };
  }

  # bonferroni correction
  foreach my $term_acc (keys %stats_by_term_acc) {
    my $hyp = 
      $stats_by_term_acc{$term_acc};
    $hyp->{corrected_p_value} = $correction_factor * $hyp->{p_value};
  }

  return \%stats_by_term_acc;
}


#select DISTINCT dbxref.xref_dbname, dbxref.xref_key, gene_product.symbol, term.acc, parent.acc AS parent_acc from dbxref, gene_product, association, term, graph_path, term AS parent where dbxref.id = gene_product.dbxref_id and gene_product.id = association.gene_product_id and association.term_id = term.id and term.id = graph_path.term2_id and graph_path.term1_id = parent.id and dbxref.xref_key IN ('S000001777', 'S000005221') and association.is_not = 0
=item term_anc_bypass

Arg: dbh, list of gp accs, iea usage flag (defaults to 0)
Returns: a hashref containing data on the ancestors of terms 
         associated with the gps.

more information by examining the associated SQL

=cut
sub term_anc_bypass {

  my $dbh = shift;
  my $background_prods = shift;
  my $iea_p = shift || 0; # whether we should use ieas

  ## For every incoming background product...
  my @prod_acc_list = ();
  my $tick = "\'";
  foreach my $p (@$background_prods) {
    my $acc = $p->acc;
    push @prod_acc_list, $tick . $acc . $tick;
  }

  ## construct pseudo-query.
  my $tables =
    ["dbxref",
     "gene_product",
     "association",
     "term",
     "graph_path",
     "term AS ancestor"];
  my $wheres =
    ["dbxref.id = gene_product.dbxref_id",
     "gene_product.id = association.gene_product_id",
     "association.term_id = term.id",
     "term.id = graph_path.term2_id",
     "graph_path.term1_id = ancestor.id",
     "dbxref.xref_key IN (". join(', ', @prod_acc_list) .")",
     #"dbxref.xref_key IN ('S000001777', 'S000005221')", # DEBUG
     "association.is_not = 0"];
  my $res_heads =
    ["DISTINCT dbxref.xref_dbname",
     "dbxref.xref_key",
     "gene_product.id AS gp_id",
     "gene_product.symbol",
     "term.acc",
     "term.id",
     "ancestor.id AS ancestor_id",
     "ancestor.acc AS ancestor_acc"];

  ## Add parts to filter out IEAs if they are not wanted.
  if( defined $iea_p && ! $iea_p ){
    push @$tables,
      'evidence';
    push @$wheres,
      'association.id = evidence.association_id';
    push @$wheres,
      "evidence.code != 'IEA'";
  }

  my $tl =
    select_hashlist($dbh,
		    $tables,
		    $wheres,
		    $res_heads);
  return $tl;
}


=item fast_get_enriched_term_hash

Args: sample_prods (GP aref)
      background_prods (GP aref) (may be optionally empty)
      speciesdb arrayref
      iea use flag (defaults to 0)
Returns: enrichment hash array
   {
       term=>$term,
       n_gps_in_sample=>$n_gps_in_sample,
       n_gps_in_sample_annotated=>$k,
       gps_in_sample_annotated=>$sample_gps_by_term_id{$term_id},
       gps=>$gp_array,
       n_gps_in_background=>$n_gps_in_background,
       n_gps_in_background_annotated=>$M,
       p_value=>$pvalue
   }

NOTE: Experimental version built for speed.
WARNING: Check these optimizations against future DB changes.

=cut
## If an explicit background set is not given, we will use everything
## in the ontology. An offshoot of this is that the database filter
## will effectively become a way of creating a background set.
sub fast_get_enriched_term_hash {

  require "GO/TermFinder/Native.pm";

  #require AmiGO;
  #my $core = AmiGO->new();
  #$core->kvetch('_start:_fast_get_enriched_term_hash_');

  my $self = shift; # apph
  my $sample_prods = shift || []; # gene product sample set
  my $background_prods = shift || []; # gene product background set
  my $incoming_species = shift || []; # the species that we can use.
  my $iea_p = shift; # whether we should use ieas

  ## Since it's a boolean coming in, sanity check.
  if( ! defined $iea_p ){
    $iea_p = 1;
  }

  ## Database handle should already have species filter set. TODO:
  ## just pull this from self.
  my $dbh = $self->dbh; # database handle

  ###
  ### Set defaults for species, use_all
  ###

  ## We want to make sure that the default is defined--we'll be using
  ## it later as a filter. TODO: this should just be pulled out of the
  ## apphande above and we should remove the species list from the
  ## argument (I guess it's there to be backwards complatible).
  my %species = ();
  foreach (@$incoming_species) {
    $species{$_} = 1;
  }

  ## If no background is incoming, just use the whole ontology.
  my $use_all = 0;
  $use_all = 1 if ! $background_prods || ! scalar(@$background_prods);

  #$core->kvetch('_fgeth_: $use_all: ' . $use_all);

  ###
  ### Given that we have an incoming background list, get the
  ### associated gene products.
  ###

  ##
  ## Process incoming background gene products (if any).
  ##

  my @complete_background_prods = ();
  my %background_gp_by_id = (); # hash of GO gps indexed by id.
  my %background_gp_count_by_term_id = ();
  my %background_gps_by_term_id = ();
  my $n_gps_in_background = undef;
  if( $use_all ) {

    ## Get # gps in db. For error, either database has no gene
    ## products or a filter that filters everything has been
    ## used.
    ## BUG: this needs an IEA filter on it to work with not IEAs in
    ## the filtering case, but that is not support right now so that
    ## case is prevented further up the pipe.
    $n_gps_in_background = $self->get_deep_product_count;
    return {} if ! $n_gps_in_background;

    #$core->kvetch('_fgeth_: $n_gps_in_background (a): '.$n_gps_in_background);

  }else{

    ## Union the background and the sample.
    my @unioned_background_set = ();
    push @unioned_background_set, @$background_prods;
    push @unioned_background_set, @$sample_prods;

    ## Get a proper list of the incoming background products (which,
    ## frankly, we should already have, but I don't know who's using
    ## this so I'm not removing it).
    #foreach (@$background_prods){
    foreach (@unioned_background_set){
      my $ps;
      if( ref($_) && ref($_) =~ /GeneProduct/ ){
	$ps = [$_];
      }else{
	$ps = $self->get_products($_);
      }
      ## Only add it if we haven't seen it.
      if( ! $background_gp_by_id{$_->id} ){
	push @complete_background_prods, @$ps;
	$background_gp_by_id{$_->id} = $_ foreach @$ps;
	#print STDERR "_bg_prod_id_:" . $_->id . "\n";
      }
    }

    #$core->kvetch('_fgeth_: mark (a)');

    ## Get # gps in db. For error, either database has no gene
    ## products or a filter that filters everything has been
    ## used.
    $n_gps_in_background = scalar(@complete_background_prods);

    #$core->kvetch('_fgeth_: mark (b)');

    return {} if ! $n_gps_in_background;

    #$core->kvetch('_fgeth_: $n_gps_in_background (b): '.$n_gps_in_background);

    ## This segment is a bypass for older looping below.
    my $tl = term_anc_bypass($dbh, \@complete_background_prods, $iea_p);
    my %cache_hash = ();
    foreach my $item (@$tl) {

      my $dbname = $item->{xref_dbname};
      my $term_id = $item->{id};
      my $ancestor_id = $item->{ancestor_id};
      my $gp_id = $item->{gp_id};

      if( scalar(keys %species) == 0 ||
	  defined($species{$dbname}) ){

	$cache_hash{$gp_id} = {} if ! defined $cache_hash{$gp_id};
	$cache_hash{$gp_id}{$ancestor_id} = 1;
      }
    }
    ## Count and add to another cache.
    foreach my $gkey (keys %cache_hash) {
      foreach my $akey (keys %{$cache_hash{$gkey}}){
	$background_gp_count_by_term_id{$akey}++;
	push(@{$background_gps_by_term_id{$akey}},
	     $background_gp_by_id{$gkey});
      }
    }
  }

  ###
  ### Process sample gene products.
  ###

  #$core->kvetch('_fgeth_: ref sp: ' . scalar(@$sample_prods));

  ## Get a proper list of the incoming sample products (which, frankly, we
  ## should already have, but I don't know who's using this so I'm not
  ## removing it).
  my @sample_prods = (); # list of GO gps.
  my %sample_gp_by_id = (); # hash of GO gps indexed by id.
  my $n_gps_in_sample = -1; #
  foreach (@$sample_prods){
    my $ps;
    if( ref($_) && ref($_) =~ /GeneProduct/ ){
      #$core->kvetch('_fgeth_: mark (d)');
      $ps = [$_];
    }else{
      #$core->kvetch('_fgeth_: mark (e)');
      $ps = $self->get_products($_);
    }
    #$core->kvetch('_fgeth_: mark (f)');

    ## Only add if the gene product is in the background or we are
    ## using all of the ontology. Of course, this shouldn't be a
    ## problem since we've unioned the sample and background.
    if( $use_all || $background_gp_by_id{$_->id} ){
      ## Only add it if we haven't seen it.
      if( ! $sample_gp_by_id{$_->id} ){
       push @sample_prods, @$ps;
       $sample_gp_by_id{$_->id} = $_ foreach @$ps;
      }
    }
  }
  $n_gps_in_sample = scalar(@sample_prods);

  # $core->kvetch('_fgeth_: $n_gps_in_sample: ' . $n_gps_in_sample .
  # 	       ', ' . scalar(@sample_prods));

  ## For every (remaining, see above) incoming sample product...
  my %sample_gp_count_by_term_id = ();
  my %sample_gps_by_term_id = ();

  ## This segment is a bypass for older looping below.
  my $tl = term_anc_bypass($dbh, \@sample_prods, $iea_p);
  my %cache_hash = ();
  foreach my $item (@$tl) {

    my $dbname = $item->{xref_dbname};
    my $term_id = $item->{id};
    my $ancestor_id = $item->{ancestor_id};
    my $gp_id = $item->{gp_id};

    if( scalar(keys %species) == 0 ||
	defined($species{$dbname}) ){

      $cache_hash{$gp_id} = {} if ! defined $cache_hash{$gp_id};
      $cache_hash{$gp_id}{$ancestor_id} = 1;
    }
  }
  ##
  foreach my $gkey (keys %cache_hash) {
    foreach my $akey (keys %{$cache_hash{$gkey}}){
      $sample_gp_count_by_term_id{$akey}++;
      push(@{$sample_gps_by_term_id{$akey}},
	   $sample_gp_by_id{$gkey});
    }
  }

  #$core->kvetch('_fgeth_: mark (h)');

  ###
  ### Calculate.
  ###

  ##
  my $distribution_calculator =
    GO::TermFinder::Native::Distributions->new($n_gps_in_background);

  #$core->kvetch('_fgeth_: mark (i)');
  #$core->kvetch('_fgeth_: mark (i:count):' .
  #		scalar(keys %sample_gp_count_by_term_id));

  my %stats_by_term_acc = ();
  my $correction_factor = 0;
  foreach my $term_id (keys %sample_gp_count_by_term_id) {

    #$core->kvetch('_fgeth_: mark (j:1)');

    my $total = -1;
    my $term = $self->get_term({id=>$term_id});
    if( $use_all ){
      ## Again, the filter-as-db case would only work if we could use
      ## an IEA flag here, but it's not and the database doesn't
      ## support it yet.
      #$core->kvetch('_fgeth_: mark (j:2)');
      $total = $self->get_deep_product_count({term=>$term, is_not=>0});
    }else{
      #$core->kvetch('_fgeth_: mark (j:3)');
      $total = $background_gp_count_by_term_id{$term_id};
    }

    # And increment the correction factor if that node has more
    # than 1 annotation in the background.
    $correction_factor++ if $total > 1;

    #$core->kvetch('_fgeth_: mark (j:8)');

    ##
    my $k = $sample_gp_count_by_term_id{$term_id};
    my $n = $n_gps_in_sample;
    my $M = $total;
    my $N = $n_gps_in_background;

    my $term_acc = $term->acc;

    # $core->kvetch('_fgeth_: mark (j:9): ' . $term_acc);
    # $core->kvetch('_fgeth_: mark (j:9k): ' . $k);
    # $core->kvetch('_fgeth_: mark (j:9n): ' . $n);
    # $core->kvetch('_fgeth_: mark (j:9M): ' . $M);
    # $core->kvetch('_fgeth_: mark (j:9N): ' . $N);

    ## NOTE: Make sure we're not doing something stupid with 'all'.
    if( $term_acc ne 'all' ){

      ## Try for p-value. Even though it's mathematically valid, results
      ## might be confusing.
      my $pvalue = undef;
      if( $k >= $n - ($N - $M) &&
	  $k <= $n && $k <= $M){
	$pvalue = $distribution_calculator->pValueByHypergeometric($k,$n,$M,$N);
      }
      ## P-value sanity check.
      if( ! defined $pvalue ){
	die "Possibly due to a bug in the system, this term enrichment has reached an undefined state (with k=".$k.", n=".$n.", M=".$M.", N=".$N."). Please report this message, as well as your inputs, to the GO Helpdesk. Our apologies for the inconvenience";
      }elsif( $pvalue < 0.0 ){
	die "Possibly due to a rounding error in the system (e.g. " . $pvalue . ", with k=".$k.", n=".$n.", M=".$M.", N=".$N."), this term enrichment has generated an impossible probability. Please report this message, as well as your inputs, to the GO Helpdesk. Our apologies for the inconvenience";
      }elsif( $pvalue > 1.0){
	## Making the impossible possible because of logs and floats.
	$pvalue = 1.0;
      }

      #$core->kvetch('_fgeth_: mark (j:10)');

      ## Assemble the names of the gene products for returning.
      my $gp_array = [];
      foreach my $gp ( @{$sample_gps_by_term_id{$term_id}} ){

	#print STDERR '_0_' . $sample_gps_by_term_id{$term_id} . "\n";
	#print STDERR '_1_' . $gp . "\n";
	#print STDERR '_2_' . $gp->acc . "\n";
	#sleep 1;

	if( $gp ){
	  my $gp_hash = {
			 ACC => 'n/a',
			 SYMBOL => 'n/a',
			 FULL_NAME => 'n/a',
			 SPECIESDB => 'n/a',
			};
	  $gp_hash->{ACC} = sprintf("%s", $gp->acc) if $gp->acc;
	  $gp_hash->{SYMBOL} = sprintf("%s", $gp->symbol) if $gp->symbol;
	  $gp_hash->{FULL_NAME} =sprintf("%s",$gp->full_name) if $gp->full_name;
	  $gp_hash->{SPECIESDB} =sprintf("%s",$gp->speciesdb) if $gp->speciesdb;

	  push @$gp_array, $gp_hash;
	}
      }

      #$core->kvetch('_fgeth_: mark (k)');
      #$core->kvetch('_fgeth_: mark (k1:acc): ' . $term_acc);

      $stats_by_term_acc{$term_acc} =
	{
	 term => $term,
	 n_gps_in_sample => $n_gps_in_sample,
	 n_gps_in_sample_annotated => $k,
	 gps_in_sample_annotated => $sample_gps_by_term_id{$term_id},
	 gps => $gp_array,
	 n_gps_in_background => $n_gps_in_background,
	 n_gps_in_background_annotated => $M,
	 p_value => $pvalue,
	};
    }
  }

  # bonferroni correction
  foreach my $term_acc (keys %stats_by_term_acc) {
    my $hyp = 
      $stats_by_term_acc{$term_acc};
    $hyp->{corrected_p_value} = $correction_factor * $hyp->{p_value};
  }

  #$core->kvetch('_end:_fast_get_enriched_term_hash_');

  return \%stats_by_term_acc;
}


##

sub fill_path_table {
    my $self = shift;
    my $reasoner = GO::Reasoner->new;
    $reasoner->dbh($self->dbh);
    $reasoner->run;
    $self->has_path_table(1);
    return;
}

sub old_fill_path_table {
    my $self = shift;
    my $dbh = $self->dbh;
    sql_delete($dbh, "$PATH");
    my $roots = $self->get_root_terms(-template=>{id=>'y'});
    if (@$roots != 1) {
        $self->throw("Expected 1 root. Got: @$roots");
    }
    my $root = $roots->[0];
    my $graph = 
#      $self->get_graph(-acc=>[map {$_->acc} @$roots],
      $self->get_graph(-acc=>$root->acc,
                       -depth=>-1,
                       -template=>{terms=>{id=>'y', acc=>'y'}});
    my @nodes = @{$graph->get_all_nodes};
    my $it = $graph->create_iterator({direction=>"up"});
    my %done = ();
    foreach my $node (@nodes) {
        $it->reset_cursor($node->acc);
        while (my $ni = $it->next_node_instance) {
            my ($t1,$t2,$d) =
              ($ni->term->id,
               $node->id,
               $ni->depth);
            my $str = "$t1/$t2/$d";
            next if $done{$str};
            insert_h($dbh,
                     "$PATH",
                     {term1_id=>$t1,
                      term2_id=>$t2,
                      distance=>$d});
            $done{$str} = 1;
        }
    }
    $self->has_path_table(1);
}

sub has_word_table {
    my $self = shift;
    $self->{_has_word_table} = shift if @_;
    if (!defined($self->{_has_word_table})) {
        eval {
            my $h=
              select_hash($self->dbh,
                          "wordunit2term",
                          undef,
                          "count(*) AS c");
            $self->{_has_word_table} = $h->{c} ? 1:0;
        };
        if ($@) {
            $self->{_has_word_table} = 0;
        }
    }
    return $self->{_has_word_table};
}

sub fill_word_table {
    my $self = shift;
    my $dbh = $self->dbh;
    sql_delete($dbh, "wordunit2term");
    my @wstruct =
      $self->get_wordstruct;
    foreach my $ws (@wstruct) {
        my $id = shift @$ws;
        my $type = shift @$ws;
        if (!@$ws) {
            warn("fill_word_table: $id $type has no words");
            next;
        }

        foreach my $w (@$ws) {
            my $h =
              {"term_id"=>$id,
               "is_synonym"=>($type eq "synonym" ? 1 : 0),
               "is_definition"=>($type eq "definition" ? 1 : 0),
               "wordunit"=>sql_quote($w)};
            if ($self->dbms eq "mysql") {
                $h->{wordsound} = "soundex(wordunit)";
            }
            my @k = keys %$h;
            my $sql =
              "insert into wordunit2term (".join(", ", @k).") ".
                "values (".
                  join(", ", map {$h->{$_}} @k).")";
            $dbh->do($sql);
        }
    }
    $self->has_word_table(1);
}

########### select distinct(wordunit), count(term_id) c from wordunit2term group by wordunit order by c;
########### select u1.wordunit AS w1, u2.wordunit AS w2 from wordunit2term u1, wordunit2term u2 where u1.wordsound = u2.wordsound and u1.wordunit != u2.wordunit

sub get_wordstruct {
    my $self = shift;
    my $dbh = $self->dbh;
    my $splitexpr = shift || '[\W\d_]';
    my $thl = 
      select_hashlist($dbh,
                      "term",
                      [],
                      ["name AS phrase",
                       "id AS id",
                       "'term' AS type"]);
    my $shl = 
      select_hashlist($dbh,
                      "term_synonym",
                      [],
                      ["term_synonym AS phrase",
                       "term_id AS id",
                       "'synonym' AS type"]);
    my $dhl = 
      select_hashlist($dbh,
                      "term_definition",
                      [],
                      ["term_definition AS phrase",
                       "term_id AS id",
                       "'definition' AS type"]);

    my @ws = ();
    foreach my $h (@$thl, @$shl, @$dhl) {
        my $ph = $h->{phrase};
        my @words = split(/$splitexpr/, $ph);
        @words = grep {$_} @words;
        push(@ws, [$h->{id}, $h->{type}, @words]);
    }
    return @ws;
}

#false = no such a table or table is empty
sub has_gp_property_table {
    my $self = shift;
    $self->{_has_gp_property_table} = shift if @_;
    if (!defined($self->{_has_gp_property_table})) {
        eval {
            my $h=
              select_hash($self->dbh,
                          "$GPPROPERTY",
                          undef,
                          "count(*) AS c");
            $self->{_has_gp_property_table} = $h->{c} ? 1:0;
        };
        if ($@) {
            $self->{_has_gp_property_table} = 0;
        }
    }
    return $self->{_has_gp_property_table};
}

sub has_path_table {
    my $self = shift;
    $self->{_has_path_table} = shift if @_;
    if (!defined($self->{_has_path_table})) {
        eval {
            my $h=
              select_hash($self->dbh,
                          "$PATH",
                          undef,
                          "count(*) AS c");
            $self->{_has_path_table} = $h->{c} ? 1:0;
        };
        if ($@) {
            $self->{_has_path_table} = 0;
        }
    }
    return $self->{_has_path_table};
}

sub fill_count_table {
    my $self = shift;
    my $evcodes = shift;
    my $reltypes = shift;

    my $dbh = $self->dbh;

    ## BUG: in some cases, the filters seem to be set badly. Force
    ## them to a known (and likely useful) setting.
    my $orig_evcodes = [];
    my $orig_speciesdb = [];
    if( defined $self->filters ){
      if( defined $self->filters->{evcodes} ){
	$orig_evcodes = $self->filters->{evcodes};
      }
      if( defined $self->filters->{speciesdb} ){
	$orig_evcodes = $self->filters->{speciesdb};
      }
    }
    $self->filters({evcodes=>[],speciesdb=>[]});

    # if an argument is passed, this is used as the
    # list of evcode combinations to use.
    # if there is no argument, only one combination,
    # the current filter, is used
    if (!defined($evcodes)) {
      #print STDERR "___" . 'evcodes not defined' . "\n";
        $evcodes = [$self->filters->{evcodes}];
    }
    #print STDERR "_f_" . $self->filters->{evcodes} . "\n";
    #print STDERR "_s_" . scalar(@$evcodes) . "\n";
    #$evcodes = ['EXP', 'IC', 'IDA', 'IEA', 'IEP', 'IGC', 'IGI', 'IMP', 'IPI', 'ISA', 'ISM', 'ISO', 'ISS', 'NAS' ,'ND', 'NR', 'RCA', 'TAS'];

    ##
    #my $oldcodes = $self->filters->{evcodes};
    #my $oldspdb = $self->filters->{speciesdb};

    if (@$evcodes > 1) {
      confess("For now you can only populate gpc with one evcode combination");
    }

    sql_delete($dbh, "gene_product_count");

    ## We only fill the count table for non IEAs and for all evcodes.
    ## COMMENTARY: Besides the statement above, there appears to be no
    ## open reference in this section about IEAs, so maybe it's
    ## refering to a property inherited from another piece if
    ## code. Trying to leave as-is.
    foreach my $ev (@$evcodes) {

      #print STDERR "__loop ev_" . $ev . "_\n";

        my $evstr = $ev || undef;
        if (ref($ev)) {
            $evstr = join(";", sort @{$ev || []});
        }

        my $r = $self->get_root_term(-template=>{id=>'y', acc=>'y'});
        my $g = $self->get_graph_by_terms([$r], -1, {terms=>{id=>'y'}});
        my $nodes = $g->get_all_nodes;

        if ($ev) {
            $self->filters->{evcodes} = $ev;
        }

        if ($GO_HAS_COUNT_BY_SPECIES) {

	  #print STDERR "_by spec_" . '' . "\n";

            my $sql =
              "SELECT p.species_id,count(distinct p.id)
           FROM association AS a, graph_path AS g, evidence AS e, gene_product AS p
           WHERE is_not=0 AND p.id = a.gene_product_id AND e.association_id=a.id AND a.term_id=g.term2_id AND g.term1_id=?";
            if( $ev && scalar(@$ev) ){
                my @extra = ();
                if (!ref($ev)) {
                    $ev=[$ev];
                }
                foreach (@$ev) {
                    if ($_ =~ /\!(\w+)/) {
                        push(@extra, "e.code != '$1'");
                    } else {
                        push(@extra, "e.code = '$_'");
                    }
                }
                $sql.= " AND (".join(' OR ',@extra).")";
            }
            $sql.= " GROUP BY p.species_id";
	    #print STDERR "sql: $sql\n";
            my $sth = $dbh->prepare($sql);
            # pre-compute by species
            foreach my $n (@$nodes) {
	      #printf STDERR "\texecuting for %s %s\n", $n->id, $n->acc;
	      $sth->execute($n->id);
	      my $spcs = $sth->fetchall_arrayref();
	      foreach (@$spcs) {
		insert_h($dbh,
			 "gene_product_count",
			 {term_id=>$n->id,
			  product_count=>($_->[1] || 0),
			  species_id=>$_->[0],
			  code=>$evstr});
	      }
            }
	  }

        my $spdbh = $self->get_speciesdb_dict;
        # pre-computed by speciesdb (eg GOA, FlyBase, DictyBase,...)
        foreach my $spdb (keys %$spdbh) {

	    #print STDERR "_specdb_ " . $spdb;

            if($ev){
	      $self->filters->{evcodes} = $ev;
            }else{
	      delete $self->filters->{evcodes};
	    }
	    #print STDERR " _filter_" . $self->filters->{evcodes} . "\n";

            $self->filters->{speciesdb}=$spdb;
            my $nodes = $g->get_all_nodes;
            foreach my $n (@$nodes) {
                my $pc =
                  $self->_get_dyna_computed_product_count
                    (-constraints=>{deep=>1, 
                                    term=>$n,
                                    is_not=>0},
                     -options=>{count=>1, reltype_filter=>$reltypes});
                insert_h($dbh,
                         "gene_product_count",
                         {term_id=>$n->id,
                          product_count=>($pc || 0),
                          speciesdbname=>$spdb,
                          code=>$evstr});
            }

        }
    }

    # restore settings
    #$self->filters->{evcodes} = $oldcodes;
    #$self->filters->{speciesdb} = $oldspdb;
    $self->has_count_table(1);

    ## BUG: restore from above bug.
    $self->filters({evcodes=>$orig_evcodes,speciesdb=>$orig_speciesdb});
}

#empty reltype filter: all reltypes
sub _get_dyna_computed_product_count {
    my $self = shift;
    my ($inconstr, $template, $options) =
      rearrange([qw(constraints template options)], @_);
    my $constr = {%{$inconstr || {}}};        # make copy
    $options ||= {};

    my $total = 0;
    my $reltypes = $options->{reltype_filter};
    my $term = $inconstr->{term} || confess("must have term to compute term's gene product count");
    if ($reltypes) {
        unless (ref($reltypes)) {
            $reltypes = [$reltypes];
        }
        my $graph = $self->get_graph_below($term->acc, -1);
        my $ni = $graph->create_iterator;
        $ni->reltype_filter($reltypes);
        my %done_node_h = ();
        while (my $node = $ni->next_node_instance) {
            my $t = $node->term;
            next if (exists $done_node_h{$t->id});
            $total += $self->get_products(-constraints=>{term=>$t,is_not=>0},-options=>{count=>1});
            $done_node_h{$t->id} = 1;
        }
    } else {
        # no relationship type filter  - we can do a fast query
        $total = $self->get_products(-constraints=>$inconstr, -options=>$options);
    }
    return $total;
}
sub has_count_table {
    my $self = shift;
    $self->{_has_count_table} = shift if @_;
    if (!defined($self->{_has_count_table})) {
        eval {
            my $h=
              select_hash($self->dbh,
                          "gene_product_count",
                          undef,
                          "count(*) AS c");
            $self->{_has_count_table} = $h->{c} ? 1:0;
        };
        if ($@) {
            $self->{_has_count_table} = 0;
        }
    }
    return $self->{_has_count_table};
}


sub get_term_loadtime {
    my $self = shift;
    my $dbh = $self->dbh;
    my $acc = shift;

    my $t = select_val($dbh,
		       ["term_audit",
			"term"],
		       ["term.id = term_id",
			"term.acc = ".sql_quote($acc)],
		       "term_loadtime");
    return $t;
}


sub source_audit {
    my $self = shift;
    my $dbh = $self->dbh;

    my $hl =
      select_hashlist($dbh,
		      "source_audit");
    return $hl;
}


sub instance_data {
    my $self = shift;
    my $dbh = $self->dbh;

    my $h =
      select_hash($dbh,
		  "instance_data");
    return $h;

}


sub get_distance {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($t1, $t2, $nosiblings) =
      rearrange([qw(term1 term2 nosiblings)], @_);

    # be very liberal in what we accept
    ($t1, $t2) =
      map {
          if (ref($_)) {
              if (ref($_) eq "HASH") {
                  if ($_->{acc}) {
                      $_->{acc};
                  }
                  else {
                      $self->get_term($_)->acc;
                  }
              }
              else {
                  $_->acc;
              }
          }
          elsif (int($_)) {
	      sprintf("GO:%07d", $_);
          }
          else {
	      $_;
          }
      } ($t1, $t2);

    # t1 and t2 should now be acc numbers

    my $h;
    if ($nosiblings) {
        $h=
          select_hash($dbh,
                      ["$PATH",
                       "term AS t1",
                       "term AS t2"],
                      ["t1.acc = '$t1'",
                       "t2.acc = '$t2'",
                       "(($PATH.term1_id = t1.id AND ".
                       "  $PATH.term2_id = t2.id) OR ".
                       " ($PATH.term1_id = t2.id AND ".
                       "  $PATH.term2_id = t1.id))"
                      ],
                      "min($PATH.distance) AS dist");
        return -1 unless $h;
    }
    else {
        $h=
          select_hash($dbh,
                      ["$PATH AS path1", 
                       "$PATH AS path2", 
                       "term AS t1", 
                       "term AS t2"],
                      ["t1.acc = '$t1'",
                       "t2.acc = '$t2'",
                       "path1.term2_id = t1.id",
                       "path2.term2_id = t2.id",
                       "path1.term1_id = path2.term1_id"],
                      "min(path1.distance + path2.distance) AS dist");
    }
    if ($h) {
        return defined($h->{dist}) ? $h->{dist} : -1;
    }
    else {
        confess("Assertion error: Can't find distance $t1 to $t2");
    }
}

sub reset_acc2name_h {
    my $self = shift;
    delete $self->{_acc2name_h};
    return;
}

sub acc2name_h {
    my $self = shift;
    if (@_) {
        $self->{_acc2name_h} = shift;
    }
    if (!$self->{_acc2name_h}) {
        my $hl = 
          select_hashlist($self->dbh,
                          "term",
                          [],
                          "acc,name");
        my $a2n = {};
        foreach (@$hl) {
            $a2n->{$_->{acc}} = $_->{name};
        }
        $self->{_acc2name_h} = $a2n;
    }
    return $self->{_acc2name_h};
}

sub dbxref2id_h {
    my $self = shift;
    if (@_) {
        $self->{_dbxref2id_h} = shift;
    }
    if (!$self->{_dbxref2id_h}) {
	my $max = 1;
        my $hl = 
          select_hashlist($self->dbh,
                          "dbxref",
                          [],
                          "xref_dbname, xref_key,id");
        my $d2i = {};
        foreach (@$hl) {
            $d2i->{uc($_->{xref_dbname})}->{uc($_->{xref_key})}= $_->{id};
	    $max = $_->{id} if $_->{id} > $max;
	}
        $d2i->{max} = $max;
        $self->{_dbxref2id_h} = $d2i;
    }
    return $self->{_dbxref2id_h};
    
}

sub dbxref2gpid_h {
    my $self = shift;
    if (@_) {
        $self->{_dbxref2gpid_h} = shift;
    }
    if (!$self->{_dbxref2gpid_h}) {
        my $hl = 
          select_hashlist($self->dbh,
                          ["dbxref","gene_product"],
                          ["dbxref.id = gene_product.dbxref_id"],
                          ["xref_dbname, xref_key, gene_product.id"]);
        my $d2i = {};
        foreach (@$hl) {
            $d2i->{uc($_->{xref_dbname})}->{uc($_->{xref_key})}= $_->{'gene_product.id'};
	}
        $self->{_dbxref2gpid_h} = $d2i;
    }
    return $self->{_dbxref2gpid_h};
    
}

sub acc2id_h {
    my $self = shift;
    if (@_) {
        $self->{_acc2id_h} = shift;
    }
    if (!$self->{_acc2id_h}) {
        my $hl = 
          select_hashlist($self->dbh,
                          "term",
                          [],
                          "acc,id");
        my $a2i = {};
        foreach (@$hl) {
            $a2i->{$_->{acc}} = $_->{id};
        }
        $self->{_acc2id_h} = $a2i;
    }
    return $self->{_acc2id_h};
}

sub source2id_h {
    my $self = shift;
    if (@_) {
        $self->{_source2id_h} = shift;
    }
    if (!$self->{_source2id_h}) {
        my $hl = 
          select_hashlist($self->dbh,
                          "db",
                          [],
                          "name,id");
        my $s2i = {};
        foreach (@$hl) {
            $s2i->{uc($_->{name})} = $_->{id};
        }
        $self->{_source2id_h} = $s2i;
    }
    return $self->{_source2id_h};
}

sub taxon2id_h {
    my $self = shift;
    if (@_) {
        $self->{_taxon2id_h} = shift;
    }
    if (!$self->{_taxon2id_h}) {
        my $hl = 
          select_hashlist($self->dbh,
                          "species",
                          [],
                          "ncbi_taxa_id, id");
        my $n2i = {};
        foreach (@$hl) {
            $n2i->{$_->{ncbi_taxa_id}} = $_->{id};
        }
        $self->{_taxon2id_h} = $n2i;
    }
    return $self->{_taxon2id_h};
}

sub dbxref2gp_h {
    my $self = shift;
    if (@_) {
        $self->{_dbxref2gp_h} = shift;
    }
    if (!$self->{_dbxref2gp_h}) {
	my @tables_arr = ("gene_product", "dbxref");
	my @constr_arr = ("gene_product.dbxref_id = dbxref.id");
	my @cols = ("dbxref.xref_key", "dbxref.xref_dbname", "gene_product.symbol");
	my $hl = 
              select_hashlist(-dbh=>$self->dbh,
                              -tables=>\@tables_arr,
                              -where=>\@constr_arr,
                              -columns=>\@cols);

        my $d2i = {};
        foreach (@$hl) {
	    #enforce case
	    my $dbname = uc($_->{xref_dbname});
            $d2i->{uc($_->{xref_key})}->{$dbname} = $_->{symbol};
	}
        $self->{_dbxref2gp_h} = $d2i;
    }
    return $self->{_dbxref2gp_h};
    
}

sub get_term {
    my $self = shift;
    my $dbh = $self->dbh;
    my $terms = $self->get_terms(@_);
#    if (scalar(@$terms ) > 1) {
#	warn("get_term(",join(", ", @_).") returned >1 term");
#    }
    return $terms->[0];   # returns undef if empty
}

# only returns terms that have associations attached
sub get_terms_with_associations {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($inconstr, $template) =
      rearrange([qw(constraints template)], @_);
    my $interms = $self->get_terms($inconstr, {id=>'y'});
    $template = $template || {};
    $template->{association_list} = [];
    my $graph =
      $self->get_graph_by_terms($interms,
                                -1,
                                {terms=>{id=>'y'},
                                 with_associations=>1,
                                 traverse_up=>0});
    my @ids = map { $_->id } @{$graph->get_all_nodes};
    my @lookup = ();  # use array idx for speed
    my $terms = $self->get_terms({idlist=>\@ids,
                                  with_associations=>1},
                                 $template);
    foreach (@$terms) { $lookup[$_->id] = $_ }
    my $it = $graph->create_iterator;
    # now order the terms by graph, but no duplicates
    my @o_terms = ();
    while (my $n = $it->next_node) {
        if ($lookup[$n->id]) {
            push(@o_terms, $lookup[$n->id]);
            # don't include twice
            $lookup[$n->id] = undef;
        }
    }
    return \@o_terms;
}


## Attempt to access damaged filters (hack to try and work around
## other recent recent hacks to get IEAs working...).
sub _try_filter {
  my $self = shift;
  my $key = shift || die "died trying--need an arguement: $!";

  my $retval = undef;

  ## Try and safely get the data out.
  if( defined $self->filters ){
    if( defined $self->filters->{$key} ){
      $retval = $self->filters->{$key};
    }
  }

  return $retval;
}


sub get_terms {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($inconstr, $template) =
      rearrange([qw(constraints template)], @_);
    $template = GO::Model::Term->get_template($template);
    my $constr = pset2hash($inconstr);
    my @where_arr = ();
    my @table_arr = ("term");

    my $fetch_all = 0;

    # allow API users to pass in
    # an accession no as alternate
    # to hashref
    if ($constr && !ref($constr)) {
        if ($constr =~ /^\d+$/) {
            $constr = {"acc"=>int($constr)};
        }
        elsif ($constr =~ /\w+:\d+$/) {
            $constr = {"acc"=>$constr};
        }
        elsif ($constr eq "*") {
            $constr = {};
        }
        else {
            $constr = {"name"=>$constr};
        }
    }

    my $with_associations = 0;
    if ($constr->{with_associations}) {
        delete $constr->{with_associations};
        $with_associations = 1;
    }

    if (!$constr || !%{$constr || {}}) {
        $fetch_all = 1;
    }

    my $accs = $constr->{acc} || $constr->{accs};
    if ($accs) {
        # allow either single acc or list of accs
        if (!ref($accs)) {
            $accs = [$accs];
        }
        map {
            # HACK! turn acc from integer to GO:nnnnnnn
            if (/^\d+$/) {
                $_ = sprintf("GO:%07d", $_);
            }
        } @$accs;
        my $orq = "acc in (".join(", ", 
                                  map {sql_quote($_) }
                                  @$accs).")";
#        my $orq = join(" OR ", map {"acc = $_"} @$accs);
#        if (@$accs > 1) {
#            $orq = "($orq)";
#        }
        push(@where_arr, $orq) if @$accs;
        delete $constr->{acc};
        delete $constr->{accs};
    }

    # this boolean determines whether
    # we are adding selected associations
    # to the terms during creation
    my $add_select_assocs = 0;

    my $hl;

    if ($constr->{search} && $constr->{search} eq "*") {
        delete $constr->{search};
        $fetch_all = 1;
    }
    if ($constr->{search}) {
	my $srch = $constr->{search};
        my $fieldsstr = 
          $constr->{search_fields} || "";
        my @fields = split(/[\,\:\;]/, $fieldsstr);
        my @yes =  grep {$_ !~ /^\!/} @fields;
        my @no =  grep {/^\!/} @fields;
        if (!@yes) {
            @yes = ("name", "synonym", "definition", "dbxref", "subset", "comment");
        }
        my $selected = {};
        map {$selected->{$_} = 1} @yes;
        map {$selected->{substr($_, 1)} = 0} @no;
        my $srch_l = [$srch];
        if (ref($srch) eq "ARRAY") {
            # allow api users to pass in
            # single term of list of terms
            $srch_l = $srch;
        }
#	$srch =~ tr/A-Z/a-z/;
	map {s/\*/\%/g} @$srch_l;

        my @extra = ();
        if ($constr->{type}) {
            $constr->{term_type} = $constr->{type};
            delete $constr->{type};
        }
	my $ttype = $constr->{term_type};
        if ($ttype) {
            delete $constr->{term_type};
            $ttype = [$ttype] unless (ref($ttype) eq 'ARRAY');
            push @extra, "term.term_type IN (".join(",",map{sql_quote($_)}@$ttype).")";
        }

        # we have to do these in seperate
        # queries and merge in-memory; this is
        # because some terms don't have an
        # entry in the many-1 relations so
        # we could have to use an outer join
        # which is slow
        my ($hl1, $hl2, $hl3, $hl4, $hl5) = ([],[],[],[],[]);
        if ($selected->{name}) {
            $hl1=
              select_hashlist($dbh,
                              "term",
                              ["(".
                               join(" OR ",
                                    map {
                                        "term.name like ".sql_quote($_)
                                    } @$srch_l).
                              ")",
                               @extra
                               ]
                              );
        }
        if ($selected->{synonym}) {
            $hl2=
              select_hashlist($dbh,
                              ["term", "term_synonym"],
                              ["(".
                               join(" OR ",
                                    map {
                                        "term_synonym.term_synonym like ".
                                          sql_quote($_)
                                      } @$srch_l).
                               ")",
                               "term.id=term_synonym.term_id",
                               @extra
                              ],
                              "term.*");
        }
        if ($selected->{definition} || $selected->{comment}) {
            $hl3=
              select_hashlist($dbh,
                              ["term", "term_definition"],
                              ["(".
                               join(" OR ",
                                    map {
                                        ($selected->{definition} ? 
					 ("term_definition.term_definition like ".
					  sql_quote($_)) : (),
					 $selected->{comment} ?
					 ("term_definition.term_comment like ".
					  sql_quote($_)) : ())
				    } @$srch_l).
                               ")",
                               "term.id=term_definition.term_id",
                               @extra
                               #			   "term.name not like ".sql_quote($srch),
                              ],
                              "term.*");
        }
        if ($selected->{dbxref}) {
	
            $hl4=
              select_hashlist($dbh,
                              ["term", "term_dbxref", "dbxref"],
                              ["(".
                               join(" OR ",
                                    map {
                                        if (/(.*):(.*)/) {
                                            # ignore DB part of dbxref
                                            $_ = $2;
                                        }
                                        "dbxref.xref_key like ".
                                          sql_quote($_)
                                      } @$srch_l).
                               ")",
                               "term.id=term_dbxref.term_id",
                               "dbxref.id=term_dbxref.dbxref_id",
                               @extra
                              ],
                              "term.*");
        }

        if ($selected->{subset}) {
	
            $hl5=
              select_hashlist($dbh,
                              ["term", "term_subset", "term AS subset"],
                              ["(".
                               join(" OR ",
                                    (map {
                                        "subset.name like ".
                                          sql_quote($_)
                                      } @$srch_l),
                                    (map {
                                        "subset.acc like ".
                                          sql_quote($_)
                                      } @$srch_l),
                                   ).
                               ")",
                               "term.id=term_subset.term_id",
                               "subset.id=term_subset.subset_id",
                               @extra
                              ],
                              "term.*");
        }
	
	$hl = [];
	my @id_lookup = ();
	map {
	    dd($_) if $ENV{DEBUG};
	    if (!$id_lookup[$_->{id}]) {
		$id_lookup[$_->{id}] = 1;
		push(@$hl, $_);
	    }
	} (@$hl1, @$hl2, @$hl3, @$hl4, @$hl5);
	
    }

    else {
	# dynamically generate SQL
	# for query

        # prepare selected columns
	my @select_arr = ("distinct term.*");

        # negation not dealt with properly...
        if ($constr->{tree}) {
#            no strict "vars";
#            my $t = "term";
#            my $deep;
#            if ($constr->{deep}) {
#                $deep = 1;
#                $t = "superterm";
#                push(@table_arr, 
#                     "term superterm",
#                     "graph_path");
#                push(@where_arr, 
#                     "superterm.id = graph_path.term1_id",
#                     "graph_path.term2_id = term.id");
#            }
#            # eg [and [[not [acc 3677]] [or [[acc 1] [acc 2]]]]]
#            my $tree = $constr->{tree};
#            delete $constr->{tree};
#            delete $constr->{deep};
#            sub r {
#                my $tree = shift;
#                confess($tree) unless ref $tree;
#                my ($n, $v) = @$tree;
#                $n = lc($n);
#                if ($n eq "or") {
#                    return "(".join(" OR ", map {r($_)} @$v).")";
#                }
#                elsif ($n eq "and") {
#                    return "(".join(" AND ", map {r($_)} @$v).")";
#                }
#                elsif ($n eq "not") {
#                    return "(NOT (".r($v)."))";
#                }
#                else {
#                    return "$t.$n = ".sql_quote($v);
#                }
#            }
#            my $where = r($tree);
#            push(@where_arr, $where);
#            use strict "vars";
        }

	if ($constr->{synonym}) {
		$constr->{synonyms} = $constr->{synonym};
		delete $constr->{synonym};
	}

	if ($constr->{synonyms}) {
		my $syns = $constr->{synonyms};
		# allow either single synonym or a list
		if (!ref($syns)) {
			$syns = [$syns];
		}
		push(@table_arr, "term_synonym");
		push(@where_arr,
		"term.id=term_synonym.term_id",
		"(".
			join(" OR ",
				map {
					"term_synonym.term_synonym like ".
						sql_quote($_)
				} @$syns).
			")");
		delete $constr->{synonyms};
	}

	if ($constr->{subset}) {
            my $ss_qt = sql_quote($constr->{subset});
	    push(@table_arr, "term_subset", "term AS subset");
	    push(@where_arr,
		 "term.id=term_subset.term_id",
		 "subset.id=term_subset.subset_id",
		 "(subset.name = $ss_qt OR subset.acc = $ss_qt)",
                );
		 
	    delete $constr->{subset};
	}

	if ($constr->{dbxref}) {
            $constr->{dbxrefs} = [$constr->{dbxref}];
            delete $constr->{dbxref};
        }
	if ($constr->{dbxrefs}) {
	    my $dbxrefs = $constr->{dbxrefs};

            my @q =
              map {
                  if (ref($_)) {
                      "(dbxref.xref_dbname = ".sql_quote($_->{xref_dbname}).
                        " AND ".
                          "dbxref.xref_key = ".sql_quote($_->{xref_key}).")";
                  }
                  else {
                      if ($_ =~ /(.*):(.*)/) {
                          "(dbxref.xref_dbname = ".sql_quote($_->{xref_dbname}).
                            " AND ".
                              "dbxref.xref_key = ".sql_quote($_->{xref_key}).")";
                      }
                      else {
                          confess("$_ not a valid dbxref");
                      }
                  }
              } @$dbxrefs;
            if (@q) {
                push(@table_arr, "term_dbxref", "dbxref");
                push(@where_arr,
                     "term.id=term_dbxref.term_id",
                     "term_dbxref.dbxref_id = dbxref.id",
                     "(".join(" OR ", @q).")");
            }

	    delete $constr->{dbxrefs};
	}
	if ($constr->{dbxref_key}) {
	    push(@table_arr, "term_dbxref", "dbxref");
	    push(@where_arr,
		 "term.id=term_dbxref.term_id",
		 "term_dbxref.dbxref_id = dbxref.id",
		 "dbxref.xref_key=".
		 sql_quote($constr->{dbxref_key}));
	    delete $constr->{dbxref_key};
	}
	if ($constr->{dbxref_dbname}) {
	    push(@table_arr, "term_dbxref", "dbxref");
	    push(@where_arr,
		 "term.id=term_dbxref.term_id",
		 "term_dbxref.dbxref_id = dbxref.id",
		 "dbxref.xref_dbname = ".
		 sql_quote($constr->{dbxref_dbname}));
	    delete $constr->{dbxref_dbname};
	}
	if ($constr->{idlist}) {
	    my @ids = @{$constr->{idlist}};
	    if (!@ids) {@ids=(0)}
	    push(@where_arr,
                 "id in (".join(", ", @ids).")");
	    delete $constr->{idlist};
	}

        if ($constr->{where}) {
            push(@where_arr, "($constr->{where})");
            delete $constr->{where};
        }

	# allow api users to specify
	# a stringlist for product accs
	# for convenience
	if ($constr->{product_accs}) {
	    $constr->{products} =
		[
		 map {
		     {xref_key=>$_}
		 } @{$constr->{product_accs}}
		 ];
	    delete $constr->{product_accs};
	}

	# use same code for product/products
	if ($constr->{products}) {
	    $constr->{product} = $constr->{products};
	    delete $constr->{products};
	}

	# constrain terms by products
	if ($constr->{product}) {

            # include the products
            # constrained upon to the
            # term objects
            $add_select_assocs = 1;

	    # join gene_product, association
	    # and (optionally) dbxref
	    my $prods = [$constr->{product}];
	    if (ref($constr->{product}) eq "ARRAY") {
		$prods = $constr->{product};
	    }
	    if (!@$prods) { $prods = [{id=>0}] } # fake this for empty list
	    my @orq = ();
	    my $use_dbxref_table = 0;
	    my $use_tax_table = 0;

	    ## COMMENTARY: The whole filter thing seems to be broken,
	    ## try another way...
#             my $constr_sp = $constr->{speciesdb}
# 	      || ($self->filters && $self->filters->{speciesdb});
#             my $constr_taxid = $constr->{taxid}
# 	      || ($self->filters && $self->filters->{taxid});
            my $constr_sp = $constr->{speciesdb}
	      || $self->_try_filter("speciesdb");
            my $constr_taxid = $constr->{taxid}
	      || $self->_try_filter("taxid");

            if ($constr_sp) {
                if (!ref($constr_sp)) {
                    $constr_sp = [$constr_sp];
                }
                my @yes = grep {$_ !~ /^\!/} @$constr_sp;
                my @no = grep {/^\!/} @$constr_sp;
                map {s/^\!//} @no;
                $use_dbxref_table = 1;
                if (@yes) {
                    push(@where_arr,
                         "gp_dbxref.xref_dbname in ".
                         "(".join(", ", map {sql_quote($_)} @yes).")");
                }
                if (@no) {
                    push(@where_arr,
                         "gp_dbxref.xref_dbname not in ".
                         "(".join(", ", map {sql_quote($_)} @no).")");
                }
                delete $constr->{speciesdb};
            }

            if ($constr_taxid) {
                if (!ref($constr_taxid)) {
                    $constr_taxid = [$constr_taxid];
                }
                my @yes = grep {$_ !~ /^\!/} @$constr_taxid;
                my @no = grep {/^\!/} @$constr_taxid;
                map {s/^\!//} @no;
                $use_tax_table = 1;
                if (@yes) {
                    push(@where_arr,
                         "gp_tax.ncbi_taxa_id in ".
                         "(".join(", ", @yes).")");
		}
		if (@no) {
                    push(@where_arr,
                         "gp_tax.ncbi_taxa_id in ".
                         "(".join(", ", @no).")");
                }
                delete $constr->{taxid};
            }
            
            #evidence
	    ## Filtering problems may show-up here too, trying to head
	    ## them off a the pass with _try_filter...
#             my $constr_ev = $constr->{evcodes} || $self->filters->{evcodes}
#                          || $constr->{evcode} || $self->filters->{evcode};
            my $constr_ev = $constr->{evcodes}
	      || $self->_try_filter("evcodes")
		|| $constr->{evcode}
		  || $self->_try_filter("evcode");
            if ($constr_ev) {
                # hmm we have some redundant code here;
                # doing the same kind of thing as get_products
                # i think a little redundancy is ok balanced against
                # even more complex o/r code
                if (!ref($constr_ev)) {
                    $constr_ev = [$constr_ev];
                }
                my @yes = grep {$_ !~ /^\!/} @$constr_ev;
                my @no = grep {/^\!/} @$constr_ev;
                map {s/^\!//} @no;
                if (@$constr_ev) {
                    push(@table_arr, "evidence");
                    push(@where_arr,
                         "evidence.association_id = association.id");
                }
                if (@yes) {
                    push(@where_arr,
                         "evidence.code in ".
                         "(".join(", ", map {sql_quote($_)} @yes).")");
                }
                if (@no) {
                    push(@where_arr,
                         "evidence.code not in ".
                         "(".join(", ", map {sql_quote($_)} @no).")");
                }
                delete $constr->{evcodes};
                delete $constr->{evcode};
                #constrain by evidence dbxrefs (only support id for now)
                #reasoning: get terms in association for certain evidence
                #e.g. evidence from one experiment
                #only make sense when constrained by product and evidence code?
                if ($constr->{evidence_dbxref}) {
                    $constr->{evidence_dbxrefs} = $constr->{evidence_dbxref};
                    delete $constr->{evidence_dbxref};
                }
                if ($constr->{evidence_dbxrefs}) {
                    my $e_dbxrefs = $constr->{evidence_dbxrefs};
                    delete $constr->{evidence_dbxrefs};
                    $e_dbxrefs = [$e_dbxrefs] unless (ref($e_dbxrefs) eq 'ARRAY');
                    if ($e_dbxrefs->[0] =~ /^\d+$/) {
                        push(@where_arr,
                             "evidence.dbxref_id in ".
                             "(".join(', ', @$e_dbxrefs).")");
                    } else {
                        confess("Support evidence dbxref id only for now ".$e_dbxrefs->[0]);
                    }
                }
            }
	    foreach my $prod (@$prods) {
		if (!ref($prod)) {
		    $prod = {"symbol"=>$prod};
		}
#                if (ref($prod) eq "HASH") {
#                    $prod = $self->create_gene_product_obj($prod);
#                }
		my @w = ();
                my %phash;
                
                # if the user is passing in a product object,
                # use the ID
                # otherwise use the keys they pass in
                if (ref($prod) ne "HASH") {
                    $prod->isa("GO::Model::GeneProduct") || 
                      confess("assertion error");
                    if ($prod->id) {
                        %phash = (id=>$prod->id);
                    }
                    else {
                        %phash = (xref=>$prod->xref);
                    }
                }
                else {
                    %phash = %$prod;
                }
		map {

		    if (/^synonym$/) {
			my $syn = $prod->{$_};
			my $op = '=';
			if ($syn =~ /\*/) {
			    $syn =~ s/\*/\%/g;
			    $op = 'like';
			}
			$syn = sql_quote($syn);
			push(@table_arr,
			     "gene_product_synonym");
			push(@w,
			     "gene_product_synonym.gene_product_id = gene_product.id",
			     "gene_product_synonym.product_synonym $op $syn");
		    }
		    elsif (/^acc$/ || /^xref$/) {
                        if (ref($prod->{$_})) {
                            push(@w,
                                 "gp_dbxref.xref_key = ".
                                 sql_quote($prod->{$_}->{xref_key}),
                                 "gp_dbxref.xref_dbname = ".
                                 sql_quote($prod->{$_}->{xref_dbname}),
                                );
                        }
                        else {
                            push(@w,
                                 "gp_dbxref.xref_key = ".
                                 sql_quote($prod->{$_}))
                        }
			$use_dbxref_table = 1;
		    }
		    elsif (/^xref/) {
			push(@w,
			     "gp_dbxref.$_ = ".sql_quote($prod->{$_}));
			$use_dbxref_table = 1;
		    }
		    else {
                        if (/apph/) {
                            # skip non queryable/peristent fields
                        }
                        else {
                            my $op = "=";
                            my $val = $prod->{$_};
                            if ($val =~ /\*/) {
                                $val =~ s/\*/\%/g;
                                $op = "like";
                            }
                            push(@w, "gene_product.$_ $op ".sql_quote($val));
                        }
		    }
		} keys %phash;
		my $q =
		    join(" AND ", @w);
		if (scalar(@w) > 1) {
		    push(@orq, "($q)");
		}
		else {
		    push(@orq, "$q");
		}
	    }
            if (defined $constr->{is_not}) {
                push(@where_arr, "association.is_not=$constr->{is_not}");
                delete $constr->{is_not};
            }
	    push(@table_arr,
		 qw(association gene_product));
	    push(@where_arr, 
		 ("term.id = association.term_id",
		  "gene_product.id = association.gene_product_id"));
	    push(@where_arr, 
		 "(".join(" OR ", @orq).")"); #This is like assertion for a unconstrained query: error in sql stmt
	    if (1 || $use_dbxref_table) {
		push(@table_arr,
		     "dbxref gp_dbxref");
		push(@where_arr, 
		     "gene_product.dbxref_id = gp_dbxref.id");
	    }
	    if ($use_tax_table) {
		push(@table_arr,
		     "species AS gp_tax");
		push(@where_arr, 
		     "gene_product.species_id = gp_tax.id");
	    }
            # SHULY Nov 30, 04 - added gene_product.type_id to the select statement
            push(@select_arr,
                 "association.id a_id",
                 "association.term_id",
                 "association.gene_product_id",
                 "association.is_not",
                 "association.assocdate",
                 "association.role_group",
                 "association.source_db_id",
                 "gp_dbxref.xref_key gp_xref_key",
                 "gp_dbxref.xref_dbname gp_xref_dbname",
                 "gene_product.symbol",
                 "gene_product.type_id",
                 "gene_product.species_id",
                 "gene_product.full_name");

	    delete $constr->{product};
	}

        
        if ($constr->{synonym}) {
            $constr->{term_synonym} = $constr->{synonym};
            delete $constr->{synonym};
        }
        if ($constr->{type}) {
            $constr->{term_type} = $constr->{type};
            delete $constr->{type};
        }
	my $ttype = $constr->{term_type};
        if ($ttype) {
            delete $constr->{term_type};
            $ttype = [$ttype] unless (ref($ttype) eq 'ARRAY');
            push @where_arr, "term.term_type IN (".join(",",map{sql_quote($_)}@$ttype).")";
        }
	# guess any unconsumed keys
	if (keys %$constr) {
	    push(@where_arr, 
		 map {"$_ = ".sql_quote($constr->{$_})} grep {defined($constr->{$_})} keys %$constr);
	}

	# do the dynamically generated sql
	if ($ENV{GO_EXPERIMENTAL_OJ}) {
	    # question: we could get dbxrefs/syns/defs here
	    # using left outer joins;
	    # but is it any more efficient?
	    my @ojs =
		("term_synonym ts",
		 "term_definition td",
		 "term_dbxref tx");
	    push(@table_arr,
		 map {/(\w+)$/;
		      "left outer join $_ on $1.term_id=term.id"
		      } @ojs,
		 "dbxref dbx");
	    push(@where_arr,
		 "dbx.id=tx.dbxref_id");
	    # todo - use these rows
	}
        # -- end of experimental section --

        if ($add_select_assocs) {
        }

	$hl =
          select_hashlist($dbh,
                          \@table_arr,
                          \@where_arr,
                          \@select_arr);
    }

    my @term_l = ();

    my @term_lookup = (); # use array as lookup for speed
    my @term_ids = ();
    foreach my $h (@$hl) {
        my $term = $term_lookup[$h->{id}];
        if (!$term) {
            $term = $self->create_term_obj($h);
            $term_lookup[$term->id] = $term;
            my $id = $term->id;
            push(@term_ids, $id);
            push(@term_l, $term);
        }
    }

    # if the search was constrained by assocs/products,
    # adorn the term with selected products
    if ($add_select_assocs) {
        # we have associations
        my $assocs = $self->create_assocs_from_hashlist($hl, $constr);
        foreach my $assoc (@$assocs) {
            $term_lookup[$assoc->{term_id}]->add_selected_association($assoc);
        }
	# remove terms without assocs
	# (rational: we got here by doing a product-based query;
	#  some of these should be filtered as the evidence didnt
	#  match the criteria)
	@term_l = ();
	foreach (@term_ids) {
	    my $term = $term_lookup[$_];
	    if (@{$term->selected_association_list || []}) {
		push(@term_l, $term);
	    }
	}
	@term_ids = map {$_->id} @term_l;
    }

    # now lets populate our GO::Model::Term objects with
    # other adornments such as synonyms, dbxrefs and defintions
    # (it's faster to do this as a seperate step; unfortunately we
    #  can't do *everything* in a single SQL statement as this would
    #  require outer joins, as the relationship of term to other tables
    #  is ZERO to many)
    if (@term_ids) {
        my @where = ();
	if (%$template && $template->{association_list}) {
            # clear old associations
            map { $_->association_list( [] ) } @term_l;
	    my $al=
		$self->get_direct_associations(\@term_l);
	    
            map {
                $term_lookup[$_->{term_id}]->add_association($_);
            } @$al;
            if ($with_associations) {
                # weed out all terms without assocs
                @term_ids = 
                  grep { @{$term_lookup[$_]->association_list} } @term_ids;
                @term_l =
                  grep { @{$_->association_list} } @term_l;
            }
	}
        unless ($fetch_all) {
            @where = ("term_id in (".join(", ", @term_ids).")");
        }
        if ($fetch_all) {
            $self->{_term_count} = scalar(@term_ids);
        }
        if (!%$template || $template->{synonym_list}) {
            my $sl =
              select_hashlist($dbh,
                              "term_synonym LEFT OUTER JOIN term ON (term.id=term_synonym.synonym_type_id)",
                              \@where,
                              "term_id, term_synonym, term.name AS type");
            map {
                $term_lookup[$_->{term_id}] && $term_lookup[$_->{term_id}]->add_synonym_by_type($_->{type}, $_->{term_synonym});
            } @$sl;
        }
        if (!%$template || $template->{subset_list}) {
            my $sl =
              select_hashlist($dbh,
                              "term_subset LEFT OUTER JOIN term AS subset ON (subset.id=term_subset.subset_id)",
                              \@where,
                              "term_id, subset.acc AS subset_acc");
            map {
                $term_lookup[$_->{term_id}] && $term_lookup[$_->{term_id}]->add_subset($_->{subset_acc});
            } @$sl;
        }
	if (!%$template || $template->{definition}) {
	    my $dl =
	      select_hashlist($dbh,
                              "term_definition",
                              \@where);
            map {
                if ($term_lookup[$_->{term_id}]) {
                    $term_lookup[$_->{term_id}]->definition($_->{term_definition});
                    $term_lookup[$_->{term_id}]->comment($_->{term_comment});
                }
	    } @$dl;
	}
	if (!%$template || $template->{n_deep_products}) {
            my $c = {per_term=>1};
            unless ($fetch_all) { $c->{terms} = \@term_l }
	    my $countl =
              $self->get_deep_product_count($c);
	    
            map {
                if ($term_lookup[$_->{term_id}]) { 
                    $term_lookup[$_->{term_id}]->n_deep_products($_->{"c"});
                }
            } @$countl;
	}
	if (%$template && $template->{n_products}) {
	    my $countl =
              $self->get_product_count({terms=>\@term_l,
                                        per_term=>1});
	    
            map {
                if ($term_lookup[$_->{term_id}]) { 
                    $term_lookup[$_->{term_id}]->n_products($_->{"c"});
                }
            } @$countl;
	}
	if (%$template && $template->{n_associations}) {
	    my $al =
	      select_hashlist($dbh,
                              "association",
                              \@where,
                              "term_id, count(association.id) AS n",
                              undef,
                              "term_id",
                             );
            map {
                $term_lookup[$_->{term_id}]->n_associations($_->{"n"});
            } @$al;
	}
	if (!%$template || $template->{dbxref_h} || $template->{dbxref_list}) {
	    my $xl=
	      select_hashlist($dbh,
			      ["term_dbxref", "dbxref"],
			      [@where,
			       "term_dbxref.dbxref_id = dbxref.id"],
			      ["dbxref.*", "term_id", "is_for_definition"],
                              ["dbxref.xref_dbname", "dbxref.xref_key", "dbxref.xref_desc", "is_for_definition"]);
	    map {	
		my $isdef = $_->{is_for_definition};
		delete $_->{is_for_definition};
		my $term = $term_lookup[$_->{term_id}];
		if ($isdef) {
		    $term->add_definition_dbxref(GO::Model::Xref->new($_));
		}
		else {
		    $term->add_dbxref(GO::Model::Xref->new($_));
		}
	    } @$xl;
	}
    }

    return \@term_l;
}

sub get_terms_by_product_symbols {
    my $self = shift;
    my $dbh = $self->dbh;

    my ($syms, $constr, $template) =
      rearrange([qw(symbols constraints template)], @_);
    $constr = $constr || {};
    $constr->{products} = $syms;
    return $self->get_terms($constr, $template);
    
}

sub get_term_by_acc {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($acc, $attrs) =
      rearrange([qw(acc attributes)], @_);
    return $self->get_term({acc=>$acc});
}

sub get_terms_by_search {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($search, $attrs) =
      rearrange([qw(search attributes)], @_);
    return $self->get_terms({search=>$search});
}

sub get_root_terms {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($template) =
      rearrange([qw(template)], @_);

    my $roots =
      $self->get_terms(-constraints=>{is_root=>1},
                       -template=>$template);
    if (!$roots || !@$roots) {
        my $hl =
          select_hashlist($dbh,
                          "term LEFT OUTER JOIN term2term ON (term.id=term2term.term2_id)",
                          ["term2term.term1_id IS NULL","term.is_obsolete=0"],
                          "term.acc, term.id");
        my @accs = ();
        foreach (@$hl) {
            push(@accs, $_->{acc});
        }
        update_h($dbh,
                 "term",
                 {is_root=>1},
                 sqlin("id", [map {$_->{id}} @$hl]));
        $roots = $self->get_terms(-constraints=>{acc=>[@accs]},
                                  -template=>$template);
    }
    return $roots;
}

sub get_root_term {
    my $self = shift;
    my $roots = $self->get_root_terms(@_);
    if (@$roots == 0) {
        confess("no root term!");
    }
    elsif (@$roots > 1) {
      #printf STDERR "%s\n", $_->name foreach @$roots;
      confess("multiple root terms!");
    }
    else {
        return $roots->[0];
    }
}

sub get_ontology_root_terms {
    my $self = shift;
    my $dbh = $self->dbh;

    # yuck, ugly way to do this,
    # but mysql has no subselects
    # so this is the easiest way for now
    #my $root = $self->get_root_term(@_);
    #return $self->get_child_terms($root, @_);
    my $roots = $self->get_root_terms();
    if (@$roots ==1 && $roots->[0]->acc eq 'all') {
        return $self->get_child_terms($roots->[0], @_);
    }
    return $roots;
}

sub get_db {
    my $dbs = shift->get_dbs(@_);
    return $dbs->[0];
}

sub get_dbs {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($inconstr) =
      rearrange([qw(constraints)], @_);
    my $constr = pset2hash($inconstr);
    my $hl = 
      select_hashlist($dbh,
                      "db",
                      $constr);
    my @dbs = map {$self->create_db_obj($_)} @$hl;
    return \@dbs;
}

sub get_association_count {
    my $self = shift;
    my ($termh, $constr, $templ, $o) =
      rearrange([qw(term constraints template options)], @_);
    $self->get_direct_associations($termh, $constr, $templ, {count=>1, %{$o||{}}});
}

sub get_direct_associations {
    my $self = shift;
    my ($termh, $constr, $templ, $o) =
      rearrange([qw(term constraints template options)], @_);
    $self->get_associations($termh, $constr, $templ, {direct=>1, %{$o||{}}});
}

sub get_all_associations {
    my $self = shift;
    my ($termh, $constr, $templ, $o) =
      rearrange([qw(term constraints template options)], @_);
    $self->get_associations($termh, $constr, $templ, {direct=>0, %{$o||{}}});
}

sub get_associations {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($termh, $constr, $templ, $options) =
      rearrange([qw(term constraints template options)], @_);
    if (!$templ) {$templ = {};}
    if (!$options) {$options = {};}
    if (!$constr) {$constr = {};}
    my $terms;
    if (ref($termh) eq "ARRAY") {
	$terms= $termh;
    }
    elsif (ref($termh) eq "HASH" || !$termh->id) {
	my $tt = $templ->{term_template} || "shallow";
	$terms= $self->get_terms($termh, $tt);
    }
    else {
	$terms = [$termh];
    }
    my @term_ids = map {$_->id} @$terms;
    if ($constr->{all}) {
	my $hl=select_hashlist($dbh, "term", undef, "id");
	@term_ids = map {$_->{id}} @$hl;
    }
    if (!$options->{direct}) {
	# make sure the subgraph doesnt get associations or 
	# we get into a cycle!
	my $g = 
	    $self->get_graph_by_terms($terms, 
				  -1, 
				  {traverse_up=>0,
				   terms=>{acc=>1,id=>1}});
	@term_ids = map {$_->id} @{$g->get_all_nodes || []};
    }
    
    my $tc = "term_id in (".join(", ", @term_ids).")";

    # SHULY Nov 28, 04 - added the term table to the query to select the gene_product type_id name.
    my @tables =
      ("association", 
       "gene_product", 
        # SHULY added term table
    #   "term", 
       "dbxref", 
       "evidence", 
       "dbxref evdbxref");
    my @where =
      ($tc,
       "gene_product.id = association.gene_product_id",
       # SHULY - match the type_id of gene_product to it's name in the term table
    #   "gene_product.type_id = term.id",
       "dbxref.id = gene_product.dbxref_id",
       "evidence.association_id = association.id",
       "evidence.dbxref_id = evdbxref.id");
    my @cols =
      ("association.id",
       "association.term_id",
       "association.gene_product_id ",
       "association.is_not",
       "association.assocdate",
       "association.role_group",
       "association.source_db_id",
       "dbxref.xref_key",
       "dbxref.xref_dbname",
       "gene_product.symbol",
       "gene_product.full_name",
       "gene_product.species_id",
       "gene_product.type_id",
       # SHULY - retrieve the name of the gene_product type
    #   "term.name",
       "evidence.id AS ev_id",
       "evidence.code",
       "evidence.seq_acc",
       "evdbxref.xref_key AS evdbxref_acc",
       "evdbxref.xref_dbname AS evdbxref_dbname",
      );


    my $filters = $self->filters || {};
    my $spdbs = 
      $constr->{speciesdb} ||
        $constr->{speciesdbs} ||
          $filters->{speciesdb} ||
            $filters->{speciesdbs};

    if ($spdbs) {
        if (!ref($spdbs)) {
            $spdbs = [$spdbs];
        }

	my @wanted = grep {$_ !~ /^\!/} @$spdbs;
	my @unwanted = grep {/^\!/} @$spdbs;

	if (@wanted) {
	    push(@where,
		 "(".join(" OR ", 
			  map{"dbxref.xref_dbname=".sql_quote($_,1)} @wanted).")");
	}
	if (@unwanted) {
	    push(@where,
		 map{"dbxref.xref_dbname!=".sql_quote(substr($_,1))} @unwanted);
	}
    }
    #subset assoc constr by product ids
    my $pids = $constr->{product_ids} || $constr->{product_id};
    if ($pids) {
        $pids = [$pids] unless (ref($pids));
        push @where,
          "gene_product.id IN (".join(",", @$pids).")";
    }
    # NCBI Taxa IDs
    my $taxids = 
      $constr->{taxid} ||
        $constr->{taxids} ||
          $filters->{taxid} ||
            $filters->{taxids};

    if ($taxids) {
        if (!ref($taxids)) {
            $taxids = [$taxids];
        }

	push(@tables, "species");
	push(@where, "species.id = gene_product.species_id");

	my @wanted = grep {$_ !~ /^\!/} @$taxids;
	my @unwanted = grep {/^\!/} @$taxids;

	if (@wanted) {
	    push(@where,
		 "(".join(" OR ", 
			  map{"species.ncbi_taxa_id=$_"} @wanted).")");
	}
	if (@unwanted) {
	    push(@where,
		 map{"species.ncbi_taxa_id!=$_"} @unwanted);
	}
    }

    # Secondary NCBI Taxa IDs
    # for multi-species interactions
    my $qualifier_taxids = 
      $constr->{qualifier_taxid} ||
        $constr->{qualifier_taxids} ||
          $filters->{qualifier_taxid} ||
            $filters->{qualifier_taxids};

    if ($qualifier_taxids) {
        if (!ref($qualifier_taxids)) {
            $qualifier_taxids = [$qualifier_taxids];
        }

	push(@tables, "species AS qualifier_species");
	push(@tables, "association_species_qualifier");
	push(@where, "qualifier_species.id = association_species_qualifier.species_id");
	push(@where, "association_species_qualifier.association_id = association.id");

	my @wanted = grep {$_ !~ /^\!/} @$qualifier_taxids;
	my @unwanted = grep {/^\!/} @$qualifier_taxids;

	if (@wanted) {
	    push(@where,
		 "(".join(" OR ", 
			  map{"qualifier_species.ncbi_taxa_id=$_"} @wanted).")");
	}
	if (@unwanted) {
	    push(@where,
		 map{"qualifier_species.ncbi_taxa_id!=$_"} @unwanted);
	}
    }

    if ($constr->{acc}) {
        push(@where,
             "dbxref.xref_acc = ".sql_quote($constr->{acc}));
    }

    if ($options->{count}) {
	@cols = "count(distinct association.id) n"
    }


    my $evcodes = $constr->{evcodes} || $filters->{evcodes} || $constr->{evcode} || $filters->{evcode};
    my @w=();
    if ($evcodes) {
# often $evcodes points to an empty ARRAY
# added extra check, this removed unneeded join on evidence table.
# jmc Oct2006
	if (defined(@$evcodes)) {
	    my @wanted = grep {$_ !~ /^\!/} @$evcodes;
	    my @unwanted = grep {/^\!/} @$evcodes;
        
	    if (@wanted) {
		push(@w,
		     "(".join(" OR ", 
			      map{"evidence.code=".sql_quote($_,1)} @wanted).")");
	    }
	    if (@unwanted) {
		push(@w,
		     map{"evidence.code!=".sql_quote(substr($_,1))} @unwanted);
	    }
	    unshift(@where,
		    "evidence.association_id = association.id", 
		    @w);
	}
    }

    my $hl = 
	select_hashlist($dbh, \@tables, \@where, \@cols);

    if ($options->{count}) {
	return $hl->[0]->{n};
    }

    if (!@$hl) {
	return [];
    }

### TODO - this code is duplicated in create_assocs_from_hashlist
######### OLD    my $assocs = $self->create_assocs_from_hashlist($hl, $constr);

    my @assocs = ();
    my @assoc_lookup = ();
    my @assoc_ids = ();
    my %gph = ();
    foreach my $h (@$hl) {
        if ($h->{a_id}) {
            $h->{id} = $h->{a_id};
        }
        if ($h->{gp_xref_key}) {
            $h->{xref_key} = $h->{gp_xref_key};
        }
        if ($h->{gp_xref_dbname}) {
            $h->{xref_dbname} = $h->{gp_xref_dbname};
        }
        my $assoc = $assoc_lookup[$h->{id}];
        if (!$assoc) {
            $assoc =
              $self->create_association_obj($h);
            $assoc_lookup[$assoc->id] = $assoc;
            $assoc->{term_id} = $h->{term_id};
            push(@assoc_ids, $assoc->id);
            push(@assocs, $assoc);
        } 


        my $gp = $assoc->gene_product;
        if (!$gp || !$gp->id){
            dd($h);confess("assertion err")
        }
        $gph{$gp->id} = $gp;

        my $ev = GO::Model::Evidence->new({
                                           code=>$h->{code},
                                           seq_acc=>$h->{seq_acc},
                                          });
        $ev->id($h->{ev_id});
        $ev->xref(GO::Model::Xref->new({xref_key=>$h->{evdbxref_acc},
                                        xref_dbname=>$h->{evdbxref_dbname}}));
        $assoc->add_evidence($ev);

        if (!$assoc->gene_product || !$assoc->gene_product->id){
            dd($h);confess("assertion err")
        }
    }


    # make sure objects are shared
    foreach my $assoc (@assocs) {
	my $gp = $gph{$assoc->gene_product->id};
	if (!$gp) {
	    confess("no gp for $assoc ".$assoc->gene_product->id);
	}
        $assoc->gene_product($gp);
    }

    if (!@assoc_ids) {
        return [];
    }
    my @pl;
    map {
        my $p = $_->gene_product;
        push (@pl, $p) if ($p);
    } @assocs;
    if ($self->has_gp_property_table) {
        $self->_get_product_property(\@pl);
    }
    $self->_get_product_seqs(\@pl);
    $self->_get_product_synonyms(\@pl);
    $self->_get_product_species(\@pl);
    $self->_get_product_types(\@pl);
    $self->_get_evidence_seq_xrefs(\@assocs);
    $self->get_qualifiers(\@assocs);
    $self->get_assigned_by(\@assocs);

    return \@assocs;
}

sub create_assocs_from_hashlist {
    my $self = shift;
    my $dbh = $self->dbh;
    my $hl = shift;
    my $constr = shift || {};

    my @assocs = ();
    my @assoc_lookup = ();
    my @assoc_ids = ();
    my %gph = ();
    foreach my $h (@$hl) {
        if ($h->{a_id}) {
            $h->{id} = $h->{a_id};
        }
        if ($h->{gp_xref_key}) {
            $h->{xref_key} = $h->{gp_xref_key};
        }
        if ($h->{gp_xref_dbname}) {
            $h->{xref_dbname} = $h->{gp_xref_dbname};
        }
	my $assoc = $assoc_lookup[$h->{id}];
        if (!$assoc) {
            $assoc =
              $self->create_association_obj($h);
#            printf STDERR "NEW ASSOC $h->{id} %s\n", $assoc->id;
            $assoc_lookup[$assoc->id] = $assoc;
            $assoc->{term_id} = $h->{term_id};
            push(@assoc_ids, $assoc->id);
#            push(@assocs, $assoc);  don't include yet - wait for evidence
        }
        my $gp = $assoc->gene_product;
	if (!$gp){
            dd($h);confess("assertion err")
        }
        $gph{$gp->id} = $gp;
    }

    if (!@assoc_ids) {
        return [];
    }
    my $idq = "association_id in (".join(", ", @assoc_ids).")";

#    join(" OR ", map{"association_id = $_"} @assoc_ids);


    # filter based on evidence if requested
    my $filters = $self->filters || {};
    my $evcodes = $constr->{evcodes} || $filters->{evcodes}
               || $constr->{evcode} || $filters->{evcode};
    my @w=();
    if ($evcodes) {
# often $evcodes points to an empty ARRAY
# added extra check, this removed unneeded join on evidence table.
# jmc Oct2006
	if (defined(@$evcodes)) {
	    my @wanted = grep {$_ !~ /^\!/} @$evcodes;
	    my @unwanted = grep {/^\!/} @$evcodes;

	    if (@wanted) {
		push(@w,
		     "(".join(" OR ", 
			      map{"code=".sql_quote($_,1)} @wanted).")");
	    }
	    if (@unwanted) {
		push(@w,
		     map{"code!=".sql_quote(substr($_,1))} @unwanted);
	    }
	}
    }

    my $el = select_hashlist
      ($dbh,
       ["evidence", "dbxref"],
       ["($idq)",
        @w,
        "evidence.dbxref_id = dbxref.id"],
       ["evidence.id AS ev_id","evidence.code","evidence.seq_acc",
        "evidence.association_id","evidence.dbxref_id","dbxref.*"],
      );

    my %ev_h;
    foreach my $evh (@$el) {
        my $assoc =
          $assoc_lookup[$evh->{association_id}];
        if (!@{$assoc->evidence_list || []}) {
            # only include an association if
            # it has allowable evidence
            push(@assocs, $assoc);
#            printf STDERR "AND ANOTHER ASSOC %s (%s)\n", $assoc->id, $evh->{association_id};
        }
        my $ev = GO::Model::Evidence->new($evh);
        $ev->id($evh->{ev_id});
        $ev->xref(GO::Model::Xref->new($evh));
        $assoc->add_evidence($ev);
    }
    # make sure objects are shared
    foreach my $assoc (@assocs) {
        $assoc->gene_product($gph{$assoc->gene_product->id});
    }

    my %ph = ();
    my @pl;
    foreach (@assocs) {
	$ph{$_->gene_product->id} = $_->gene_product;
    }
    my @ps = values %ph;
    $self->_get_product_species(\@ps);
    $self->_get_product_seqs(\@ps);
    $self->_get_product_synonyms(\@ps);
    $self->_get_product_types(\@ps);
    $self->_get_evidence_seq_xrefs(\@assocs);
    $self->get_qualifiers(\@assocs);
    $self->get_assigned_by(\@assocs);

    return \@assocs;
}

sub get_relationships {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($constr) =
      rearrange([qw(constraints)], @_);

    my @constr_arr = ();
    if (!ref($constr)) {
	confess("constraints must be hashref!");
#	$constr = {"term.name"=>$constr};
    }

    # allow parent/child to be used as synonym for acc1/acc2
    map {
	if ($_ eq "parent") {
	    $constr->{acc1} = $constr->{$_}->acc;
	    delete $constr->{$_};
	}
	if ($_ eq "child") {
	    $constr->{acc2} = $constr->{$_}->acc;
	    delete $constr->{$_};
	}
	if ($_ eq "parent_acc") {
	    $constr->{acc1} = $constr->{$_};
	    delete $constr->{$_};
	}
	if ($_ eq "child_acc") {
	    $constr->{acc2} = $constr->{$_};
	    delete $constr->{$_};
	}
    } keys %$constr;

    my @tables = ("term2term", "term term1", "term term2");
    push(@constr_arr,
	 "term1.id = term2term.term1_id");
    push(@constr_arr,
	 "term2.id = term2term.term2_id");
    my @select_arr = "term2term.*";
    push(@select_arr, "term1.acc AS acc1");
    push(@select_arr, "term2.acc AS acc2");
    if ($constr->{acc1}) {
	push(@constr_arr,
	     "term1.acc = ".sql_quote($constr->{acc1}));
	delete $constr->{acc1};
    }
    if ($constr->{acc2}) {
	push(@constr_arr,
	     "term2.acc = ".sql_quote($constr->{acc2}));
	delete $constr->{acc2};
    }
    push(@constr_arr, 
	 map {"$_ = ".sql_quote($constr->{$_})} keys %$constr);
    my $hl=
      select_hashlist($dbh,
		      \@tables,
		      \@constr_arr,
		      \@select_arr);
    foreach my $h (@$hl) {
        # support old and new ways of
        # doing rtypes
        if ($h->{relationship_type_id}) {
            $self->refresh unless $self->{rtype_by_id};
            $h->{type} =
              $self->{rtype_by_id}->{$h->{relationship_type_id}};
            delete $h->{relationship_type_id};
        }
    }
    my @rels =
      map {GO::Model::Relationship->new($_)} @{$hl};
    return \@rels;
}

sub get_parent_terms {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($node) =
      rearrange([qw(term)], @_);
    my @constr_arr = ();
    my @tables = ("term2term", "term term1", "term term2");
    push(@constr_arr,
	 "term2.id = term2term.term2_id",
	 "term1.id = term2term.term1_id");
    my @select_arr = "term1.*";
    push(@constr_arr,
	 "term2.acc = ".sql_quote($node->{acc}));
    my $hl=
      select_hashlist($dbh,
		      \@tables,
		      \@constr_arr,
		      \@select_arr);
    my @terms =
      map {$self->create_term_obj($_)} @{$hl};
    return \@terms;
}

sub get_child_terms {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($node, $template) =
      rearrange([qw(term template)], @_);
    my @constr_arr = ();
    my @tables = ("term2term", "term term1", "term term2");
    push(@constr_arr,
	 "term2.id = term2term.term2_id",
	 "term1.id = term2term.term1_id");
    my @select_arr = "term2.*";
    if (!ref($node) && int($node)) {
        $node = {acc=>$node};
    }
    $node->{acc} || confess("must specify valid obj/hash - you said $node");
    push(@constr_arr,
	 "term1.acc = ".sql_quote($node->{acc}));
    my $hl=
      select_hashlist($dbh,
		      \@tables,
		      \@constr_arr,
		      \@select_arr);
    my @terms =
      map {$self->create_term_obj($_)} @{$hl};
    return \@terms;
}

sub get_node_graph {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($acc, $max_depth, $template, $termh) =
      rearrange([qw(acc depth template termh)], @_);
    $template = $template || {};
    my $term_template = $template->{terms} || undef;

    my $term;
    # be liberal in what we accept:
    # either an accession as an ID or
    # a query constraint hash
    $acc = $acc || $termh;
    if (!$acc) {
        # no acc specified - get graph from root
        $term = $self->get_root_term(-template=>$term_template);
    }
    elsif (ref($acc)) {
        $term = $self->get_term($acc, $term_template);
    }
    else {
        $term = $self->get_term({acc=>$acc}, $term_template);
    }
    my @terms = ($term);
    if (!$term) {@terms = () }
    my $graph = 
      $self->get_graph_by_terms([@terms], $max_depth, $template);
    return $graph;
}

sub graph_template {
    my $self = shift;
    my $t = shift || {};
    if ($t eq "all") {
        $t = {terms=>"all"};
    }
    return $t;
}

sub get_graph {
    my $self = shift;
    my $dbh = $self->dbh;
    $self->get_node_graph(@_);
}

sub extend_graph {
    my $self = shift;

    my ($graph, $acc, $max_depth, $template, $termh) =
      rearrange([qw(graph acc depth template termh)], @_);
    my $g2 = $self->get_graph(
                              -acc=>$acc,
                              -depth=>$max_depth,
                              -template=>$template,
                              -termh=>$termh,
                             );

    $graph->merge($g2);
}

sub get_graph_below {
    my $self = shift;
    my ($acc, $max_depth, $template, $termh) =
      rearrange([qw(acc depth template termh)], @_);
    my %t = %{$self->graph_template($template) || {}};
    $t{traverse_down} = 1;
    $t{traverse_up} = 0;
    $self->get_node_graph($acc, $max_depth, \%t, $termh);
}

sub get_graph_above {
    my $self = shift;
    my ($acc, $max_depth, $template, $termh) =
      rearrange([qw(acc depth template termh)], @_);
    my %t = %{$self->graph_template($template) || {}};
    $t{traverse_down} = 0;
    $t{traverse_up} = 1;
    $self->get_graph($acc, $max_depth, \%t, $termh);
}

#get graph for a term which show its direct parents, all siblings and all children
sub get_graph_DPSC {
    my $self = shift;
    my ($term,$template, $termh) =
      rearrange([qw(term template termh)], @_);

    unless (ref($term) eq 'ARRAY') {
        $term = [$term];
    }
    unless ($term->[0]->isa('GO::Model::Term')) {
        $term = $self->get_terms({acc=>$term});
    }
    my $accs = join(",",map{sql_quote($_->acc)}@$term);
    my %t = %{$self->graph_template($template) || {}};
    my $hl = select_hashlist($self->dbh,["term p", "term2term t2t", "term c"],
                             ["p.id=t2t.term1_id", "c.id=t2t.term2_id", "c.acc IN ($accs)"],
                             "p.*");
    $t{traverse_down} = 1;
    $t{traverse_up} = 0;
    $t{no_n_children} = 1;
    #graph with term's parents and parents' immediate children (depth = 1)
    my $p_terms = $self->get_terms({acc=>[map{$_->{acc}}@{$hl || []}]}) if (@{$hl || []});
    #my $graph = $self->get_graph_by_terms($p_terms, 1, \%t, $termh);
    my @all_terms = @{$p_terms || []};
    push @all_terms, @{$termh->{open_terms} || []};
    #add seeded term so its children are shown and theirs if expanded
    push @all_terms, @$term;
    #printf STDERR "all t num: %d\n",scalar(@all_terms);
    my $graph = $self->get_graph_by_terms_on_path(\@all_terms,$p_terms, \%t);
    #graph for all children of the term (depth = -1)
    #all children is too overwhelming, this is done above
#    my $graph2 = $self->get_graph_by_terms($term, 1, \%t);
#    $graph->merge($graph2);
    $graph->focus_nodes([@$term, @all_terms]);
    $self->_get_n_children_h($graph);
    return $graph;
}

sub get_graph_by_acc {
    my $self = shift;
    my $dbh = $self->dbh;
    $self->get_node_graph(@_);
}


##
sub get_graph_by_terms {
    my $self = shift;
    my ($terms, $max_depth, $template, $close_below) =
      rearrange([qw(terms depth template close_below)], @_);

    my $g;
    if ($self->has_path_table) {
      #print STDERR "DENORM:\n"; sleep 2;
      $g = $self->_get_graph_by_terms_denormalized(-terms=>$terms,
						   -depth=>$max_depth,
						   -template=>$template);
    }else {
      #print STDERR "RECUR:\n"; sleep 2;
      $g = $self->_get_graph_by_terms_recursive(-terms=>$terms,
						-depth=>$max_depth,
						-template=>$template);
    }
    if ($close_below) {
        $g->close_below($close_below);
    }
    return $g;
}


##
sub _get_graph_by_terms_denormalized {
    my $self = shift;

    my ($terms, $max_depth, $template) =
      rearrange([qw(terms depth template)], @_);

    $template = $template || {};
    my $term_template = $template->{terms} || {};

    my $dbh = $self->dbh;

    my $traverse_up = 1;
    if (defined($template->{traverse_up})) {
	$traverse_up = $template->{traverse_up};
    }

    my $traverse_down = 1;
    if (defined($template->{traverse_down})) {
	$traverse_down = $template->{traverse_down};
    }
    my $graph = $self->create_graph_obj;

    my $fetch_all = 0;
    my $all = $self->{_term_count};
    if ($all && scalar(@$terms) == $all) {
        $fetch_all = 1;
    }
    else {
        if (!@$terms) { return $graph }
    }

    my @rhl; # list of term2term row hashes
    if ($traverse_down &&
        !(defined($max_depth) && $max_depth == 0)) {
        my @cl = ();
        unless ($fetch_all) {
            @cl =
              "(".
                join(" OR ",
                     map {"$PATH.term1_id=".$_->id} @$terms).
                       ")";
        }
        push(@cl,
             "$PATH.term2_id=term2term.term1_id");
        if (defined($max_depth) && $max_depth > -1) {
            push(@cl, "distance <= ".($max_depth-1));
        }
        my $hl =
          select_hashlist($dbh,
                          ["term2term", "$PATH"],
                          \@cl,
                          "distinct term2term.*");
        @rhl = @$hl;
    }
    else {
        @rhl = ();
    }

    if ($traverse_up) {
        my @cl = ();
        unless ($fetch_all) {
            push(@cl,
                 sqlin("$PATH.term2_id", [map {$_->id} @$terms]));
        }
        push(@cl,
             "$PATH.term1_id=term2term.term2_id");
        #why traverse up won't have max depth effect like traverse down,
        #want to add the following, don't know if it has unforseen effects
        #        if (defined($max_depth) && $max_depth > -1) {
        #            push(@cl, "distance <= ".($max_depth-1));
        #        }
        my $hl =
          select_hashlist($dbh,
                          ["term2term", "$PATH"],
                          \@cl,
                          "distinct term2term.*");
        push(@rhl, @$hl);
    }

    # keep a fast array lookup table, keyed by db id
    my @term_lookup = ();

    # fill it with what we already know
    map {$term_lookup[$_->id] = $_} @$terms;

    # todo : use an array for speed
    my %ids_to_get = ();
    foreach my $rh (@rhl) {
        foreach my $id ($rh->{term1_id}, $rh->{term2_id}) {
            if (!$term_lookup[$id] && !$ids_to_get{$id}) {
                $ids_to_get{$id} =1
            }
        }
    }
    my %extra_h = ();
    my $new_terms = 
      $self->get_terms({idlist=>[keys %ids_to_get],
                       %extra_h},
                       $term_template);
    map {$term_lookup[$_->id] = $_} @$new_terms;
        
    my @all_terms = (@$terms, @$new_terms);
    map {$graph->add_term($_)} @all_terms;

    # now lets add the arcs to the graph
    foreach my $rh (@rhl) {

        my $type = $rh->{relationship_type_id};

	#print STDERR "___" . $type . "\n"; sleep 2;

        my $t1 = $term_lookup[$rh->{term1_id}];
        my $t2 = $term_lookup[$rh->{term2_id}];
        if ($t2) {
            $t1 || confess("assertion error");
            my $rel =
              GO::Model::Relationship->new({acc1=>$t1->acc,
                                            acc2=>$t2->acc});
            $type && $rel->type($type);
            #$rel->type($type);
            $rel->complete(1) if $rh->{complete};

            # N+S conditions are marked 'complete'
            $rel->complete(1) if $rh->{complete};
            if ($rh->{relationship_type_id}) {
                $self->refresh unless $self->{rtype_by_id};
                $rel->type($self->{rtype_by_id}->{$rh->{relationship_type_id}});
            }
            $graph->add_relationship($rel);
	    #print STDERR "INNER EDGE:" . $t2->acc;
	    #print STDERR " " . $type;
	    #print STDERR " " . $t1->acc;
	    #print STDERR "\n";
        }else {
            $graph->add_trailing_edge($t1->acc, $rh->{term2_id});
	    #print STDERR "TRAILING EDGE:" . $t1->acc;
        }
    }

    $graph->focus_nodes([@$terms]);

    if (0) {
        # populate leaf node counts
        map {$_->n_deep_products(-1)} @{$graph->get_all_nodes};
        my $leafs = $graph->get_leaf_nodes;
        my $countl = $self->get_deep_product_count({terms=>$leafs, per_term=>1});
        foreach my $c (@$countl) {
            $term_lookup[$c->{term_id}]->n_deep_products($c->{"c"});
        }
    }
    $self->_get_n_children_h($graph) unless ($template->{no_n_children});
    return $graph;
}


#construct graph based on term obj passed in (e.g. Amigo open nodes)
#prerequisite: root term is among terms passed
sub get_graph_by_terms_on_path {
    my $self = shift;

    my ($terms, $root_terms, $template) =
      rearrange([qw(terms root template)], @_);

    $template = $template || {};
    my $term_template = $template->{terms} || undef;
    my $dbh = $self->dbh;
    $root_terms ||= [$self->get_root_term($term_template)];
    unless (ref($root_terms) eq 'ARRAY') {
        $root_terms = [$root_terms];
    }

    my $graph = $self->create_graph_obj;
    #open nodes to their immediate children
    my $hl = select_hashlist
      ($dbh,
       ["term t", "term2term t2t"],
       ["t2t.relationship_type_id=t.id",
        "term1_id in (".(join(",",map{$_->id}@{$terms || []}) || 0).")"],
       ["t.name as type", "t2t.*"]
      );
    my (%pterm_h, %child_h, %terms_h);
    map{
        $pterm_h{$_->id} = $_;
    }(@{$terms || []});
    my @child_ids;
    map{push @child_ids, $_->{term2_id} unless (exists $pterm_h{$_->{term2_id}})}@{$hl || []};
    my $new_terms = 
      $self->get_terms({idlist=>\@child_ids}, $term_template);

    my %c2p_map;
    map{
        push @{$c2p_map{$_->{term2_id}}}, $_->{term1_id};
        $child_h{$_->{term2_id}}++;
    }@{$hl || []};
    map{
        $terms_h{$_->id} = $_;
    }(@{$terms || []}, @{$new_terms || []});

    local *p_ids = sub {
        my $cid = shift;
        my @all;
        if (scalar(@{$c2p_map{$cid} || []})) {
            push @all, @{$c2p_map{$cid}};
            map{push @all, @{p_ids($_) || []}}@{$c2p_map{$cid}};
        }
        return \@all;
    };
    local *parent_on_path = sub {
        my $n = shift;
        my @p_ids = @{p_ids($n->id) || []};
        return grep{my $id=$_;grep{$id eq $_->id}@$root_terms}@p_ids and not grep{not exists $pterm_h{$_}}@p_ids;
    };

    my %in_h;
    foreach my $h (@{$hl || []}) {
        my ($p, $c) = ($pterm_h{$h->{term1_id}}, $terms_h{$h->{term2_id}});
        my ($type, $type_id) = ($h->{type}, $h->{relationship_type_id});
        if ($p && $c) {
            next unless (parent_on_path($p) || grep{$p->acc eq $_->acc}@$root_terms);
            foreach my $n ($p, $c) {
                unless (exists $in_h{$n->id}) {
                    $graph->add_term($n);
                    $in_h{$n->id}++;
                }
            }
            my $rel =
              GO::Model::Relationship->new({acc1=>$p->acc,
                                            acc2=>$c->acc});
            $rel->type($type);
            $rel->complete(1) if $h->{complete};
            # we have to support both ways of doing rtypes
            # to support old schemas
            if ($type_id) {
                $self->refresh unless $self->{rtype_by_id};
                $rel->type($self->{rtype_by_id}->{$type_id});
            }
            $graph->add_relationship($rel);
        }
        else {
        }
    }
    unless (scalar(@{$graph->get_all_nodes || []})) {
        map{$graph->add_node($_)}@$root_terms;
    } else {
        $graph->focus_nodes([@$terms]);
    }
    $self->_get_n_children_h($graph);
    return $graph;
}

sub _get_graph_by_terms_recursive {
    my $self = shift;

    my ($terms, $max_depth, $template) =
      rearrange([qw(terms depth template)], @_);

    $template = $template || {};
    my $term_template = $template->{terms} || undef;
    my $dbh = $self->dbh;

    my $traverse_up = 1;
    if (defined($template->{traverse_up})) {
	$traverse_up = $template->{traverse_up};
    }
    my $traverse_down = 1;
    if (defined($template->{traverse_down})) {
	$traverse_down = $template->{traverse_down};
    }
    my $graph = $self->create_graph_obj;

#    my @upnodes = 
#      map {{depth=>0, acc=>$_->acc}} @$terms;

    my @donenodes = ();
    if (@$terms && $traverse_up) {
        # OK, I'm sacrificing clarity in favour of fast
        # retrieval here;
        # ascend the DAG first of all purely by using the 
        # term2term table; then fetch all the term objects

        # get the starting points
        my @nodes =
          map {$_->id} @$terms;
        
        # keep a list of all node ids
        my @allnodes = ();

        # lookup table of node->parents
        # (use array rather than hash for speed)
        # the table is indexed by child node database id;
        # the entry is actually a length-two array
        # of [type, parentnode_database_id]
        my @parentnode_lookup = ();

        # depth we have traversed so far
	# (since we are traversing upwards this
	#  is really height)
        my $depth = 0;

        # keep going while we have node ids to search
        while (@nodes && $traverse_up) {
	    
	    # filter out ones we already
	    # know the parents for
	    @nodes =
		grep {
		    !$parentnode_lookup[$_]
		} @nodes;

	    if (!@nodes) {
		next;
	    }
	    # keep running total
            push(@allnodes, @nodes);

            # lets grab the next level up in one fell swoop...
            my $hl =
              select_hashlist($dbh,
                              "term2term",
                              join(" OR ", 
                                   map {"term2_id = $_"} @nodes));
            # should we split this in case of
            # whole-graph retrievals?

            my @next_nodes = ();    # up one level
            foreach my $h (@$hl) {
                # add to the lookup table
                if (!$parentnode_lookup[$h->{term2_id}]) {
                    $parentnode_lookup[$h->{term2_id}] = 
                      [[$h->{relationship_type},
                        $h->{relationship_type_id},
			$h->{term1_id},
                        $h->{complete}]];
                }
                else {
                    push(@{$parentnode_lookup[$h->{term2_id}]},
                         [$h->{relationship_type},
                          $h->{relationship_type_id},
			  $h->{term1_id},
                          $h->{complete}]);
                }
                # we may have duplicates here but thats fine
                push(@next_nodes, $h->{term1_id});
            }
        #depth 20 is not enough to flybase Anatomy Ontology
	    if ($depth > 95) {
		# crappy way to detect cycles....
		confess("GRAPH CONTAINS CYCLE");
	    }
	    # lets continue up to the next level
            @next_nodes = 
              grep { !$donenodes[$_] } @next_nodes;
	    @nodes = @next_nodes;
            map { $donenodes[$_] = 1 } @next_nodes;
            $depth++;
        }
        # ok, now lets get the terms

        # keep a fast array lookup table, keyed by db id
        my @term_lookup = ();

        # fill it with what we already know
        map {$term_lookup[$_->id] = $_} @$terms;

        # 
        my @ids_to_get = ();
        foreach my $nodeid (@allnodes) {
            if (!$term_lookup[$nodeid]) {
                push(@ids_to_get, $nodeid);
            }
        }
        my $new_terms = 
          $self->get_terms({idlist=>\@ids_to_get}, $term_template);
        map {$term_lookup[$_->id] = $_} @$new_terms;
        
        my @all_terms = (@$terms, @$new_terms);
        map {$graph->add_term($_)} @all_terms;
        # now lets add the arcs to the graph

        foreach my $term (@all_terms) {
            foreach my $entry (@{$parentnode_lookup[$term->id]}) {
                my ($type, $type_id, $id, $complete) = @$entry;
                my $t2 = $term_lookup[$id];
                if ($t2) {
                    my $rel =
                      GO::Model::Relationship->new({acc1=>$t2->acc,
                                                    acc2=>$term->acc});
                    $rel->type($type); # old/deprecated?
                    $rel->complete(1) if $complete;
                    # we have to support both ways of doing rtypes
                    # to support old schemas
                    if ($type_id) {
                        $self->refresh unless $self->{rtype_by_id};
                        $rel->type($self->{rtype_by_id}->{$type_id});
                    }
                    $graph->add_relationship($rel);
                }
                else {
                }
            }
        }
    }

    if (@$terms && $traverse_down) {
        # OK, I'm sacrificing clarity in favour of fast
        # retrieval here;
        # descend the DAG first of all purely by using the 
        # term2term table; then fetch all the term objects

        # get the starting points
        my @nodes =
          map {$_->id} @$terms;
        
        # keep a list of all node ids
        my @allnodes = ();

        # array index of all nodes that have been traversed
        my @doneindex = ();
        # lookup table of node->children
        # (use array rather than hash for speed)
        # the table is indexed by parent node database id;
        # the entry is actually a length-two array
        # of [type, childnode_database_id]
        my @childnode_lookup = ();

        # depth we have traversed so far
        my $depth = 0;

	if (defined($max_depth) && $max_depth > -1 && !$max_depth) {
	    # done!
	    @nodes = ();
	}
        # keep going while we have node ids to search
        while (@nodes && $traverse_down) {
            push(@allnodes, @nodes);
            map { $doneindex[$_] = 1 } @nodes;
            # lets grab the next level down in one fell swoop...
            my $hl =
              select_hashlist($dbh,
                              "term2term",
                              "term1_id in (".
                              join(",", @nodes).")");
            # should we split this in case of
            # whole-graph retrievals?

            my @next_nodes = ();    # down one level
            foreach my $h (@$hl) {
                # add to the lookup table
                if (!$childnode_lookup[$h->{term1_id}]) {
                    $childnode_lookup[$h->{term1_id}] = 
                      [[$h->{relationship_type},
                        $h->{relationship_type_id},
			$h->{term2_id},
                        $h->{complete}]];
                }
                else {
                    push(@{$childnode_lookup[$h->{term1_id}]},
                         [$h->{relationship_type},
                          $h->{relationship_type_id},
			  $h->{term2_id},
                          $h->{complete}]);
                }
                # we may have duplicates here but thats fine
                push(@next_nodes, $h->{term2_id});
            }
            if (defined($max_depth) && $max_depth >-1 && 
		$depth >= $max_depth) {
                # done!
                @nodes = ();
            }
            else {
                # lets continue to the next level
                # make sure we don't follow cycles:
                @nodes = grep { !$donenodes[$_] } @next_nodes;
                map {$donenodes[$_]=1 } @nodes;
            }
            $depth++;
        }
        # ok, now lets get the terms

        # keep a fast array lookup table, keyed by db id
        my @term_lookup = ();

        # fill it with what we already know
        map {$term_lookup[$_->id] = $_} @$terms;

        # 
        my @ids_to_get = ();
        foreach my $nodeid (@allnodes) {
            if (!$term_lookup[$nodeid]) {
                push(@ids_to_get, $nodeid);
            }
        }
        my %extra_h = ();
        my $new_terms = 
          $self->get_terms({idlist=>\@ids_to_get,
                            %extra_h},
                           $term_template);
        map {$term_lookup[$_->id] = $_} @$new_terms;
        
        my @all_terms = (@$terms, @$new_terms);
        map {$graph->add_term($_)} @all_terms;
        # now lets add the arcs to the graph

        foreach my $term (@all_terms) {
            foreach my $entry (@{$childnode_lookup[$term->id]}) {
                my ($type, $type_id, $id, $complete) = @$entry;
                my $t2 = $term_lookup[$id];
                if ($t2) {
                    my $rel =
                      GO::Model::Relationship->new({acc1=>$term->acc,
                                                    acc2=>$t2->acc});
                    $rel->type($type);
                    $rel->complete(1) if $complete;
                    # we have to support both ways of doing rtypes
                    # to support old schemas
                    if ($type_id) {
                        $self->refresh unless $self->{rtype_by_id};
                        $rel->type($self->{rtype_by_id}->{$type_id});
                    }
#                    printf STDERR "%s %s %s\n",
#                      $t2->acc, $rel->type, $term->acc;
                    $graph->add_relationship($rel);
                }
                else {
                }
            }
        }
        # phew! we're done
    }
    else {
    }
    $graph->focus_nodes([@$terms]);
    $self->_get_n_children_h($graph) unless ($template->{no_n_children});
    return $graph;
}

sub get_graph_by_search {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($srch, $max_depth, $template) =
      rearrange([qw(search depth template)], @_);

    $template = $template || {};
    my $term_template = $template->{terms} || undef;

    my $term_l = 
      $self->get_terms({search=>$srch}, $term_template);
    my $graph =
      $self->get_graph_by_terms($term_l, $max_depth, $template);
    return $graph;
}

sub get_product_count {
    my $self = shift;
    my $constr = shift;
    if (!$constr->{term} && !$constr->{terms}) {
        # in count mode, you have to pass in a term constr
        $constr->{terms} = [];
    }
    my $c = 
      $self->get_products(-constraints=>$constr,
                          -options=>{count=>1});
    return $c;
}

sub get_deep_product_count {
    my $self = shift;
    my $constr = shift || {};
    my $dbh = $self->dbh;
    if (!$self->has_count_table) {
        if ($constr->{per_term}) {
            return [];
        }
        else {
            return 0;
        }
    }
    my $filters = $self->filters || {};
    my $termconstr = $constr->{terms} || $constr->{term};

    if (!$termconstr) {
        # warning: will be deprecated
        $termconstr = {acc=>'all'};
    }

    if (!ref($termconstr) || ref($termconstr) ne "ARRAY") {
        $termconstr = [$termconstr];
    }
    if ($constr->{operator} && lc($constr->{operator}) eq 'and') {
        return
          $self->get_deep_products(-constraints=>$constr,
                                   -options=>{count=>1});
    }
    my @terms = 
      map {
          if (ref($_) eq "HASH") {
              $self->get_term($_);
          }
          elsif (ref($_)) {
              # already is a term object
              my $t = $_;
              if (!$t->id) {
                  $t = $self->get_term({acc=>$t->acc});
              }
              $t;
          }
          else {
              $self->get_term({acc=>$_});
          }
      } @$termconstr;
    if (@terms != 1) {
        if (!$constr->{per_term}) {
            $self->throw("term constraint must specify a single term OR per_term must be set");
        }
    }

    my @tables = qw(gene_product_count);
    my @where = ();
    my $spdbs = 
      $constr->{speciesdb} ||
        $constr->{speciesdbs} || 
          $filters->{speciesdb} ||
            $filters->{speciesdbs};

    my $taxids = 
      $filters->{taxid} ||
	$filters->{taxids} ||
	  $constr->{taxid} ||
	    $constr->{taxids};

    # we have two ways of summing up gpcs: by summing the species partition
    # or by summing the speciesdbname partition
    # they can NOT be combined

    my $partitioned_by;
    if ($spdbs) {
        if (!ref($spdbs)) {
            $spdbs = [$spdbs];
        }

        if (@$spdbs) {
            my @wanted = grep {$_ !~ /^\!/} @$spdbs;
            my @unwanted = grep {/^\!/} @$spdbs;
        
            if (@wanted) {
                push(@where,
                     "(".join(" OR ", 
                              map{"speciesdbname=".sql_quote($_,1)} @wanted).")");
            }
            if (@unwanted) {
                push(@where,
                     map{"speciesdbname!=".sql_quote(substr($_,1))} @unwanted);
            }

            # force exclusion of alternate partition
            if ($GO_HAS_COUNT_BY_SPECIES) {
                push(@where,"species_id IS NULL");
            }

            $partitioned_by = 'speciesdbname';
        }
    }

    if ($taxids && $GO_HAS_COUNT_BY_SPECIES) {
        if (!ref($taxids)) {
            $taxids = [$taxids];
        }

        if (@$taxids) {

            if ($partitioned_by) {
                $self->throw("cannot filter by both speciesdb and taxid");
            }

            my @wanted = grep {$_ !~ /^\!/} @$taxids;
            my @unwanted = grep {/^\!/} @$taxids;
        
            # force a partition by species
            push(@where,"speciesdbname IS NULL");

            if (@wanted) {
                my $ids =
                  select_vallist($dbh,
                                 ["species"],
                                 [sqlin("ncbi_taxa_id",\@wanted)],
                                 ['id']);
                push(@where,"(".sqlin('species_id',$ids).")");
            }
            if (@unwanted) {
                my $ids =
                  select_vallist($dbh,
                                 ["species"],
                                 [sqlin("ncbi_taxa_id",\@unwanted)],
                                 ['id']);
                push(@where,"(".sqlin('species_id',$ids,0,'NOT').")");
            }

            # force exclusion of alternate partition
            push(@where,"speciesdbname IS NULL");
            $partitioned_by = 'species_id';
        }
    }

    # force exclusion of alternate partition
    if (!$partitioned_by && $GO_HAS_COUNT_BY_SPECIES) {
        push(@where,"species_id IS NULL");
    }


    if (@terms) {
        my @termids = map {$_->id} @terms;
        push(@where,
             "term_id in (".join(", ", @termids).")");
    }

    my $hl;
    if ($constr->{group_by}) {
        if ($constr->{group_by} eq 'taxid') {
            push(@tables, "species");
            push(@where, "species.id=gene_product_count.species_id");
            $hl =
              select_hashlist(-dbh=>$dbh,
                              -tables=>\@tables,
                              -where=>\@where,
                              -columns=>["term_id","ncbi_taxa_id",
                                         "sum(product_count) AS c"],
                              -group=>["term_id","ncbi_taxa_id"]);
            if (!$constr->{per_term}) {
                return {map { ($_->{ncbi_taxa_id} => $_->{c}) } @$hl};
            }
        }
        else {
            $self->throw("cannot group by $constr->{group_by}");
        }
    }
    else {
        $hl =
          select_hashlist(-dbh=>$dbh,
                          -tables=>\@tables,
                          -where=>\@where,
                          -columns=>["term_id",
                                     "sum(product_count) AS c"],
                          -group=>["term_id"]);
        if (!$constr->{per_term}) {
            return $hl->[0]->{"c"};
        }
    }
    return $hl;
}

sub get_deep_product_count_grouped_by_taxid {
    my $self = shift;
    my $constr = shift || {};
    my $dbh = $self->dbh;
    if (!$self->has_count_table) {
        if ($constr->{per_term}) {
            return [];
        }
        else {
            return 0;
        }
    }
    my $filters = $self->filters || {};
    my $termconstr = $constr->{terms} || $constr->{term};
    if (!ref($termconstr) || ref($termconstr) ne "ARRAY") {
        $termconstr = [$termconstr];
    }
    if ($constr->{operator} && lc($constr->{operator}) eq 'and') {
        return
          $self->get_deep_products(-constraints=>$constr,
                                   -options=>{count=>1});
    }
    my @terms = 
      map {
          if (ref($_) eq "HASH") {
              $self->get_term($_);
          }
          elsif (ref($_)) {
              # already is a term object
              my $t = $_;
              if (!$t->id) {
                  $t = $self->get_term({acc=>$t->acc});
              }
              $t;
          }
          else {
              $self->get_term({acc=>$_});
          }
      } @$termconstr;
    my @tables = qw(gene_product_count);
    my @where = ();
    my $spdbs = 
      $constr->{speciesdb} ||
        $constr->{speciesdbs} || 
          $filters->{speciesdb} ||
            $filters->{speciesdbs};

    my $taxids = 
      $filters->{taxid} ||
	$filters->{taxids} ||
	  $constr->{taxid} ||
	    $constr->{taxids};

    if ($spdbs) {
        if (!ref($spdbs)) {
            $spdbs = [$spdbs];
        }
        
        my @wanted = grep {$_ !~ /^\!/} @$spdbs;
        my @unwanted = grep {/^\!/} @$spdbs;
        
        if (@wanted) {
            push(@where,
                 "(".join(" OR ", 
                      map{"speciesdbname=".sql_quote($_,1)} @wanted).")");
        }
        if (@unwanted) {
            push(@where,
                 map{"speciesdbname!=".sql_quote(substr($_,1))} @unwanted);
        }
    }

    if ($taxids && $GO_HAS_COUNT_BY_SPECIES) {
        if (!ref($taxids)) {
            $taxids = [$taxids];
        }
        
        my @wanted = grep {$_ !~ /^\!/} @$taxids;
        my @unwanted = grep {/^\!/} @$taxids;
        
        if (@wanted) {
            my $ids =
              select_vallist($dbh,
                             ["species"],
                             [sqlin("ncbi_taxa_id",\@wanted)],
                             ['id']);
            push(@where,"(".sqlin('species_id',$ids).")");
        }
        if (@unwanted) {
            my $ids =
              select_vallist($dbh,
                             ["species"],
                             [sqlin("ncbi_taxa_id",\@unwanted)],
                             ['id']);
            push(@where,"(".sqlin('species_id',$ids,0,'NOT').")");
        }
    }

    if (@terms) {
        my @termids = map {$_->id} @terms;
        push(@where,
             "term_id in (".join(", ", @termids).")");
    }

    my $hl =
      select_hashlist(-dbh=>$dbh,
                      -tables=>\@tables,
                      -where=>\@where,
                      -columns=>["term_id",
                                 "sum(product_count) AS c"],
                      -group=>["term_id"]);
    if (!$constr->{per_term}) {
        return $hl->[0]->{"c"};
    }
    return $hl;
}

sub get_product {
    my $self = shift;
    my $pl = $self->get_products(@_);
    return shift @$pl;
}

sub get_deep_products {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($inconstr, $template, $options) =
      rearrange([qw(constraints template options)], @_);
    return $self->get_products({%$inconstr, deep=>1}, $template, $options);
}

sub get_products {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($inconstr, $template, $options) =
      rearrange([qw(constraints template options)], @_);
    my $constr = {%{$inconstr || {}}};        # make copy
    my @constr_arr =  ();
    $template = $template || {};
    my $filters = $self->filters || {};

    # usually the sql ensures uniqueness of results;
    # if >1 term is being queried we could get same 
    # product twice, in which case we flag this
    # and we make sure there are no duplicates at
    # the end
    my $ensure_unique = 0;

    # basic query is over 2 tables:
    my @tables_arr = ("gene_product",
                     "dbxref");
    # JOIN with dbxref table
    push (@constr_arr, "gene_product.dbxref_id = dbxref.id");

    if (!ref($constr)) {
        # be liberal in what we accept; user should
        # really pass in a hashref, but we accept GO IDs etc
        my $term = $self->get_term($constr, {acc=>1});
        if (!$term) {
            $term = $self->get_term({search=>$constr}, {acc=>1});
        }
        if (!$term) {
            confess("$constr is illegal as a first argument for get_products()");
        }
        $constr = {id=>$term->id};
    }
	if ($constr->{dbxrefs}) {
		my $dbxrefs = $constr->{dbxrefs};
		if (!ref($dbxrefs)) {
			$dbxrefs = [$dbxrefs];
		}

        my @q =
          map {
              if (ref($_)) {
                  "(dbxref.xref_dbname = ".sql_quote($_->{xref_dbname}).
                    " AND ".
                      "dbxref.xref_key = ".sql_quote($_->{xref_key}).")";
              }
              else {
                  if ($_ =~ /(.*):(.*)/) {
                      "(dbxref.xref_dbname = ".sql_quote($1).
                        " AND ".
                          "dbxref.xref_key = ".sql_quote($2).")";
                  }
                  else {
                      confess("$_ not a valid dbxref");
                  }
              }
          } @$dbxrefs;
        if (@q) {
            push(@constr_arr,"(".join(" OR ", @q).")");
        }
	    delete $constr->{dbxrefs};
	}
    if ($constr->{xref}) {
	$constr->{acc} = $constr->{xref}->{xref_key};
	$constr->{speciesdb} = $constr->{xref}->{xref_dbname};
    }
    if ($constr->{term}) {
        $constr->{terms} = [$constr->{term}];
    }
    if ($constr->{terms}) {
        my $op = $constr->{operator} || "or";
        my $n = 1;
        if ($op eq "and") {
            $n = scalar(@{$constr->{terms}});
        }
        my $terms = $constr->{terms};
        for (my $i=0; $i<$n; $i++) {
            my $sfx = $op eq "and" ? $i : "";
            my $assoc_table = "association$sfx";
            my $evid_table = "evidence$sfx";
            my $path_table = "$PATH$sfx";
            my $term_table = "term$sfx";
            if ($op eq "and") {
                $terms = [$constr->{terms}->[$i]];
            }

            if (defined $constr->{is_not}) {
                push (@constr_arr, "$assoc_table.is_not = ".$constr->{is_not});
            }
            push(@constr_arr, 
                 "$assoc_table.gene_product_id = gene_product.id");
            push(@tables_arr, 
                 "association AS $assoc_table");
            if ($constr->{deep}) {
                # this part could be speeded up...
                my @terms =
                  map {
                      if (ref($_) eq "HASH") {
                          $self->get_term($_);
                      } elsif (ref($_)) {
                          # already is a term object
                          $_;
                      } else {
                          $self->get_term($_);
                      }
                  } @$terms;
                # speed up the above!!!

                if ($self->has_path_table) {
                    # the path table has been filled;
                    # we can take advantage of this
                    # denormalisation and not do
                    # a recursive query
                    my $negop = $constr->{negoperator} || "";
                    unless ($negop) {

                        my $orq = 
                          join(",", map {$_->id} @terms);
                        
                        if ($orq) {
                            push(@tables_arr,
                                 "$PATH as $path_table");
                            push(@constr_arr,
                                 "$path_table.term2_id = $assoc_table.term_id",
                                 "$path_table.term1_id $negop in ($orq)");
                        }
                    }
                    else {
                        # fetch everything that is NOT IN termlist
                        # AND _could not_ be in term list
                        # (eg any parent of anything in termlist)
                        confess("alpha code - requires postgres");
                    }
                } else {
                    # there is no denormalised path table;
                    # we have to do this recursively
                    my $g = 
                      $self->get_graph_by_terms(\@terms, 
                                                -1,
                                                {traverse_up=>0,
                                                 terms=>{acc=>1,id=>1}});
                    my @term_ids = map {$_->id} @{$g->get_all_nodes || []};
                    $ensure_unique = 1;
                    my $orq = 
                      join(" OR ", map {"$assoc_table.term_id = $_"} @term_ids);
                    if ($orq) {
                        push(@constr_arr, "($orq)");
                    }
                }
            } else {
                # non-deep; ie directly attached to term

                if (@$terms > 1) {
                    $ensure_unique = 1;
                }
                push(@constr_arr,
                     "$assoc_table.gene_product_id = gene_product.id");
            
                # list of terms to constrain by;
                # we are liberal in what we expect;
                # can be a list of objects, refs,
                # GO IDs or names
                my @ors = ();
                foreach my $t (@$terms) {
                    my $c = "";
                    if (ref($t)) {
                        if ($t->{id}) {
                            $c = "$term_table.id = $t->{id}";
                        } elsif ($t->{acc}) {
                            $c = "$term_table.acc = $t->{acc}";
                        } elsif ($t->{name}) {
                            $c = "$term_table.name = ".sql_quote($t->{name});
                        } else {
                            confess("$t contains no keyable atts");
                        }
                    } else {
                        if ($t =~ /^(\d+)$/ || $t =~ /^GO:(\d+)$/) {
                            $c = "$term_table.acc = ".sql_quote(sprintf("GO:%07d", $1));
                        } else {
                            $c = "$term_table.name = ".sql_quote($t);
                        }
                    }
                    push(@ors, $c);
                }
                if (@ors) {
                    push(@constr_arr,
                         "(".join(" OR ", @ors).")");
                    push(@tables_arr,
                         "term $term_table");
                    push(@constr_arr,
                         "$assoc_table.term_id=$term_table.id");
                }
            }                   # -- end of term fetching

            # --- EVIDENCE CODES ---
            # uhoh - duplicated code; could do with refactoring
            my $evcodes = $constr->{evcodes} || $filters->{evcodes}
                       || $constr->{evcode} || $filters->{evcode};
            my @w=();
            if ($evcodes) {
# often $evcodes points to an empty ARRAY
# added extra check, this removed unneeded join on evidence table.
# jmc Oct2006
		if (defined(@$evcodes)) {
		    my @wanted = grep {$_ !~ /^\!/} @$evcodes;
		    my @unwanted = grep {/^\!/} @$evcodes;

		    if (@wanted) {
			push(@w,
			     "(".join(" OR ", 
				      map{"$evid_table.code=".sql_quote($_,1)} @wanted).")");
		    }
		    if (@unwanted) {
			push(@w,
			     map{"$evid_table.code!=".sql_quote(substr($_,1))} @unwanted);
		    }
		    push(@tables_arr,
			 "evidence $evid_table");
		    unshift(@constr_arr, 
			    "$evid_table.association_id = $assoc_table.id", 
			    @w);
		}
	    }
	}
        # end of terms constraint
    } else {
        if ($template->{has_association}) {
            push @tables_arr, "association";
            push @constr_arr, "association.gene_product_id=gene_product.id";
            #need join evidence to get prod with assoc of the ev code(s)
            my $evcodes = $constr->{evcodes} || $filters->{evcodes}
                       || $constr->{evcode} || $filters->{evcode};
            my @w=();
            if ($evcodes) {
# often $evcodes points to an empty ARRAY
# added extra check, this removed unneeded join on evidence table.
# jmc Oct2006
		if (defined(@$evcodes)) {
		    push @tables_arr, "evidence";
		    my @wanted = grep {$_ !~ /^\!/} @$evcodes;
		    my @unwanted = grep {/^\!/} @$evcodes;
		    if (@wanted) {
			push(@w,
			     "(".join(" OR ", 
				      map{"evidence.code=".sql_quote($_,1)} @wanted).")");
		    }
		    if (@unwanted) {
			push(@w,
			     map{"evidence.code!=".sql_quote(substr($_,1))} @unwanted);
		    }
		    unshift(@constr_arr, 
			    "evidence.association_id = association.id",
			    @w);
		}
	    }
        }
    }
    map {
	if (defined($constr->{$_})) {
            my $op = "=";
            my $v = $constr->{$_};
            if ($v =~ /\*/) {
                $v =~ s/\*/\%/g;
                $op = "like";
            }
            if (ref($v) eq 'ARRAY') {
                push(@constr_arr, "gene_product.$_ IN (".join(",",map{sql_quote($_)}@$v).")");
            } else {
                push(@constr_arr, "gene_product.$_ $op ".sql_quote($v));
            }
	}
    } qw(symbol full_name id);

    # CONSTRAIN BY SEQ ACCESSION
    if ($constr->{seq_acc} ||
	$constr->{seq_name}) {
	my $seq_acc = $constr->{seq_acc};
	my $seq_name = $constr->{seq_name};
	push(@tables_arr,
	     qw(seq gene_product_seq));
	push(@constr_arr,
	     "seq.id = gene_product_seq.seq_id",
	     "gene_product_seq.gene_product_id = gene_product.id");
	if ($seq_acc) {
	    push(@tables_arr,
		 "seq_dbxref", "dbxref AS seqxref");
	    push(@constr_arr,
		 "seq.id = seq_dbxref.seq_id",
		 "seqxref.id = seq_dbxref.dbxref_id",
		 "seqxref.xref_key = ".sql_quote($seq_acc));
	}
	if ($seq_name) {
	    push(@constr_arr,
		 "seq.display_id = ".sql_quote($seq_name));
	}
    }

    # CONSTRAIN BY GENE PRODUCT SYNONYM
    if ($constr->{synonym}) {
	my $synonym = $constr->{synonym};
	push(@tables_arr,
	     qw(gene_product_synonym));
	my $op = '=';
	if ($synonym =~ /\*/) {
	    $op = 'like';
	    $synonym =~ s/\*/\%/g;
	}
	push(@constr_arr,
	     "gene_product_synonym.product_synonym $op ".sql_quote($synonym),
	     "gene_product_synonym.gene_product_id = gene_product.id");
    }

    # CONSTRAIN BY SPECIESDB
    my $spdbs = 
      $filters->{speciesdb} ||
	$filters->{speciesdbs} ||
	  $constr->{speciesdb} ||
	    $constr->{speciesdbs};
    
    if ($spdbs) {
	delete $constr->{speciesdb};
	delete $constr->{speciesdbs};
	
	if (!ref($spdbs)) {
	    $spdbs = [$spdbs];
	}
	
	my @wanted = grep {$_ !~ /^\!/} @$spdbs;
	my @unwanted = grep {/^\!/} @$spdbs;
	
	if (@wanted) {
	    push(@constr_arr,
		 "(".join(" OR ", 
			  map{"dbxref.xref_dbname=".sql_quote($_,1)} @wanted).")");
	}
	if (@unwanted) {
	    push(@constr_arr,
		 map{"dbxref.xref_dbname!=".sql_quote(substr($_,1))} @unwanted);
	}
    }
    
    # CONSTRAIN BY NCBI Taxa ID
    my $taxids = 
      $filters->{taxid} ||
	$filters->{taxids} ||
	  $constr->{taxid} ||
	    $constr->{taxids};
    
    if ($taxids) {
	delete $constr->{taxid};
	delete $constr->{taxids};
	
	if (!ref($taxids)) {
	    $taxids = [$taxids];
	}
	
	my @wanted = grep {$_ !~ /^\!/} @$taxids;
	my @unwanted = grep {/^\!/} @$taxids;
	
	push(@tables_arr,
	     qw(species));
	push(@constr_arr,
	     "species.id = gene_product.species_id");

	if (@wanted) {
	    push(@constr_arr,
		 "(".join(" OR ", 
			  map{"species.ncbi_taxa_id=$_"} @wanted).")");
	}
	if (@unwanted) {
	    push(@constr_arr,
		 map{"species.ncbi_taxa_id!=$_"} @unwanted);
	}
    }

    # Secondary NCBI Taxa IDs
    # for multi-species interactions
    my $qualifier_taxids = 
      $constr->{qualifier_taxid} ||
        $constr->{qualifier_taxids} ||
          $filters->{qualifier_taxid} ||
            $filters->{qualifier_taxids};

    if ($qualifier_taxids) {
        if (!ref($qualifier_taxids)) {
            $qualifier_taxids = [$qualifier_taxids];
        }

	push(@tables_arr, "species AS qualifier_species");
	push(@tables_arr, "association_species_qualifier");
	push(@constr_arr, "qualifier_species.id = association_species_qualifier.species_id");
	push(@constr_arr, "association_species_qualifier.association_id = association.id");
        push @constr_arr, "association.gene_product_id=gene_product.id";

	my @wanted = grep {$_ !~ /^\!/} @$qualifier_taxids;
	my @unwanted = grep {/^\!/} @$qualifier_taxids;

	if (@wanted) {
	    push(@constr_arr,
		 "(".join(" OR ", 
			  map{"qualifier_species.ncbi_taxa_id=$_"} @wanted).")");
	}
	if (@unwanted) {
	    push(@constr_arr,
		 map{"qualifier_species.ncbi_taxa_id!=$_"} @unwanted);
	}
    }
    
    if ($constr->{acc}) {
        push (@constr_arr, "dbxref.xref_key = ".sql_quote($constr->{acc}));
    }

    my @cols = ("gene_product.*", 
                "dbxref.xref_key AS acc", "dbxref.xref_dbname AS speciesdb",
                );

#    if ($template->{seq_list}) {
#        push(@tables_arr,
#             "gene_product_seq");
#        push(@constr_arr,
#             "gene_product_seq.gene_product_id = gene_product.id");
#        push(@cols, "seq_id");
#    }

    if ($options && $options->{count}) {
#        if (!grep {/dbxref\./ && 
#                     $_ !~ /gene_product\.dbxref_id/} @constr_arr) {
#            @tables_arr = grep {$_ ne "dbxref"} @tables_arr;
#            @constr_arr = grep {$_ !~ /dbxref/} @constr_arr;
#            # remove unneeded join for speed
#        }
        # we don't need gene_product table itself
#        @tables_arr = grep {$_ ne "gene_product"} @tables_arr;
#        @constr_arr = grep {$_ !~ /gene_product/} @constr_arr;
        
        if ($constr->{per_term}) {

            my $groupcol = "$PATH.term1_id";
            if (grep {/$PATH/} @tables_arr) {
                if (!$self->has_path_table) {
                    confess("must build path table");
                }
            }
            else {
                $groupcol = "association.term_id";
            }

            @cols = 
              ("$groupcol term_id", "count(distinct gene_product_id) AS c");

            if (!grep{/association/} @tables_arr) {
                push(@tables_arr, "association");
            }
            my $hl = 
              select_hashlist(-dbh=>$dbh,
                              -tables=>\@tables_arr,
                              -where=>\@constr_arr,
                              -columns=>\@cols,
                              -group=>["$groupcol"]);
            return $hl;
        }
        else {
            @cols = ("count(distinct gene_product.id) AS c");
            my $h = 
              select_hash(-dbh=>$dbh,
                          -tables=>\@tables_arr,
                          -where=>\@constr_arr,
                          -columns=>\@cols,
                          );
            return $h->{'c'};
        }
    }
    my $hl = 
      select_hashlist(-dbh=>$dbh,
		      -tables=>\@tables_arr,
		      -where=>\@constr_arr,
                      -distinct=>1,
		      -columns=>\@cols);
    
    my @pl = ();
    my $get_term_ids;
    if (grep {/association/} @tables_arr) {
        $get_term_ids = 1;
    }


    foreach my $h (@$hl) {
	push(@pl,  $self->create_gene_product_obj($h));
	$pl[-1]->{species_id} = $h->{species_id};
	$pl[-1]->{type_id} = $h->{type_id} if $h->{type_id};
	if ($get_term_ids) {
            $pl[-1]->{term_id} = $h->{term_id};
        }
    }
    if ($template->{seq_list} && @pl) {
        map {$_->seq_list([])} @pl;
        my @pi = ();
        map {$pi[$_->id] = $_ } @pl;
        my $hl =
          select_hashlist($dbh,
                          "gene_product_seq",
                          "gene_product_id in (".
                          join(",", 
                               map {$_->id} @pl).")",
                         );
        my %seqid2gpid = ();
        foreach my $h (@$hl) {
            if ($seqid2gpid{$h->{seq_id}}) {
                warn("Code makes assumption gps may not share seq ($seqid2gpid{$h->{seq_id}} and $h->{gene_product_id})");
            }
            $seqid2gpid{$h->{seq_id}} = $h->{gene_product_id};
        }
        if (%seqid2gpid) {
            my $seqs = $self->get_seqs({ids=>[keys %seqid2gpid]});
            foreach my $seq (@$seqs) {
                $pi[$seqid2gpid{$seq->id}]->add_seq($seq);
            }
        }
    }

    # -- GET SPECIES AND TYPE --
    # it is more efficient in mysql to do this as a seperate
    # query rather than the same join
    # (or rather it was, when this code was written...)
    if (@pl and !$template->{product_only}) {
        $self->_get_product_property(\@pl) if ($self->has_gp_property_table);
        $self->_get_product_seqs(\@pl);
        $self->_get_product_synonyms(\@pl);
        $self->_get_product_species(\@pl);
        $self->_get_product_types(\@pl);
    }                       # - end of get types and species -

    return \@pl;
}

#iff graph_path is filled
sub get_all_term_product_ids {
    my $self = shift;
    my $acc = shift || return;
		my $want_term_id = shift;
    my $dbh = $self->dbh;
    $acc = [$acc] unless (ref($acc) eq 'ARRAY');

    my ($unique_accs, %has_h);
    #unique and ordered, ie in hierarchy
    map{unless (exists $has_h{$_}){push @$unique_accs, $_; $has_h{$_}++}}@$acc;
    return unless (@{$unique_accs || []});
    my $tables = ["association a", "term t", "graph_path", "term t0","gene_product p"];
    my $where =  ["t0.id=graph_path.term1_id","t.id=graph_path.term2_id","a.term_id = t.id", "a.gene_product_id = p.id", "t0.acc IN (".join(",",map{sql_quote($_)}@$unique_accs).")"];

    my $filters = $self->filters || {};
    my $spdbs =
      $filters->{speciesdb} ||
        $filters->{speciesdbs};
    if ($spdbs) {
        if (!ref($spdbs)) {
            $spdbs = [$spdbs];
        }
        push @$tables, "dbxref";
        push @$where, "p.dbxref_id=dbxref.id";

        my @wanted = grep {$_ !~ /^\!/} @$spdbs;
        my @unwanted = grep {/^\!/} @$spdbs;

        if (@wanted) {
            push(@$where,
                 "(".join(" OR ", 
                          map{"dbxref.xref_dbname=".sql_quote($_,1)} @wanted).")");
        }
        if (@unwanted) {
            push(@$where,
                 map{"dbxref.xref_dbname!=".sql_quote(substr($_,1))} @unwanted);
        }
    }
    # NCBI Taxa IDs
    my $taxids =
      $filters->{taxid} ||
        $filters->{taxids};

    if ($taxids) {
        if (!ref($taxids)) {
            $taxids = [$taxids];
        }

        push(@$tables, "species");
        push(@$where, "species.id = p.species_id");

        my @wanted = grep {$_ !~ /^\!/} @$taxids;
        my @unwanted = grep {/^\!/} @$taxids;

        if (@wanted) {
            push(@$where,
                 "(".join(" OR ", 
                          map{"species.ncbi_taxa_id=$_"} @wanted).")");
        }
        if (@unwanted) {
            push(@$where,
                 map{"species.ncbi_taxa_id!=$_"} @unwanted);
        }
    }

    my $evcodes = $filters->{evcodes} || $filters->{evcode};
    my @w=();
    if ($evcodes) {
# often $evcodes points to an empty ARRAY
# added extra check, this removed unneeded join on evidence table.
# jmc Oct2006
	if (defined(@$evcodes)) {
	    my @wanted = grep {$_ !~ /^\!/} @$evcodes;
	    my @unwanted = grep {/^\!/} @$evcodes;
	    if (@wanted) {
		push(@w,
		     "(".join(" OR ", 
			      map{"evidence.code=".sql_quote($_,1)} @wanted).")");
	    }
	    if (@unwanted) {
		push(@w,
		     map{"evidence.code!=".sql_quote(substr($_,1))} @unwanted);
	    }
	    push(@$tables, "evidence");
	    push(@$where,
		 "evidence.association_id = a.id", @w);
	}
    }
    my $hl = select_hashlist
      ($dbh,
       $tables,
       $where,
       "distinct p.id, p.symbol, t.acc, t.id as term_id",
       ["graph_path.distance", "p.symbol"]
      );

		my @acc = (); #keep order
    my %acc_h = ();
    map{
				my $k = $_->{acc};
				if ($want_term_id) {
						$k = $_->{term_id};
				}
				push @acc, $k unless (exists $acc_h{$k});
        push @{$acc_h{$k}},$_->{id};
    }@{$hl || []};
    #exists $acc_h{} to check if any assoc
    return [map{[$_ => $acc_h{$_}]}grep{exists $acc_h{$_}}@acc];
}

#quickly find product ids for associated term(s)
#ids is ordered by product symbol and return as [acc=>[ids]] and preserve acc order
sub get_term_product_ids {
    my $self = shift;
    my $acc = shift || return;
    my $dbh = $self->dbh;
    $acc = [$acc] unless (ref($acc) eq 'ARRAY');

    my ($unique_accs, %has_h);
    #unique and ordered, ie in hierarchy
    map{unless (exists $has_h{$_}){push @$unique_accs, $_; $has_h{$_}++}}@$acc;
    return unless (@{$unique_accs || []});
    my $tables = ["association a", "term t", "gene_product p"];
    my $where =  ["a.term_id = t.id", "a.gene_product_id = p.id", "t.acc IN (".join(",",map{sql_quote($_)}@$unique_accs).")"];

    my $filters = $self->filters || {};
    my $spdbs =
      $filters->{speciesdb} ||
        $filters->{speciesdbs};
    if ($spdbs) {
        if (!ref($spdbs)) {
            $spdbs = [$spdbs];
        }
        push @$tables, "dbxref";
        push @$where, "p.dbxref_id=dbxref.id";

        my @wanted = grep {$_ !~ /^\!/} @$spdbs;
        my @unwanted = grep {/^\!/} @$spdbs;

        if (@wanted) {
            push(@$where,
                 "(".join(" OR ", 
                          map{"dbxref.xref_dbname=".sql_quote($_,1)} @wanted).")");
        }
        if (@unwanted) {
            push(@$where,
                 map{"dbxref.xref_dbname!=".sql_quote(substr($_,1))} @unwanted);
        }
    }
    # NCBI Taxa IDs
    my $taxids =
      $filters->{taxid} ||
        $filters->{taxids};

    if ($taxids) {
        if (!ref($taxids)) {
            $taxids = [$taxids];
        }

        push(@$tables, "species");
        push(@$where, "species.id = p.species_id");

        my @wanted = grep {$_ !~ /^\!/} @$taxids;
        my @unwanted = grep {/^\!/} @$taxids;

        if (@wanted) {
            push(@$where,
                 "(".join(" OR ", 
                          map{"species.ncbi_taxa_id=$_"} @wanted).")");
        }
        if (@unwanted) {
            push(@$where,
                 map{"species.ncbi_taxa_id!=$_"} @unwanted);
        }
    }

    my $evcodes = $filters->{evcodes} || $filters->{evcode};
    my @w=();
    if ($evcodes) {
# often $evcodes points to an empty ARRAY
# added extra check, this removed unneeded join on evidence table.
# jmc Oct2006
	if (defined(@$evcodes)) {
	    my @wanted = grep {$_ !~ /^\!/} @$evcodes;
	    my @unwanted = grep {/^\!/} @$evcodes;
	    if (@wanted) {
		push(@w,
		     "(".join(" OR ", 
			      map{"evidence.code=".sql_quote($_,1)} @wanted).")");
	    }
	    if (@unwanted) {
		push(@w,
		     map{"evidence.code!=".sql_quote(substr($_,1))} @unwanted);
	    }
	    push(@$tables, "evidence");
	    push(@$where,
		 "evidence.association_id = a.id", @w);
	}
    }

    my $hl = select_hashlist
      ($dbh,
       $tables,
       $where,
       "distinct p.id, p.symbol, t.acc",
       "p.symbol"
      );


    my %acc_h = ();
    map{
        push @{$acc_h{$_->{acc}}},$_->{id};
    }@{$hl || []};
    #exists $acc_h{} to check if any assoc
    return [map{[$_ => $acc_h{$_}]}grep{exists $acc_h{$_}}@$unique_accs];
}

#quickly find product ids, allname means symbol, full_name and synonym
#work but slow
sub get_product_ids {
    my $self = shift;
    my $constr = shift || return;
    my $dbh = $self->dbh;

    my $tables = ["association a", "evidence e"];
    #gp added below
    my $where =  ["a.gene_product_id=gp.id", "a.id=e.association_id"];

    if ($constr->{all_names}) {
        my $n = $constr->{all_names};
        $n = [$n] unless (ref($n) eq 'ARRAY');
        push @$tables, "gene_product gp left join gene_product_synonym synonym ON (synonym.gene_product_id = gp.id)";
        push @$where,
          (
          "(".join(" or ",map{"gp.symbol = '$_' or gp.full_name = '$_' or synonym.product_synonym = '$_'"}@$n).")");
    } else {
        push @$tables, "gene_product gp";
    }

    if ($constr->{synonym}) {
        my $s = $constr->{synonym};
        $s = [$s] unless (ref($s) eq 'ARRAY');
        push @$tables, "gene_product_synonym synonym";
        push @$where, "gp.id=synonym.gene_product_id";
        push @$where, "(".join(" or ",map{"synonym.product_synonym = '$_'"} grep {$_} @$s).")";
    } elsif ($constr->{symbol}) {
        my $s = $constr->{symbol};
        $s = [$s] unless (ref($s) eq 'ARRAY');
        push @$where, "(".join(" or ",map{"gp.symbol = '$_'"}@$s).")";
    } elsif ($constr->{full_name}) {
        my $s = $constr->{full_name};
        $s = [$s] unless (ref($s) eq 'ARRAY');
        push @$where, "(".join(" or ",map{"gp.full_name = '$_'"}@$s).")";
    }
    if ($constr->{xref_key}) {
        my $x = $constr->{xref_key};
        $x = [$x] unless (ref($x) eq 'ARRAY');
        push @$tables, "dbxref";
        push @$where,
          ("gp.dbxref_id=dbxref.id",
           "(".join(" or ", map{"dbxref.xref_key = '$_'"}@$x).")");
    }
    if ($constr->{seq_acc} ||
        $constr->{seq_name}) {
        my $seq_acc = $constr->{seq_acc};
        my $seq_name = $constr->{seq_name};
        push(@$tables,
             qw(seq gene_product_seq));
        push(@$where,
             "seq.id = gene_product_seq.seq_id",
             "gene_product_seq.gene_product_id = gp.id");
        if ($seq_acc) {
            $seq_acc = [$seq_acc] unless (ref($seq_acc) eq 'ARRAY');
            push(@$tables,
                 "seq_dbxref", "dbxref AS seqxref");
            push(@$where,
                 "seq.id = seq_dbxref.seq_id",
                 "seqxref.id = seq_dbxref.dbxref_id",
                 "(".join(" or ", map{"seqxref.xref_key = ".sql_quote($_)}@$seq_acc).")");
        }
        if ($seq_name) {
            $seq_name = [$seq_name] unless (ref($seq_name) eq 'ARRAY');
            push(@$where,
                 "(".join(" or ",map{"seq.display_id = ".sql_quote($_)}@$seq_name).")");
        }
    }
    if ($constr->{term_type}) {
        my $o = $constr->{term_type};
        $o = [$o] unless (ref($o) eq 'ARRAY');
        push @$tables, "term";
        push @$where,
          ("term.id=a.term_id",
           "(".join(" or ", map{"term.term_type = ".sql_quote($_)}@$o).")");
    }
    my $filters = $self->filters || {};
    my $spdbs =
      $filters->{speciesdb} ||
        $filters->{speciesdbs};
    if ($spdbs) {
        if (!ref($spdbs)) {
            $spdbs = [$spdbs];
        }
        unless ($constr->{xref}) {
            push @$tables, "dbxref";
            push @$where, "gp.dbxref_id=dbxref.id";
        }
        my @wanted = grep {$_ !~ /^\!/} @$spdbs;
        my @unwanted = grep {/^\!/} @$spdbs;

        if (@wanted) {
            push(@$where,
                 "(".join(" OR ", 
                          map{"dbxref.xref_dbname=".sql_quote($_,1)} @wanted).")");
        }
        if (@unwanted) {
            push(@$where,
                 map{"dbxref.xref_dbname!=".sql_quote(substr($_,1))} @unwanted);
        }
    }
    # NCBI Taxa IDs
    my $taxids =
      $filters->{taxid} ||
        $filters->{taxids};

    if ($taxids) {
        if (!ref($taxids)) {
            $taxids = [$taxids];
        }
        push(@$tables, "species");
        push(@$where, "species.id = gp.species_id");

        my @wanted = grep {$_ !~ /^\!/} @$taxids;
        my @unwanted = grep {/^\!/} @$taxids;

        if (@wanted) {
            push(@$where,
                 "(".join(" OR ", 
                          map{"species.ncbi_taxa_id=$_"} @wanted).")");
        }
        if (@unwanted) {
            push(@$where,
                 map{"species.ncbi_taxa_id!=$_"} @unwanted);
        }
    }

    my $evcodes = $filters->{evcodes} || $filters->{evcode};
    my @w=();
    if ($evcodes) {
# often $evcodes points to an empty ARRAY
# added extra check, this removed unneeded join on evidence table.
# jmc Oct2006
	if (defined(@$evcodes)) {
	    my @wanted = grep {$_ !~ /^\!/} @$evcodes;
	    my @unwanted = grep {/^\!/} @$evcodes;
	    if (@wanted) {
		push(@w,
		     "(".join(" OR ", 
			      map{"e.code=".sql_quote($_,1)} @wanted).")");
	    }
	    if (@unwanted) {
		push(@w,
		     map{"e.code!=".sql_quote(substr($_,1))} @unwanted);
	    }
	    push(@$where,@w);
	}
    }
    map{
        if ($_ =~ s/\*/\%/g) {
            $_ =~ s/=/ like /g;
        }
    }@$where;
    my $hl = select_hashlist
      ($dbh,
       $tables,
       $where,
       "distinct gp.id",
       "gp.symbol"
      );


    return [map{$_->{id}}@{$hl || []}];
}

#gp hash key=id, val=symbol
# used for gene name search from AmiGO
sub get_product_hash {
    my $self = shift;
    my $constr = shift || return;
    my $dbh = $self->dbh;

    my $tables = ["gene_product gp", "association a", "evidence e"];
    my $where =  ["a.gene_product_id=gp.id", "a.id=e.association_id"];

    if ($constr->{names}) {
        my $n = $constr->{names};
        $n = [$n] unless (ref($n) eq 'ARRAY');
        push @$where,
          (
          "(".join(" or ",map{"gp.symbol = '$_' or gp.full_name = '$_'"}@$n).")");
    } elsif ($constr->{synonym}) {
        my $s = $constr->{synonym};
        $s = [$s] unless (ref($s) eq 'ARRAY');
        push @$tables, "gene_product_synonym synonym";
        push @$where,
          ("gp.id=synonym.gene_product_id",
          "(".join(" or ",map{"synonym.product_synonym = '$_'"}@$s).")"
          );
    } elsif ($constr->{symbol}) {
        my $s = $constr->{symbol};
        $s = [$s] unless (ref($s) eq 'ARRAY');
        push @$where, "(".join(" or ",map{"gp.symbol = '$_'"}@$s).")";
    } elsif ($constr->{full_name}) {
        my $s = $constr->{full_name};
        $s = [$s] unless (ref($s) eq 'ARRAY');
        push @$where, "(".join(" or ",map{"gp.full_name = '$_'"}@$s).")";
    }
    if ($constr->{xref_key}) {
        my $x = $constr->{xref_key};
        $x = [$x] unless (ref($x) eq 'ARRAY');
        push @$tables, "dbxref";
        push @$where,
          ("gp.dbxref_id=dbxref.id",
           "(".join(" or ", map{"dbxref.xref_key = '$_'"}@$x).")");
    }
    if ($constr->{seq_acc} ||
        $constr->{seq_name}) {
        my $seq_acc = $constr->{seq_acc};
        my $seq_name = $constr->{seq_name};
        push(@$tables,
             qw(seq gene_product_seq));
        push(@$where,
             "seq.id = gene_product_seq.seq_id",
             "gene_product_seq.gene_product_id = gp.id");
        if ($seq_acc) {
            $seq_acc = [$seq_acc] unless (ref($seq_acc) eq 'ARRAY');
            push(@$tables,
                 "seq_dbxref", "dbxref AS seqxref");
            push(@$where,
                 "seq.id = seq_dbxref.seq_id",
                 "seqxref.id = seq_dbxref.dbxref_id",
                 "(".join(" or ", map{"seqxref.xref_key = ".sql_quote($_)}@$seq_acc).")");
        }
        if ($seq_name) {
            $seq_name = [$seq_name] unless (ref($seq_name) eq 'ARRAY');
            push(@$where,
                 "(".join(" or ",map{"seq.display_id = ".sql_quote($_)}@$seq_name).")");
        }
    }
    if ($constr->{term_type}) {
        my $o = $constr->{term_type};
        $o = [$o] unless (ref($o) eq 'ARRAY');
        push @$tables, "term";
        push @$where,
          ("term.id=a.term_id",
           "(".join(" or ", map{"term.term_type = ".sql_quote($_)}@$o).")");
    }
    my $filters = $self->filters || {};
    my $spdbs =
      $filters->{speciesdb} ||
        $filters->{speciesdbs};
    if ($spdbs) {
        if (!ref($spdbs)) {
            $spdbs = [$spdbs];
        }
        unless ($constr->{xref_key}) {
            push @$tables, "dbxref";
            push @$where, "gp.dbxref_id=dbxref.id";
        }
        my @wanted = grep {$_ !~ /^\!/} @$spdbs;
        my @unwanted = grep {/^\!/} @$spdbs;

        if (@wanted) {
            push(@$where,
                 "(".join(" OR ", 
                          map{"dbxref.xref_dbname=".sql_quote($_,1)} @wanted).")");
        }
        if (@unwanted) {
            push(@$where,
                 map{"dbxref.xref_dbname!=".sql_quote(substr($_,1))} @unwanted);
        }
    }
    # NCBI Taxa IDs
    my $taxids =
      $filters->{taxid} ||
        $filters->{taxids};

    if ($taxids) {
        if (!ref($taxids)) {
            $taxids = [$taxids];
        }
        push(@$tables, "species");
        push(@$where, "species.id = gp.species_id");

        my @wanted = grep {$_ !~ /^\!/} @$taxids;
        my @unwanted = grep {/^\!/} @$taxids;

        if (@wanted) {
            push(@$where,
                 "(".join(" OR ", 
                          map{"species.ncbi_taxa_id=$_"} @wanted).")");
        }
        if (@unwanted) {
            push(@$where,
                 map{"species.ncbi_taxa_id!=$_"} @unwanted);
        }
    }

    my $evcodes = $filters->{evcodes} || $filters->{evcode};
    my @w=();
    if ($evcodes) {
# often $evcodes points to an empty ARRAY
# added extra check, this removed unneeded join on evidence table.
# jmc Oct2006
	if (defined(@$evcodes)) {
	    my @wanted = grep {$_ !~ /^\!/} @$evcodes;
	    my @unwanted = grep {/^\!/} @$evcodes;
	    if (@wanted) {
		push(@w,
		     "(".join(" OR ", 
			      map{"e.code=".sql_quote($_,1)} @wanted).")");
	    }
	    if (@unwanted) {
		push(@w,
		     map{"e.code!=".sql_quote(substr($_,1))} @unwanted);
	    }
	    push(@$where,@w);
	}
    }
    map{
        if ($_ =~ s/\*/\%/g) {
            $_ =~ s/=/ like /g;
        }
    }@$where;
    my $hl = select_hashlist
      ($dbh,
       $tables,
       $where,
       "distinct gp.id, gp.symbol",
      );

    return {map{$_->{id}=>$_->{symbol}}@{$hl || []}};
}

sub get_all_product_with_seq_ids {
    my $self = shift;
    my $dbh = $self->dbh;
    my $ids = select_vallist($dbh,
			     "gene_product_seq",
			     undef,
			     "distinct gene_product_id");
    return $ids;
}

# internal method:
# sometimes the assoc files don't have
# a one-to-one mapping between accession and symbol
sub get_duplicate_products {
    my $self = shift;
    my $dbh = $self->dbh;
    my $pairs =
      select_rowlist($dbh,
		     "gene_product",
		     undef,
		     "id, dbxref_id");
    my %c = ();
    
    my @xids = ();
    map {
	if ($c{$_->[1]}) {
	    push(@xids, $_->[1]);
	}
	$c{$_->[1]} = 1;
    } @$pairs;
    my $xrefs =
      select_hashlist($dbh,
		      "dbxref",
		      sqlin("id", \@xids));
    return $xrefs;
}

sub _get_product_property {
    my $self = shift;
    my $pds = shift || return;

    $pds = [$pds] unless (ref($pds) eq 'ARRAY');
    if ($self->has_gp_property_table && @{$pds || []}) {
        my %gp_h = ();
        map {push @{$gp_h{$_->id}}, $_}@$pds; #m objs for one gene product id!!
        #check properties have ever been set to prevent from double get--could be expensive
        my (%ids);
        map {$ids{$_->id}++ unless ($_->properties)}@$pds;
        my $p_ids = join(',', keys %ids);
        return unless $p_ids;
        my $p_hl = select_hashlist
          ($self->dbh,
           "$GPPROPERTY",
           ["gene_product_id in ($p_ids)"],
          );
        map {
            my $id = $_->{gene_product_id};
            my $ps = $gp_h{$id};
            foreach my $p (@{$ps || []}) {
                $p->set_property($_->{property_key}, $_->{property_val});
            }
        } @{$p_hl || []};
    }
    return $pds;
}

sub _get_product_seqs {
    my $self = shift;
    my $pds = shift || return;

    $pds = [$pds] unless (ref($pds) eq 'ARRAY');

    return unless $pds;

    my %ph = map {$_->_seqs_obtained(1); $_->id=>$_} @$pds;
    my @pids = keys %ph;
    return unless @pids;
    my $hl =
      select_hashlist
        ($self->dbh,
         ["gene_product_seq", "seq"],
         ["seq.id = seq_id", "gene_product_id in (".join(',', @pids).")"]
        );

    my (@seqs, @byid);
    foreach my $h (@$hl) {
        my $seq = $self->create_seq_obj($h);
        my $p = $ph{$h->{gene_product_id}};
        $p->add_seq($seq);
        push(@seqs, $seq);
        $byid[$seq->id] = $seq;
    }
    if (@byid) {
        $hl =
          select_hashlist($self->dbh,
                          ["dbxref", "seq_dbxref"],
                          ["dbxref.id = dbxref_id",
                           "seq_id in (".join(", ", map {$_->id} @seqs).")"],
                          "dbxref.*, seq_id");
        map {
            $byid[$_->{seq_id}]->add_xref(GO::Model::Xref->new($_));
        } @$hl;
    }
    return;
}

sub _get_product_synonyms {
    my $self = shift;
    my $pds = shift || return;

    $pds = [$pds] unless (ref($pds) eq 'ARRAY');

    return unless $pds;

    my %ph = map {$_->id=>$_} @$pds;
    my @pids = keys %ph;
    return unless @pids;
    my $hl =
      select_hashlist($self->dbh,
		      "gene_product_synonym",
		      "gene_product_id in (".join(',', @pids).")");
    foreach (@$hl) {
	$ph{$_->{gene_product_id}}->add_synonym($_->{product_synonym});
    }
    return;
}

sub _get_product_species {
    my $self = shift;
    
    # arrayref of geneproduct objects
    my $pds = shift || return;

    $pds = [$pds] unless (ref($pds) eq 'ARRAY');

    return unless $pds;

    my %taxid = map { ($_->{species_id} || 0, 1) } @$pds;
    my @taxids = grep {$_} keys %taxid;
    if (!@taxids) {
	return;
    }

    my $hl =
      select_hashlist($self->dbh,
		      "species",
                      sqlin('id', \@taxids));
    foreach (@$hl) {
	$taxid{$_->{id}} =
	  $self->create_species_obj($_);
    }
    foreach (@$pds) {
        if ($_->{species_id} && $taxid{$_->{species_id}}){
	    $_->species($taxid{$_->{species_id}});
            delete $_->{species_id};
        }
    }
    return;
}

sub _get_product_types {
    my $self = shift;
    # arrayref of geneproduct objects
    my $pds = shift || return;

    $pds = [$pds] unless (ref($pds) eq 'ARRAY');

    return unless $pds;

    # product types
    # SHULY Nov 28,04 - changed the type_id to _type - I think it was a bug - looking at the $pds structure reveals that this argument exist in the hash and not type_id
     my @pl_wt = grep {$_->{type_id}} @$pds;
    #my @pl_wt = grep {$_->{_type}} @$pds;

    if (@pl_wt) {
        # only those with non-null type
    	# SHULY Nov 28,04
        my @typeids = map {$_->{type_id}} @pl_wt;
      # my @typeids = map {$_->{_type}} @pl_wt;
        foreach my $j (@typeids){
        }
        my %utypeids = map {$_=>1} @typeids;
        my $hl =
          select_hashlist($self->dbh,
                          "term",
                          sqlin('id', [keys %utypeids]),
                          "id, name");
        foreach my $h (@$hl) {
            $utypeids{$h->{id}} = $h->{name};
        }
        foreach my $p (@pl_wt) {
            my $typeid = $p->{type_id};
          # my $typeid = $p->{_type};
          # delete $p->{type_id};
          # if ($utypeids{$typeid}) {
            $p->type($utypeids{$typeid});
          # }
        }
    }
    return;
}

sub _get_evidence_seq_xrefs {
    my $self = shift;
    my $asso = shift || return;

    unless (ref($asso) eq 'ARRAY') {
        $asso = [$asso];
    }
    my %ev_h;
    map{map{$ev_h{$_->id}=$_;$_->seq_xref_list([])}@{$_->evidence_list || []}}@{$asso || []};
    return unless (keys %ev_h);
    my $hl = select_hashlist
      ($self->dbh,
       ["evidence_dbxref", "dbxref"],
       ["dbxref.id = dbxref_id",
       "evidence_id IN (".join(",",keys %ev_h).")"],
       "distinct *",
      );
    foreach my $h (@{$hl || []}) {
        my $ev = $ev_h{$h->{evidence_id}};
        $ev->add_seq_xref(GO::Model::Xref->new($h));
    }
}

sub _get_n_children_h {
    my $self = shift;
    my $graph = shift;

    my %term_h = map{$_->id => $_}@{$graph->get_all_nodes || []};
    my %acc_h = map{$_->acc => $_}@{$graph->get_all_nodes || []};
    return unless (keys %term_h);
    my $hl = select_hashlist
      ($self->dbh,
       ["term p", "term2term"],
       ["p.id = term1_id", "term1_id in (".join(",",keys %term_h).")"],
       ["count(*) as c", "p.acc"],
       [],
       ["acc"]
      );
    map{$graph->{n_children_h}->{$_->{acc}}=$_->{c}}@{$hl || []};
    map{$graph->{n_children_h}->{$_} = 0 unless $graph->{n_children_h}->{$_}}keys %acc_h;
}

# lowercases db/acc
sub get_taxa_id_lookup {
    my $self = shift;
    my $dbh = $self->dbh;
    my $hl =
      select_hashlist($dbh,
		      ["dbxref",
		       "gene_product",
		       "species"],
		      ["dbxref.id = gene_product.dbxref_id",
		       "species_id = species.id"],
		      ["xref_dbname", "xref_key", "ncbi_taxa_id"]);
    my %look =
      map {
	  my $k = lc("$_->{xref_dbname}:$_->{xref_key}");
	  ($k => $_->{ncbi_taxa_id})
      } @$hl;
    return \%look;
}

sub get_taxa_id_for_product_acc {
    my $self = shift;
    my $pracc = shift;
    my $dbh = $self->dbh;
    my ($db, @acc) = split(/:/, $pracc);
    my $acc = join(":",@acc);
    my $h =
      select_hash($dbh,
		  ["dbxref",
		   "gene_product",
		   "species"],
		  ["dbxref.id = gene_product.dbxref_id",
		   "species_id = species.id",
		   "xref_dbname = ".sql_quote($db),
		   "xref_key = ".sql_quote($acc)],
		  ["ncbi_taxa_id"]);
    return $h->{ncbi_taxa_id};
}

sub get_seqs {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($constr, $attrs) =
      rearrange([qw(constraints attributes)], @_);
    
    my @table_arr = ("seq");
    my @constr_arr = ();
    if (!ref($constr)) {
	confess("constraints must be hashref!");
    }
    foreach my $att (qw(seq md5checksum id display_id desc)) {
	if (ref($constr) eq "HASH") {
	    if (defined($constr->{$att})) {
		push(@constr_arr, "seq.$att = ".sql_quote($constr->{$att}));
	    }
	}
	else {
	    if (defined($constr->$att())) {
		push(@constr_arr, "seq.$att = ".sql_quote($constr->$att()));
	    }
	}
    }
    if ($constr->{ids}) {
        push(@constr_arr, "id in (".join(",", @{$constr->{ids}}).")");
    }
    if ($constr->{product}) {
        my $product = $constr->{product};
        push(@table_arr, "gene_product_seq");
        if ($product->id) {
            push(@constr_arr,
                 "gene_product_seq.seq_id=seq.id",
                 "gene_product_seq.gene_product_id=".$product->id,
                );
        }
    }

    if ($constr->{dbxref_xref_key}) {
	# not fetching a dbxref object to save a query - we need to do this millions of times
	my $xref_key = $constr->{dbxref_xref_key};
        push(@table_arr, "seq_dbxref");
        push(@table_arr, "dbxref");
	push(@constr_arr,
	     "seq_dbxref.seq_id=seq.id",
	     "seq_dbxref.dbxref_id = dbxref.id",
	     "dbxref.xref_key= ".sql_quote($xref_key),
	     );
    }

    my @seqs = ();
    my $hl =
      select_hashlist($dbh,
                      \@table_arr,
                      \@constr_arr,
                      "seq.*");
    my @byid = ();
    foreach my $h (@$hl) {
	my $seq = $self->create_seq_obj($h);
	push(@seqs, $seq);
        $byid[$seq->id] = $seq;
    }
    if (@byid) {
        $hl =
          select_hashlist($dbh,
                          ["dbxref", "seq_dbxref"],
                          ["dbxref.id = dbxref_id",
                           "seq_id in (".join(", ", map {$_->id} @seqs).")"],
                          "dbxref.*, seq_id");
        map {
            $byid[$_->{seq_id}]->add_xref(GO::Model::Xref->new($_));
        } @$hl;
    }
                         
    return \@seqs;
}

sub get_seq {
    my $self = shift;
    my $pl = $self->get_seqs(@_);
    return shift @$pl;
}

sub get_species {
    my $sl = shift->get_species_list(@_);
    return $sl->[0];
}

sub get_species_list {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($constr) =
	rearrange([qw(constraints)], @_);
    require "Bio/Species.pm";  # use require in case bp not installed

    if (!$constr) {
        $constr = {};
    }
    my $hl =
	select_hashlist($dbh,
			["species", "gene_product"],
			["species.id=gene_product.species_id"],
			"distinct species.*");
    my @sl =
      map {
	  GO::Model::Species->new($_);
      } @$hl;
    return \@sl;
}

sub get_species_hash {
    my $self = shift;
    my $sl = $self->get_species_list;
    return {map{($_->ncbi_taxa_id=>$_)} @$sl};
}

sub get_speciesdb_dict {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($constr) =
	rearrange([qw(constraints)], @_);
    require "Bio/Species.pm";  # use require in case bp not installed

    if (!$constr) {
        $constr = {};
    }
    my @constr_arr = ();
    if (!ref($constr)) {
        $constr->{speciesdb} = $constr;
    }
    if (defined($constr->{speciesdb})) {
	push(@constr_arr, "xref_dbname = ".sql_quote($constr->{speciesdb}));
    }

    # check its in db
    my $hl =
	select_hashlist($dbh,
			["dbxref", "gene_product"],
			["dbxref.id=gene_product.dbxref_id",
			 @constr_arr],
			"distinct dbxref.xref_dbname AS speciesdb");
    my %sd=();
    foreach my $h (@$hl) {
	my $n = lc($h->{speciesdb});
	$n =~ s/sgd/budding yeast/;
	$n =~ s/fb/fruitfly/;
	$n =~ s/pom.*/fission yeast/;
	$n =~ s/mgi/mouse/;
	$n =~ s/tair/arabidopsis/;
	my $s = 
	  Bio::Species->new;
        $s->common_name($n);
        $sd{$h->{speciesdb}} = $s;
    }
    return \%sd;
}

sub get_speciesdbs {
    my $self = shift;
    my $h = $self->get_speciesdb_dict(@_);
    return [keys %$h];
}

sub get_dbxrefs {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($constr) =
	rearrange([qw(constraints)], @_);
    my @constr_arr = ();
    if (!ref($constr)) {
	confess("constraints must be hashref!");
    }
    my @tables = ("dbxref");
    my @select_arr = "*";
    if ($constr->{xref_key}) {
	push(@constr_arr,
	     "xref_key = ".sql_quote($constr->{xref_key}));
	delete $constr->{xref_key};
    }
    if ($constr->{xref_dbname}) {
	push(@constr_arr,
	     "xref_dbname = ".sql_quote($constr->{xref_dbname}));
	delete $constr->{xref_dbname};
    }
    if ($constr->{xref_keytype}) {
	push(@constr_arr,
	     "xref_keytype = ".sql_quote($constr->{xref_keytype}));
	delete $constr->{xref_keytype};
    }
#    push(@constr_arr, 
#	 map {"$_ = ".sql_quote($constr->{$_})} keys %$constr);
    my $hl=
	select_hashlist($dbh,
			\@tables,
			\@constr_arr,
			\@select_arr);
    my @xref_l =
	map {GO::Model::Xref->new($_)} @{$hl};
    return \@xref_l;
}

#qualifier is just a GO::Model:Term
#in the future, qualifier should be an obj by itself (qualifier value)
sub get_qualifiers {
    my $self = shift;
    my $asso = shift;

    my $dbh = $self->dbh;
    my %asso_h;
    unless (ref($asso) eq 'ARRAY') {
        $asso = [$asso];
    }
    map{$asso_h{$_->id} = $_}@{$asso || []};
    return unless (grep{$_}keys %asso_h);
    my $hl = select_hashlist
      ($dbh,
       ["association_qualifier aq", "term t"],
       ["aq.term_id = t.id", "aq.association_id IN (".join(",",keys %asso_h).")"],
       ["aq.association_id", "t.*"]
      );
    my %term_h;
    foreach my $h (@{$hl || []}) {
        push @{$term_h{$h->{association_id}}}, GO::Model::Term->new($h);
    }
    map{$asso_h{$_}->qualifier_list($term_h{$_})}keys %term_h;
    return [values %term_h];
}

#should get assigned_by when get association?
sub get_assigned_by {
    my $self = shift;
    my $asso = shift;

    my $dbh = $self->dbh;

    my %source_db_h;
    unless (ref($asso) eq 'ARRAY') {
        $asso = [$asso];
    }
    map{push @{$source_db_h{$_->source_db_id}}, $_}@{$asso || []};
    return unless (grep{$_}keys %source_db_h);
#printf STDERR "num:%d sc db id:%s\n",scalar(keys %source_db_h), join(",",keys %source_db_h);
    my $hl = select_hashlist($dbh, "db", "id IN (".join(",",keys %source_db_h).")");
    foreach my $h (@{$hl || []}) {
        map{$_->assigned_by($h->{name})}@{$source_db_h{$h->{id}} || []};
    }
}

sub show {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($constr) =
      rearrange([qw(constraints)], @_);
    my $id_h = {};
    my $term  = $self->get_term($constr);
    
}

# maybe usefor for some
# clients to sneakily bypass
# interface into sql
sub _tunnel {
    my $self = shift;
    my $sql = shift;
    my $dbh = $self->dbh;
    if ($sql =~ /^select/) {
	return $dbh->selectall_arrayref($sql);
    }
    return $dbh->do("$sql");
}

sub get_statistics {
    my $self = shift;

    require GO::Stats;
    my $s = GO::Stats->new;
    $s->apph($self);
    $s;
}

sub get_stat_tags {
    my $self = shift;
    my $dbh = $self->dbh;

    my $tags = 
      [
       "gene products"=>"select count(id) from gene_product",
      ];

    map {
	push(@$tags,
	     "$_"=>"select count(id) from term where term_type = '$_'");
    } GO::Model::Term->_valid_types;


    my $hl =
      select_hashlist($dbh,
		      ["dbxref", "gene_product"],
		      "dbxref.id = gene_product.dbxref_id",
		      "distinct dbxref.xref_dbname");
    foreach my $h (@$hl) {
	my $dbname = $h->{"xref_dbname"};
	push(@$tags, $dbname => ["select count(association.id) from association, gene_product, dbxref where association.gene_product_id = gene_product.id and gene_product.dbxref_id = dbxref.id and dbxref.xref_dbname = ".sql_quote($dbname), "select count(gene_product.id) from gene_product, dbxref where gene_product.dbxref_id = dbxref.id and dbxref.xref_dbname = ".sql_quote($dbname)]);
    }
    
    my @rtags = ();
    for (my $i=0; $i < @$tags; $i+=2) {
	my @vals;
	if (ref($tags->[$i+1]) eq "ARRAY") {
	    @vals =
	      map {
		  get_result_column($dbh, $_);
	      } @{$tags->[$i+1]};
	}
	else {
	    @vals=(get_result_column($dbh, $tags->[$i+1]));
	}
	push(@rtags, ($tags->[$i]=>join(" / ", @vals)));
    }
    return \@rtags;
}

sub get_distances {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($termh1, $termh2) =
      rearrange([qw(term1 term2)], @_);
    my $term1 = 
      (ref($termh1) eq "HASH" || !$termh1->id)
	? $self->get_term($termh1, "shallow") : $termh1;
    my $term2 = 
      (ref($termh2) eq "HASH" || !$termh2->id)
	? $self->get_term($termh2, "shallow") : $termh2;
    if ($self->has_path_table) {
        my $hl =
          select_hashlist($dbh,
                          "$PATH",
                          ["term1_id = ".$term1->id,
                           "term2_id = ".$term2->id],
                          "distance");
        return [map {$_->{distance}}@$hl];
    }
    else {
        confess("NOT IMPLEMENTED");
    }
}

sub get_paths_to_top {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($termh, $template) =
      rearrange([qw(term template)], @_);
    my $term =
      (ref($termh) eq "HASH" || !$termh->id)
	? $self->get_term($termh, $template) : $termh;
    if (!$term) {
	return;
    }
    my $graph = $self->get_graph_to_top($term->acc, $template);
    return $graph->paths_to_top($term->acc);
}

sub get_graph_to_top {
    my $self = shift;
    my $dbh = $self->dbh;
    my ($acc, $templ) = 
      rearrange([qw(term template)], @_);

    my %t = %{$self->graph_template($templ) || {}};
    $t{traverse_down} = 0;
    $t{traverse_up} = 1;
    $self->get_graph($acc, 0, \%t);
}

sub new_acc {
    my $self = shift;
    my $dbh = $self->dbh;
    my $h =
      select_hash($dbh,
                  "term",
                  undef,
                  "max(acc)+1 AS acc",
                  );
    my $acc = $h->{acc};
    confess unless $acc;
    return $acc;

}

sub get_matrix {
    my $self = shift;
    my $terms = shift;

    my $dbh = $self->dbh;

    my @term1_ids = map {$_->id} @$terms;
    my $hl =
      select_hashlist($dbh,
                      "graph_path",
                      "term1_id in (".
                      join(",",@term1_ids).")",
                     );
    my $acchl =
      select_hashlist($dbh,
                      "term");
    my %acch=();
    $acch{$_->{id}}  = $_->{acc} foreach @$acchl;

    my %matrix = ();
    foreach my $h (@$hl) {
        my $acc1 = $acch{$h->{term1_id}};
        my $acc2 = $acch{$h->{term2_id}};
        my $distance = $h->{distance};
        $matrix{$acc2} = [] unless $matrix{$acc2};
        push(@{$matrix{$acc2}},
             [$acc1, $distance]);
    }
    return \%matrix;
}

sub map_to_slim {
    my $self = shift;
    my $dbh = $self->dbh;
    #...
}

sub get_pairs {
    my $self = shift;
    my $dbh = $self->dbh;

    my $rows =
      select_rowlist($dbh,
		     "term2term, term t1, term t2, term rt",
		     "term1_id = t1.id AND t2.id= term2_id AND rt.id = relationship_type_id",
		     "rt.name, t2.acc, t1.acc");
    return $rows;
}

sub bulk_load {
    my $self = shift;
    my $tablesRef = shift; # hash ref of tables -> [ columns ]
    my $term = shift;
    my $dbh = $self->dbh;

    my $lock = "lock tables ".join(' write, ',keys %$tablesRef);

    $lock .= ' write';
    print STDERR "$lock\n";

    $dbh->do($lock);

    for my $tab (keys %$tablesRef) {

	next unless -e "$tab.txt";

	my $cols = join(',', @{$tablesRef->{$tab}});

	my $stmt = "load data local infile \'$tab.txt\' into table $tab";
	$stmt .= " fields terminated by \'$term\'" if $term;
	$stmt .= "  ($cols)";

	print STDERR "$stmt\n";
	$dbh->do($stmt);
    }

    $dbh->do("unlock tables");

}
    

sub get_closure {
    my $self = shift;
    my $dbh = $self->dbh;

    my $root_term = $self->get_root_term;
    my $root_id = $root_term->id;

    my $closure =
      select_rowlist($dbh,
		     ["graph_path AS graph_path",
		      "term2term r",
		      "term p", 
		      "term c", 
		      "term rt",
		     ],
		     [
		      "r.term1_id = p.id",
		      "c.id= r.term2_id",
		      "rt.id = relationship_type_id",
		      "graph_path.term1_id = $root_id",
		      "graph_path.term2_id = r.term2_id",
		     ],
		     [qw(graph_path.term2_id graph_path.id rt.name c.acc p.acc)],
		     [qw(graph_path.term2_id graph_path.id graph_path.distance)],
#		     [qw(graph_path.id)],
		    );
    my @paths = ();
    my $last_path_id = 0;
    foreach my $c (@$closure) {
	if ($c->[0] != $last_path_id) {
	    push(@paths, []);
	}
	$last_path_id = shift @$c;
	push(@{$paths[-1]}, $c);
    }
    return \@paths;
}

# temp hack
sub __fix_interpro {
    my $self = shift;
    my $dbh = $self->dbh;

    my $hl=
      select_hashlist($dbh,
                      "dbxref",
                      {xref_dbname=>"InterPro"});
    foreach my $h (@$hl) {
        if ($h->{xref_key} =~ /(IPR\d+) (.*)/) {
            my ($k, $d) = ($1, $2);
            my $got =
              select_hash($dbh,
                          "dbxref",
                          {xref_key=>$k,
                           xref_dbname=>"interpro"});
            if ($got) {
                eval {
                    update_h($dbh,
                             "term_dbxref",
                             {dbxref_id=>$got->{id}},
                             "dbxref_id=$h->{id}");
                };
                sql_delete($dbh,
                           "dbxref",
                           "id=$h->{id}");
            }
            else {
                update_h($dbh,
                         "dbxref",
                         {xref_key=>$k,
                          xref_desc=>$d},
                         "id=$h->{id}");
            }
        }
    }
}

1;
