package GOBO::Parsers::OBOParser;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Types;

extends 'GOBO::Parsers::Parser';
with 'GOBO::Parsers::GraphParser';

use Data::Dumper;
use GOBO::Graph;

has default_namespace => (is=>'rw', isa=>'Str');
has format_version => (is=>'rw', isa=>'Str');

#has header_record_separator => (is=>'ro', isa=>'Str', default=>"\n");
has body_record_separator => (is=>'ro', isa=>'Str', default=>"\n\n\[");

has '+input_record_separator' => (default => "\n\n[");

has header_check_sub => (is=>'rw', isa=>'CodeRef', default=>sub{ return sub { return 1 }; }, writer => 'set_header_check_sub', reader => 'get_header_check_sub');
has stanza_check_sub => (is=>'rw', isa=>'CodeRef', default=>sub{ return sub { return 1 }; }, writer => 'set_stanza_check_sub', reader => 'get_stanza_check_sub');
has tag_check_sub    => (is=>'rw', isa=>'CodeRef', default=>sub{ return sub { return 1 }; }, writer => 'set_tag_check_sub', reader => 'get_tag_check_sub');

## how much gubbins to allow during parsing
has strict_mode => (is=>'rw', isa=>'Bool', default => 1);

## whether to use the dispatch hash or the if/else parser
has parse_method => (is=>'rw', isa=>'GOBO::Parsers::ParserMode', default=>'dispatch_hash');

## whether to include obsoletes in the results or not
has ignore_obsoletes => (is=>'rw', isa=>'Bool');

before 'get_header_check_sub' => sub {
	my $self = shift;
	$self->check_options if ! $self->checked_options;
};

before 'get_stanza_check_sub' => sub {
	my $self = shift;
	$self->check_options if ! $self->checked_options;
};

before 'get_tag_check_sub' => sub {
	my $self = shift;
	$self->check_options if ! $self->checked_options;
};

#=cut
override 'set_options' => sub {
	my $self = shift;
	my $o = shift;

	if ($o->{header} || $o->{body} || $o->{ignore_obsoletes})
	{	$self->check_options($o);
	}
	$self->SUPER::set_options($o);

};
#=cut

#sub set_input_record_separator {
#	my $self = shift;
#	my $sep = shift;
#	return unless $sep;
#	$self->input_record_separator($sep);
#}

## validate the options that we have

sub check_options {
	my $self = shift;
	my $options = $self->options;
#	print STDERR "OBOParser check options: " . Dumper($options);
	if ($options && values %$options)
	{	if ($options->{ignore_obsoletes})
		{	delete $options->{ignore_obsoletes};
			$self->ignore_obsoletes(1);
		}

		# get rid of any existing options
		my $hpo = _check_header_parser_options($options);
		if (! $hpo)
		{	delete $options->{header};
		}
		else
		{	$options->{header} = $hpo;
			if ($hpo->{parse_only})
			{	my $arr = $hpo->{parse_only};
				my $code =
				$self->set_header_check_sub( sub {
					my $t = lc(shift);
					return 1 if grep { $t eq $_ } @$arr;
					return undef;
				} );
			}
			elsif ($hpo->{ignore})
			{	my $arr = $hpo->{ignore};
				$self->set_header_check_sub( sub {
					my $t = lc(shift);
					return 1 unless grep { $t eq $_ } @$arr;
					return undef;
				} );
			}
			elsif ($hpo->{ignore_all})
			{	$self->set_header_check_sub( sub { return undef; } );
			}
		}

		my $bpo = _check_body_parser_options($options);
		if ($bpo)
		{	$options->{body} = $bpo;
			if ($bpo->{parse_only})
			{	my $b_hash = $bpo->{parse_only};
				# parse this stanza if the stanza type exists in the parse_only set
				# otherwise, go to the next stanza
				## are we ignoring obsoletes?
##				if ($options->{ignore_obsoletes})
				$self->set_stanza_check_sub( sub {
					my $s = lc(shift);
					return 1 if $b_hash->{$s};
#					$self->next_stanza([ keys %$b_hash ]);
					return undef;
				} );

				# if the stanza type exists and the tag exists, we're good
				# otherwise, go to the next stanza
				$self->set_tag_check_sub( sub {
					my ($s, $t) = @_;
					if ($b_hash->{$s})
					{	if ( $b_hash->{$s}[0] eq '*' || grep { $t eq $_ } @{$b_hash->{$s}} )
						{	return 1;
						}
						return undef;
					}
					# we should have already caught incorrect stanzas, but n'mind...
					warn "Incorrect stanza type!\n";
#					$self->next_stanza([ keys %$b_hash ]);
					return undef;
				} );
			}
			elsif ($bpo->{ignore})
			{	my $b_hash = $bpo->{ignore};
				my @ignore_all = grep { $b_hash->{$_}[0] eq '*' } keys %$b_hash;
				if (@ignore_all)
				{	# ignore this stanza if the stanza type exists in the ignore all set
					$self->set_stanza_check_sub( sub {
						my $s = lc(shift);
						if (grep { $s eq $_ } @ignore_all)
						{	#$self->next_stanza(\@ignore_all, 'ignore');
							return undef;
						}
						return 1;
					} );
				}

				# ignore the stanza if the stanza type exists in the ignore set
				# skip the line if the line type exists or the full stanza is to be ignored
				$self->set_tag_check_sub( sub {
					my ($s, $t) = @_;
	#				print STDERR "\n$s_type $t";
					return 1 if ! $b_hash->{$s};
					return undef if ( $b_hash->{$s}[0] eq '*' || grep { /^$t$/i } @{$b_hash->{$s}} );
	#				print STDERR "=> OK\n";
					return 1;
				} );
			}
		}
	}
#	$options->{checked} = 1;
	$self->options($options);
	$self->checked_options(1);
#	print STDERR "OBOParser post-check options: " . Dumper($self->options);
}

sub _check_header_parser_options {
	my $o = shift;
	return undef if ( ! $o || ! values %$o || ! $o->{header} );
	my $h = $o->{header};
	if (! ref $h)
	{	if ($h eq 'ignore_all')
		{ return { ignore_all => 1 }; }
		else
		{ return undef; }
	}
	return { ignore_all => 1 } if $h->{ignore_all};
	return undef unless $h->{ignore} || $h->{parse_only};

	if ($h->{ignore} && $h->{parse_only})
	{	warn "Warning: both ignore and parse_only specified in header parsing options; using setting in parse_only";
		delete $h->{ignore};
	}

	foreach my $x qw(ignore parse_only)
	{	next unless $h->{$x};
		if (! ref $h->{$x})
		{	if ($h->{$x} eq '*')
			{	return undef if $x eq 'parse_only';
				return { ignore_all => 1 };
			}
			else
			{	return { $x => [ $h->{ lc($x) } ] };
			}
		}
		elsif (ref $h->{$x} eq 'ARRAY')
		{	my %hash;
			map { $hash{lc($_)}++ } @{$h->{$x}};
			return { $x => [ keys %hash ] };
		}
		elsif (ref $h->{$x} eq 'HASH')
		{	## convert the keys into the ARRAY
			return { $x => [ map { lc($_) } keys %{$h->{$x}} ] };
		}
		else
		{	warn "wrong header options format";
		}
	}
	return undef;
}

sub _check_body_parser_options {
	my $o = shift;
	return undef if ( ! $o || ! values %$o || ! $o->{body} );
	my $h = $o->{body};
	if (! ref $h)
	{	if ($h eq 'ignore_all')
		{ return { ignore_all => 1 }; }
		else
		{ return undef; }
	}
	if ($h->{ignore} && $h->{parse_only})
	{	warn "Warning: both ignore and parse_only specified in header parsing options; using setting in parse_only";
		delete $h->{ignore};
	}
	my $b_hash;
	foreach my $x qw(ignore parse_only)
	{	next unless $h->{$x};
		if (! ref $h->{$x})
		{	if ($h->{$x} eq '*')  ## i.e. either ignore_all or parse_only
			{	if ($x eq 'ignore')
				{	return { ignore_all => 1 };
				}
				else
				{	return undef;
				}
			}
			else
			{	$b_hash->{ $h->{$x} } = ['*'];
			}
		}
		elsif (ref $h->{$x} eq 'ARRAY')
		{	my %hash;  ## make sure we don't have dupes
			map { $hash{$_}++ } @{$h->{$x}};
			map { $b_hash->{$_} = ['*'] } keys %hash;
		}
		elsif (ref $h->{$x} eq 'HASH')
		{	## stanza types
			foreach my $s_type (keys %{$h->{$x}})
			{	if (! ref $h->{$x}{$s_type})
				{	$b_hash->{$s_type} = [ $h->{$x}{$s_type} ];
				}
				elsif (ref $h->{$x}{$s_type} eq 'ARRAY')
				{	my %hash;
					map { $hash{$_}++ } @{$h->{$x}{$s_type}};
					$b_hash->{$s_type} = [ keys %hash ];
				}
				elsif (ref $h->{$x}{$s_type} eq 'HASH')
				{	$b_hash->{$s_type} = [ keys %{$h->{$x}{$s_type}} ];
				}
				else
				{	warn "wrong body options format";
				}
			}
		}
		else
		{	warn "wrong body options format";
		}

		if ($b_hash)
		{	return { $x => $b_hash };
		}
	}
	return undef;
}


=head2 parse_from_array

Parse from an array of lines

input:  self, args with $args->{array} being the array of lines in question
output: the Graph object

=cut

sub parse_from_array {
	my $self = shift;
	my %args = (@_);
	confess( (caller(0))[3] . ": missing required arguments" ) unless defined $args{array} && @{$args{array}};

	## we need the "lines" to be separated by the body stanza separator
	my @new_arr = split( quotemeta($self->body_record_separator), join("\n", @{$args{array}}));
	$new_arr[0] =~ s/^\s*//s;
	if ($new_arr[0] =~ /^\[\w+\]/)
	{	## no header present?
	}
	else
	{	$self->parse_header( header => [ split("\n", shift @new_arr) ] );
	}
	$self->lines( [ @new_arr ] );
	$self->parse_body;
}

=head2 parse_header_from_array

Get a header from an array of lines, rather than passing in a file

input:  self, args with $args->{array} being the array of lines in question
output: the Graph object

=cut

sub parse_header_from_array {
	my $self = shift;
	my %args = (@_);
	confess( (caller(0))[3] . ": missing required arguments" ) unless defined $args{array} && @{$args{array}};
	my @header;
	foreach (@{$args{array}})
	{	last if /^\[.*?\]/;
		push @header, $_;
	}
#	print STDERR "header: " . Dumper(\@header) . "\n";
	$self->parse_header( header => \@header );
}


=head2 parse_body_from_array

Get a graph from an array of lines, rather than passing in a file

input:  self, args with $args->{array} being the array of lines in question
output: the Graph object

=cut

sub parse_body_from_array {
	my $self = shift;
	my %args = (@_);
	confess( (caller(0))[3] . ": missing required arguments" ) unless defined $args{array} && @{$args{array}};

	## we need the lines to be separated by the body stanza separator
	my @new_arr = split(quotemeta($self->body_record_separator), join("\n", @{$args{array}} ));
	$new_arr[0] =~ s/^\s*//s;
	while ($new_arr[0] !~ /[a-z]/i && $new_arr[0] !~ /^\[{0,1}\w+\]/)
	{	$new_arr[0] =~ s/^\s*//s;
		shift @new_arr;
	}
	$self->lines( [ @new_arr ] );
	$self->parse_body;
}


my $header_subs = {
	'data-version' => sub {
		my ($self, $args) = @_;
		$args->{graph}->version($args->{value});
	},
	'date' => sub {
		my ($self, $args) = @_;
		$args->{graph}->date($args->{value});
	},
	'default' => sub {
		my ($self, $args) = @_;
		$args->{graph}->set_property_value($args->{tag},$args->{value});
	},
	'default-namespace' => sub {
		my ($self, $args) = @_;
		$self->default_namespace($args->{value});
	},
	'format-version' => sub {
		my ($self, $args) = @_;
		$self->format_version($args->{value});
	},
	'remark' => sub {
		my ($self, $args) = @_;
		$args->{graph}->comment($args->{value});
	},
	'subsetdef' => sub {
		my ($self, $args) = @_;
		# subsetdef: gosubset_prok "Prokaryotic GO subset"
		if ($args->{value} =~ /^(\S+)\s+\"(.*)\"/)
		{	my ($id,$label) = ($1,$2);
			my $ss = new GOBO::Subset(id=>$id, label=>$label);
			$args->{graph}->subset_index->{$id} = $ss;
		}
		else {
			warn "Uh-oh... subset value " . $args->{value};
		}
	},
#	'synonymtypedef' => {
#	## TODO!
#	},
};


sub parse_header {
	my $self = shift;
	my %args;
	if (@_)
	{	%args = (@_);
	}
	if (! $args{header} || ! @{$args{header}})
	{	my @header;
		## make sure that we're using the correct record separator
#		$self->set_input_record_separator($self->body_record_separator);
		while($_ = $self->next_line)
		{	next if ! /[a-z]/si;
			$_ =~ s/^\s*//s;
			if (/^\[{0,1}\w+\]/)
			{	$self->unshift_line($_);
				last;
			}
			## add this to our header array
			push @header, ( split("\n", $_) );
		}
		$args{header} = [ @header ];
	}

	if ($self->parse_method eq 'if_else')
	{	#warn "Parsing in if/else mode!";
		return $self->parse_header_ie(%args);
	}
	else
	{	return $self->parse_header_dh(%args);
	}
}


sub parse_header_dh {
	my $self = shift;
	my %args = (@_);
	my $g = $self->graph;
	my $header_check = $self->get_header_check_sub;
	if (! $args{header} || ! @{$args{header}})
	{	warn "No header lines found!";
		return;
	}

	foreach (@{$args{header}}) {
		next unless /\S/;

#		print STDERR "DH line: $_\n";
		if (/^\[\w/) {
			warn "Start of content found in header arr!";
			$self->unshift_line($_);
			last;
		}

		if (/^(\S+):\s*(.*?)$/) {
			next unless &$header_check($1);
			if ($header_subs->{$1})
			{	$header_subs->{$1}->($self, { tag => $1, value => $2, graph => $g });
			}
			else
			{	$header_subs->{default}->($self, { tag => $1, value => $2, graph => $g });
			}
		}
	}

	# set the parse_header to 1
	$self->parsed_header(1);
	return;
};


sub parse_header_ie {
	my $self = shift;
	my %args = (@_);
	my $g = $self->graph;
	my $header_check = $self->get_header_check_sub;
	if (! $args{header} || ! @{$args{header}})
	{	warn "No header lines found!";
		return;
	}

	foreach (@{$args{header}}) {
		next unless /\S/;

		if (/^\[/) {
			warn "Start of content found in header arr!";
			$self->unshift_line($_);
			last;
		}

		if (/^(\S+):\s*(.*?)$/) {
			next unless &$header_check($1);
			my ($t,$v) = ($1,$2);
			if ($1 eq 'default-namespace') {
				$self->default_namespace($2);
			}
			elsif ($t eq 'subsetdef') {
				# subsetdef: gosubset_prok "Prokaryotic GO subset"
				if ($v =~ /^(\S+)\s+\"(.*)\"/) {
					my ($id,$label) = ($1,$2);
					my $ss = new GOBO::Subset(id=>$id,
											  label=>$label);
					$g->subset_index->{$id} = $ss;
				}
				else {
					warn $v;
				}
			}
			elsif ($t eq 'date') {
				$g->date($v);
			}
			elsif ($t eq 'remark') {
				$g->comment($v);
			}
			elsif ($t eq 'format-version') {
				$self->format_version($v);
			}
			elsif ($t eq 'data-version') {
				$g->version($v);
			}
			else {
				$g->set_property_value($t,$v);
			}
		}
	}
	# set the parse_header to 1
	$self->parsed_header(1);
	return;
}



my $body_subs = {
	"id" => sub {
		my ($self, $args) = @_;
#		print STDERR "node before: " . Dumper(${$args->{node}}) . "\n";
		if ($args->{stanzaclass} eq 'term') {
#			$args->{node} = $args->{graph}->add_term($args->{value});
			${$args->{node}} = ${$args->{graph}}->add_term($args->{value});
		}
		elsif ($args->{stanzaclass} eq 'typedef') {
#			$args->{node} = $args->{graph}->add_relation($args->{value});
			${$args->{node}} = ${$args->{graph}}->add_relation($args->{value});
		}
		elsif ($args->{stanzaclass} eq 'instance') {
#			$args->{node} = $args->{graph}->add_instance($args->{value});
			${$args->{node}} = ${$args->{graph}}->instance_noderef($args->{value});
			${$args->{graph}}->add_instance(${$args->{node}});
		}
		elsif ($args->{stanzaclass} eq 'annotation') {
			# TODO
#			print STDERR "got an annotation!\n";
		}
		else {
			warn "Unknown stanza class " . $args->{stanzaclass};
		}

#		if (! $args->{node} ) {
		if (!${$args->{node}}) {
			die "cannot parse: $_";
		}

#		$args->{node}->namespace($self->default_namespace) if ! $args->{node}->namespace && $self->default_namespace;

		${$args->{node}}->namespace($self->default_namespace) if (!${$args->{node}}->namespace && $self->default_namespace);
	},
	"name" => sub {
		my ($self, $args) = @_;
		$args->{node}->label($args->{value});
	},
	"namespace" => sub {
		my ($self, $args) = @_;
		$args->{node}->namespace($args->{value});
	},
	"alt_id" => sub {
		my ($self, $args) = @_;
		$args->{node}->add_alt_ids($args->{value});
	},

	"def" => sub {
		my ($self, $args) = @_;

			my $d = $args->{value};
			if ($d =~ /^\"(.*)\"\s*(\[.*)/) {
				$args->{node}->definition($1);
				my $refs = $2;
				## parse the xrefs
				my $print;
				## turn print boolean on if we have some refs
				if ($refs =~ /\"[^\"]+\"/)
				{	$print++;
#					print STDERR "def xrefs: $refs\n";
				}
				my $vals = {};
				$self->_parse_xrefs($refs, $vals);
#				print STDERR "post parse xrefs: vals: " . Dumper($vals);
				if ($vals->{xrefs}) {
					$args->{node}->definition_xrefs( [ map { $_ = new GOBO::Node($_) } @{$vals->{xrefs}} ]);
#					print STDERR "xrefs now: " . Dumper($args->{node}->definition_xrefs) . "\n" if $print;
				}
			}
			else {
				warn "check def format: $d";
		#		die "Parse error: $s\nDying!";
			}



#		my $vals = [];
#		_parse_vals($args->{value},$vals);
#		$args->{node}->definition($vals->[0]); # TODO
#		if ($vals->[1] && @{$vals->[1]}) {
#			$args->{node}->definition_xrefs( [ map { $_ = new GOBO::Node({ id => $_ }) } @{$vals->[1]} ]);
#		}
	},
	"is_obsolete" => sub {
		my ($self, $args) = @_;
		if ($args->{value} eq 'true')
		{	$args->{node}->obsolete(1);
		}
	},
	"property_value" => sub {
		my ($self, $args) = @_;
		## format:
		## property_value: relation value
		## value == node ID OR
		## "string" datatype_ID
#		print STDERR "value: " . $args->{value} . "\n";
		my ($prop, $val) = split(' ', $args->{value}, 2);
#		print STDERR "node: " . $args->{node} . "; property: $prop; val: $val\n";
		$args->{node}->add_property_value(prop => $prop, value => $val);
	},
	"comment" => sub {
		my ($self, $args) = @_;
		$args->{node}->comment($args->{value});
	},
	"subset" => sub {
		my ($self, $args) = @_;
		my $ss = $args->{graph}->subset_noderef($args->{value});
		$args->{node}->add_subsets($ss);

		if ($self->liberal_mode && ! $args->{graph}->subset_index->{$ss->id})
		{	print STDERR $args->{value} . " was not in the subset index. Crap!\n";
			$args->{graph}->subset_index->{$args->{value}} = $ss;
		}
	},
	"consider" => sub {
		my ($self, $args) = @_;
		$args->{node}->add_considers($args->{value});
	},
	"replaced_by" => sub {
		my ($self, $args) = @_;
		$args->{node}->add_replaced_bys($args->{value});
	},
	"created_by" => sub {
		my ($self, $args) = @_;
		$args->{node}->created_by($args->{value});
	},
	"creation_date" => sub {
		my ($self, $args) = @_;
		$args->{node}->creation_date($args->{value});
	},
	"synonym" => sub {
		my ($self, $args) = @_;
#		print STDERR "found a synonym! " . $args->{value} . "\n";
		my $vals = {};
		$self->_parse_vals($args->{value},$vals);
#		print STDERR "now: " . Dumper($vals) . "\n";
		my $syn = new GOBO::Synonym(label=>$vals->{quoted}[0]);
		$args->{node}->add_synonym($syn);
		if ($vals->{atoms} && @{$vals->{atoms}})
		{	if ($syn->is_valid_synonym_scope($vals->{atoms}[0]))
			{	$syn->scope(shift @{$vals->{atoms}});
			}
			else {
				warn "no scope specified for " . $args->{node}->id . " synonym $syn";
			}
			while (@{$vals->{atoms}})
			{	$syn->synonym_type(shift @{$vals->{atoms}});
			}
		}

		if ($vals->{xrefs}) {
			$syn->xrefs( [ map { $_ = new GOBO::Node($_) } @{$vals->{xrefs}} ]);
#			print STDERR "xrefs now: " . Dumper($syn->xrefs) . "\n";
		}
#		print STDERR "Synonym now: " . Dumper($syn) . "\n";
	},
	"xref" => sub {
		my ($self, $args) = @_;
#		if ($args->{value} =~ /^(.*?):(.*)\s?"?(.*?)\"?/
		my $vals = {};
		$self->_parse_xrefs($args->{value}, $vals);
		$args->{node}->add_xrefs($vals->{xrefs}[0]);
	},
	"is_a" => sub {
		my ($self, $args) = @_;
		if ($args->{value} =~ /^(\S+)(.*)/) {
			#	my $tn = $self->getnode($1, $args->{stanzaclass} eq 'typedef' ? 'r' : 'c');
			my $tn;
			my $rn = $args->{graph}->relation_noderef('is_a');
#			my $rn = 'is_a';
			if ($args->{stanzaclass} eq 'typedef')
			{	$tn = $args->{graph}->relation_noderef($1);
			}
			else
			{	$tn = $args->{graph}->term_noderef($1);
			}
			my $s = new GOBO::LinkStatement(node=>$args->{node},relation=>$rn,target=>$tn);
			$self->add_metadata($s,$2);
			$args->{graph}->add_statement($s);
			if ($args->{stanzaclass} eq 'typedef') {
				$args->{node}->add_subrelation_of($tn);
			}
		}
	},
	"relationship" => sub {
		my ($self, $args) = @_;
		if ($args->{value} =~ /(\S+)\s+(\S+)(.*)/) {
			my $rn = $args->{graph}->relation_noderef($1);
			#	my $tn = $self->getnode($2, $args->{stanzaclass} eq 'typedef' ? 'r' : 'c');
			my $tn;
			if ($args->{stanzaclass} eq 'typedef')
			{	$tn = $args->{graph}->relation_noderef($2);
			}
			else
			{	$tn = $args->{graph}->term_noderef($2);
			}
			my $s = new GOBO::LinkStatement(node=>$args->{node},relation=>$rn,target=>$tn);
			$self->add_metadata($s,$3);
			$args->{graph}->add_statement($s);
		}
	},
	"complement_of" => sub {
		my ($self, $args) = @_;
		#	my $tn = $self->getnode($args->{value}, $args->{stanzaclass} eq 'typedef' ? 'r' : 'c');
		my $tn;
		if ($args->{stanzaclass} eq 'typedef')
		{	$tn = $args->{graph}->relation_noderef($args->{value});
		}
		else
		{	$tn = $args->{graph}->term_noderef($args->{value});
		}
		$args->{node}->complement_of($tn);
	},
	"negation_of" => sub {
		my ($self, $args) = @_;
		my $tn = $args->{graph}->relation_noderef($args->{value});
		$args->{node}->add_negation_of($tn);
	},
	"disjoint_from" => sub {
		my ($self, $args) = @_;
		#	my $tn = $self->getnode($args->{value}, $args->{stanzaclass} eq 'typedef' ? 'r' : 'c');
		my $tn;
		if ($args->{stanzaclass} eq 'typedef')
		{	$tn = $args->{graph}->relation_noderef($args->{value});
		}
		else
		{	$tn = $args->{graph}->term_noderef($args->{value});
		}
		$args->{node}->add_disjoint_from($tn);
	},
	"domain" => sub {
		my ($self, $args) = @_;
#		my $tn = $self->getnode($args->{value}, 'c');
		my $tn = $args->{graph}->term_noderef($args->{value});
		$args->{node}->domain($tn);
	},
	"range" => sub {
		my ($self, $args) = @_;
#		my $tn = $self->getnode($args->{value}, 'c');
		my $tn = $args->{graph}->term_noderef($args->{value});
		$args->{node}->range($tn);
	},
	"disjoint_over" => sub {
		my ($self, $args) = @_;
#		my $tn = $self->getnode($args->{value}, 'r');
		my $tn = $args->{graph}->relation_noderef($args->{value});
		$args->{node}->add_disjoint_over($tn);
	},
	"inverse_of" => sub {
		my ($self, $args) = @_;
#		my $tn = $self->getnode($args->{value}, 'r');
		my $tn = $args->{graph}->relation_noderef($args->{value});
		$args->{node}->add_inverse_of($tn);
	},
	"inverse_of_on_instance_level" => sub {
		my ($self, $args) = @_;
#		my $tn = $self->getnode($args->{value}, 'r');
		my $tn = $args->{graph}->relation_noderef($args->{value});
		$args->{node}->add_inverse_of_on_instance_level($tn);
	},
	"instance_of" => sub {
		my ($self, $args) = @_;
		if ($args->{value} =~ /^(\S+)/)
		{	#my $tn = $self->getnode($1, 'c');
			my $tn = $args->{graph}->term_noderef($1);
			$args->{node}->add_type($tn);
		}
	},
	"equivalent_to" => sub {
		my ($self, $args) = @_;
		#	my $tn = $self->getnode($args->{value}, $args->{stanzaclass} eq 'typedef' ? 'r' : 'c');
		my $tn;
		if ($args->{stanzaclass} eq 'typedef')
		{	$tn = $args->{graph}->relation_noderef($args->{value});
		}
		else
		{	$tn = $args->{graph}->term_noderef($args->{value});
		}
		$args->{node}->add_equivalent_to($tn);
	},

	"intersection_of" => sub {
		my ($self, $args) = @_;
		# TODO: generalize
		if ($args->{value} =~ /^(\S+)\s+(\S+)/) {
			my $rn = $args->{graph}->relation_noderef($1);
			#	my $tn = $self->getnode($2, $args->{stanzaclass} eq 'typedef' ? 'r' : 'c');
			my $tn;
			if ($args->{stanzaclass} eq 'typedef')
			{	$tn = $args->{graph}->relation_noderef($2);
			}
			else
			{	$tn = $args->{graph}->term_noderef($2);
			}
			my $s = new GOBO::LinkStatement(node=>$args->{node},relation=>$rn,target=>$tn, is_intersection=>1);
			$args->{graph}->add_ontology_link($s);
		}
		elsif ($args->{value} =~ /^(\S+)/) {
			#	my $tn = $self->getnode($1, $args->{stanzaclass} eq 'typedef' ? 'r' : 'c');
			my $tn;
			my $rn = $args->{graph}->relation_noderef('is_a');
#			my $rn = 'is_a';
			if ($args->{stanzaclass} eq 'typedef')
			{	$tn = $args->{graph}->relation_noderef($1);
			}
			else
			{	$tn = $args->{graph}->term_noderef($1);
			}
			my $s = new GOBO::LinkStatement(node=>$args->{node},relation=>$rn,target=>$tn, is_intersection=>1);
			$args->{graph}->add_ontology_link($s);
		}
		else {
			$self->throw("badly formatted intersection: $_");
		}
	},
	"union_of" => sub {
		my ($self, $args) = @_;
		#	my $u = $self->getnode($args->{value}, $args->{stanzaclass} eq 'typedef' ? 'r' : 'c');
		my $u;
		if ($args->{stanzaclass} eq 'typedef')
		{	$u = $args->{graph}->relation_noderef($args->{value});
		}
		else
		{	$u = $args->{graph}->term_noderef($args->{value});
		}
		my $ud = $args->{node}->union_definition;
		if (!$ud) {
			$ud = new GOBO::ClassExpression::Union;
			$args->{node}->union_definition($ud);
		}
		$ud->add_argument($u);
	},
	"transitive_over" => sub {
		my ($self, $args) = @_;

			if ($args->{node}->isa('GOBO::RelationNode'))
			{	my $rn = $args->{graph}->relation_noderef($args->{value});
				$args->{node}->transitive_over($rn);
			}
			else
			{	warn "transitive_over is not a valid attribute for " . ref($args->{node}) . " " . $args->{node};
			}
	},
	"holds_over_chain" => sub {
		my ($self, $args) = @_;
#		my @rels  = map { $self->getnode($_,'r') } split(' ',$args->{value});
		my @rels  = map { $args->{graph}->relation_noderef($_) } split(' ',$args->{value});
		$args->{node}->add_holds_over_chain(\@rels);
	},
	"equivalent_to_chain" => sub {
		my ($self, $args) = @_;
#		my @rels  = map { $self->getnode($_,'r') } split(' ',$args->{value});
		my @rels  = map { $args->{graph}->relation_noderef($_) } split(' ',$args->{value});
		$args->{node}->add_equivalent_to_chain(\@rels);
	},
	"is_" => sub {
		my ($self, $args) = @_;
		my $att = $args->{tag};
		if ($args->{value} eq 'true' && $args->{node}->can($att))
		{	$args->{node}->$att( 1 );
		}
		elsif (! $args->{node}->can($att))
		{	warn "is_$att is not a valid attribute for " . ref($args->{node}) . " " . $args->{node};
		}
		# TODO: check!
	},
	# following for annotation stanzas only
	"subject" => sub {
		my ($self, $args) = @_;
#		$args->{node}->node($self->getnode($args->{value}));
		$args->{node}->node($args->{graph}->noderef($args->{value}));
#		print STDERR "subject: $args->{value}; noderef type: " . ref($args->{graph}->noderef($args->{value})) . "\n";
	},
	"relation" => sub {
		my ($self, $args) = @_;
#		$args->{node}->relation($self->getnode($args->{value},'r'));
		$args->{node}->relation($args->{graph}->relation_noderef($args->{value}));
	},
	"object" => sub {
		my ($self, $args) = @_;
#		$args->{node}->target($self->getnode($args->{value}));
		$args->{node}->target($args->{graph}->noderef($args->{value}));
	},
	"description" => sub {
		my ($self, $args) = @_;
		$args->{node}->description($args->{value});
	},
	"source" => sub {
		my ($self, $args) = @_;
#		$args->{node}->provenance($self->getnode($args->{value}));
		$args->{node}->provenance($args->{graph}->noderef($args->{value}));
	},
	"assigned_by" => sub {
		my ($self, $args) = @_;
#		$args->{node}->source($self->getnode($args->{value}));
		$args->{node}->source($args->{graph}->noderef($args->{value}));
	},
	"formula" => sub {
		my ($self, $args) = @_;
		my $vals = {};
		$self->_parse_vals($args->{value},$vals);
### TO COMPLETE / CHECK ###
		if (! $vals->{quoted})
		{	warn $args->{node}->id . ": no formula text found in " . $args->{value} . "\n";
		}
		else
		{	my $f = new GOBO::Formula(text=>$vals->{quoted}[0]);
			if ($vals->{atom})
			{	$f->language($vals->{atom}[0]);
			}
			if ($vals->{xrefs})
			{	$f->add_xrefs($vals->{xrefs});
			}
			$f->associated_with($args->{node});
			$args->{graph}->add_formula($f);
		}
	},
## obo 1.0 stuff
	"obo_1_0_synonym" => sub {
		my ($self, $args) = @_;
		my $vals = {};
		$self->_parse_vals($args->{value},$vals);
		my $syn = new GOBO::Synonym(label=>$vals->{quoted}[0]);
		$args->{node}->add_synonym($syn);
		$syn->scope($args->{scope});
		while ($vals->{atoms} && @{$vals->{atoms}})
		{	$syn->type(shift @{$vals->{atoms}});
		}
		if ($vals->{xrefs}) {
			$syn->xrefs( [ map { $_ = new GOBO::Node($_) } @{$vals->{xrefs}} ]);
#			print STDERR "xrefs now: " . Dumper($syn->xrefs) . "\n";
		}
	},
#		print STDERR "Synonym now: " . Dumper($syn) . "\n";



};


sub parse_body {
	my $self = shift;

	## ignore the whole lot
	if ($self->options && $self->options->{body} && $self->options->{body}{ignore_all})
	{	return;
	}

	## the graph header has not been parsed but we must carry on regardless!
	if (! $self->parsed_header )
	{
		## set the record separator to the header value
#		$self->set_input_record_separator($self->header_record_separator);
		while ($_ = $self->next_line)
		{	next unless /^\[{0,1}\w+\]/;
#			print STDERR "Content starts here!\n";
			$self->unshift_line($_);
			last;
		}
	}

	## set the record separator to the new value
#	$self->set_input_record_separator($self->body_record_separator);

	if ($self->parse_method eq 'if_else')
	{	#warn "Parsing in if/else mode!";
		return $self->parse_body_ie;
	}
	else
	{	return $self->parse_body_dh;
	}
}


sub parse_body_dh {
	my $self = shift;

	my $stanza_check = $self->get_stanza_check_sub;
	my $tag_check = $self->get_tag_check_sub;

	my @anns = ();
	my $g = $self->graph;
	my $stanzaclass;
	while( ($stanzaclass, $_) = $self->next_stanza ) {

#		print STDERR "stanza class: $stanzaclass; stanza: " . Dumper($_) . "\n\n";
		my @stanza_lines = @$_;
		my $n;
		if ($stanzaclass eq 'annotation') {
			$n = new GOBO::Annotation;
			push(@anns, $n);
		}

		## look at each line in the stanza
		foreach (@stanza_lines) {
			next unless /\S/;
			next if /^!/; ## skip comments

			s/\s\!\s.*//; # TODO
			s/\s+$//;

			if (/^id:\s*(.*)\s*$/) {
	#			print STDERR "id: $1; stanzaclass: $stanzaclass; node: " . Dumper($n) . "\n";
				$body_subs->{id}->($self, { value => $1, graph => \$g, node => \$n, stanzaclass => $stanzaclass });
	#			print STDERR "node: $1\n";
				next;
			}

			if (/^(.*?):\s*/) {
				next unless &$tag_check( $stanzaclass, $1 );
	#			print STDERR "passed the tag check!\n";
			}

			if (/^(.*?):\s*(.*)\s*$/) {
				if ($body_subs->{$1}) {
					$body_subs->{$1}->($self, { tag => $1, value => $2, graph => $g, node => $n, stanzaclass => $stanzaclass });
					next;
				}
				elsif (/^is_(\w+):\s*(\w+)/) {
					$body_subs->{'is_'}->($self, { tag => $1, value => $2, graph => $g, node => $n } );
					next;
				}
				elsif (/^(broad|narrow|exact|related)_synonym:\s*(.*)/)
				{	$body_subs->{obo_1_0_synonym}->($self, { scope => uc($1), value => $2, graph => $g, node => $n } );
					next;
				}
				elsif (! $self->strict_mode)
				{	$body_subs->{'property_value'}->($self, { tag => $1, value => $1." ".$2, graph => $g, node => $n, stanzaclass => $stanzaclass });
					next;
				}
			}

			# we don't know what's going on here!
			warn "ignored: $_" if /\w/;
		}
	}
	if (@anns) {
		$g->add_annotations(\@anns);
	}
	return;
};


sub parse_body_ie {
	my $self = shift;

#	my $stanza_check = sub { return 1; };
#	my $tag_check = sub { return 1; };
	my $stanza_check = $self->get_stanza_check_sub;
	my $tag_check = $self->get_tag_check_sub;

	if ($self->options && $self->options->{body} && $self->options->{body}{ignore_all})
	{	# ignore the whole thing
		# no more body parsing required
	#	warn "Found that I don't have to parse the body. Returning!";
		return;
	}

	my $id;
	my %union_h = ();
	my @anns = ();
	my $g = $self->graph;

	my $stanzaclass;
	while( ($stanzaclass, $_) = $self->next_stanza ) {

#		print STDERR "stanza class: $stanzaclass; stanza: " . Dumper($_) . "\n\n";
		my @stanza_lines = @$_;
		my $n;
		if ($stanzaclass eq 'annotation') {
			$n = new GOBO::Annotation;
			push(@anns, $n);
		}

		## look at each line in the stanza
		foreach (@stanza_lines) {
			next unless /\S/;

#			if (/^\[(\S+)\]/) {
#				undef $n;
#				$stanzaclass = lc($1);
#				next unless &$stanza_check( $stanzaclass );
	#			print STDERR "passed the stanza check!\n";
#				if ($stanzaclass eq 'annotation') {
#					$n = new GOBO::Annotation;
#					push(@anns, $n);
#				}
#				next;
#			}


			if (/^(.*?):\s*/) {
				next unless &$tag_check( $stanzaclass, $1 );
	#			print STDERR "passed the tag check!\n";
			}

			chomp;
			s/\s\!\s.*//; # TODO
			s/\s+$//;
			if (/^id:\s*(.*)\s*$/) {
				$id = $1;
				if ($stanzaclass eq 'term') {
					#$n = $g->term_noderef($id);
					$n = $g->add_term($id);
				}
				elsif ($stanzaclass eq 'typedef') {
					#$n = $g->relation_noderef($id);
					$n = $g->add_relation($id);
				}
				elsif ($stanzaclass eq 'instance') {
					$n = $g->instance_noderef($id);
					$g->add_instance($n);
				}
				elsif ($stanzaclass eq 'annotation') {
					# TODO
				}
				else {
				}

				if (!$n) {
					warn "cannot parse $_";
	#				die "cannot parse: $_";
				}

				$n->namespace($self->default_namespace)
					if (!$n->namespace &&
						$self->default_namespace);
				next;
			}

			if (/^name:\s*(.*)/) {
				$n->label($1);
			}
			elsif (/^namespace:\s*(.*)/) {
				$n->namespace($1);
			}
			elsif (/^alt_id:\s*(.*)/) {
				$n->add_alt_ids($1);
			}
			elsif (/^def:\s*(.*)/) {
				$body_subs->{'def'}->($self, { tag => 'def', value => $1, graph => $g, node => $n, stanzaclass => $stanzaclass });

	#			my $d = $1;
	#			if ($d =~ /^\"(.*)\"\s*(\[.*)/) {
	#				$n->definition($1);
	#				my $refs = $2;
	#				## parse the xrefs
	#				my $vals = {};
	#				_parse_xrefs($refs, $vals);
	#				if ($vals->{xrefs}) {
	#					$n->definition_xrefs( [ map { $_ = new GOBO::Node($_) } @{$vals->{xrefs}} ]);
	#				}
	#			}
	#			else {
	#				warn "check def format: $d";
	#			}
			}
			elsif (/^property_value:\s*(.*)/) {
				my ($prop, $val) = split(' ', $1, 2);
	#		print STDERR "node: " . $args->{node} . "; property: $prop; val: $val\n";
				$n->add_property_value(prop => $prop, value => $val);
	## TO FIX
	## 		$n->add_property_value($vals->[0], $vals->[1]); # TODO
	## 		$n->add_property_value($vals->{quoted}[0], $vals->[1]); # TODO
			}
			elsif (/^comment:\s*(.*)/) {
				$n->comment($1);
			}
			elsif (/^subset:\s*(\S+)/) {
				my $ss = $g->subset_noderef($1);
				$n->add_subsets($ss);

				if ($self->liberal_mode && ! $g->subset_index->{$ss->id})
				{	#print STDERR "$1 was not in the subset index. Crap!\n";
					$g->subset_index->{$1} = $ss;
				}
			}
			elsif (/^consider:\s*(\S+)/) {
				$n->add_considers($1);
			}
			elsif (/^replaced_by:\s*(\S+)/) {
				$n->add_replaced_bys($1);
			}
			elsif (/^created_by:\s*(\S+)/) {
				$n->created_by($1);
			}
			elsif (/^creation_date:\s*(\S+)/) {
				$n->creation_date($1);
			}
			elsif (/^synonym:\s*(.*)/) {
				$body_subs->{'synonym'}->($self, { tag => 'synonym', value => $1, graph => $g, node => $n, stanzaclass => $stanzaclass });
	#			my $vals = {};
	#			_parse_vals($1,$vals);
	#			my $syn = new GOBO::Synonym(label=>shift @{$vals->{quoted}});
	#			$n->add_synonym($syn);
	#			if ($vals->{atoms} && @{$vals->{atoms}})
	#			{	if ($syn->is_valid_synonym_scope($vals->{atoms}[0]))
	#				{	$syn->scope(shift @{$vals->{atoms}});
	#				}
	#				else {
	#					warn "no scope specified for $n synonym $syn";
	#				}
	#				while (@{$vals->{atoms}})
	#				{	$syn->type(shift @{$vals->{atoms}});
	#				}
	#			}
	#			if ($vals->{xrefs}) {
	#				$syn->xrefs( [ map { $_ = new GOBO::Node($_) } @{$vals->{xrefs}} ]);
	#			}
			}
			elsif (/^xref:\s*(\S+)/) {
				$body_subs->{'xref'}->($self, { tag => 'xref', value => $1, graph => $g, node => $n, stanzaclass => $stanzaclass });
			}
			elsif (/^is_a:\s*(\S+)(.*)/) {
				#my $tn = $stanzaclass eq 'typedef' ? $g->relation_noderef($1) : $g->term_noderef($1);
				my $tn = $self->getnode($1, $stanzaclass eq 'typedef' ? 'r' : 'c');
	#			my $rn = 'is_a';
				my $rn = $g->relation_noderef('is_a');
				my $s = new GOBO::LinkStatement(node=>$n,relation=>$rn,target=>$tn);
				$self->add_metadata($s,$2);
				$g->add_statement($s);
				if ($stanzaclass eq 'typedef')
				{	$n->add_subrelation_of($tn);
				}
			}
			elsif (/^relationship:\s*(\S+)\s+(\S+)(.*)/) {
				my $rn = $g->relation_noderef($1);
				#my $tn = $stanzaclass eq 'typedef' ? $g->relation_noderef($2) : $g->term_noderef($2);
				my $tn = $self->getnode($2, $stanzaclass eq 'typedef' ? 'r' : 'c');
				#my $tn = $g->term_noderef($2);
				my $s = new GOBO::LinkStatement(node=>$n,relation=>$rn,target=>$tn);
				$self->add_metadata($s,$3);
				$g->add_statement($s);
			}
			elsif (/^complement_of:\s*(\S+)/) {
				my $tn = $self->getnode($1, $stanzaclass eq 'typedef' ? 'r' : 'c');
				$n->complement_of($tn);
			}
			elsif (/^negation_of:\s*(\S+)/) {
				my $tn = $self->getnode($1, 'r');
				$n->add_negation_of($tn);
			}
			elsif (/^disjoint_from:\s*(\S+)/) {
				my $tn = $self->getnode($1, $stanzaclass eq 'typedef' ? 'r' : 'c');
				$n->add_disjoint_from($tn);
			}
			elsif (/^domain:\s*(\S+)/) {
				my $tn = $self->getnode($1, 'c');
				$n->domain($tn);
			}
			elsif (/^range:\s*(\S+)/) {
				my $tn = $self->getnode($1, 'c');
				$n->range($tn);
			}
			elsif (/^disjoint_over:\s*(\S+)/) {
				my $tn = $self->getnode($1, 'r');
				$n->add_disjoint_over($tn);
			}
			elsif (/^inverse_of:\s*(\S+)/) {
				my $tn = $self->getnode($1, 'r');
				$n->add_inverse_of($tn);
			}
			elsif (/^inverse_of_on_instance_level:\s*(\S+)/) {
				my $tn = $self->getnode($1, 'r');
				$n->add_inverse_of_on_instance_level($tn);
			}
			elsif (/^instance_of:\s*(\S+)/) {
				my $tn = $self->getnode($1, 'c');
				$n->add_type($tn);
			}
			elsif (/^equivalent_to:\s*(\S+)/) {
				my $tn = $self->getnode($1, $stanzaclass eq 'typedef' ? 'r' : 'c');
				$n->add_equivalent_to($tn);
			}
			elsif (/^intersection_of:/) {
				# TODO: generalize
				if (/^intersection_of:\s*(\S+)\s+(\S+)/) {
					my $rn = $g->relation_noderef($1);
					#my $tn = $g->term_noderef($2);
					my $tn = $self->getnode($2, $stanzaclass eq 'typedef' ? 'r' : 'c');
					#my $tn = $stanzaclass eq 'typedef' ? $g->relation_noderef($2) : $g->term_noderef($2);
					my $s = new GOBO::LinkStatement(node=>$n,relation=>$rn,target=>$tn, is_intersection=>1);
					$g->add_statement($s);
				}
				elsif (/^intersection_of:\s*(\S+)/) {
					#my $tn = $g->term_noderef($1);
					#my $tn = $stanzaclass eq 'typedef' ? $g->relation_noderef($1) : $g->term_noderef($1);
					my $tn = $self->getnode($1, $stanzaclass eq 'typedef' ? 'r' : 'c');
					my $rn = $g->relation_noderef('is_a');
	#				my $rn = 'is_a';
					my $s = new GOBO::LinkStatement(node=>$n,relation=>$rn,target=>$tn, is_intersection=>1);
					$g->add_statement($s);
				}
				else {
					$self->throw("badly formatted intersection: $_");
				}
			}
			elsif (/^union_of:\s*(\S+)/) {
				my $u = $self->getnode($1, $stanzaclass eq 'typedef' ? 'r' : 'c');
				my $ud = $n->union_definition;
				if (!$ud) {
					$ud = new GOBO::ClassExpression::Union;
					$n->union_definition($ud);
				}
				$ud->add_argument($u);
			}
			elsif (/^is_(\w+):\s*(\w+)/) {
				my $att = $1;
				#$n->{$att} = $val; # TODO : check
				if ($n->can($att) && $2 eq 'true')
				{	$n->$att(1);
				}
				elsif (! $n->can($att) )
				{	warn "is_$att is not a valid attribute for " . ref($n) . " $n";
				}
			}
			elsif (/^transitive_over:\s*(\w+)/) {
				if ($n->isa('GOBO::RelationNode'))
				{	my $rn = $g->relation_noderef($1);
					$n->transitive_over($rn);
				}
				else
				{	warn "transitive_over is not a valid attribute for " . ref($n) . " $n";
				}
			}
			elsif (/^(holds_over_chain|equivalent_to_chain):\s*(.*)/) {
				my $ct = $1;
				my @rels  = map { $self->getnode($_,'r') } split(' ',$2);
				$ct eq 'holds_over_chain' ? $n->add_holds_over_chain(\@rels) : $n->add_equivalent_to_chain(\@rels);
			}
			# following for annotation stanzas only
			elsif (/^subject:\s*(.*)/) {
				$n->node($self->getnode($1));
			}
			elsif (/^relation:\s*(.*)/) {
				$n->relation($self->getnode($1,'r'));
			}
			elsif (/^object:\s*(.*)/) {
				$n->target($self->getnode($1));
			}
			elsif (/^description:\s*(.*)/) {
				$n->description($1);
			}
			elsif (/^source:\s*(.*)/) {
				$n->provenance($self->getnode($1));
			}
			elsif (/^assigned_by:\s*(.*)/) {
				$n->source($self->getnode($1));
			}
			elsif (/^formula:\s*(.*)/) {
				$body_subs->{'formula'}->($self, { tag => 'formula', value => $1, graph => $g, node => $n, stanzaclass => $stanzaclass });
	#			my $vals = {};
	#			_parse_vals($1,$vals);
	### TO COMPLETE / CHECK ###
	#			if (! $vals->{quoted})
	#			{	warn "$n: no formula text found in $1\n";
	#			}
	#			else
	#			{	my $f = new GOBO::Formula(text=>$vals->{quoted}[0]);
	#				if ($vals->{atom})
	#				{	$f->language($vals->{atom}[0]);
	#				}
	#				if ($vals->{xrefs})
	#				{	$f->add_xrefs($vals->{xrefs});
	#				}
	#				$f->associated_with($n);
	#				$g->add_formula($f);
	#			}
			}
			elsif (! $self->strict_mode)
			{	if (/^(\S+?):\s?(.*)/)
				{	## in non-strict mode, add unknown things as tag-val pairs
					$body_subs->{'property_value'}->($self, { tag => $1, value => $1." ".$2, graph => $g, node => $n, stanzaclass => $stanzaclass });
					next;
				}
			}
			else
			{	warn "ignored: $_" if /\w/;
			}
		}
	}
	if (@anns) {
		$g->add_annotations(\@anns);
	}
	return;
}

sub getnode {
	my $self = shift;
	my $id = shift;
	my $metatype = shift || '';
	my $g = $self->graph;
	my $n;
	if ($metatype eq 'c') {
		$n = $g->term_noderef($id);
	}
	elsif ($metatype eq 'r') {
		$n = $g->relation_noderef($id);
	}
	elsif ($metatype eq 'i') {
		$n = $g->instance_noderef($id);
	}
	else {
		$n = $g->noderef($id);
	}
	return $n;
}

sub add_metadata {
	my $self = shift;
	my $s = shift;
	my $v = shift;
	if ($v =~ /^\s*\{(.*)\}/) {
		my $tq = $1;
		my @tvs = ();
		while ($tq) {
			if ($tq =~ /(\w+)=\"([^\"]*)\"(.*)/) {
				push(@tvs,[$1,$2]);
				$tq = $3;
			}
			elsif ($tq =~ /(\w+)=(\w+)(.*)/) {
				push(@tvs,[$1,$2]);
				$tq = $3;
			}
			else {
				$self->throw($v);
			}
			if ($tq =~ /^s*\,\s*(.*)/) {
				$tq = $1;
			}
			elsif ($tq =~ /^\s*$/) {
				# ok
			}
			else {
				$self->throw($v);
			}
		}
		my @sub_statements = ();
		foreach (@tvs) {
			my ($t,$v) = @$_;
			my $ss = new GOBO::LiteralStatement(relation=>$t,target=>$v);
			push(@sub_statements,$ss);
		}
		$s->sub_statements(\@sub_statements);
	}
	return;
}


sub _parse_vals {
	my $self = shift;
	my $s = shift;
	my $vals = shift;

#	print STDERR "input: s: $s\nvals: $vals\n";
#
	# optionally leads with quoted sections
	if ($s =~ /^(\".*)/) {
		$s = $self->_parse_quoted($s,$vals);
	}

	# follows with optional list of atoms
	while ($s =~ /^([^\{\[]\S*)\s*(.*)/) {
		push @{$vals->{atoms}}, $1;
#		push(@$vals,$1);
		$s = $2;
	}

	# option xrefs
	if ($s =~ /^(\[)/) {
		$s = $self->_parse_xrefs($s,$vals);
	}
#	print STDERR "now: s: $s\nvals: ". Dumper($vals);
#
}


sub _parse_quoted {
	my $self = shift;
	my $s = shift;
	my $vals = shift;
	if ($s =~ /^\"(([^\"\\]|\\.)*)\"\s*(.*)/) {
#		push(@$vals,$1);
		push @{$vals->{quoted}}, $1;
		return $3;
	}
	else {
		warn "Parse error in _parse_quoted: $s";
	}
}


sub _parse_xrefs_OLD {
	my $self = shift;
	my $s = shift;
	my $vals = shift;
#	print STDERR "input: s: $s; vals: " . Dumper($vals) . "\n";
	if ($s =~ /^\[(([^\]\\]|\\.)*)\]\s*(.*)/) {
		$s = $2;
		push @{$vals->{xrefs}}, map {{ id => $_ }} split(/,\*/,$1);
#		push(@$vals, [split(/,\s*/,$1)]); # TODO
	}
	else {
		warn "Parse error in _parse_xrefs: $s";
	}
#	print STDERR "output: s: $s; vals: " . Dumper($vals) . "\n";
}

sub _parse_xrefs {
	my $self = shift;
	my ($s, $vals) = @_;
	if ($s =~ /^\[(([^\]\\]|\\.)*)\]\s*(.*)/) {
		$s = $1;
	}
	$s =~ s/\s*$//;
#	if ($s =~ /"/)
#	{	$self->verbose(1);
#		print STDERR "input: $s\n";
#	}
	##

	if ($s =~ /wikipedia/i)
	{	$self->verbose(1);
	}

	while ($s =~ /\S/)
	{	#print STDERR "s: $s\n";
		if ($s =~ /^\s*(\S.*?)(, | ".*?"|$)(.*|$)?/)
		{	my $ref = $1;
			my $before = $s;
			$s =~ s/^\s*\Q$ref\E,? ?//;
			if ($s eq $before)
			{	warn "search/replace did not work!\n";
				last;
			}
			if ($s =~ /^"(.*?)"(, (.*)|$)/)
			{	push @{$vals->{xrefs}}, { id => $ref, label => $1 };
				$s = $3 || "";
			}
			else
			{	push @{$vals->{xrefs}}, { id => $ref };
			}
#			print STDERR "vals: " . Dumper($vals->{xrefs}[-1]) . "$s\n" if $self->verbose;
		}
		else {
			warn "Parse error in _parse_xrefs while loop: $s";
			last;
	#		die "$s";
		}
	}
#	push(@$vals, [split(/,\s*/,$1)]); # TODO
#	print STDERR "vals: " . Dumper($vals) . "\n";
	$self->verbose(0);
}

=head2 next_stanza

Skip the rest of this stanza and go to the next

input:  self, optional hashref of stanza types to parse

if the hashref is specified, will continue to skip stanzas until the stanza type
matches one of those in the hash ref

=cut

sub next_stanza {
	my $self = shift;
	my $s_check = $self->get_stanza_check_sub;

	while( $_ = $self->next_line )
	{	#print STDERR "stanza: START -->$_<--END\n\n";
		next unless /[a-z]/si;
		if ($self->ignore_obsoletes)
		{	next if $_ =~ /is_obsolete: true/si;
		}
		$_ =~ s/\n\n/\n/gs;
		$_ =~ s/^\s*//s;
		$_ =~ s/^[^a-z]*$//gsmi;
		if ($_ =~ s/^\[{0,1}(\S+)\]\s*\n//)
		{	my $s_type = lc($1);
			if (&$s_check($s_type))
			{	my $rtn = [ grep { /[a-z]+/i } split("\n", $_) ];
				return ( $s_type, $rtn );
			}
			## stanza check failed; go on to the next stanza
			next;
		}
		else
		{
			warn "No stanza type found! stanza:\n" . $_;
		}
	}
	return;
}


## alter the reset_parser function so that the check subs are reset

after 'reset_parser' => sub {
	my $self = shift;
	$self->set_header_check_sub( sub { return 1 } );
	$self->set_stanza_check_sub( sub { return 1 } );
	$self->set_tag_check_sub( sub { return 1 } );
};

__PACKAGE__->meta->make_immutable;

1;

=head1 NAME

GOBO::Parsers::OBOParser

=head1 SYNOPSIS

  my $parser = new GOBO::Parsers::OBOParser(file => "t/data/cell.obo");
  $parser->parse;
  print $parser->graph;

  my $writer = new GOBO::Writers::OBOWriter;
  $writer->graph($parser->graph);
  $writer->write();

=head1 DESCRIPTION

An GOBO::Parsers::Parser that parses OBO Files.

The goal is to be obof1.3 compliant:

http://www.geneontology.org/GO.format.obo-1_3.shtml

however, obof1.2 and obof1.0 are also supported

=head2 Term stanzas

These are converted to GOBO::TermNode objects

=head2 Typedef stanzas

These are converted to GOBO::RelationNode objects

=head2 Instance stanzas

These are converted to GOBO::InstanceNode objects

=head2 Statements

is_a and relationship tags are converted to GOBO::LinkStatement objects and added to the graph

=head2 intersection_of tags

These are added to the graph as GOBO::LinkStatement objects, with is_intersection=>1

You can call

  $g->convert_intersection_links_to_logical_definitions

To move these links from the graph to $term->logical_definition

TBD: do this as the default?
TBD: generalize for all links? sometimes it is convenient to have the links available in the Node object...?

=cut

=head2 Parser options

The default behaviour of the parser is to parse everything it comes across.
Customized parsing can be achieved by giving the parser a hash ref of options
encoding the parsing preferences:

 $parser->set_options($options);

To set parser options, use the following structures:

=head3 Header-related parsing options

Header parsing instructions should be contained in the options hash with the key
'header':

 $options->{header} = ...

 # parse only tag_1, tag_2 and tag_3, and ignore any other tags in the header
 $options->{header} = {
 	parse_only => [ 'tag_1', 'tag_2', 'tag_3' ],
 }


 # parse everything apart from tag_4, tag_5 and tag_6
 $options->{header} = {
 	ignore =>  [ 'tag_4', 'tag_5', 'tag_6' ],
 }

 # ignore all information in the header
 $options->{header}{ignore} = '*';

There is no need to specify $options->{header}{parse_only} = '*' : this is the
default behaviour. There is also no need to specify both 'ignore' and 'parse_only'.


=head3 Body parsing options

Body parsing instructions should be contained in the options hash with the key
'body':

 $options->{body} = ...


=head4 parsing or ignore tags

 # parse only tag_1, tag_2 and tag_3 from $stanza_type stanzas
 $options->{body}{parse_only}{$stanza_type} = [ 'tag_1', 'tag_2', 'tag_3' ],


 # ignore 'tag_4', 'tag_5', 'tag_6' from $stanza_type stanzas
 $options->{body}{ignore}{$stanza_type} = [ 'tag_4', 'tag_5', 'tag_6' ],


=head4 parsing or ignoring stanzas

 # parse only stanzas where the type matches the key $stanza_type
 $options->{body}{parse_only}{ $stanza_type } = '*'


 # ignore stanzas where the type matches the key $stanza_type
 $options->{body}{ignore}{ $stanza_type } = '*'


 # ignore all information in the body
 $options->{body}{ignore} = '*';

There is no need to specify $options->{body}{parse_only} = '*' : this is the
default behaviour. There is also no need to specify both 'ignore' and 'parse_only'.


=head3 Obsoletes

To ignore all obsolete terms in the input file, set the 'ignore_obsoletes' option

 $options->{ignore_obsoletes} = 1;

=head3 Examples

parse everything from the header; parse only instance stanzas and the id, name
and namespace tags from term stanzas

 $parser->set_options({ body => { parse_only => { term => [ qw(id name namespace) ] }, instance => '*' } });


ignore the header; parse everything in the body

 $parser->set_options({ header => { ignore => '*' } });


parse the date from the header; ignore instance and annotation stanzas and obsoletes

 $parser->set_options({
   header => { parse_only => [ 'date' ] },
   body => {
     ignore => { instance => '*', annotation => '*' },
   },
   ignore_obsoletes => 1,
 });

=cut
