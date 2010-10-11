#!/usr/bin/perl -w
# daily_files.pl

=head1 NAME

daily_from_obo.pl - generate various daily files from the v1.2 OBO file

=head1 SYNOPSIS

## Run from the go/ directory or its parent

 perl daily_from_obo.pl /path/to/obo_file

## Run from somewhere else

 perl daily_from_obo.pl /path/to/obo_file /path/to/doc/and/external2go

=head1 DESCRIPTION

Parses an OBO v1.2 file and generates a number of files from it.

In the go/doc directory:

 - terms_and_ids
 - terms_ids_obs
 - terms_alt_ids
 - obsoletes-exact
 - obsoletes-inexact
 - obsoletes-all

In the go/external2go directory:

 - ec2go
 - metacyc2go
 - reactome2go
 - resid2go
 - um-bbd_enzymeid2go
 - um-bbd_pathwayid2go
 - wikipedia2go


Assumes that the script will be run from a file system of the following structure:

 go/ontology/$ontology_file (may be in a subdirectory)
   /doc/
   /external2go/

The script may either be in the go/ directory or its parent

To specify a path to the doc/ and external2go/ directories if using an ontology
file elsewhere, run the script as

 perl daily_from_obo.pl /path/to/obo_file /path/to/doc/and/external2go

If an output file differs significantly from the old version of the file, the
old file will be saved as I<file_name>.old for manual comparison.

=cut


use strict;
use FileHandle;
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Maxdepth = 2;
#use Time::HiRes qw( gettimeofday tv_interval );

use File::stat;


# use lib "geneontology/go-moose/";
use GOBO::Graph;
use GOBO::Parsers::OBOParserDispatchHash;
# use GOBO::InferenceEngine;
# use GOBO::Writers::OBOWriter;

my $verbose = $ENV{GO_VERBOSE} || 1;
my $timestring = localtime();
my $messages;

my $obo_file = shift @ARGV;
if (!$obo_file || ! -f $obo_file)
{	die "Missing the required file $obo_file\n";
}

my $path_to_go = "";
if (@ARGV)
{	$path_to_go = shift @ARGV;
	$path_to_go .= "/" unless substr($path_to_go, -1, 1) eq '/';
}
else
{	if ($obo_file =~ /\/go\/ontology\//)
	{	($path_to_go = $obo_file) =~ s/(.*?\/go\/)ontology.+/$1/;
	}
	elsif ($obo_file =~ /^ontology\//)
	{	## in the go/ directory
		## no need to do anything with the path to go
	}
	else
	{	## help! No idea what's going on here.
	}
}

## make sure we can find the dirs we need

if (-d $path_to_go . 'external2go' && -d $path_to_go . 'doc')
{ ## we're ok!
	print STDERR "Found $path_to_go" . "external2go and $path_to_go" . "doc!\n";
}
else
{	die "Could not find the doc/ or external2go/ directories!";
}

my $fh = new FileHandle($obo_file);
my $parser = new GOBO::Parsers::OBOParserDispatchHash(fh=>$fh);
$parser->parse;

#print STDERR "parser: " . Dumper( $parser );

my $data;
my $graph = $parser->graph;

open(IN, '<'.$obo_file) or die "The file $obo_file could not be opened: $!";

print "Loading current ontology...\n";
$/ = "\n\n";
while (<IN>)
{	my $block = $_;
	$block =~ s/\n\n/\n/gm;
	foreach my $line (split("\n", $block))
	{	if ($line =~ /(\S.*?):(.+)/)
		{	my ($k, $v) = ($1, $2);
			if ($v =~ /cvs version/)
			{	$k = 'cvs_revision';
				$v =~ s/\s*\$\s*//g;
				$v =~ s/cvs version:\s*Revision:\s*//i;
				$data->{cvs_revision} = $v;
			}
			$data->{header}{$k} = $v;
		}
	}
	last;
}
print "Finished loading ontology.\n";
close(IN);

#print STDERR "header: " . Dumper($data->{header});

# check that we have all the bits we need
die "Crap! Could not parse the file! Dying" if (! defined $graph || scalar @{$graph->terms} == 0 || ! $parser->parsed_header);

# get the CVS version and date
my $date = $data->{header}{date} || 'unknown';
my $cvs_version = $data->{header}{cvs_revision} || 'unknown';


# ec metacyc um-bbd_pathwayid um-bbd_enzymeid resid reactome kegg rhea wikipedia

## file header data
my $headers = {
	default =>
		"! version: \$Revision\$\n! date: \$Date\$\n!\n! Generated from file $obo_file,\n! CVS revision: $cvs_version; date: $date\n!\n",
	terms_and_ids =>
		"! GO IDs (primary only) and name text strings\n! GO:0000000 [tab] text string [tab] F\|P\|C\n! where F = molecular function, P = biological process, C = cellular component\n!\n",

	terms_ids_obs =>
		"! GO IDs and text strings (primary only) and obsolete indicator\n! GO:0000000 [tab] text string [tab] F\|P\|C [tab] (obs)\n! where F = molecular function, P = biological process, C = cellular component\n! obs = term is obsolete\n!\n",

	terms_alt_ids => "! GO IDs (primary and secondary) and text strings\n! GO:0000000 (primary) [tab] GO:0000000 (secondary, separated by space(s) if >1) [tab] text string [tab] F\|P\|C\n! where F = molecular function, P = biological process, C = cellular component\n! obs = term is obsolete\n!\n",

	'obsoletes-exact' =>
"! Obsolete terms and direct annotation substitutes\n!\n!Obsolete	Alternative\n",

	'obsoletes-inexact' =>
"! Obsolete terms and possible annotation substitutes\n!\n!Obsolete	Alternative\n",

	'obsoletes-all' =>
"! Obsolete terms and alternatives, marked as either direct [replaced-by] or possible [consider]\n!\n!Obsolete	Alternative\n",
	reactome => "! Mapping of Reactome entries to Gene Ontology terms\n! Manually created by Reactome staff and integrated into GO\n! http://www.reactome.org/\n! Last update at $timestring by the script ".$0."\n!\n",
	ec => "! Mapping of Gene Ontology terms to Enzyme Commission entries\n! Enzyme Commission: http://www.chem.qmul.ac.uk/iubmb/enzyme/\n! Last update at $timestring by the script ".$0."\n!\n",
	resid => "! Mapping of RESID entries to Gene Ontology terms\n! RESID: http://www.ebi.ac.uk/RESID/\n! Last update at $timestring by the script ".$0."\n!\n",
	metacyc => "! Mapping of Gene Ontology terms to MetaCyc database references.\n! MetaCyc: http://metacyc.org/\n! Last update at $timestring by the script ".$0."\n!\n",
	rhea => "! Mapping of Gene Ontology terms to RHEA database references.\n! RHEA, the Annotated Reactions Database: http://www.ebi.ac.uk/rhea/\n! Last update at $timestring by the script ".$0."\n!\n",
	'um-bbd_enzymeid' => "! Mapping of Gene Ontology terms to UM-BBD enzyme IDs\n! UM-BBD (The University of Minnesota Biocatalysis/Biodegradation Database): http://umbbd.msi.umn.edu/\n! Last update at $timestring by the script ".$0."\n!\n",
	'um-bbd_pathwayid' => "! Mapping of Gene Ontology terms to UM-BBD pathway IDs\n! UM-BBD (The University of Minnesota Biocatalysis/Biodegradation Database): http://umbbd.msi.umn.edu/\n! Last update at $timestring by the script ".$0."\n!\n",
	'um-bbd_reactionid' => "! Mapping of Gene Ontology terms to UM-BBD reaction IDs\n! UM-BBD (The University of Minnesota Biocatalysis/Biodegradation Database): http://umbbd.msi.umn.edu/\n! Last update at $timestring by the script ".$0."\n!\n",
	'wikipedia' => "! Mapping of Gene Ontology terms to Wikipedia entries.\n! Wikipedia: http://en.wikipedia.org\n! Last update at $timestring by the script ".$0."\n!\n",
	kegg => "! Mapping of Gene Ontology terms to KEGG database entities.\n! KEGG, the Kyoto Encyclopedia of Genes and Genomes: http://www.genome.jp/kegg/\n! Last update at $timestring by the script ".$0."\n!\n",
};


my $ontology_mapping_hash = {
	biological_process => 'P',
	cellular_component => 'C',
	molecular_function => 'F',
};


### Terms / IDs / Obsoletes

## create the files and put the file handles in a hash
my $path = $path_to_go . 'doc';
my $prefix = 'GO.';
my $suffix = '';

my @files = qw(terms_and_ids terms_ids_obs terms_alt_ids);
my $files_by_ref;

#push @$messages, "Creating files..." if $verbose;

foreach (@files)
{	my $fh = FileHandle->new("> $path/$prefix$_$suffix.new") or die join("\n", @$messages, "Couldn't open $path/$prefix$_$suffix for writing: $!\n");
	print $fh $headers->{default} . $headers->{$_};
	$files_by_ref->{$_} = $fh;
}

## terms_and_ids: GO:0000000 [tab] text string [tab] F|P|C
## terms_alt_ids: GO:0000000 (primary) [tab] GO:0000000 (secondary, separated by space(s) if >1) [tab] text string [tab] F|P|C [tab] (obs)
## terms_ids_obs: GO:0000000 [tab] text string [tab] F|P|C [tab] (obs)

## Go through the terms and print out the info for the terms / etc. files
## save the obsolete and xref data
foreach my $n (sort { $a->id cmp $b->id } @{$graph->terms} )
{	my $id = $n->id;
	if ( ! $n->label || ! $n->namespace )
	{	push @$messages, $n->id . ": missing ID / name / namespace!";
		next;
	}

	print { $files_by_ref->{terms_and_ids} }
		join("\t",
			$id,
			$n->label,
			$ontology_mapping_hash->{ $n->namespace }
		) . "\n";

	print { $files_by_ref->{terms_ids_obs} }
		map { $_ . "\t" }
		(
			$id,
			$n->label,
			$ontology_mapping_hash->{ $n->namespace }
		);

	print { $files_by_ref->{terms_alt_ids} }
		map { $_ . "\t" }
		(	$id,
			# alt ids, joined by a space
			join(" ", ( sort @{ $n->alt_ids || [] } ) ),
			$n->label,
			$ontology_mapping_hash->{ $n->namespace }

		);

	# store the obsoletes for future reference
	if ($n->obsolete)
	{	print { $files_by_ref->{terms_ids_obs} } "obs";
		print { $files_by_ref->{terms_alt_ids} } "obs";
		$data->{obsoletes}{$id}++;
	}

	# last bit of the line to print...
	print { $files_by_ref->{terms_ids_obs} } "\n";
	print { $files_by_ref->{terms_alt_ids} } "\n";

## some checking stuff here...
	if ($n->alt_ids)
	{	foreach (@{$n->alt_ids})
		{	# ensure we don't have duplicate alt_ids
			if ($data->{alt_id}{$_})
			{	warn "Error: $_ is an alt ID for $id and ". $data->{alt_id}{$_} ."!";
				next;
			}
			$data->{alt_id}{$_} = $id;
		}
	}

	if ($n->xrefs)
	{	if ($n->obsolete)
		{#	print STDERR "Error: xrefs found for obsolete term $id\n";
			next;
		}
		# store the xrefs for usage in a minute
		foreach my $ref (@{$n->xrefs})
		{	my ($db, $key) = split(":", $ref, 2);
			# if there are any gubbins at the end, they're likely to be the xref
			# label. Check and label if a label exists
			my $name;
			if ($key =~ / \"(.+?)\"/)
			{	$name = $1;
			}
			$key =~ s/ \".+//;
			# check that if the ref is from EC, it is a complete xref
			if ($db eq 'EC')
			{	next if $key =~ /-/;
			}

			push @{ $data->{xref}{$db}{$key} }, $id;
			$data->{xref_map}{ lc($db) } = $db if ! $data->{xref_map}{ lc($db) };
			if ($name)
			{	$data->{xref_name}{$db}{$key} = $name;
			}
		}
	}

#	if ($n->subsets)
#	{	foreach ($n->subsets)
#		{	push @{$data->{subset}{$_}}, $id;
#		}
#		$data->{all_slim_terms}{$id}++;
#	}
}

## close and save files
foreach (keys %$files_by_ref)
{	$files_by_ref->{$_}->close;
	check_file_size_and_save($prefix.$_.$suffix, $path);
}

## create the files for the obsolete terms
undef $files_by_ref;
$prefix = '';
foreach my $f ('obsoletes-exact', 'obsoletes-inexact')
{	my $fh = FileHandle->new("> $path/$prefix$f$suffix.new") or die "Couldn't open $path/$prefix$f$suffix for writing: $!\n";
	print $fh $headers->{default} . $headers->{$f};
	$files_by_ref->{$f} = $fh;
}

## go through the obsolete terms and print out the consider and replaced bys
my $file_obs_type = { 'consider' => 'obsoletes-inexact', 'replaced_by' => 'obsoletes-exact' };

foreach my $id (sort keys %{$data->{obsoletes}})
{	foreach (keys %$file_obs_type)
	{	if ($graph->noderef($id)->$_)
		{	foreach my $c (@{ $graph->noderef($id)->$_ })
			{	# check the term exists
				if ($graph->noderef($c))
				{	if ($data->{obsoletes}{$c})
					{	push @$messages, "Warning! $id has obsolete term $c in $_ list!";
					}
				}
				elsif ($data->{alt_id}{$c})
				{	push @$messages, "Warning! $id has $c in $_ list; $c is an alt ID for ".$data->{alt_id}{$c};
					if ($data->{obsoletes}{ $data->{alt_id}{$c} })
					{	$messages->[-1] .= " and ".$data->{alt_id}{$c}." is obsolete!";
					}
				}
				else
				{	push @$messages, "$c is not a term or an alt_id!";
			#		next;
				}
				print { $files_by_ref->{ $file_obs_type->{$_} } } "$id\t$c\n";
			}
		}
	}
}

## close and save files
foreach (keys %$files_by_ref)
{	$files_by_ref->{$_}->close;
	check_file_size_and_save($prefix.$_.$suffix, $path);
}

### External mappings files

undef $files_by_ref;
$path = $path_to_go . 'external2go';
$prefix = '';
$suffix = '2go';
#my @dbxrefs_to_get = qw(ec metacyc um-bbd_pathwayid um-bbd_enzymeid reactome resid);
my @dbxrefs_to_get = qw(ec metacyc um-bbd_pathwayid um-bbd_enzymeid um-bbd_reactionid rhea kegg resid reactome wikipedia);

#	print STDERR "Starting the external2go file mappings.\nCreating files...\n" if $verbose;

#	create the files
foreach (@dbxrefs_to_get)
{	my $fh = FileHandle->new("> $path/$prefix$_$suffix.new") or die join("\n", @$messages, "Couldn't open $path/$prefix$_$suffix for writing: $!");
	print $fh $headers->{default} . $headers->{$_};
	$files_by_ref->{$_} = $fh;
}

## print out mappings for each of the xrefs

foreach (@dbxrefs_to_get)
{	my $db = $data->{xref_map}{$_} || (push @$messages, "Could not find $_ in the xref map!" && next);

#	print STDERR "\$_: $_; db: $db\n";

	if ( ! $data->{xref}{$db} || ! values %{$data->{xref}{$db}} )
	{	push @$messages, "Could not find any xrefs for $db!";
		next;
	}
	foreach my $ref (sort keys %{$data->{xref}{$db}})
	{	foreach my $id (sort @{$data->{xref}{$db}{$ref}})
		{	if ($data->{xref_name}{$db} && $data->{xref_name}{$db}{$ref})
			{	print { $files_by_ref->{ lc($db) } } "$db:$ref " . $data->{xref_name}{$db}{$ref} . " > GO:" . $graph->noderef($id)->label . " ; $id\n";
			}
			else
			{	print { $files_by_ref->{ lc($db) } } "$db:$ref > GO:" . $graph->noderef($id)->label . " ; $id\n";
			}
		}
	}
}

push @$messages, "Closing files and saving.\n" if $verbose;

foreach (keys %$files_by_ref)
{	$files_by_ref->{$_}->close;
	check_file_size_and_save($prefix.$_.$suffix, $path);
}

push @$messages, "$0 Done" if $verbose;

if (@$messages)
{	print STDERR join("\n", @$messages)."\n\n";
}

exit(0);

## this subr checks that the new file isn't massively different
## in size to the existing file
sub check_file_size_and_save {
	my ($file_name, $path) = @_;

	if ($path)
	{	$file_name = $path . '/' . $file_name;
	}

	my $new_file = stat($file_name.'.new') or die join("\n", @$messages, "Couldn't stat $file_name.new : $!");
	if (-e $file_name)
	{	my $old_file = stat($file_name) or die join("\n", @$messages, "Couldn't stat $file_name : $!");

		#	check that the file no less than 10% smaller than the original file size
		my $ten_percent = $old_file->size / 10;

		if ($new_file->size < $ten_percent)
		{	#	if it's 90% out, fail it
			push @$messages, "$file_name: Error: new file size ".$new_file->size.", old file size ".$old_file->size.", ten percent: $ten_percent\nNew file saved as $file_name.new, old file kept as $file_name.";
			return;
		}
		elsif ($new_file->size < ($old_file->size - $ten_percent))
		{	#	file is between 10% and 90% of the size of the original -> warn
			push @$messages, "$file_name: Warning: new file size ".$new_file->size.", old file size ".$old_file->size."\nKeeping old file as $file_name.old just in case.";
			rename($file_name, $file_name.'.old');
			rename($file_name.'.new', $file_name);
			return;
		}
	}
	#	this looks OK. rename the new file to the old file name
	rename($file_name.'.new', $file_name);
	push @$messages, "$file_name looks fine! Removing '.new' suffix.";
	return;
}


