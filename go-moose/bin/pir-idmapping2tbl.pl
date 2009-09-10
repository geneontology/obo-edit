#!/usr/bin/perl

# from
# ftp://ftp.pir.georgetown.edu/databases/idmapping/idmapping.tb.gz

my $uniprot;
while (<>) {
    chomp;
    my @vals = split(/\t/,$_);
    $uniprot = $vals[0];
    wpair('NCBIGene', split(/;\s+/,$vals[2]));
    wpair('RefSeq', split(/;\s+/,$vals[3]));
    wpair('NCBI_GI', split(/;\s+/,$vals[4]));
    wpair('ENSEMBL', split(/;\s+/,$vals[18]));
    wpair('GenBank', split(/;\s+/,$vals[20]));  # actually GenBank/EMBL/DDBJ
    wpair('protein_id', split(/;\s+/,$vals[21]));
}
exit 0;

sub wpair {
    my $db = shift;
    print "$uniprot\t$db\t$_\n" foreach @_;
}


=head1

This table includes the following IDs (or ACs) delimited by tab:

1. UniProtKB accession
2. UniProtKB ID
3. EntrezGene
4. RefSeq
5. NCBI GI number
6. PDB
7. Pfam
8. GO
9. PIRSF
10. IPI
11. UniRef100
12. UniRef90
13. UniRef50
14. UniParc
15. PIR-PSD accession
16. NCBI taxonomy
17. MIM
18. UniGene
19. Ensembl
20. PubMed ID
21. EMBL/GenBank/DDBJ
22. EMBL protein_id

=cut
