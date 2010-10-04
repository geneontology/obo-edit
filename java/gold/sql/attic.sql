CREATE TABLE gene (
  gene VARCHAR NOT NULL, -- e.g. FB:FBgn00000001
  symbol VARCHAR NOT NULL,
  full_name VARCHAR NOT NULL,
  ncbi_taxon_id INT
);

CREATE TABLE gene_product (
  gene_product VARCHAR NOT NULL, -- e.g. UniProtKB:P12345
  gene VARCHAR NOT NULL, -- e.g. FB:FBgn00000001
  symbol VARCHAR,
  full_name VARCHAR,
  ncbi_taxon_id INT -- redundant
);
