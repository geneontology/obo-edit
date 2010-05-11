SELECT xref_dbname, count(DISTINCT gene_product.id) FROM   gene_product INNER JOIN dbxref ON (gene_product.dbxref_id=dbxref.id) GROUP BY xref_dbname;
