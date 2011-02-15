SELECT xref_dbname, avg(distance) AS avg_dist_from_root
FROM   gene_product
 INNER JOIN dbxref ON (gene_product.dbxref_id=dbxref.id)
 INNER JOIN association ON (gene_product.id=association.gene_product_id)
 INNER JOIN graph_path ON (graph_path.term2_id=association.term_id)
 INNER JOIN term AS root ON (graph_path.term1_id=root.id)
WHERE
 is_root=1 AND distance>1
GROUP BY xref_dbname;

