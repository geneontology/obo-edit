SELECT
 term.name AS superterm_name,
 term.acc AS superterm_acc,
 term.term_type AS superterm_type,
 association.*,
 gene_product.symbol AS gp_symbol,
 gene_product.dbxref_id AS gp_dbxref_id,
 gene_product.species_id AS gp_species_id
FROM term
 INNER JOIN graph_path ON (term.id=graph_path.term1_id)
 INNER JOIN association ON (graph_path.term2_id=association.term_id)
 INNER JOIN gene_product ON (association.gene_product_id=gene_product.id)
WHERE
 term.name = 'nucleus';

