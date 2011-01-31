select distinct gp.id, gp.symbol from gene_product gp,
association a, evidence e where a.gene_product_id=gp.id and
a.id=e.association_id and (gp.symbol like '%cftr%' or
gp.full_name like '%cftr%');

