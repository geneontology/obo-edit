SELECT species.common_name, species.genus, species.species, count(*) AS n_gps
FROM   gene_product INNER JOIN species ON (gene_product.species_id=species.id)
GROUP BY species.common_name, species.genus, species.species
HAVING n_gps > 10;

