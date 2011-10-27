--ALTER TABLE bioentity ADD FOREIGN KEY (type_cls) REFERENCES cls(id);
--ALTER TABLE bioentity ADD FOREIGN KEY (taxon_cls) REFERENCES cls(id);
--ALTER TABLE bioentity ADD FOREIGN KEY (gaf_document) REFERENCES gaf_document(id);

--ALTER TABLE bioentity_relationship ADD FOREIGN KEY (bioentity) REFERENCES bioentity(id);
--ALTER TABLE bioentity_relationship ADD FOREIGN KEY (relation) REFERENCES relation(id);
--ALTER TABLE bioentity_relationship ADD FOREIGN KEY (parent_bioentity) REFERENCES bioentity(id);

--ALTER TABLE gene_annotation ADD FOREIGN KEY (bioentity) REFERENCES bioentity(id);
--ALTER TABLE gene_annotation ADD FOREIGN KEY (cls) REFERENCES cls(id);
--ALTER TABLE gene_annotation ADD FOREIGN KEY (evidence_cls) REFERENCES cls(id);
--ALTER TABLE gene_annotation ADD FOREIGN KEY (acts_on_taxon_id) REFERENCES cls(id);
--ALTER TABLE gene_annotation ADD FOREIGN KEY (composite_qualifier) REFERENCES composite_qualifier(id);
--ALTER TABLE gene_annotation ADD FOREIGN KEY (with_expression) REFERENCES with_info(id);
--ALTER TABLE gene_annotation ADD FOREIGN KEY (extension_expression) REFERENCES extension_expression(id);

--ALTER TABLE extension_expression ADD FOREIGN KEY (cls) REFERENCES cls(id);
--ALTER TABLE extension_expression ADD FOREIGN KEY (relation) REFERENCES relation(id);

