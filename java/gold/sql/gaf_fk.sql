ALTER TABLE gene_association ADD FOREIGN KEY (composite_qualifier) REFERENCES composite_qualifier.id;
ALTER TABLE gene_association ADD FOREIGN KEY (bioentity) REFERENCES bioentity.id;
ALTER TABLE gene_association ADD FOREIGN KEY (with_expression) REFERENCES with_info.id;
ALTER TABLE gene_association ADD FOREIGN KEY (extension_expression) REFERENCES extension_expression.id;
ALTER TABLE extension_expression ADD FOREIGN KEY (cls) REFERENCES cls.id;
ALTER TABLE extension_expression ADD FOREIGN KEY (relation) REFERENCES relation.id;

