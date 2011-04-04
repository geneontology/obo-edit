CREATE INDEX gene_annotation_ix_cls ON gene_annotation(cls);
CREATE INDEX gene_annotation_ix_bioentity ON gene_annotation(bioentity);
CREATE INDEX gene_annotation_ix_cls_bioentity ON gene_annotation(cls,bioentity);

CREATE INDEX composite_qualifier_ix_id ON composite_qualifier(id);
CREATE INDEX composite_qualifier_ix_obj ON composite_qualifier(qualifier_obj);
CREATE INDEX composite_qualifier_ix_id_obj ON composite_qualifier(id,qualifier_obj);

CREATE INDEX with_info_ix_id ON with_info(id);
CREATE INDEX with_info_ix_xref ON with_info(with_xref);
CREATE INDEX with_info_ix_id_xref ON with_info(id,with_xref);

CREATE INDEX extension_expression_ix_id ON extension_expression(id);
CREATE INDEX extension_expression_ix_obj ON extension_expression(cls);
CREATE INDEX extension_expression_ix_id_cls ON extension_expression(id,cls);
