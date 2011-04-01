CREATE INDEX gene_annotation_ix_cls ON gene_annotation(cls);
CREATE INDEX gene_annotation_ix_bioentity ON gene_annotation(bioentity);
CREATE INDEX gene_annotation_ix_cls_bioentity ON gene_annotation(cls,bioentity);
