CREATE OR REPLACE VIEW inferred_partof AS
 SELECT cls,target_cls FROM
  inferred_all_some_relationship
 WHERE
  relation='go:part_of';

CREATE OR REPLACE VIEW inferred_regulates AS
 SELECT cls,target_cls FROM
  inferred_all_some_relationship
 WHERE
  relation='go:regulates';

CREATE OR REPLACE VIEW inferred_isa_or_partof AS
 SELECT cls,target_cls FROM
  inferred_subclass_of
 UNION
 SELECT cls,target_cls FROM
  inferred_partof;

CREATE OR REPLACE VIEW basic_annotation_closure AS
 SELECT 
  a.*,
  i.target_cls
 FROM gene_annotation AS a
  INNER JOIN inferred_isa_or_partof AS i ON (a.cls=i.cls);

CREATE OR REPLACE VIEW regulation_annotation_closure AS
 SELECT 
  a.*,
  i.target_cls
 FROM gene_annotation AS a
  INNER JOIN inferred_regulates AS i ON (a.cls=i.cls);

CREATE OR REPLACE VIEW regulation_annotation_closure AS
 SELECT 
  a.*,
  i.target_cls
 FROM gene_annotation AS a
  INNER JOIN inferred_regulates AS i ON (a.cls=i.cls);

CREATE OR REPLACE VIEW annotation_extension_closure AS
 SELECT 
  a.*,
  i.target_cls
 FROM gene_annotation AS a
  INNER JOIN extension_expression AS x ON (a.extension_expression=x.id)
  INNER JOIN inferred_isa_or_partof AS i ON (x.cls=i.cls);

