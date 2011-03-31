CREATE OR REPLACE VIEW inferred_partof AS
 SELECT cls.target_cls FROM
  inferred_all_some_relationship
 WHERE
  relation='TODO:part_of';
-- TODO!!

CREATE OR REPLACE VIEW inferred_regulates AS
 SELECT cls.target_cls FROM
  inferred_all_some_relationship
 WHERE
  relation='TODO:regulates';
-- TODO!!

CREATE OR REPLACE VIEW inferred_isa_or_partof AS
 SELECT cls.target_cls FROM
  inferred_subclass_of
 UNION
 SELECT cls.target_cls FROM
  inferred_part_of;

CREATE OR REPLACE VIEW basic_annotation_closure AS
 SELECT 
  *,
  i.target_cls
 FROM gene_annotation AS a
  INNER JOIN inferred_isa_or_partof AS i ON (a.cls=i.cls);

CREATE OR REPLACE VIEW regulation_annotation_closure AS
 SELECT 
  *,
  i.target_cls
 FROM gene_annotation AS a
  INNER JOIN inferred_regulates AS i ON (a.cls=i.cls);

CREATE OR REPLACE VIEW regulation_annotation_closure AS
 SELECT 
  *,
  i.target_cls
 FROM gene_annotation AS a
  INNER JOIN inferred_regulates AS i ON (a.cls=i.cls);

CREATE OR REPLACE VIEW annotation_extension_closure AS
 SELECT 
  *,
  i.target_cls
 FROM gene_annotation AS a
  INNER JOIN annotation_extension AS x ON (a.extension_expression=x.id);
  INNER JOIN inferred_isa_or_partof AS i ON (x.cls=i.cls);

