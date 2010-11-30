/*
CREATE VIEW named_entity AS
 SELECT *
 FROM
  cls
 UNION
 SELECT *
 FROM
  relation
 UNION
 SELECT *
 FROM
  annotation_property;
*/

CREATE VIEW inferred_relationship
 AS
  SELECT
   cls,
   target_cls,
   is_direct,
   is_reflexive,
   relation,
   NULL AS quantifier,
   ontology
  FROM
   inferred_subclass_of
  UNION
  SELECT
   cls,
   target_cls,
   is_direct,
   is_reflexive,
   relation,
   quantifier,
   ontology
  FROM
   inferred_all_some_relationship
  UNION
  SELECT
   cls,
   target_cls,
   is_direct,
   is_reflexive,
   relation,
   quantifier,
   ontology
  FROM
   inferred_all_only_relationship
   UNION
  SELECT
   cls,
   target_cls,
   is_direct,
   is_reflexive,
   relation,
   quantifier,
   ontology
  FROM
   inferred_all_only_relationship;

COMMENT ON VIEW inferred_relationship IS 'A path between cls and target_cls. The values for this table can be filled in by running a reasoner. The recommended method is getOutgoingEdgesClosureReflexive(cls).';

COMMENT ON COLUMN inferred_relationship.quantifier IS 'One of: SOME,
 ONLY or null. At this time we do not infer other relationships. If
 the inferred relationship is subclass, then the value "is_a" will go
 in the relation column, and the quantifier will be null ';

