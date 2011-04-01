CREATE UNIQUE INDEX cls_ix_id_label ON cls(id,label);

CREATE INDEX inferred_all_some_relationship_cls ON inferred_all_some_relationship(cls);
CREATE INDEX inferred_all_some_relationship_tg ON inferred_all_some_relationship(target_cls);
CREATE INDEX inferred_all_some_relationship_cls_tg ON inferred_all_some_relationship(cls,target_cls);
CREATE INDEX inferred_all_some_relationship_cls_r_tg ON inferred_all_some_relationship(cls,relation,target_cls);

CREATE INDEX inferred_subclass_of_cls ON inferred_subclass_of(cls);
CREATE INDEX inferred_subclass_of_tg ON inferred_subclass_of(target_cls);
CREATE INDEX inferred_subclass_of_cls_tg ON inferred_subclass_of(cls,target_cls);

