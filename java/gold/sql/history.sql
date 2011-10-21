

CREATE TABLE database_changes_history(
object_id varchar,
change_time TIMESTAMP
);

COMMENT ON TABLE database_changes_history IS 'This table maintains  when (date & time)  ontology, gaf and phlotree
	 documents are updated.';
