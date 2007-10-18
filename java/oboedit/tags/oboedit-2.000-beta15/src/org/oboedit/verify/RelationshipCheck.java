package org.oboedit.verify;

import org.obo.datamodel.*;

import java.util.List;

public interface RelationshipCheck extends Check {

	public List checkRelationship(Link rel, OBOSession history);
}
