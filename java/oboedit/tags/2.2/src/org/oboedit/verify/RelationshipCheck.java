package org.oboedit.verify;

import org.obo.datamodel.*;

import java.util.List;

public interface RelationshipCheck extends Check {

	public List<CheckWarning> checkRelationship(Link rel, OBOSession history);
}
