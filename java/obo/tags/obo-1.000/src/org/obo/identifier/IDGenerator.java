package org.obo.identifier;

import org.obo.datamodel.*;

import java.util.Collection;

public interface IDGenerator {

	public String generateID(OBOSession session, LinkedObject parent,
			Collection reservedIDs, boolean temporary);
}
