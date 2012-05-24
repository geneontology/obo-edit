package org.oboedit.verify;

import org.obo.datamodel.*;

import java.util.*;

public interface ObjectCheck extends Check {

	public Collection<CheckWarning> check(OBOSession session,
			IdentifiedObject currentObject, byte condition,
			boolean checkObsoletes);
}
