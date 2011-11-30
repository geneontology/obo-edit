package org.oboedit.verify;

import org.obo.datamodel.*;

import java.util.*;

public interface OntologyCheck extends Check {

	public Collection<CheckWarning> check(OBOSession history,
			IdentifiedObject currentObject, byte condition,
			boolean checkObsoletes);
}