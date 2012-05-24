package org.oboedit.verify;

import org.obo.datamodel.*;

import java.util.List;

public interface TermCheck extends Check {

	public List<CheckWarning> checkTerm(OBOClass term, OBOSession history, boolean expectObs,
			boolean expectType);
}
