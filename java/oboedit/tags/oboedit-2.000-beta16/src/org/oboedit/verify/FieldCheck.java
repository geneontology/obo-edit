package org.oboedit.verify;

import java.util.Collection;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.OBOSession;

public interface FieldCheck extends Check {

	public Collection<CheckWarning> check(OBOSession session, FieldPath path,
			byte condition, boolean checkObsoletes);
	
	public Collection<FieldPathSpec> getPaths();
}
