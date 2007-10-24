package org.oboedit.gui.event;

import java.util.EventObject;

import org.bbop.util.MultiMap;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.oboedit.verify.CheckWarning;

public class IncrementalVerificationEvent extends EventObject {

	protected MultiMap<FieldPath, CheckWarning> warnings;

	public IncrementalVerificationEvent(Object source,
			MultiMap<FieldPath, CheckWarning> warnings) {
		super(source);
		this.warnings = warnings;
	}

	public MultiMap<FieldPath, CheckWarning> getWarnings() {
		return warnings;
	}
}
