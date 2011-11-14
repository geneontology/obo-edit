package org.oboedit.gui;

import java.util.Collection;

import org.obo.datamodel.FieldPath;
import org.oboedit.verify.CheckWarning;

public interface ErrorDecorator {

	public void setWarnings(FieldPath path, Collection<CheckWarning> warnings);
	public void clearWarnings();
	
	public void cleanup();
}
