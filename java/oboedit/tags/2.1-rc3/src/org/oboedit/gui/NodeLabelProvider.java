package org.oboedit.gui;

import org.obo.datamodel.IdentifiedObject;

public interface NodeLabelProvider {
	public String getLabel(ObjectSelector selector, IdentifiedObject io);
}
