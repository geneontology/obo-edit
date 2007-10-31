package org.oboedit.gui;

import org.obo.datamodel.IdentifiedObject;

public interface NodeLabelProvider {
	public String getLabel(IdentifiedObject io);
}
