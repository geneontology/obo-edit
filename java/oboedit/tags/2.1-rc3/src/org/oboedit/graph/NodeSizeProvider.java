package org.oboedit.graph;

import java.awt.Dimension;

import org.obo.datamodel.IdentifiedObject;
import org.oboedit.gui.ObjectSelector;

public interface NodeSizeProvider {
	public Dimension getSize(ObjectSelector selector, IdentifiedObject lo);
}
