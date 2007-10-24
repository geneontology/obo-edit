package org.oboedit.graph;

import java.awt.Dimension;

import org.obo.datamodel.IdentifiedObject;

public interface NodeSizeProvider {
	public Dimension getSize(IdentifiedObject lo);
}
