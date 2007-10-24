package org.oboedit.graph;

import org.obo.datamodel.OBOProperty;

import edu.umd.cs.piccolo.PNode;

public interface TypeIconManager {
	public PNode getIcon(OBOProperty type);
}
