package org.oboedit.graph;

import org.obo.datamodel.PathCapable;

import edu.umd.cs.piccolo.PNode;

public interface FocusedNodeListener {
	
	public void focusedChanged(PathCapable oldFocus, PathCapable newFocus);

}
