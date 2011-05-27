package org.oboedit.graph;

import org.obo.datamodel.PathCapable;

public interface FocusedNodeListener {
	
	public void focusedChanged(PathCapable oldFocus, PathCapable newFocus);

}
