package org.oboedit.graph;

import edu.umd.cs.piccolo.PNode;

public interface RelayoutListener {

	public void relayoutComplete();
	public void relayoutStarting();
}
