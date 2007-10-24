package org.oboedit.graph;

import java.util.EventListener;

public interface ExpandCollapseListener extends EventListener {
	public void expandStateChanged(ExpansionEvent e);
}
