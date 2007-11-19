package org.oboedit.gui.event;

import java.util.EventListener;


public interface ExpandCollapseListener extends EventListener {
	public void expandStateChanged(ExpansionEvent e);
}
