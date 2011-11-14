package org.oboedit.gui.event;

import java.util.EventListener;

public interface SelectionListener extends EventListener {
	public void selectionChanged(SelectionEvent e);
}
