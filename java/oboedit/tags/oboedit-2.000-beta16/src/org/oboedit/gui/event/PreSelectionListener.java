package org.oboedit.gui.event;

import java.util.EventListener;

public interface PreSelectionListener extends EventListener {

	public boolean isPreSelectOkay(PreSelectionEvent e);
}
