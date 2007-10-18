package org.oboedit.gui.event;

import java.util.EventListener;

public interface IOListener extends EventListener {
	public boolean willChangeRoot(IOEvent e);
	public boolean willSaveRoot(IOEvent e);
}
