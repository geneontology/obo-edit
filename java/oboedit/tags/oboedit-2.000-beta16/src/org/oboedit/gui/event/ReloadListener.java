package org.oboedit.gui.event;

import java.util.EventListener;

public interface ReloadListener extends EventListener {
	public void reload(ReloadEvent e);
}
