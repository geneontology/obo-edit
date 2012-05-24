package org.oboedit.gui.event;

import java.util.EventListener;

public interface GUIUpdateListener extends EventListener {

	public void guiupdated(GUIUpdateEvent e);
}
