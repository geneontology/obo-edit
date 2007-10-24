package org.oboedit.gui.event;

import java.util.EventListener;

public interface ReasonerStatusListener extends EventListener {

	public void statusChanged(ReasonerStatusEvent e);
}
