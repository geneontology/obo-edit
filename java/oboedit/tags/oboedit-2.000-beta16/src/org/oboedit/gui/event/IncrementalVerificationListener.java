package org.oboedit.gui.event;

import java.util.EventListener;

public interface IncrementalVerificationListener extends EventListener {
	public void cycleComplete(IncrementalVerificationEvent e);
}
