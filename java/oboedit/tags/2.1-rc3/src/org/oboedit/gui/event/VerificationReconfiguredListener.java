package org.oboedit.gui.event;

import java.util.EventListener;

public interface VerificationReconfiguredListener extends EventListener {
	public void reconfigured(VerificationReconfiguredEvent e);
}
