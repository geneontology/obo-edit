package org.oboedit.gui.event;

import java.util.*;

public interface ReconfigListener extends EventListener {
	public void configReloaded(ReconfigEvent e);
}
