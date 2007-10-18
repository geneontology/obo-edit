package org.oboedit.gui.event;

import java.util.*;

public interface HistoryListener extends EventListener {

	public void applied(HistoryAppliedEvent event);

	public void reversed(HistoryAppliedEvent event);
}
