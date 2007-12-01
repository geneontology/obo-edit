package org.bbop.framework;

import java.util.EventListener;

public interface VetoableShutdownListener extends EventListener {

	public boolean willShutdown();
}
