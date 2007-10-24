package org.oboedit.gui.event;

import java.util.EventListener;

public interface PluginListener extends EventListener {

	public void pluginActivated(PluginEvent e);

	public void pluginDeactivated(PluginEvent e);
}
