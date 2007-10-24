package org.oboedit.gui.event;

import org.bbop.framework.AbstractGUIComponent;
import org.oboedit.gui.*;

import java.util.EventObject;

public class PluginEvent extends EventObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private AbstractGUIComponent plugin;

	public PluginEvent(Object source, AbstractGUIComponent plugin) {
		super(source);
		this.plugin = plugin;
	}

	public AbstractGUIComponent getPlugin() {
		return plugin;
	}
}
