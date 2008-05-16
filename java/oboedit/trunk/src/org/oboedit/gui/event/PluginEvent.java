package org.oboedit.gui.event;

import org.bbop.framework.AbstractGUIComponent;
import org.oboedit.gui.*;

import java.util.EventObject;

import org.apache.log4j.*;

public class PluginEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PluginEvent.class);

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
