package org.bbop.framework;

import java.util.List;

import javax.swing.JComponent;

import org.bbop.util.CollectionUtil;

import org.apache.log4j.*;

public class GUIComponentWrapper implements GUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GUIComponentWrapper.class);

	protected String id;
	protected JComponent comp;
	protected String title;

	public GUIComponentWrapper(String id, String title, JComponent comp) {
		this.comp = comp;
		this.id = id;
		this.title = title;
	}

	public void cleanup() {
	}

	public JComponent getComponent() {
		return comp;
	}

	public ComponentConfiguration getConfiguration() {
		return null;
	}

	public ConfigurationPanel getConfigurationPanel() {
		return null;
	}

	public String getID() {
		return id;
	}

	public String getTitle() {
		return title;
	}

	public void init() {
	}

	public boolean isXMLSettable() {
		return false;
	}

	public void setConfiguration(ComponentConfiguration config) {
	}

	public void setTitle(String name) {
		this.title = name;
	}

	public void setXML(String xml) {
	}

	public boolean teardownWhenHidden() {
		return false;
	}

}
