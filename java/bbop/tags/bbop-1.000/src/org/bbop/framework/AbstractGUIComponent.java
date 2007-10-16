package org.bbop.framework;

import javax.swing.JComponent;
import javax.swing.JPanel;



public abstract class AbstractGUIComponent extends JPanel implements
		GUIComponent {

	protected String title;
	private String id;

	public AbstractGUIComponent(String id) {
		this.id = id;
	}

	public ConfigurationPanel getConfigurationPanel() {
		return null;
	}

	public boolean teardownWhenHidden() {
		return true;
	}

	public String getID() {
		return id;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getTitle() {
		if (title == null)
			return getID();
		return title;
	}

	public boolean isXMLSettable() {
		return false;
	}

	public void cleanup() {
	}

	public JComponent getComponent() {
		return this;
	}

	public ComponentConfiguration getConfiguration() {
		return null;
	}

	public void setConfiguration(ComponentConfiguration config) {
	}

	public void init() {
	}

	public void setXML(String xml) {
	}
}
