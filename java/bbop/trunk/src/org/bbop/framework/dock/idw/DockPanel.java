package org.bbop.framework.dock.idw;

import java.awt.BorderLayout;

import javax.swing.JComponent;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;

import org.apache.log4j.*;

public class DockPanel extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DockPanel.class);

	protected IDWDriver driver;

	public DockPanel(String id, IDWDriver driver) {
		super(id);
		this.driver = driver;
	}

	public void cleanup() {
		remove(driver.getRootWindow());
	}

	public JComponent getComponent() {
		return this;
	}

	public ComponentConfiguration getConfiguration() {
		return null;
	}

	public void init() {
		setLayout(new BorderLayout(1, 1));
		removeAll();
		add(driver.getRootWindow(), "Center");
	}

	public void setConfiguration(ComponentConfiguration config) {
	}

	public void setXML(String xml) {
	}

	public boolean isXMLSettable() {
		return false;
	}
}
