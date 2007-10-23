package org.bbop.framework;

import javax.swing.JPanel;

public abstract class ConfigurationPanel extends JPanel {
	
	protected GUIComponent comp;
	
	public abstract void commit();
	public abstract void init();
	public void setComponent(GUIComponent comp) {
		this.comp = comp;
	}
	
	public GUIComponent getComponent() {
		return comp;
	}
}
