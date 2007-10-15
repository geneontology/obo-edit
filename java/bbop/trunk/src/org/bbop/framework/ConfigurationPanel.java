package org.bbop.framework;

import javax.swing.JPanel;

public abstract class ConfigurationPanel extends JPanel {
	
	public abstract void commit();
	public abstract void init();
}
