package org.bbop.framework;

import javax.swing.JComponent;

public interface GUIComponent {

	public void init();

	public void cleanup();
	
	public String getTitle();
	
	public void setTitle(String name);

	public ComponentConfiguration getConfiguration();

	public void setConfiguration(ComponentConfiguration config);
	
	public ConfigurationPanel getConfigurationPanel();

	public String getID();

	// usually, the getComponent() method will just contain the line
	// return this;
	public JComponent getComponent();

	public boolean isXMLSettable();
	
	public void setXML(String xml);
	
	public boolean teardownWhenHidden();
}
