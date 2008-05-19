/**
 * 
 */
package org.bbop.framework.dock.idw;

import java.awt.CardLayout;

import javax.swing.JPanel;

import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIComponent;

import org.apache.log4j.*;

public class ComponentConfigCard extends JPanel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ComponentConfigCard.class);
	public static final String COMPONENT_KEY = "component";
	public static final String CONFIG_KEY = "config";

	protected GUIComponent component;
	protected ConfigurationPanel configScreen;
	protected CardLayout cardLayout;

	public ComponentConfigCard(GUIComponent c) {
		this.component = c;
		this.configScreen = c.getConfigurationPanel();
		cardLayout = new CardLayout();
		setLayout(cardLayout);
		add(component.getComponent(), COMPONENT_KEY);
		if (configScreen != null)
			add(configScreen, CONFIG_KEY);
		show(COMPONENT_KEY);
	}

	public GUIComponent getComponent() {
		return component;
	}

	public ConfigurationPanel getConfigScreen() {
		return configScreen;
	}

	public void show(String key) {
		cardLayout.show(this, key);
	}

	public void toggle() {
		cardLayout.next(this);
	}
}
