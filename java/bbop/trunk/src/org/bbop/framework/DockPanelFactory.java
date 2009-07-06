package org.bbop.framework;

import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.apache.log4j.*;

public class DockPanelFactory extends
	AbstractComponentFactory<GUIComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DockPanelFactory.class);
	
	protected static final DockPanelFactory factory = new DockPanelFactory();
	
	public static DockPanelFactory getInstance() {
		return factory;
	}

	public DockPanelFactory() {
	}
	
	public String getID() {
		return "DOCK_PANEL";
	}

	@Override
	public GUIComponent doCreateComponent(String id) {
		GUIComponent out = ComponentManager.getManager().getDriver()
				.createMainPanel(id);
		return out;
	}

	public String getDefaultID() {
		return "main";
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	public String getName() {
		return "Dock Panel";
	}

	@Override
	public boolean showInMenus() {
		return false;
	}

	public FactoryCategory getCategory() {
		return null;
	}
}
