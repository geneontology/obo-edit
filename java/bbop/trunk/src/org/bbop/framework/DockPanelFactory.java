package org.bbop.framework;

import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.dock.idw.IDWDriver;

public class DockPanelFactory extends
		AbstractComponentFactory<GUIComponent> {
	
	protected static final DockPanelFactory factory = new DockPanelFactory();
	
	public static DockPanelFactory getInstance() {
		return factory;
	}

	public DockPanelFactory() {
		addID("DOCK_PANEL");
	}

	public GUIComponent doCreateComponent(String id) {
		GUIComponent out = ComponentManager.getManager().getDriver()
				.createMainPanel(id);
		return out;
	}

	public String getDefaultID() {
		return "main";
	}

	public boolean isSingleton() {
		return true;
	}

	public String getName() {
		return "Dock Panel";
	}

	public boolean showInMenus() {
		return false;
	}

	public FactoryCategory getCategory() {
		return null;
	}
}
