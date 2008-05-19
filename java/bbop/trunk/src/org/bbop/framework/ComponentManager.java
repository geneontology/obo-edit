package org.bbop.framework;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.sql.Driver;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.ButtonGroup;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButtonMenuItem;

import org.bbop.framework.dock.LayoutDriver;
import org.bbop.framework.dock.LayoutListener;
import org.bbop.framework.dock.Perspective;
import org.bbop.framework.dock.idw.IDWDriver;
import org.bbop.framework.event.GUIComponentEvent;
import org.bbop.framework.event.GUIComponentListener;
import org.bbop.swing.AbstractDynamicMenuItem;
import org.bbop.swing.DynamicMenu;
import org.bbop.util.ObjectUtil;

import org.apache.log4j.*;

public class ComponentManager {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ComponentManager.class);

	protected static ComponentManager manager;

	protected static File prefsPath;

	protected Map<String, GUIComponentFactory> factoryMap = new LinkedHashMap<String, GUIComponentFactory>();

	protected Map<String, List<GUIComponent>> currentConfig = new HashMap<String, List<GUIComponent>>();

	protected Map<GUIComponent, GUIComponentFactory> componentToFactoryMap = new HashMap<GUIComponent, GUIComponentFactory>();

	public Map<String, GUIComponent> activeComponents = new HashMap<String, GUIComponent>();

	protected List<GUIComponentListener> componentListeners = new LinkedList<GUIComponentListener>();

	protected final static String DEFAULT_COMPONENT_ID_SUFFIX = "main";

	protected static int idgen = 0;

	protected LayoutDriver driver;


	public void addComponentListener(GUIComponentListener listener) {
		componentListeners.add(listener);
	}

	public void removeComponentListener(GUIComponentListener listener) {
		componentListeners.remove(listener);
	}

	public void setDriver(LayoutDriver driver) {
		if (this.driver != null) {
			this.driver.cleanup();
		}
		this.driver = driver;
		driver.init();
	}

	public LayoutDriver getDriver() {
		if (driver == null) {
			setDriver(new IDWDriver());
		}
		return driver;
	}

	public static ComponentManager getManager() {
		if (manager == null) {
			manager = new ComponentManager();
			GUIManager.addShutdownHook(new Runnable() {
				public void run() {
					manager.cleanup();
				}
			});
		}
		return manager;
	}

	protected void cleanup() {
		if (manager.getDriver() != null) {
			manager.getDriver().cleanup();
		}
		Collection<String> keys = new LinkedList<String>(activeComponents
				.keySet());
		for (String id : keys) {
			GUIComponent c = activeComponents.get(id);
			removeActiveComponent(c);
		}
	}


	public void addLayoutListener(LayoutListener listener) {
		driver.addLayoutListener(listener);
	}

	public void removeLayoutListener(LayoutListener listener) {
		driver.removeLayoutListener(listener);
	}

	public Perspective getPerspective(String name) {
		return driver.getPerspective(name);
	}

	public void setPerspective(String name) {
		Perspective p = getPerspective(name);
		if (p != null)
			setPerspective(p);
	}

	public void setPerspective(Perspective perspective) {
		driver.setPerspective(perspective);
	}

	public List<Perspective> getPerspectives() {
		return driver.getPerspectives();
	}

	public Perspective getCurrentPerspective() {
		return driver.getCurrentPerspective();
	}

	public void deletePerspective(Perspective p) {
		driver.deletePerspective(p);
		// MG - also need to save perspectives state, perspective.xml, without this
		// if you crash deleted perspective will come back on reboot
		driver.savePerspectives();
	}

	public void savePerspectiveAs(Perspective p, String name) {
		driver.savePerspectiveAs(p, name);
		// MG - also need to save perspectives state, perspective.xml, without this
		// if you crash or whatnot new perspectives wont be seen on reboot
		driver.savePerspectives();
	}

	public String showComponent(GUIComponentFactory factory,
			boolean showInNewWindow) {
		return showComponent(factory, null, null, showInNewWindow);
	}

	public String showComponent(GUIComponentFactory factory, GUIComponent target) {
		return showComponent(factory, target, null, false);
	}

	public String showComponent(GUIComponentFactory factory,
			GUIComponent target, boolean showInNewWindow) {
		return showComponent(factory, target, null, showInNewWindow);
	}

	public String showComponent(GUIComponentFactory factory,
	        GUIComponent target, String label, boolean showInNewWindow) {
	    if (factory.isSingleton()) {
	        // shouldn't create a new component if one already exists, just bring it forward
	        for (GUIComponent component : this.getActiveComponents()) {
            if (getFactory(component) == null) {
              String m = "Cant find factory for "+component;
              logger.fatal(m); System.out.println(m);
              return null; // ?
            }
	            if (this.getFactory(component).equals(factory)) {
	                this.focusComponent(component);
	                return component.getID();
	            }
	        }
	    }
	    return getDriver().showComponent(factory, target, null, label,
	            factory.getPreferSeparateWindow() || showInNewWindow, null);
	}
	
	/**
	 * Focus component in interface, bringing it to the front or
	 * unminimizing if necessary.
	 */
	public void focusComponent(GUIComponent component) {
	    this.getDriver().restoreComponent(component); //unminimizes
	    this.getDriver().focusComponent(component); //focuses
	}

	public static File getPrefsPath() {
		if (prefsPath == null)
			prefsPath = new File(GUIManager.getPrefsDir(),
					"components.prefs.xml");
		return prefsPath;
	}

	public static Map<String, List<ComponentConfiguration>> getConfigurationMap(
			File file) {
		try {
			XMLDecoder decoder = new XMLDecoder(new BufferedInputStream(
					new FileInputStream(getPrefsPath())));
			Map<String, List<ComponentConfiguration>> out = (Map) decoder
					.readObject();
			decoder.close();
			return out;
		} catch (Exception ex) {
			return new HashMap<String, List<ComponentConfiguration>>();
		}
	}
	
	public boolean isFloating(GUIComponent c) {
		return driver.isFloating(c);
	}
	
	public void setFloating(GUIComponent c, boolean floating) {
		driver.setFloating(c, floating);
	}


	public void setLabel(GUIComponent c, String label) {
		driver.setComponentLabel(c, label);
	}

	public String getLabel(GUIComponent c) {
		return driver.getComponentLabel(c);
	}
	
	public void setTitlebarTooltip(GUIComponent c, String tooltip) {
		driver.setComponentTitlebarTooltip(c, tooltip);
	}
	
	public String getTitlebarTooltip(GUIComponent c) {
		return driver.getComponentLabel(c);
	}
	
	public void setTitlebarColor(GUIComponent c, Color color) {
		driver.setComponentTitlebarColor(c, color);
	}
	
	public Color getTitlebarColor(GUIComponent c) {
		return driver.getComponentTitlebarColor(c);
	}

	public void resetCurrentConfigurationMap() {
		currentConfig = new HashMap<String, List<GUIComponent>>();
		componentToFactoryMap = new HashMap<GUIComponent, GUIComponentFactory>();
	}

	public Map<String, List<ComponentConfiguration>> getCurrentConfigurationMap() {
		Map<String, List<ComponentConfiguration>> out = new HashMap<String, List<ComponentConfiguration>>();
		for (String factoryID : currentConfig.keySet()) {
			List<GUIComponent> l = currentConfig.get(factoryID);
			if (l != null) {
				List<ComponentConfiguration> configList = new LinkedList<ComponentConfiguration>();
				for (GUIComponent c : l) {
					configList.add(c.getConfiguration());
				}
				out.put(factoryID, configList);
			}
		}
		return out;
	}

	public void install(GUIComponentFactory<?> factory) {
		factoryMap.put(factory.getID(), factory);
	}

	public void uninstall(GUIComponentFactory<?> factory) {
		factoryMap.remove(factory.getID());
	}

	public GUIComponent createComponent(String factoryID, String componentID) {
		GUIComponentFactory<?> factory = factoryMap.get(factoryID);
		if (factory != null) {
			return createComponent(factory, componentID);
		} else
			return null;
	}

	public GUIComponent createComponent(GUIComponentFactory<?> factory,
			String componentID) {
		if (componentID == null)
			componentID = factory.getID() + ":" + DEFAULT_COMPONENT_ID_SUFFIX;
		if (componentID != null)
			if (getActiveComponent(componentID) != null) {
				componentID = null;
			}
		if (componentID == null) {
			int idgen = 1;
			do {
				componentID = factory.getID() + ":" + (idgen++);
			} while (getActiveComponent(componentID) != null);
		}

		GUIComponent c = factory.createComponent(componentID);
		return c;
	}

	public void destroyComponent(GUIComponent c) {
		GUIComponentFactory<?> factory = componentToFactoryMap.get(c);
		if (factory != null) {
			List<GUIComponent> l = currentConfig.get(factory.getID());
			if (l != null) {
				l.remove(c);
			}
			l.add(c);
			componentToFactoryMap.remove(c);
		}
	}

	public Collection<GUIComponentFactory<?>> getFactories() {
		Collection<GUIComponentFactory<?>> out = new LinkedHashSet(factoryMap
				.values());
		return out;
	}

	public Map<String, GUIComponent> getActiveComponentMap() {
		return activeComponents;
	}

	public Collection<GUIComponent> getActiveComponents() {
		return activeComponents.values();
	}

	public void clearActiveComponents() {
		Collection<String> it = new LinkedList<String>(activeComponents
				.keySet());
		for (String id : it) {
			GUIComponent c = activeComponents.get(id);
			c.cleanup();
			activeComponents.remove(id);
		}
	}

	protected static File getFile(GUIComponent comp) {
		File compPrefsDir = new File(GUIManager.getPrefsDir(),
				"component_prefs");
		compPrefsDir.mkdirs();
		File f = new File(compPrefsDir, comp.getID());
		return f;
	}

	public void addActiveComponent(GUIComponent comp) {
		activeComponents.put(comp.getID(), comp);
		ComponentConfiguration config = comp.getConfiguration();
		File f = getFile(comp);
		if (f.exists()) {
			try {
				XMLDecoder decoder = new XMLDecoder(new BufferedInputStream(
						new FileInputStream(f)));
				config = (ComponentConfiguration) decoder.readObject();
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		comp.setConfiguration(config);
		comp.init();
		for (GUIComponentListener listener : componentListeners) {
			listener.componentShown(new GUIComponentEvent(this, comp, true,
					false));
		}
	}

	public void removeActiveComponent(GUIComponent comp) {
		activeComponents.remove(comp.getID());
		for (GUIComponentListener listener : componentListeners) {
			listener.componentHidden(new GUIComponentEvent(this, comp, false,
					true));
		}
		File f = getFile(comp);
		try {
			XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(
					new FileOutputStream(f)));
			encoder.writeObject(comp.getConfiguration());
			encoder.close();
		} catch (IOException ex) {
			logger.info("Couldn't flush component config successfully");
		}
		comp.cleanup();
	}

	public GUIComponent getActiveComponent(String id) {
		return activeComponents.get(id);
	}

	public static File getComponentConfigDir() {
		return new File(GUIManager.getPrefsDir(), "component_prefs");
	}

	public static String getFactoryID(String componentID) {
		int endIndex = componentID.indexOf(':');
		return componentID.substring(0, endIndex);
	}

	public static String getIDSuffix(String componentID) {
		int endIndex = componentID.indexOf(':');
		return componentID.substring(endIndex + 1, componentID.length());
	}

	public GUIComponentFactory getFactory(String id) {
		return factoryMap.get(id);
	}

	public GUIComponentFactory getFactory(GUIComponent c) {
		String factoryID = getFactoryID(c.getID());
		return factoryMap.get(factoryID);
	}
}
