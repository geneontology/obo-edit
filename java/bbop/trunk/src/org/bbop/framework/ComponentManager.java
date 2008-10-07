package org.bbop.framework;

import java.awt.Color;
import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.bbop.framework.dock.LayoutDriver;
import org.bbop.framework.dock.LayoutListener;
import org.bbop.framework.dock.Perspective;
import org.bbop.framework.dock.idw.IDWDriver;
import org.bbop.framework.event.GUIComponentEvent;
import org.bbop.framework.event.GUIComponentListener;



/**
 * <p>@author John Day-Richter</p>
 * 
 * <p>Docs by Jennifer Deegan and Nicolas Rodriguez, October 2008.</p>
 * 
 * <p>Manages aspects of GUI component generation, configuration and cleanup. </p>
 * 
 * <p> For example when a Tree Viewer component is started this class is responsible for reading in the XML configuration file.</p>
 * 
 * <p> See also org.bbop.framework.ComponentConfiguration docs. </p>
 * 
 * 
 * 
 *
 */
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

	public void importPerspective(File file) {
		driver.importPerspective(file);
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
			componentID = factory.getID() + "." + DEFAULT_COMPONENT_ID_SUFFIX;// ; changed to . to make config files visible in windows. Search on getID to find all. 
		if (componentID != null)
			if (getActiveComponent(componentID) != null) {
				componentID = null;
			}
		if (componentID == null) {
			int idgen = 1;
			do {
				componentID = factory.getID() + "." + (idgen++);// ; changed to . to make config files visible in windows.
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
		Collection<GUIComponentFactory<?>> out = new LinkedHashSet(factoryMap.values());
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





	/**
	 * @param comp
	 * @throws FileNotFoundException
	 * 
	 * <p>Reads in the saved configuration settings for a component that is being newly opened. 
	 * The settings class is a <a href="http://en.wikipedia.org/wiki/JavaBeans">JavaBean</a> 
	 * and is serializable. This is necessary so it can be read by the 
	 * <a href="http://java.sun.com/j2se/1.5.0/docs/api/java/beans/XMLDecoder.html">XMLDecoder</a>. </p>
	 * 
	 */
	public void addActiveComponent(GUIComponent comp) throws FileNotFoundException   {
		logger.debug("ComponentManager: addActiveComponent. comp = " + comp);
		activeComponents.put(comp.getID(), comp);
		//logger.debug("ComponentManager: addActiveComponent. comp.getID() = " + comp.getID());
		ComponentConfiguration config = comp.getConfiguration();
		//logger.debug("ComponentManager: addActiveComponent. config = " + config);
		File f = getFile(comp);

		//logger.debug("ComponentManager: addActiveComponent. file f = " + f);
		if (f.exists()) {
			//logger.debug("ComponentManager: addActiveComponent, file name is " + f.toString());
			//logger.debug("ComponentManager: addActiveComponent, file contents:\n");


			//This section below enables the contents of the config file to be written to the console for debugging purposes. 

			//			BufferedReader fileInputReader = new BufferedReader(new FileReader(f));
			//			StringBuffer buffer = new StringBuffer();
			//			String text;
			//			try {
			//				while ((text = fileInputReader.readLine()) !=null)
			//					buffer.append(text + "\n");
			//			} catch (IOException e) {
			//				e.printStackTrace();
			//			}

			//logger.debug(buffer.toString());
			//logger.debug("ComponentManager: addActiveComponent. File f exists.");
			try {
				XMLDecoder decoder = new XMLDecoder(new BufferedInputStream(
						new FileInputStream(f)));
				config = (ComponentConfiguration) decoder.readObject();
				//logger.debug("ComponentManager: addActiveComponent. New XMLDecoder created.");
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		else{
			//logger.debug("ComponentManager: addActiveComponent. file f does not exist.");
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
		int endIndex = componentID.indexOf('.');
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
