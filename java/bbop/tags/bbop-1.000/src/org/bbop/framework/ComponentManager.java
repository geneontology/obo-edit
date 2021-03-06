package org.bbop.framework;

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
import org.bbop.framework.dock.Perspective;
import org.bbop.framework.dock.idw.IDWDriver;
import org.bbop.swing.AbstractDynamicMenuItem;
import org.bbop.swing.DynamicMenu;
import org.bbop.util.ObjectUtil;

public class ComponentManager {

	protected static ComponentManager manager;

	protected static File prefsPath;

	protected Map<String, GUIComponentFactory> factoryMap = new LinkedHashMap<String, GUIComponentFactory>();

	protected Map<String, List<GUIComponent>> currentConfig = new HashMap<String, List<GUIComponent>>();

	protected Map<GUIComponent, GUIComponentFactory> componentToFactoryMap = new HashMap<GUIComponent, GUIComponentFactory>();

	public Map<String, GUIComponent> activeComponents = new HashMap<String, GUIComponent>();

	protected static int idgen = 0;

	protected LayoutDriver driver;

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
			JMenu viewMenu = createShowMenu();
			GUIManager.getManager().installMenuItem(null, viewMenu);
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
	}

	public void savePerspectiveAs(Perspective p, String name) {
		driver.savePerspectiveAs(p, name);
	}

	protected static JMenu createShowMenu() {
		JMenu out = new JMenu("View", true);
		out.setName("View");
		DynamicMenu perspectivesMenu = new DynamicMenu("Layouts");
		perspectivesMenu.add(new AbstractDynamicMenuItem("Layouts", true, true,
				true) {
			public List<Component> getItems() {
				ButtonGroup group = new ButtonGroup();
				List<Component> out = new LinkedList<Component>();
				for (final Perspective perspective : getManager()
						.getPerspectives()) {
					JRadioButtonMenuItem item = new JRadioButtonMenuItem(
							new AbstractAction(perspective.getName()) {
								public void actionPerformed(ActionEvent e) {
									getManager().setPerspective(perspective);
								}
							});
					group.add(item);
					if (ObjectUtil.equals(getManager().getCurrentPerspective(),
							perspective))
						item.setSelected(true);
					out.add(item);
				}
				return out;
			}
		});
		AbstractDynamicMenuItem ioOps = new AbstractDynamicMenuItem("Show",
				true, true, true) {
			public List<Component> getItems() {
				final Perspective current = getManager().getDriver()
						.getCurrentPerspective();
				List<Component> out = new LinkedList<Component>();
				JMenuItem savePerspectiveAsItem = new JMenuItem(
						new AbstractAction("Save current perspective as...") {
							public void actionPerformed(ActionEvent e) {
								String name = JOptionPane
										.showInputDialog("New perspective name");
								if (name != null) {
									getManager().savePerspectiveAs(current,
											name);
								}
							}
						});
				JMenuItem deleteItem = new JMenuItem(new AbstractAction(
						"Delete current perspective") {
					public void actionPerformed(ActionEvent e) {
						getManager().deletePerspective(current);
					}
				});
				boolean builtin = current == null ? false : current
						.getBuiltIn();
				deleteItem.setEnabled(!builtin);
				out.add(savePerspectiveAsItem);
				out.add(deleteItem);
				return out;
			}
		};

		perspectivesMenu.add(ioOps);
		DynamicMenu showMenu = new DynamicMenu("Show");
		showMenu.add(new AbstractDynamicMenuItem("Show") {
			public List<Component> getItems() {
				List<Component> out = new LinkedList<Component>();
				Map<GUIComponentFactory.FactoryCategory, List<GUIComponentFactory>> factoryMap = new HashMap<GUIComponentFactory.FactoryCategory, List<GUIComponentFactory>>();
				for (final GUIComponentFactory factory : ComponentManager
						.getManager().getFactories()) {
					if (!factory.showInMenus())
						continue;
					List<GUIComponentFactory> factories = factoryMap
							.get(factory.getCategory());
					if (factories == null) {
						factories = new ArrayList<GUIComponentFactory>();
						factoryMap.put(factory.getCategory(), factories);
					}
					factories.add(factory);
				}
				Comparator<GUIComponentFactory> factoryComparator = new Comparator<GUIComponentFactory>() {

					public int compare(GUIComponentFactory o1,
							GUIComponentFactory o2) {
						return o1.toString().compareTo(o2.toString());
					}

				};
				for (GUIComponentFactory.FactoryCategory category : GUIComponentFactory.FactoryCategory
						.values()) {
					List<GUIComponentFactory> factories = factoryMap
							.get(category);
					if (factories != null && factories.size() > 0) {
						Collections.sort(factories, factoryComparator);
						DynamicMenu subMenu = new DynamicMenu(category
								.toString());
						for (final GUIComponentFactory factory : factories) {
							JMenuItem item = new JMenuItem(new AbstractAction(
									factory.getName()) {
								public void actionPerformed(ActionEvent e) {
									getManager().showComponent(factory, null);
								}
							});
							subMenu.add(item);
						}
						out.add(subMenu);
					}
				}
				return out;
			}
		});
		out.add(perspectivesMenu);
		out.add(showMenu);
		return out;
	}

	public String showComponent(GUIComponentFactory factory,
			GUIComponent target) {
		return showComponent(factory, target, null);
	}

	public String showComponent(GUIComponentFactory factory,
			GUIComponent target, String label) {
		return getDriver().showComponent(factory, target, null, label,
				factory.getPreferSeparateWindow(), null);
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
		for (String id : factory.getIDs()) {
			if (factoryMap.containsKey(id)) {
				throw new IllegalArgumentException("The factory "
						+ factoryMap.get(id)
						+ " is already installed under the id " + id);
			}
			factoryMap.put(id, factory);
		}
	}

	public void uninstall(GUIComponentFactory<?> factory) {
		for (String id : factory.getIDs()) {
			factoryMap.remove(id);
		}
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
		if (componentID == null && factory.getDefaultID() != null)
			componentID = factory.getIDs().get(0) + ":"
					+ factory.getDefaultID();
		if (componentID != null)
			if (getActiveComponent(componentID) != null) {
				componentID = null;
			}
		if (componentID == null) {
			int idgen = 1;
			do {
				componentID = factory.getIDs().get(0) + ":" + (idgen++);
			} while (getActiveComponent(componentID) != null);
		}

		GUIComponent c = factory.createComponent(componentID);
		return c;
	}

	public void destroyComponent(GUIComponent c) {
		GUIComponentFactory<?> factory = componentToFactoryMap.get(c);
		if (factory != null) {
			List<GUIComponent> l = currentConfig.get(factory.getIDs()
					.get(0));
			if (l != null) {
				l.remove(c);
			}
			l.add(c);
			componentToFactoryMap.remove(c);
		}
	}

	public Collection<GUIComponentFactory<?>> getFactories() {
		Collection<GUIComponentFactory<?>> out = new LinkedHashSet(
				factoryMap.values());
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
		File f = getFile(comp);
		if (f.exists()) {
			try {
				XMLDecoder decoder = new XMLDecoder(new BufferedInputStream(
						new FileInputStream(f)));
				ComponentConfiguration c = (ComponentConfiguration) decoder
						.readObject();
				comp.setConfiguration(c);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		comp.init();
	}

	public void removeActiveComponent(GUIComponent comp) {
		activeComponents.remove(comp.getID());
		File f = getFile(comp);
		try {
			XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(
					new FileOutputStream(f)));
			encoder.writeObject(comp.getConfiguration());
			encoder.close();
		} catch (IOException ex) {
			System.err.println("Couldn't flush component config successfully");
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
