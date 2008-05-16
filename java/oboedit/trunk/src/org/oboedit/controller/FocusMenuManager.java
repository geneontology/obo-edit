package org.oboedit.controller;

import java.awt.Component;
import java.awt.KeyboardFocusManager;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.JMenu;
import javax.swing.SwingUtilities;

import org.bbop.framework.GUIManager;
import org.bbop.framework.GUIComponent;

import org.apache.log4j.*;

public class FocusMenuManager {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FocusMenuManager.class);

	protected static FocusMenuManager manager;

	protected PropertyChangeListener listener;

	protected Map<GUIComponent, List<JMenu>> menuMap;

	protected Component lastFocused;

	public static FocusMenuManager getManager() {
		return manager;
	}

	protected FocusMenuManager() {
		listener = new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				changeFocus((Component) evt.getNewValue());
			}

		};
		menuMap = new LinkedHashMap<GUIComponent, List<JMenu>>();
		KeyboardFocusManager.getCurrentKeyboardFocusManager()
				.addPropertyChangeListener("permanentFocusOwner", listener);
	}
	
	protected Component fetchBestComponent(GUIComponent oec) {
		Component c = (Component) oec;
		return c;
	}

	public void installMenu(GUIComponent c, JMenu menu) {
		
		List<JMenu> menus = menuMap.get(c);
		if (menus == null) {
			menus = new LinkedList<JMenu>();
			menuMap.put(c, menus);
		}
		menus.add(menu);
	}

	public void uninstallMenu(GUIComponent c, JMenu menu) {
		List<JMenu> menus = menuMap.get(c);
		if (menus != null) {
			menus.remove(menu);
			if (menus.size() == 0)
				menuMap.remove(c);
		}
	}

	protected GUIComponent findMapComponent(Component c) {
		if (c != null) {
			for (GUIComponent parent : menuMap.keySet()) {
				Component best = fetchBestComponent(parent);
				if (SwingUtilities.isDescendingFrom(c, best)) {
					return parent;
				}
			}
		}
		return null;
	}

	protected void changeFocus(Component newComponent) {
		GUIComponent parent = findMapComponent(newComponent);
		if (parent != null) {
			for (JMenu menu : menuMap.get(parent)) {
				GUIManager.getManager().installMenuItem(null, menu);
			}
		}
		GUIComponent oldParent = findMapComponent(lastFocused);
		if (oldParent != null) {
			for (JMenu menu : menuMap.get(oldParent)) {
				GUIManager.getManager().uninstallMenuItem(null, menu);
			}
		}
		lastFocused = newComponent;
	}

	public static void install() {
		manager = new FocusMenuManager();
	}

	protected void cleanup() {
		KeyboardFocusManager.getCurrentKeyboardFocusManager()
				.removePropertyChangeListener("focusOwner", listener);
	}

	public static void uninstall() {
		manager.cleanup();
		manager = null;
	}
}
