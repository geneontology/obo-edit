package org.bbop.swing;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EventListener;
import java.util.List;
import java.util.Map;

import org.bbop.util.ObjectUtil;

import org.apache.log4j.*;

public class IconFactoryRegistry {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IconFactoryRegistry.class);

	public static interface IconRegistryListener extends EventListener {
		public void factoryInstalled(String suffix, IconFactory factory);

		public void factoryRemoved(String suffix, IconFactory factory);
	}

	protected static Map<String, IconFactory> iconFactoryMap = new java.util.HashMap<String, IconFactory>();

	protected static List<IconRegistryListener> listeners = new ArrayList<IconRegistryListener>();

	private IconFactoryRegistry() {
	}

	public static void addListener(IconRegistryListener listener) {
		listeners.add(listener);
	}

	public static void removeListener(IconRegistryListener listener) {
		listeners.remove(listener);
	}
	
	public static Collection<String> getSuffixes() {
		return iconFactoryMap.keySet();
	}
	
	public static IconFactory getFactory(String suffix) {
		return iconFactoryMap.get(suffix);
	}

	public static void installFactory(String suffix, IconFactory factory) {
		iconFactoryMap.put(suffix, factory);
		for (IconRegistryListener listener : listeners) {
			listener.factoryInstalled(suffix, factory);
		}
	}

	public static void uninstallFactory(String suffix, IconFactory factory) {
		if (ObjectUtil.equals(iconFactoryMap.get(suffix), factory)) {
			iconFactoryMap.remove(suffix);
			for (IconRegistryListener listener : listeners) {
				listener.factoryRemoved(suffix, factory);
			}
		}
	}
}
