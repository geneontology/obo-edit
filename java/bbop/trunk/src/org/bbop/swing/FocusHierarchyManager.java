package org.bbop.swing;

import java.awt.Component;
import java.awt.KeyboardFocusManager;
import java.awt.event.FocusEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;

import org.apache.log4j.*;

public class FocusHierarchyManager {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FocusHierarchyManager.class);
	
	protected static boolean enabled = false;
	protected static MultiMap<Component, FocusHierarchyListener> listeners;
	protected static PropertyChangeListener listener = new PropertyChangeListener() {
		
		protected PropertyChangeEvent lastEvent = null;

		public void propertyChange(PropertyChangeEvent evt) {
			if (lastEvent == null) {
				lastEvent = evt;
			} else {
				PropertyChangeEvent localLast = lastEvent;
				lastEvent = null;
				dispatchFocusChange((Component) localLast.getOldValue(), (Component) evt.getNewValue());
			}
		}

		
	};
	
	protected static Collection<Component> getAncestors(Component oldValue) {
		Collection<Component> out = new HashSet<Component>();
		while(oldValue != null) {
			if (listeners.containsKey(oldValue))
				out.add(oldValue);
			oldValue = oldValue.getParent();
		}		
		return out;
	}

	protected static void dispatchFocusChange(Component oldValue, Component newValue) {
		Collection<Component> previouslyFocused = getAncestors(oldValue);
		Collection<Component> newlyFocused = getAncestors(newValue);

		for(Component c : previouslyFocused) {
			if (!newlyFocused.contains(c)) {
				fireFocusedLost(c);
			}
		}
		
		for(Component c : newlyFocused) {
			if (!previouslyFocused.contains(c)) {
				fireFocusedGained(c);
			}
		}
	
	}
	

	protected static void fireFocusedLost(Component c) {
		for(FocusHierarchyListener listener : new LinkedList<FocusHierarchyListener>(listeners.get(c))) {
			listener.focusLost(new FocusEvent(c, FocusEvent.FOCUS_LOST));
		}		
	}
	
	protected static void fireFocusedGained(Component c) {
		for(FocusHierarchyListener listener : new LinkedList<FocusHierarchyListener>(listeners.get(c))) {
			listener.focusGained(new FocusEvent(c, FocusEvent.FOCUS_GAINED));
		}
	}

	public static void install() {
		listeners = new MultiHashMap<Component, FocusHierarchyListener>();
		enabled = true;
		KeyboardFocusManager.getCurrentKeyboardFocusManager().addPropertyChangeListener("focusOwner", listener);
	}
	
	protected static void ensureInstalled() {
		if (!enabled)
			install();
	}
	
	public static void addFocusHierarchyListener(Component c, FocusHierarchyListener listener) {
		ensureInstalled();
		listeners.add(c, listener);
	}

	public static void removeFocusHierarchyListener(Component c, FocusHierarchyListener listener) {
		ensureInstalled();
		listeners.remove(c, listener);
	}
	
	public static Collection<FocusHierarchyListener> getFocusHierarchyListeners(Component c) {
		ensureInstalled();
		return listeners.get(c);
	}
	
	public static void uninstall() {
		KeyboardFocusManager.getCurrentKeyboardFocusManager().removePropertyChangeListener("focusOwner", listener);
		enabled = false;
		listeners = null;
	}
	
}
