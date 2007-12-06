package org.bbop.framework.dock;

import org.bbop.framework.GUIComponent;

/**
 * A vetoable listener for screen layout modification events. If a method returns true,
 * the event proceeds normally. If it returns false, the event is cancelled.
 * 
 * Some events cannot be vetoed.
 * @author jrichter
 *
 */

public abstract class LayoutAdapter implements LayoutListener {

	public void add(GUIComponent parent, GUIComponent child) {
	}

	public boolean closing(GUIComponent c) {
		return true;
	}

	public boolean docking(GUIComponent component) {
		return true;
	}

	public void focusChanged(GUIComponent old, GUIComponent newComponent) {
	}

	public boolean maximizing(GUIComponent component) {
		return true;
	}

	public boolean minimizing(GUIComponent component) {
		return true;
	}

	public boolean restoring(GUIComponent component) {
		return true;
	}

	public void titleChanged(GUIComponent component, String newTitle) {
	}

	public boolean undocking(GUIComponent component) {
		return true;
	}

}
