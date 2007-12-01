package org.bbop.framework.dock;

import java.util.EventListener;

import org.bbop.framework.GUIComponent;

public interface LayoutListener extends EventListener {

	public void close(GUIComponent c);
	
	public void focusChanged(GUIComponent old, GUIComponent newComponent);
	
	public void add(GUIComponent parent, GUIComponent child);
	
	public void undocked(GUIComponent component);
	
	public void docked(GUIComponent component);
	
	public void maximized(GUIComponent component);
	
	public void minimized(GUIComponent component);
	
	public void restored(GUIComponent component);
	
	public void titleChanged(GUIComponent component, String newTitle);
}
