package org.bbop.framework.dock;

import java.util.EventListener;

import org.bbop.framework.GUIComponent;

public interface LayoutListener extends EventListener {

	public void focusChanged(GUIComponent old, GUIComponent newComponent);
	
	public void add(GUIComponent parent, GUIComponent child);
	
	public void titleChanged(GUIComponent component, String newTitle);

	public boolean closing(GUIComponent c);

	public boolean undocking(GUIComponent component);
	
	public boolean docking(GUIComponent component);
	
	public boolean maximizing(GUIComponent component);
	
	public boolean minimizing(GUIComponent component);
	
	public boolean restoring(GUIComponent component);
	
}
