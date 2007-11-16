package org.bbop.framework.dock;

import java.awt.Rectangle;
import java.util.List;

import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUIComponentFactory;

public interface LayoutDriver {
	public void init();

	public List<Perspective> getPerspectives();

	public void setPerspective(Perspective p);
	
	public Perspective getCurrentPerspective();

	public Perspective savePerspectiveAs(Perspective p, String name);

	public Perspective getPerspective(String name);
	
	public boolean deletePerspective(Perspective p);

	public String showComponent(GUIComponentFactory factory, GUIComponent target,
			String name, String label, boolean preferFloat, Rectangle floatRect);
	
	public void setComponentLabel(GUIComponent target, String label);
	
	public void cleanup();
	
	public GUIComponent createMainPanel(String id);

	public String getComponentLabel(GUIComponent c);
}
