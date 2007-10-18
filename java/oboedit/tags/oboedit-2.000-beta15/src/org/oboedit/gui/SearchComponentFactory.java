package org.oboedit.gui;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.Collection;

import javax.swing.JComponent;

import org.obo.datamodel.PathCapable;
import org.obo.filters.Filter;
import org.obo.filters.RenderSpec;
import org.oboedit.gui.event.GUIUpdateListener;

public interface SearchComponentFactory {

	public JComponent createSubEditor();
	
	public JComponent getSpecEditor();
	
	public RenderSpec getRenderSpec(Component editor);
	
	public void setRenderSpec(Component editor, RenderSpec spec);
	
	public Filter getFilter(Component editor);
	
	public void setFilter(Component editor, Filter filter);
	
	public Collection<PathCapable> getRelevantValues(Collection<PathCapable> items);
	
	public void addUpdateListener(Component c, GUIUpdateListener listener);

	public void removeUpdateListener(Component c, GUIUpdateListener listener);
	
	public void addActionListener(Component c, ActionListener listener);
	
	public void removeActionListener(Component c, ActionListener listener);
}
