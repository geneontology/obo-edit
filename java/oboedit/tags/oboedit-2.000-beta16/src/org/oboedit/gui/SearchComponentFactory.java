package org.oboedit.gui;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.Collection;

import javax.swing.JComponent;

import org.obo.datamodel.PathCapable;
import org.obo.filters.Filter;
import org.obo.filters.RenderSpec;
import org.obo.query.impl.SearchHit;
import org.oboedit.gui.event.GUIUpdateListener;

public interface SearchComponentFactory<T> {
	
	public Class<T> getResultType();

	public JComponent createSubEditor();
	
	public JComponent getSpecEditor();
	
	public JComponent getResultsDisplay(Collection<SearchHit<?>> results);
	
	public RenderSpec getRenderSpec(Component editor);
	
	public void setRenderSpec(Component editor, RenderSpec spec);
	
	public Filter<T> getFilter(Component editor);
	
	public void setFilter(Component editor, Filter<T> filter);
	
	public Collection<T> getRelevantValues(Collection<?> items);
	
	public void addUpdateListener(Component c, GUIUpdateListener listener);

	public void removeUpdateListener(Component c, GUIUpdateListener listener);
	
	public void addActionListener(Component c, ActionListener listener);
	
	public void removeActionListener(Component c, ActionListener listener);
}
