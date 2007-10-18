package org.oboedit.gui;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.Collection;

import javax.swing.JComponent;

import org.obo.datamodel.PathCapable;
import org.obo.filters.Filter;
import org.obo.filters.RenderSpec;
import org.oboedit.gui.event.GUIUpdateListener;
import org.oboedit.gui.widget.ObjectSpecEditor;

import com.sun.jndi.toolkit.dir.SearchFilter;

public class SearchPanelFactory implements SearchComponentFactory {

	protected SearchComponentFactory subComponentFactory;
	
	public SearchPanelFactory(SearchComponentFactory subComponentFactory) {
		this.subComponentFactory = subComponentFactory;
	}
		

	public JComponent createSubEditor() {
		return new SearchPanel(subComponentFactory);
	}

	public Filter getFilter(Component editor) {
		return ((SearchPanel) editor).getFilter();
	}
	
	public void setFilter(Component editor, Filter filter) {
		((SearchPanel) editor).setFilter(filter);
	}

	public Collection<PathCapable> getRelevantValues(
			Collection<PathCapable> items) {
		return subComponentFactory.getRelevantValues(items);
	}

	public void removeUpdateListener(Component c, GUIUpdateListener listener) {
		if (c instanceof SearchPanel) {
			((SearchPanel) c).removeUpdateListener(listener);
		}
	}

	public void addUpdateListener(Component c, GUIUpdateListener listener) {
		if (c instanceof SearchPanel) {
			((SearchPanel) c).addUpdateListener(listener);
		}
	}

	public RenderSpec getRenderSpec(Component editor) {
		return ((ObjectSpecEditor) editor).getSpec();
	}

	public JComponent getSpecEditor() {
		return new ObjectSpecEditor();
	}

	public void setRenderSpec(Component editor, RenderSpec spec) {
		((ObjectSpecEditor) editor).setSpec(spec);
	}


	public void addActionListener(Component c, ActionListener listener) {
		if (c instanceof SearchPanel) {
			((SearchPanel) c).addActionListener(listener);
		}
	}


	public void removeActionListener(Component c, ActionListener listener) {
		if (c instanceof SearchPanel) {
			((SearchPanel) c).removeActionListener(listener);
		}
		
	}

}
