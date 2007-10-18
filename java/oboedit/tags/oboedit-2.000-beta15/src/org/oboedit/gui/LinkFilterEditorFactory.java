package org.oboedit.gui;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.LinkedList;

import javax.swing.JComponent;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.PathCapable;
import org.obo.filters.Filter;
import org.obo.filters.RenderSpec;
import org.oboedit.gui.event.GUIUpdateListener;
import org.oboedit.gui.widget.LinkSpecEditor;

public class LinkFilterEditorFactory implements SearchComponentFactory {
	public JComponent createSubEditor() {
		return new LinkFilterEditor();
	}

	public Filter<?> getFilter(Component editor) {
		return ((LinkFilterEditor) editor).getFilter();
	}

	public Collection<PathCapable> getRelevantValues(
			Collection<PathCapable> items) {
		Collection<PathCapable> pcs = new LinkedList<PathCapable>();
		for (PathCapable pc : items) {
			if (pc instanceof IdentifiedObject) {
				pcs.add(pc);
			}
		}
		return pcs;
	}

	public void addUpdateListener(Component c, GUIUpdateListener listener) {
		if (c instanceof LinkFilterEditor) {
			((LinkFilterEditor) c).addUpdateListener(listener);
		}
	}

	public void removeUpdateListener(Component c, GUIUpdateListener listener) {
		if (c instanceof LinkFilterEditor) {
			((LinkFilterEditor) c).removeUpdateListener(listener);
		}
	}

	public void setFilter(Component editor, Filter filter) {
		((LinkFilterEditor) editor).setFilter(filter);
	}

	public RenderSpec getRenderSpec(Component editor) {
		return ((LinkSpecEditor) editor).getSpec();
	}

	public JComponent getSpecEditor() {
		return new LinkSpecEditor();
	}

	public void setRenderSpec(Component editor, RenderSpec spec) {
		((LinkSpecEditor) editor).setSpec(spec);
	}

	public void addActionListener(Component c, ActionListener listener) {
		if (c instanceof LinkFilterEditor) {
			((LinkFilterEditor) c).addActionListener(listener);
		}	
	}

	public void removeActionListener(Component c, ActionListener listener) {
		if (c instanceof LinkFilterEditor) {
			((LinkFilterEditor) c).removeActionListener(listener);
		}	
	}
}
