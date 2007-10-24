package org.oboedit.gui;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.LinkedList;

import javax.swing.JComponent;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.PathCapable;
import org.obo.filters.Filter;
import org.obo.filters.RenderSpec;
import org.obo.query.impl.SearchHit;
import org.oboedit.gui.TermFilterEditorFactory.IdentifiedObjectModel;
import org.oboedit.gui.event.GUIUpdateListener;
import org.oboedit.gui.widget.LinkSpecEditor;

public class LinkFilterEditorFactory implements SearchComponentFactory<Link> {
	
	protected static class LinkModel extends AbstractSearchResultsTableModel<Link> {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public LinkModel() {
			super(Link.class);
			setSortColumn(0, true);
		}

		public int getColumnCount() {
			return 5;
		}

		@Override
		public Object getColumnVal(Object row, int column) {
			Link link = (Link) row;
			if (column == 0)
				return link.getChild().getName();
			else if (column == 1)
				return link.getChild().getID();
			else if (column == 2)
				return link.getType().getID();
			else if (column == 3)
				return link.getParent().getName();
			else if (column == 4)
				return link.getParent().getID();
			else
				throw new IllegalArgumentException("column out of range");
		}

		@Override
		public boolean columnHasMaxWidth(int column) {
			if (column == 1 || column == 2 || column == 4)
				return true;
			else
				return false;
		}

		@Override
		public String getColumnName(int index) {
			if (index == 0)
				return "Child name";
			else if (index == 1)
				return "Child id";
			else if (index == 2)
				return "Type id";
			else if (index == 3)
				return "Parent name";
			else if (index == 4)
				return "Parent id";
			else
				return "?!";
		}
	}
	
	public JComponent createSubEditor() {
		return new LinkFilterEditor();
	}

	public Filter<Link> getFilter(Component editor) {
		return ((LinkFilterEditor) editor).getFilter();
	}

	public Collection<Link> getRelevantValues(
			Collection<?> items) {
		Collection<Link> pcs = new LinkedList<Link>();
		for (Object pc : items) {
			if (pc instanceof Link) {
				pcs.add((Link) pc);
			}
		}
		return pcs;
	}
	
	public Class<Link> getResultType() {
		return Link.class;
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

	public JComponent getResultsDisplay(Collection<SearchHit<?>> results) {
		return new SearchResultsTable(new LinkModel(), results);
	}
}
