package org.oboedit.gui;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;

import javax.swing.JComponent;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.PathCapable;
import org.obo.filters.Filter;
import org.obo.query.impl.BasicSearchHit;
import org.obo.query.impl.SearchHit;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.event.GUIUpdateListener;
import org.oboedit.gui.filter.RenderSpec;
import org.oboedit.gui.widget.ObjectSpecEditor;

import org.apache.log4j.*;

public class TermFilterEditorFactory implements
	SearchComponentFactory<IdentifiedObject> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermFilterEditorFactory.class);

	protected static class IdentifiedObjectModel extends
			AbstractSearchResultsTableModel<IdentifiedObject> {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public IdentifiedObjectModel() {
			super(IdentifiedObject.class);
			setSortColumn(1, true);
		}

		@Override
		public boolean columnHasMaxWidth(int column) {
			if (column == 0)
				return true;
			else
				return false;
		}

		public int getColumnCount() {
			return 2;
		}

		@Override
		public Object getColumnVal(Object row, int column) {
			IdentifiedObject obj = (IdentifiedObject) row;
			if (column == 0)
				return obj.getID();
			else if (column == 1)
				return obj.getName();
			else
				throw new IllegalArgumentException("column out of range");
		}

		@Override
		public String getColumnName(int index) {
			if (index == 0)
				return "ID";
			else if (index == 1)
				return "Name";
			else
				return "?!";
		}
	};

	public JComponent createSubEditor() {
		return new TermFilterEditor();
	}

	public Filter<IdentifiedObject> getFilter(Component editor) {
		return ((TermFilterEditor) editor).getFilter();
	}

	public Collection<IdentifiedObject> getRelevantValues(Collection<?> items) {
		Collection<IdentifiedObject> pcs = new LinkedList<IdentifiedObject>();
		for (Object pc : items) {
			if (pc instanceof IdentifiedObject) {
				pcs.add((IdentifiedObject) pc);
			}
		}
		return pcs;
	}

	public void addUpdateListener(Component c, GUIUpdateListener listener) {
		if (c instanceof TermFilterEditor) {
			((TermFilterEditor) c).addUpdateListener(listener);
		}
	}

	public void removeUpdateListener(Component c, GUIUpdateListener listener) {
		if (c instanceof TermFilterEditor) {
			((TermFilterEditor) c).removeUpdateListener(listener);
		}
	}

	public void setFilter(Component editor, Filter filter) {
		((TermFilterEditor) editor).setFilter(filter);
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
		if (c instanceof TermFilterEditor) {
			((TermFilterEditor) c).addActionListener(listener);
		}
	}

	public void removeActionListener(Component c, ActionListener listener) {
		if (c instanceof TermFilterEditor) {
			((TermFilterEditor) c).removeActionListener(listener);
		}
	}

	public JComponent getResultsDisplay(Collection<SearchHit<?>> results) {
		return new SearchResultsTable(new IdentifiedObjectModel(), results);
	}

	public Class<IdentifiedObject> getResultType() {
		return IdentifiedObject.class;
	}

	public Object serializeResults(Collection<SearchHit<?>> results) {
		Collection<String> out = new ArrayList<String>(results.size());
		for (SearchHit<?> result : results) {
			if (result.getHit() instanceof IdentifiedObject) {
				out.add(((IdentifiedObject) result.getHit()).getID());
			}
		}
		return out;
	}

	public Collection<SearchHit<?>> deserializeResults(Object o) {
		Collection<String> ids = (Collection<String>) o;
		Collection<SearchHit<?>> out = new ArrayList<SearchHit<?>>();
		for (String id : ids) {
			out.add(new BasicSearchHit<IdentifiedObject>(SessionManager
					.getManager().getSession().getObject(id)));
		}
		return out;
	}

}
