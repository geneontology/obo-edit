package org.oboedit.gui.components;

import java.awt.*;
import java.util.*;
import javax.swing.*;
import javax.swing.table.*;

import org.obo.datamodel.*;
import org.obo.history.*;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.Preferences;

import org.apache.log4j.*;

public class SubsetEditorComponent extends AbstractTextEditComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SubsetEditorComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Comparator subsetComparator = new Comparator() {
		public int compare(Object a, Object b) {
			TermSubset tca = (TermSubset) a;
			TermSubset tcb = (TermSubset) b;
			return tca.toString().compareTo(tcb.toString());
		}
	};

	protected class SubsetTableModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected Vector subsetList = new Vector();
		protected Vector valList = new Vector();

		public SubsetTableModel() {
		}

		public void reload() {
			subsetList.clear();
			valList.clear();
			subsetList.addAll(SessionManager.getManager().getSession()
					.getSubsets());
			Collections.sort(subsetList, subsetComparator);
			Iterator it = subsetList.iterator();
			while (it.hasNext()) {
				TermSubset sub = (TermSubset) it.next();
				if (currentObject != null
						&& currentObject instanceof SubsetObject)
					valList.add(new Boolean(((SubsetObject) currentObject)
							.getSubsets().contains(sub)));
			}

			fireTableStructureChanged();
		}

		@Override
		public Class getColumnClass(int col) {
			if (col == 0)
				return TermSubset.class;
			else
				return Boolean.class;
		}

		public int getColumnCount() {
			return 2;
		}

		@Override
		public String getColumnName(int col) {
			if (col == 0)
				return "Subset";
			else
				return "Active?";
		}

		public int getRowCount() {
			return valList.size();
		}

		public Object getValueAt(int row, int column) {
			if (column == 0)
				return subsetList.get(row);
			else {
				return valList.get(row);
			}
		}

		@Override
		public boolean isCellEditable(int row, int col) {
			if (col == 0)
				return false;
			else
				return true;
		}

		@Override
		public void setValueAt(Object val, int row, int col) {
			if (col == 0)
				return;
			else {
				valList.set(row, val);
			}
		}
	}

	protected JTable subsetTable = new JTable();
	protected JPanel tablePanel = new JPanel();
	protected SubsetTableModel subsetTableModel = new SubsetTableModel();

	protected JScrollPane subsetScroller;

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		if (id.equals("table"))
			return subsetScroller;
		else
			return new JButton(id);
	}

	public SubsetEditorComponent() {
		subsetTable.setModel(subsetTableModel);
		subsetTable.setDefaultRenderer(TermSubset.class,
				new DefaultTableCellRenderer());
		subsetTable.setTableHeader(null);
		subsetTable.setColumnSelectionAllowed(false);
		subsetTable.setRowSelectionAllowed(false);
		subsetTable.setOpaque(false);
		tablePanel.setLayout(new GridLayout(1, 1));
		tablePanel.add(subsetTable);
		subsetScroller = new JScrollPane(tablePanel,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	}

	@Override
	protected boolean useSubLayout() {
		return true;
	}

	@Override
	protected String getDefaultLayout() {
		return "<component id='table' titleborder='Subsets'/>";
	}

	@Override
	protected void loadGUI() {
		subsetTableModel.reload();
	}

	protected String getWarningLabel() {
		return "";
	}

	@Override
	protected void initializeGUI() {
	}

	public java.util.List getWarnings() {
		return Collections.EMPTY_LIST;
	}

	public String getID() {
		return "SUBSET_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
		IdentifiedObject term = io;
		if (term instanceof SubsetObject) {
			for (int i = 0; i < subsetTableModel.getRowCount(); i++) {
				TermSubset cat = (TermSubset) subsetTableModel
						.getValueAt(i, 0);
				boolean selected = ((Boolean) subsetTableModel.getValueAt(i,
						1)).booleanValue();
				if (selected)
					((SubsetObject) term).addCategory(cat);
				else
					((SubsetObject) term).removeCategory(cat);
			}
		}
	}

	public java.util.List getChanges() {
		if (currentObject != null) {
			java.util.List out = new LinkedList();
			if (currentObject instanceof SubsetObject) {
				for (int i = 0; i < subsetTableModel.getRowCount(); i++) {
					TermSubset sub = (TermSubset) subsetTableModel
							.getValueAt(i, 0);
					boolean selected = ((Boolean) subsetTableModel
							.getValueAt(i, 1)).booleanValue();
					if (selected != ((SubsetObject) currentObject)
							.getSubsets().contains(sub)) {
						SubsetChangeHistoryItem item = new SubsetChangeHistoryItem(
								sub.getName(), !selected, currentObject.getID());
						out.add(item);
					}
				}
			}
			return out;
		} else
			return Collections.EMPTY_LIST;
	}
}
