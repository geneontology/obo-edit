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

public class CategoryEditorComponent extends AbstractTextEditComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Comparator catComparator = new Comparator() {
		public int compare(Object a, Object b) {
			TermCategory tca = (TermCategory) a;
			TermCategory tcb = (TermCategory) b;
			return tca.toString().compareTo(tcb.toString());
		}
	};

	protected class CategoryTableModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected Vector catList = new Vector();
		protected Vector valList = new Vector();

		public CategoryTableModel() {
		}

		public void reload() {
			catList.clear();
			valList.clear();
			catList.addAll(SessionManager.getManager().getSession()
					.getCategories());
			Collections.sort(catList, catComparator);
			Iterator it = catList.iterator();
			while (it.hasNext()) {
				TermCategory cat = (TermCategory) it.next();
				if (currentObject != null
						&& currentObject instanceof CategorizedObject)
					valList.add(new Boolean(((CategorizedObject) currentObject)
							.getCategories().contains(cat)));
			}

			fireTableStructureChanged();
		}

		@Override
		public Class getColumnClass(int col) {
			if (col == 0)
				return TermCategory.class;
			else
				return Boolean.class;
		}

		public int getColumnCount() {
			return 2;
		}

		@Override
		public String getColumnName(int col) {
			if (col == 0)
				return "Category";
			else
				return "Active?";
		}

		public int getRowCount() {
			return valList.size();
		}

		public Object getValueAt(int row, int column) {
			if (column == 0)
				return catList.get(row);
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

	protected JTable categoryTable = new JTable();
	protected JPanel tablePanel = new JPanel();
	protected CategoryTableModel categoryTableModel = new CategoryTableModel();

	protected JScrollPane categoryScroller;

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		if (id.equals("table"))
			return categoryScroller;
		else
			return new JButton(id);
	}

	public CategoryEditorComponent() {
		categoryTable.setModel(categoryTableModel);
		categoryTable.setDefaultRenderer(TermCategory.class,
				new DefaultTableCellRenderer());
		categoryTable.setTableHeader(null);
		categoryTable.setColumnSelectionAllowed(false);
		categoryTable.setRowSelectionAllowed(false);
		categoryTable.setOpaque(false);
		tablePanel.setLayout(new GridLayout(1, 1));
		tablePanel.add(categoryTable);
		categoryScroller = new JScrollPane(tablePanel,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	}

	@Override
	protected boolean useSubLayout() {
		return true;
	}

	@Override
	protected String getDefaultLayout() {
		return "<component id='table' titleborder='Categories'/>";
	}

	@Override
	protected void loadGUI() {
		categoryTableModel.reload();
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
		return "CATEGORY_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
		IdentifiedObject term = io;
		if (term instanceof CategorizedObject) {
			for (int i = 0; i < categoryTableModel.getRowCount(); i++) {
				TermCategory cat = (TermCategory) categoryTableModel
						.getValueAt(i, 0);
				boolean selected = ((Boolean) categoryTableModel.getValueAt(i,
						1)).booleanValue();
				if (selected)
					((CategorizedObject) term).addCategory(cat);
				else
					((CategorizedObject) term).removeCategory(cat);
			}
		}
	}

	public java.util.List getChanges() {
		if (currentObject != null) {
			java.util.List out = new LinkedList();
			if (currentObject instanceof CategorizedObject) {
				for (int i = 0; i < categoryTableModel.getRowCount(); i++) {
					TermCategory cat = (TermCategory) categoryTableModel
							.getValueAt(i, 0);
					boolean selected = ((Boolean) categoryTableModel
							.getValueAt(i, 1)).booleanValue();
					if (selected != ((CategorizedObject) currentObject)
							.getCategories().contains(cat)) {
						CategoryChangeHistoryItem item = new CategoryChangeHistoryItem(
								cat.getName(), !selected, currentObject.getID());
						out.add(item);
					}
				}
			}
			return out;
		} else
			return Collections.EMPTY_LIST;
	}
}
