package org.oboedit.gui.components;

import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIComponent;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;
import org.obo.query.impl.SearchHit;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.Selection;

public class SearchResultsComponent extends AbstractGUIComponent implements
		GUIComponent {

	protected abstract class AbstractSearchModel extends AbstractTableModel {
		protected java.util.List<PathCapable> itemList = new ArrayList<PathCapable>();

		protected boolean reverse = false;

		protected int sortColumn = 0;

		protected Comparator comparator = new java.util.Comparator() {
			public int compare(Object o1, Object o2) {
				String compVal1 = getColumnVal(o1, sortColumn).toString();
				String compVal2 = getColumnVal(o2, sortColumn).toString();
				int compVal = compVal1.compareToIgnoreCase(compVal2);
				if (reverse)
					return -compVal;
				else
					return compVal;
			}
		};

		public abstract boolean columnHasMaxWidth(int column);

		public Object getValueAt(int row) {
			return itemList.get(row);
		}

		public List<PathCapable> getObjects() {
			return itemList;
		}

		public void setSortColumn(int sortColumn) {
			if (sortColumn == this.sortColumn)
				reverse = !reverse;
			else
				reverse = false;
			this.sortColumn = sortColumn;
			doSort();
		}

		public abstract Object getColumnVal(Object rowObj, int col);

		public Object getValueAt(int row, int column) {
			return getColumnVal(getValueAt(row), column);
		}

		protected void doSort() {
			Collections.sort(itemList, comparator);
			fireTableDataChanged();
		}

		public int getRowCount() {
			return itemList.size();
		}
	}

	protected class IdentifiedObjectModel extends AbstractSearchModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public IdentifiedObjectModel(Collection<SearchHit<PathCapable>> results) {
			for (SearchHit<PathCapable> hit : results) {
				if (hit.getHit() instanceof IdentifiedObject) {
					itemList.add(hit.getHit());
				}
			}
			setSortColumn(1);
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
	}

	protected class LinkModel extends AbstractSearchModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public LinkModel(Collection results) {
			Iterator it = results.iterator();
			while (it.hasNext()) {
				Object o = it.next();
				if (o instanceof Link) {
					it.remove();
					itemList.add((PathCapable) o);
				}
			}
			setSortColumn(0);
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

	protected JTable table = new JTable();

	public SearchResultsComponent(String id) {
		super(id);
	}

	@Override
	public void init() {
		setLayout(new GridLayout(1, 1));
		add(new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER));
		table.getSelectionModel().addListSelectionListener(
				new ListSelectionListener() {

					public void valueChanged(ListSelectionEvent e) {
						AbstractSearchModel model = (AbstractSearchModel) table
						.getModel();
						Collection<LinkedObject> selection = new LinkedList<LinkedObject>();
						for(int rowNum : table.getSelectedRows()) {
							selection.add((LinkedObject) model.getObjects().get(rowNum));
						}
						Selection s = SelectionManager.getManager()
								.createSelectionFromTerms(table, selection, null,
										false);
						SelectionManager.getManager().select(s);
					}

				});
	}

	protected void setDefaultColumnSizes(JTable table) {
		JTableHeader header = table.getTableHeader();
		TableColumnModel columnModel = header.getColumnModel();
		AbstractSearchModel model = (AbstractSearchModel) table.getModel();
		for (int i = 0; i < columnModel.getColumnCount(); i++) {
			TableColumn tc = columnModel.getColumn(i);
			int width = 0;
			for (int j = 0; j < table.getRowCount(); j++) {
				Object o = model.getValueAt(j, i);
				TableCellRenderer renderer = table.getCellRenderer(j, i);
				Component c = renderer.getTableCellRendererComponent(table, o,
						true, true, j, i);
				if ((int) c.getPreferredSize().getWidth() > width)
					width = (int) c.getPreferredSize().getWidth();
			}
			if (model.columnHasMaxWidth(i)) {
				tc.setMinWidth(width + 10);
				tc.setMaxWidth(width + 10);
			}
			tc.setPreferredWidth(width + 10);

		}
	}

	public void setResults(Collection<SearchHit<PathCapable>> results) {
		final JTableHeader header = table.getTableHeader();
		header.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				TableColumnModel columnModel = header.getColumnModel();
				int viewColumn = columnModel.getColumnIndexAtX(e.getX());
				int column = columnModel.getColumn(viewColumn).getModelIndex();
				((AbstractSearchModel) table.getModel()).setSortColumn(column);
			}
		});
		table.setModel(new IdentifiedObjectModel(results));
		setDefaultColumnSizes(table);
		validate();
	}

}
