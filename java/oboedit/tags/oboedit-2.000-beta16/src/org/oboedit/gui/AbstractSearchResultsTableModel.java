package org.oboedit.gui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;
import org.obo.query.impl.SearchHit;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;

public abstract class AbstractSearchResultsTableModel<T> extends
		AbstractTableModel implements SearchResultsTableModel<T> {
	protected java.util.List<T> itemList = Collections.emptyList();

	protected boolean reverse = false;

	protected int sortColumn = 0;

	protected Class<T> objectType;

	public AbstractSearchResultsTableModel(Class<T> objectType) {
		this.objectType = objectType;
	}

	protected Comparator<T> comparator = new java.util.Comparator<T>() {
		public int compare(Object o1, Object o2) {
			String compVal1 = getColumnVal(o1, getSortColumn()).toString();
			String compVal2 = getColumnVal(o2, getSortColumn()).toString();
			int compVal = compVal1.compareToIgnoreCase(compVal2);
			if (reverse)
				return -compVal;
			else
				return compVal;
		}
	};

	protected T getResult(SearchHit<?> o) {
		if (objectType.isAssignableFrom(o.getHit().getClass()))
			return (T) o.getHit();
		else
			return null;
	}

	public void setResults(Collection<SearchHit<?>> results) {
		itemList = new ArrayList<T>();
		for (SearchHit<?> o : results) {
			T t = getResult(o);
			if (t != null)
				itemList.add(t);
		}
		setSortColumn(getSortColumn(), true);
	}

	protected int getSortColumn() {
		return sortColumn;
	}

	public boolean columnHasMaxWidth(int column) {
		return false;
	}

	public ListSelectionListener getSelectionListener(final JTable table) {
		ListSelectionListener listener = new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				SearchResultsTableModel model = (SearchResultsTableModel) table
						.getModel();
				Collection<PathCapable> selection = new LinkedList<PathCapable>();
				for (int rowNum : table.getSelectedRows()) {
					T obj = getValueAt(rowNum);
					PathCapable selectMe = getPathCapable(obj);
					if (selectMe != null)
						selection.add(selectMe);
				}
				Selection s = SelectionManager
						.createSelection(table, selection);
				SelectionManager.getManager().select(s);
			}
		};
		return listener;
	}
	
	protected PathCapable getPathCapable(T t) {
		if (t instanceof PathCapable)
			return (PathCapable) t;
		else
			return null;
	}

	public T getValueAt(int row) {
		return itemList.get(row);
	}

	public List<T> getObjects() {
		return itemList;
	}

	public void setSortColumn(int col) {
		setSortColumn(col, false);
	}

	public void setSortColumn(int sortColumn, boolean ignoreReverse) {
		if (!ignoreReverse) {
			if (sortColumn == this.sortColumn)
				reverse = !reverse;
			else
				reverse = false;
		}
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
