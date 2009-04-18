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

import org.obo.datamodel.IdentifiableObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.PathCapable;
import org.oboedit.controller.SelectionManager;


import org.apache.log4j.*;

public abstract class AbstractAssertLinksTableModel<T extends IdentifiableObject>
extends AbstractTableModel implements AssertLinksTableModel<T> {
	protected java.util.List<T> itemList = Collections.emptyList();

//	initialize logger
	protected final static Logger logger = Logger.getLogger(AbstractAssertLinksTableModel.class);

	protected boolean reverse = false;

	protected int sortColumn = 0;

	protected Class<T> objectType;

	public AbstractAssertLinksTableModel(Class<T> objectType) {
		this.objectType = objectType;
	}

	public Class<T> getObjectType() {
		return objectType;
	}

	protected Comparator<T> comparator = new java.util.Comparator<T>() {
		public int compare(T o1, T o2) {
//			logger.debug("AbstractAssertLinksTableModel - comparator");
			String compVal1 = getColumnVal(o1, getSortColumn()).toString();
			String compVal2 = getColumnVal(o2, getSortColumn()).toString();
			int compVal = compVal1.compareToIgnoreCase(compVal2);
//			logger.debug("compVal: " + compVal);
			if (reverse)
				return -compVal;
			else{
				return compVal;
			}

		}
	};

	protected T getResult(Link link) {
		if(link != null)
			return (T) link;
		else
			return null;
	}

	public void setResults(Collection<Link> results) {
		itemList = new ArrayList<T>();
		for (Link o : results) {
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
				AssertLinksTableModel model = (AssertLinksTableModel) table.getModel();

				Collection<PathCapable> linkSelection = new LinkedList<PathCapable>();
				Collection<PathCapable> colSelection = new LinkedList<PathCapable>();

				logger.debug("\n>> selected row: " + table.getSelectedRow());
				logger.debug(">> selected col: " + table.getSelectedColumn());
				if(table.getSelectedColumn() >=0){
					logger.debug(">> value in selected column;  " + table.getValueAt(table.getSelectedRow(), table.getSelectedColumn()));
//					T colobj = (T) getValueAt(table.getSelectedRow(), table.getSelectedColumn());
//
//					PathCapable selectedCol = getPathCapable(colobj);
//					colSelection.add(selectedCol);
				}
				

				//multiple rows selected
				for (int rowNum : table.getSelectedRows()) {
						T rowobj = getValueAt(rowNum);
						PathCapable selectMe = getPathCapable(rowobj);
						if (selectMe != null){
							logger.debug("selected row: " + selectMe);
							linkSelection.add(selectMe);
					}
				}

				logger.debug("linkSelection.size(): " + linkSelection.size());
				Selection s = SelectionManager.createSelection(table, linkSelection);
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
	
	protected PathCapable getPathCapable(T r, T c) {
		if (c instanceof PathCapable)
			return (PathCapable) c;
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
//		logger.debug("itemList.size(): " + itemList.size());
		try{
			Collections.sort(itemList, comparator) ;
		}
		catch(Exception e){
			logger.debug("AbstractAssertLinksTableModel - doSort()--compartor exception e: " + e);
		}
		fireTableDataChanged();
	}

	public int getRowCount() {
		return itemList.size();
	}
}
