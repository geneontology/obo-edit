package org.oboedit.gui;

import java.util.Collection;

import javax.swing.JTable;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableModel;

import org.obo.query.impl.SearchHit;

public interface SearchResultsTableModel<T> extends TableModel {

	public void setResults(Collection<SearchHit<?>> results);

	public void setSortColumn(int col);

	public boolean columnHasMaxWidth(int column);

	public String getColumnName(int index);
	
	public ListSelectionListener getSelectionListener(JTable table);
}
