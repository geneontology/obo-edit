package org.oboedit.gui;

import java.util.Collection;
import java.util.List;

import javax.swing.JTable;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableModel;

import org.obo.datamodel.Link;

public interface AssertLinksTableModel<T> extends TableModel {
	public Class<T> getObjectType();
	
	public void setResults(Collection<Link> results);

	public void setSortColumn(int col);
	
	public boolean columnHasMaxWidth(int column);

	public String getColumnName(int index);
	
	public Class getColumnClass(int column);
	
	public ListSelectionListener getSelectionListener(JTable table);
	
	public List<T> getObjects();
}
