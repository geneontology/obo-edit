package org.oboedit.gui;

import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;

import javax.swing.DefaultListSelectionModel;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.bbop.swing.ReselectListSelectionModel;
import org.obo.query.impl.SearchHit;

public class SearchResultsTable extends JTable {

	protected SearchResultsTableModel<?> searchModel;

	public SearchResultsTable(SearchResultsTableModel<?> model,
			Collection<SearchHit<?>> results) {
		this.searchModel = model;
		setModel(searchModel);
		setSelectionModel(new ReselectListSelectionModel());
		getSelectionModel().addListSelectionListener(
				searchModel.getSelectionListener(this));
		final JTableHeader header = getTableHeader();
		header.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				TableColumnModel columnModel = header.getColumnModel();
				int viewColumn = columnModel.getColumnIndexAtX(e.getX());
				int column = columnModel.getColumn(viewColumn).getModelIndex();
				((SearchResultsTableModel<?>) getModel()).setSortColumn(column);
			}
		});
		if (results != null)
			setResults(results);
	}

	public void setResults(Collection<SearchHit<?>> results) {
		searchModel.setResults(results);
		setDefaultColumnSizes(this);
		validate();
	}

	protected static void setDefaultColumnSizes(JTable table) {
		JTableHeader header = table.getTableHeader();
		TableColumnModel columnModel = header.getColumnModel();
		SearchResultsTableModel<?> model = (SearchResultsTableModel<?>) table
				.getModel();
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
}
