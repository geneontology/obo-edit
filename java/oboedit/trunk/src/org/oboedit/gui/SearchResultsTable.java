package org.oboedit.gui;

import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;

import javax.swing.DefaultListSelectionModel;
import javax.swing.JTable;
import javax.swing.ListCellRenderer;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.bbop.swing.ReselectListSelectionModel;
import org.obo.datamodel.IdentifiedObject;
import org.obo.query.impl.SearchHit;

public class SearchResultsTable extends JTable {

	protected SearchResultsTableModel<?> searchModel;
	protected long maximumFormattingTime = 1000;

	protected static class SearchResultsRenderer extends
			DefaultTableCellRenderer {

		String[] columnExpressions;
		HTMLNodeLabelProvider provider = new HTMLNodeLabelProvider();

		public SearchResultsRenderer(String... expressions) {
			columnExpressions = expressions;
		}

		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			provider.setHtmlExpression(columnExpressions[column]);
			IdentifiedObject io = ((SearchResultsTableModel<IdentifiedObject>) table
					.getModel()).getObjects().get(row);
			Component c = super.getTableCellRendererComponent(table, value,
					isSelected, hasFocus, row, column);
			setText(provider.getLabel(null, io));
			return c;
		}
	}

	public SearchResultsTable(SearchResultsTableModel<?> model,
			Collection<SearchHit<?>> results) {
		this.searchModel = model;
		setModel(searchModel);
		if (IdentifiedObject.class.isAssignableFrom(model.getObjectType())) {
			setDefaultRenderer(Object.class, new SearchResultsRenderer("$id$",
					"$name$"));
		}
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

	protected void setDefaultColumnSizes(JTable table) {
		JTableHeader header = table.getTableHeader();
		TableColumnModel columnModel = header.getColumnModel();
		SearchResultsTableModel<?> model = (SearchResultsTableModel<?>) table
				.getModel();
		long time = System.currentTimeMillis();
		for (int i = 0; i < columnModel.getColumnCount(); i++) {
			if (System.currentTimeMillis() - time >= maximumFormattingTime)
				return;
			TableColumn tc = columnModel.getColumn(i);
			int width = 0;
			for (int j = 0; j < table.getRowCount(); j++) {
				Object o = model.getValueAt(j, i);
				TableCellRenderer renderer = table.getCellRenderer(j, i);
				Component c = renderer.getTableCellRendererComponent(table, o,
						true, true, j, i);
				if ((int) c.getPreferredSize().getWidth() > width)
					width = (int) c.getPreferredSize().getWidth();
				if (System.currentTimeMillis() - time >= maximumFormattingTime)
					break;
			}
			if (model.columnHasMaxWidth(i)) {
//				tc.setMinWidth(width + 10);
				tc.setMinWidth(width-10);
				// Don't restrict the maximum width
//				tc.setMaxWidth(width + 10);
			}
			tc.setPreferredWidth(width + 10);

		}
	}

	public long getMaximumFormattingTime() {
		return maximumFormattingTime;
	}

	public void setMaximumFormattingTime(long maximumFormattingTime) {
		this.maximumFormattingTime = maximumFormattingTime;
	}
}
