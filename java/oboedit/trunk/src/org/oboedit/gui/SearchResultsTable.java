package org.oboedit.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;

import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.bbop.swing.ReselectListSelectionModel;
import org.obo.datamodel.IdentifiedObject;
import org.obo.query.impl.SearchHit;
import org.oboedit.gui.Preferences;

import org.apache.log4j.*;

public class SearchResultsTable extends JTable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SearchResultsTable.class);

	protected SearchResultsTableModel<?> searchModel;
	protected long maximumFormattingTime = 1000;
//        public static final Color LIGHT_BLUE = new Color(210,220,240);  // Color for light blue alternate rows in search results table
        public static final Color LIGHT_BLUE = new Color(230,235,250);  // Color for light blue alternate rows in search results table
//        public static final Color SELECTED_COLOR = Color.orange;  // Highlight color used when user selects a search result
//        public static final Color SELECTED_COLOR = new Color(160,170,255);
        public static final Color SELECTED_COLOR = Preferences.getPreferences().getSelectionColor();

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

			if(System.getProperty("os.name").startsWith("Win")){

				c.setBackground(Color.white);
				c.setForeground(Color.black);

			} else {
				//alternate blue and white row colors 
				//			if (row % 2 == 0 && !isSelected) {
				if (isSelected) {
					c.setBackground(SELECTED_COLOR);
					c.setForeground(Color.black);
				} 
				else if(row %2 ==0){
					c.setBackground(LIGHT_BLUE);
					c.setForeground(Color.black);
				}
				else{
					c.setBackground(Color.white);
					c.setForeground(Color.black);
				}
			}
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
		setDefaultRowHeight(this);
		validate();
	}

	/**
	 * Sets the height of the rows in the table that is used to display the results of a search.
	 * The height is set relative to the current font size so that if the font is made bigger or
	 * smaller, the rows will change height to accommodate this. 
	 * @param searchResultsTable
	 */
	private void setDefaultRowHeight(SearchResultsTable searchResultsTable) {
		Font currentFont = Preferences.getPreferences().getFont();
		int fontSize = currentFont.getSize();
		int rowSize = (int) (fontSize*1.3);
		searchResultsTable.setRowHeight(rowSize); //Set row height here.
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
