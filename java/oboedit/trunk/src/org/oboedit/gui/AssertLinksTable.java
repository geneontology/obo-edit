package org.oboedit.gui;

import java.awt.Component;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;

import javax.swing.JCheckBox;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.bbop.swing.ReselectListSelectionModel;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;

import org.apache.log4j.*;

public class AssertLinksTable extends JTable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AssertLinksTable.class);

	protected AssertLinksTableModel<?> assertModel;
	protected long maximumFormattingTime = 1000;
	
	JCheckBox checkBox = new JCheckBox();

	protected static class AssertResultsRenderer extends DefaultTableCellRenderer {

		String[] columnExpressions;
		HTMLNodeLabelProvider provider = new HTMLNodeLabelProvider();

		public AssertResultsRenderer(String... expressions) {
			columnExpressions = expressions;
		}

		public Component getTableCellRendererComponent(
				JTable table,
				Object value, 
				boolean isSelected, 
				boolean hasFocus, 
				int row,
				int column) {	
			
			logger.debug("AssertLinksTable.getTableCellRendererComponent");			
			provider.setHtmlExpression(columnExpressions[column]);
			IdentifiedObject io = ((AssertLinksTableModel<IdentifiedObject>) table
					.getModel()).getObjects().get(row);
			Component c = super.getTableCellRendererComponent(table, value,
					isSelected, hasFocus, row, column);
			setText(provider.getLabel(null, io));
			return c;
		}
	}

	
	public AssertLinksTable(AssertLinksTableModel<?> model,
			Collection<Link> results) {
		
		this.assertModel = model;
		setModel(assertModel);
		if (IdentifiedObject.class.isAssignableFrom(model.getObjectType())) {
			setDefaultRenderer(Object.class, new AssertResultsRenderer("$id$",
			"$name$"));
		}
		setSelectionModel(new ReselectListSelectionModel());
		
		getSelectionModel().addListSelectionListener(assertModel.getSelectionListener(this));
		
		final JTableHeader header = getTableHeader();
		header.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				TableColumnModel columnModel = header.getColumnModel();
				int viewColumn = columnModel.getColumnIndexAtX(e.getX());
				int column = columnModel.getColumn(viewColumn).getModelIndex();
				((AssertLinksTableModel<?>) getModel()).setSortColumn(column);
			}
		});
		if (results != null)
			setResults(results);
	}

	public void setResults(Collection<Link> results) {
		assertModel.setResults(results);
		setDefaultColumnSizes(this);
		setDefaultRowHeight(this);
		validate();
	}

	/**
	 * Sets the height of the rows in the table that is used to display asserted links
	 * The height is set relative to the current font size so that if the font is made bigger or
	 * smaller, the rows will change height to accommodate this. 
	 * @param assertLinksTable
	 */
	private void setDefaultRowHeight(AssertLinksTable assertedLinksTable) {
		Font currentFont = Preferences.getPreferences().getFont();
		int fontSize = currentFont.getSize();
		int rowSize = (int) (fontSize*1.3);
		assertedLinksTable.setRowHeight(rowSize); //Set row height here.
	}

	protected void setDefaultColumnSizes(JTable table) {
		JTableHeader header = table.getTableHeader();
		TableColumnModel columnModel = header.getColumnModel();		
		AssertLinksTableModel<?> model = (AssertLinksTableModel<?>) table
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
