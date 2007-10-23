package org.bbop.swing;

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

public class HTMLTableRenderer extends DefaultTableCellRenderer {
	@Override
	public Component getTableCellRendererComponent(JTable table, Object value,
			boolean isSelected, boolean hasFocus, int row, int column) {
		String html = getHTML(table, value, isSelected, hasFocus, row, column);
		return super.getTableCellRendererComponent(table, html, isSelected,
				hasFocus, row, column);
	}

	public String getHTML(JTable table, Object value, boolean isSelected,
			boolean hasFocus, int row, int column) {
		return "<html>"+value+"</html>";
	}
}
