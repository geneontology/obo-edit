package org.bbop.swing;

import java.awt.Component;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JList;

public class HTMLListRenderer extends DefaultListCellRenderer {
	
	public String getHTML(JList list, Object value, int index, boolean isSelected,
			boolean cellHasFocus) {
		return "<html>"+value.toString()+"</html>";
	}
	
	@Override
	public Component getListCellRendererComponent(JList list, Object value,
			int index, boolean isSelected, boolean cellHasFocus) {
		String html = getHTML(list, value, index, isSelected, cellHasFocus);
		return super.getListCellRendererComponent(list, html, index,
				isSelected, cellHasFocus);
	}
}
