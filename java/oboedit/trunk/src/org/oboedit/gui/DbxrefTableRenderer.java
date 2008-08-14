/**
 * 
 */
package org.oboedit.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;

import org.obo.datamodel.Dbxref;
import org.oboedit.gui.components.AbstractDbxrefEditorComponent;

import org.apache.log4j.*;

public class DbxrefTableRenderer extends DefaultTableCellRenderer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DbxrefTableRenderer.class);
	
	protected static final Color REALLY_LIGHT_GRAY = new Color(230, 230, 230);

	
	@Override
	public Component getTableCellRendererComponent(JTable table,
			Object value, boolean isSelected, boolean hasFocus, int row,
			int column) {
		JLabel out = (JLabel) super.getTableCellRendererComponent(table,
									  value, isSelected, hasFocus, row, column);
		if (value instanceof Dbxref)
			configureLabel(table, out, (Dbxref) value, row, isSelected);
		return out;
	}
	
	protected void configureLabel(JTable table, JLabel out, Dbxref dbxref,
			int index, boolean isSelected) {
		out.setOpaque(true);
		out.setBorder(new EmptyBorder(2, 2, 2, 2));
		out.setMinimumSize(new Dimension(table.getWidth(), 0));
		if (!isSelected) {
			if (index % 2 == 0)
				out.setBackground(REALLY_LIGHT_GRAY);
			else
				out.setBackground(Color.white);
		}
		String s = "<html>"
				+ dbxref.getDatabase()
				+ ":"
				+ dbxref.getDatabaseID()
				+ "<br>"
				+ (dbxref.getDesc() != null ? "<i>" + dbxref.getDesc() + "</i>"
						: "") + "</html>";
		out.setText(s);
	}
}
