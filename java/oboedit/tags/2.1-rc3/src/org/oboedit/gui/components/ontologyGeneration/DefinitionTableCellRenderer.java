package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Color;
import java.awt.Component;
import java.io.Serializable;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;

/** 
 * Variant of the {@link DefaultTableCellRenderer} which returns always a new {@link JLabel}.
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2009
 **/
public class DefinitionTableCellRenderer implements TableCellRenderer, Serializable
{

	private static final long serialVersionUID = 2731215098905995428L;
	
    protected static Border noFocusBorder = new EmptyBorder(1, 1, 1, 1); 
    private static final Border SAFE_NO_FOCUS_BORDER = new EmptyBorder(1, 1, 1, 1);

	private Color unselectedForeground;
	private Color unselectedBackground;

	/**
	 * TODO describe me!
	 * 
	 * @param table
	 * @param value
	 * @param isSelected
	 * @param hasFocus
	 * @param row
	 * @param column
	 * @return
	 * @see javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object,
	 *      boolean, boolean, int, int)
	 */
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
	    int row, int column)
	{
		JLabel label = new JLabel();
		label.setOpaque(true);
		label.setBorder(getNoFocusBorder());
		if (isSelected) {
			label.setForeground(table.getSelectionForeground());
			label.setBackground(table.getSelectionBackground());
		}
		else {
			label.setForeground((unselectedForeground != null) ? unselectedForeground : table.getForeground());
			label.setBackground((unselectedBackground != null) ? unselectedBackground : table.getBackground());
		}
		label.setFont(table.getFont());

		if (hasFocus) {
			Border border = null;
			if (isSelected) {
				border = UIManager.getBorder("Table.focusSelectedCellHighlightBorder");
			}
			if (border == null) {
				border = UIManager.getBorder("Table.focusCellHighlightBorder");
			}
			label.setBorder(border);

			if (!isSelected && table.isCellEditable(row, column)) {
				Color col;
				col = UIManager.getColor("Table.focusCellForeground");
				if (col != null) {
					label.setForeground(col);
				}
				col = UIManager.getColor("Table.focusCellBackground");
				if (col != null) {
					label.setBackground(col);
				}
			}
		}
		else {
			label.setBorder(getNoFocusBorder());
		}
		label.setText(value.toString());
		return label;
	}
	
    private static Border getNoFocusBorder() {
        if (System.getSecurityManager() != null) {
            return SAFE_NO_FOCUS_BORDER;
        } else {
            return noFocusBorder;
        }
    }
}
