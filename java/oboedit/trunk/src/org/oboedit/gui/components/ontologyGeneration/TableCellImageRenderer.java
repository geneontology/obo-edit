package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Component;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * A renderer that renders an image in the cell it is applied to
 * 
 * @author Atif Iqbal, 2008
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class TableCellImageRenderer extends DefaultTableCellRenderer
{
	private static final long serialVersionUID = 7535592713217922765L;

	private JLabel lbl = new JLabel();
	private ImageIcon icon = null;

	TableCellImageRenderer(String pathToImage)
	{
		icon = new ImageIcon(getClass().getResource(pathToImage));
	}

	@Override
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column)
	{
		lbl.setIcon(icon);
		return lbl;
	}
}
