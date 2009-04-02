package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.event.MouseEvent;
import java.util.List;

import javax.swing.JTable;
import javax.swing.JToolTip;

/**
 * JTable to show instances of {@link CandidateDefinition}.
 * 
 * @author Atif Iqbal, 2008
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class DefinitionsTable extends JTable
{
	private static final long serialVersionUID = -1511859431547268636L;
	private int minScrollableViewPortHeight = 40;
	private int maxScrollableViewPortHeight = 200;

	/**
	 * Constructs a {@link DefinitionsTable}
	 */
	public DefinitionsTable()
	{
		super(new DefinitionsTableModel());
		setGridColor(Color.LIGHT_GRAY);
		setRowHeight(getRowHeight() * 3);
		setAlignmentY(TOP_ALIGNMENT);
		setAlignmentX(LEFT_ALIGNMENT);
		getColumnModel().getColumn(0).setMaxWidth(50);
		getColumnModel().getColumn(0).setResizable(false);
		getColumnModel().getColumn(2).setMaxWidth(30);
		setPreferredScrollableViewportSize(new Dimension(minScrollableViewPortHeight, maxScrollableViewPortHeight));
		tableHeader.setReorderingAllowed(false);
	}

	/**
	 * Set the Collection of {@link CandidateDefinition} to be displayed by the {@link DefinitionsTable}.
	 * 
	 * @param definitions
	 */
	public void setDefinitions(List<CandidateDefinition> definitions)
	{
		getModel().setDefinitions(definitions);
		JTableHelper.recalculateScrollableViewportSize(this, minScrollableViewPortHeight, maxScrollableViewPortHeight);
	}

	/**
	 * Sets the minimal height of the visible area of the {@link JTable}
	 * 
	 * @param minHeight
	 */
	public void setMinimumPreferedeScrollableViewportHeight(int minHeight)
	{
		this.minScrollableViewPortHeight = minHeight;
	}

	/**
	 * Sets the maximal height of the visible area of the {@link JTable}
	 * 
	 * @param maxHeight
	 */
	public void setMaximumPreferedeScrollableViewportHeight(int maxHeight)
	{
		this.maxScrollableViewPortHeight = maxHeight;
	}

	/*
	 * PRIVATE METHODS
	 */

	/*
	 * OVERRIDDEN METHODS
	 */
	@Override
	public void removeAll()
	{
		this.getModel().removeAllDefinitions();
		JTableHelper.recalculateScrollableViewportSize(this, minScrollableViewPortHeight, maxScrollableViewPortHeight);
	}

	@Override
	public DefinitionsTableModel getModel()
	{
		return (DefinitionsTableModel) super.getModel();
	}

	@Override
	public String getToolTipText(MouseEvent e)
	{
		String tip = null;
		StringBuffer toolTipBuffer = new StringBuffer();

		java.awt.Point p = e.getPoint();
		int rowIndex = rowAtPoint(p);
		int colIndex = columnAtPoint(p);
		int realColumnIndex = convertColumnIndexToModel(colIndex);

		if (realColumnIndex == 2) {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			tip = "Click for more information on sources of the definitions";
		}
		else {
			/*
			 * You can omit this part if you know you don't have any renderer that supply their own tool tips.
			 */
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			tip = super.getToolTipText(e);
		}
		return tip;
	}

	// // Returns the preferred height of a row.
	// // The result is equal to the tallest cell in the row.
	// public int getPreferredRowHeight(JTable table, int rowIndex, int margin)
	// {
	// // Get the current default height for all rows
	// int height = table.getRowHeight(rowIndex);
	// table.setAlignmentY(TOP_ALIGNMENT);
	//
	// // Determine highest cell in the row
	// for (int c = 1; c < table.getColumnCount()-1; c++) {
	// TableCellRenderer renderer = table.getCellRenderer(rowIndex, c);
	// Component comp = table.prepareRenderer(renderer, rowIndex, c);
	// int h = comp.getPreferredSize().height * margin;
	// height = Math.max(height, h);
	// }
	// return height;
	// }
	//
	// // The height of each row is set to the preferred height of the
	// // tallest cell in that row.
	// public void packRows(JTable table, int margin)
	// {
	// packRows(table, 0, table.getRowCount(), margin);
	// }
	//
	// // For each row >= start and < end, the height of a
	// // row is set to the preferred height of the tallest cell
	// // in that row.
	// public void packRows(JTable table, int start, int end, int margin)
	// {
	// for (int r = start; r < table.getRowCount() && r < end; r++) {
	// // Get the preferred height
	// int h = getPreferredRowHeight(table, r, margin);
	//
	// // Now set the row height using the preferred height
	// if (table.getRowHeight(r) != h) {
	// table.setRowHeight(r, h);
	// }
	// }
	// }

	// /**
	// * TODO describe me!
	// *
	// * @see javax.swing.JTable#updateUI()
	// */
	// @Override
	// public void updateUI()
	// {
	// super.updateUI();
	// packRows(this, 2);
	// }

}
