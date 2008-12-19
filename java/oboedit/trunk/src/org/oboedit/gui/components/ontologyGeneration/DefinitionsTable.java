package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.event.MouseEvent;
import java.util.List;

import javax.swing.JTable;

/**
 * JTable to show instances of {@link CandidateDefinition}.
 * 
 * @author Atif Iqbal, 2008
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class DefinitionsTable extends JTable
{
	private static final long serialVersionUID = -1511859431547268636L;
	private int minScrollableViewPortHeight;
	private int maxScrollableViewPortHeight;

	/**
	 * Constructs a {@link DefinitionsTable}
	 */
	public DefinitionsTable()
	{
		super(new DefinitionsTableModel());
		setGridColor(Color.LIGHT_GRAY);
		setRowHeight(getRowHeight() + 4);
		getColumnModel().getColumn(0).setMaxWidth(50);
		getColumnModel().getColumn(0).setResizable(false);
		getColumnModel().getColumn(2).setMaxWidth(30);
		tableHeader.setReorderingAllowed(true);
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
			setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			String tempTip = getModel().getDefinitionAt(rowIndex).getUrl();

			if (tempTip.length() > 100) {
				toolTipBuffer.append("<html>");
				toolTipBuffer.append(tempTip.substring(0, tempTip.length() / 2));
				toolTipBuffer.append("<br>");
				toolTipBuffer.append(tempTip.substring((tempTip.length() / 2) + 1, tempTip.length() - 1));
				toolTipBuffer.append("</html>");

				tip = toolTipBuffer.toString();
			}
			else
				tip = tempTip;

		}
		else if (realColumnIndex == 1) {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			tip = getModel().getDefinitionAt(rowIndex).getDefinition();

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
}
