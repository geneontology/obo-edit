package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Dimension;
import java.awt.Rectangle;

import javax.swing.JTable;
import javax.swing.JViewport;

/**
 * Helper to manipulate {@link JTable}
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), Dec 10, 2008
 */
public class JTableHelper
{
	public static void scrollToCenter(JTable table, int rowIndex, int vColIndex)
	{
		if (!(table.getParent() instanceof JViewport)) {
			return;
		}
		JViewport viewport = (JViewport) table.getParent();

		// This rectangle is relative to the table where the
		// northwest corner of cell (0,0) is always (0,0).
		Rectangle rect = table.getCellRect(rowIndex, vColIndex, true);

		// The location of the view relative to the table
		Rectangle viewRect = viewport.getViewRect();

		// Translate the cell location so that it is relative
		// to the view, assuming the northwest corner of the
		// view is (0,0).
		rect.setLocation(rect.x - viewRect.x, rect.y - viewRect.y);

		// Calculate location of rect if it were at the center of view
		int centerX = (viewRect.width - rect.width) / 2;
		int centerY = (viewRect.height - rect.height) / 2;

		// Fake the location of the cell so that scrollRectToVisible
		// will move the cell to the center
		if (rect.x < centerX) {
			centerX = -centerX;
		}
		if (rect.y < centerY) {
			centerY = -centerY;
		}
		rect.translate(centerX, centerY);

		// Scroll the area into view.
		viewport.scrollRectToVisible(rect);
	}

	public static void scrollToTopAndSelectFirst(JTable table)
	{
		synchronized (table.getModel()) {
			if (!(table.getParent() instanceof JViewport)) {
				return;
			}
			table.scrollRectToVisible(new Rectangle(0, 0, 0, table.getRowHeight()));
			// select first
			if (table.getModel().getRowCount() > 0) {
				table.getSelectionModel().setSelectionInterval(0, 0);
			}
		}
	}

	public static void recalculateScrollableViewportSize(JTable table, int minScrollableViewPortHeight, int maxScrollableViewPortHeight)
	{
		int requiredHeight = table.getModel().getRowCount() * table.getRowHeight();
		requiredHeight = Math.max(requiredHeight, minScrollableViewPortHeight);
		requiredHeight = Math.min(requiredHeight, maxScrollableViewPortHeight);
		table.setPreferredScrollableViewportSize(new Dimension(table.getPreferredScrollableViewportSize().width, requiredHeight));
	}
}
