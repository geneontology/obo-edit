/**
 * MultiheightTableUI
 *
 * UI for Multiheight table. Based on an example written by Zafir Anjum at
 * http://codeguru.earthweb.com/java/articles/121.shtml
 */

package org.bbop.swing.multiheighttable;

import javax.swing.*;
import javax.swing.table.*;
import java.util.Enumeration;
import java.awt.*;
import javax.swing.plaf.basic.*;

import org.bbop.swing.MultiheightTable;

import org.apache.log4j.*;

public class MultiheightTableUI extends BasicTableUI
	{

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultiheightTableUI.class);
    private Dimension createTableSize(long width) {
	int height = ((MultiheightTable)table).getCellRect(table.getRowCount(), 0, false).y;
	int totalMarginWidth =  table.getColumnModel().getColumnMargin() * 
                                                                                                                                
	    table.getColumnCount(); 
	long widthWithMargin = Math.abs(width) + totalMarginWidth;
	if (widthWithMargin > Integer.MAX_VALUE) {
	    widthWithMargin = Integer.MAX_VALUE;
	}
	return new Dimension((int)widthWithMargin, height);
    }
    
    /**
     * Return the minimum size of the table. The minimum height is the 
     * row height (plus inter-cell spacing) times the number of rows. 
     * The minimum width is the sum of the minimum widths of each column 
     * (plus inter-cell spacing).
     */
    public Dimension getMinimumSize(JComponent c) {
	long width = 0;
	Enumeration enumeration = table.getColumnModel().getColumns();
	while (enumeration.hasMoreElements()) {
	    TableColumn aColumn = (TableColumn)enumeration.nextElement();
	    width = width + aColumn.getMinWidth();
	}
	return createTableSize(width);
    }
    
    /**
     * Return the preferred size of the table. The preferred height is the 
     * row height (plus inter-cell spacing) times the number of rows. 
     * The preferred width is the sum of the preferred widths of each column 
     * (plus inter-cell spacing).
     */
    public Dimension getPreferredSize(JComponent c) {
	long width = 0;
	Enumeration enumeration = table.getColumnModel().getColumns();
	while (enumeration.hasMoreElements()) {
	    TableColumn aColumn = (TableColumn)enumeration.nextElement();
	    width = width + aColumn.getPreferredWidth();
	}
	return createTableSize(width);
    }
    
    /**
     * Return the maximum size of the table. The maximum height is the 
     * row height (plus inter-cell spacing) times the number of rows. 
     * The maximum width is the sum of the maximum widths of each column 
     * (plus inter-cell spacing).
     */
    public Dimension getMaximumSize(JComponent c) {
	long width = 0;
	Enumeration enumeration = table.getColumnModel().getColumns();
	while (enumeration.hasMoreElements()) {
	    TableColumn aColumn = (TableColumn)enumeration.nextElement();
	    width = width + aColumn.getMaxWidth();
	}
	return createTableSize(width);
    }
    
    public void paint(Graphics g, JComponent c) {
	Rectangle oldClipBounds = g.getClipBounds();
	Rectangle clipBounds = new Rectangle(oldClipBounds);
	int tableWidth = table.getColumnModel().getTotalColumnWidth();
	clipBounds.width = Math.min(clipBounds.width, tableWidth);
	g.setClip(clipBounds);
	
	// Paint the grid
	paintGrid(g);
	
	// Paint the rows
	int firstIndex = table.rowAtPoint(new Point(0, clipBounds.y));
	int  lastIndex = lastVisibleRow(clipBounds);
	
	Rectangle rowRect = new Rectangle(0, 0,
					  tableWidth,
					  ((MultiheightTable)table).getRowHeight(firstIndex) + table.getRowMargin());
	rowRect.y = table.getCellRect(firstIndex, 0, false).y;
	
	for (int index = firstIndex; index <= lastIndex; index++) {
	    // Paint any rows that need to be painted
	    if (rowRect.intersects(clipBounds)) {
		paintRow(g, index);
	    }
	    rowRect.y += rowRect.height;
	    rowRect.height = ((MultiheightTable)table).getRowHeight(index+1);
	}
	g.setClip(oldClipBounds);
    }
    
    private void paintGrid(Graphics g) {
	g.setColor(table.getGridColor());
	
	if (table.getShowHorizontalLines()) {
	    paintHorizontalLines(g);
	}
	if (table.getShowVerticalLines()) {
	    paintVerticalLines(g);
	}
    }
    
    /*
     * This method paints horizontal lines regardless of whether the
     * table is set to paint one automatically.
     */
    private void paintHorizontalLines(Graphics g) {
	Rectangle r = g.getClipBounds();
	Rectangle rect = r;
	//              int delta = table.getRowHeight() + table.getRowMargin();
	int rowMargin = table.getRowMargin();
	int firstIndex = table.rowAtPoint(new Point(0, r.y));
	int  lastIndex = lastVisibleRow(r);
	//              int y = delta*firstIndex+(delta-1);
	int y = table.getCellRect(firstIndex+1, 0, false).y -1;
	
	for (int index = firstIndex; index <= lastIndex; index ++) {
	    if ((y >= rect.y) && (y <= (rect.y + rect.height))) {
                                                         g.drawLine(rect.x, y, rect.x + rect.width - 1, y);
	    }
	    y += ((MultiheightTable)table).getRowHeight(index+1) + rowMargin;
	}
    }
    
    /*
     * This method paints vertical lines regardless of whether the
     * table is set to paint one automatically.
     */
    private void paintVerticalLines(Graphics g) {
	Rectangle rect = g.getClipBounds();
	int x = 0;
	int count = table.getColumnCount();
	int horizontalSpacing = table.getIntercellSpacing().width;
	for (int index = 0; index <= count; index ++) {
	    if ((x > 0) && (((x-1) >= rect.x) && ((x-1) <= (rect.x +
							    rect.width)))){
		g.drawLine(x - 1, rect.y, x - 1, rect.y + rect.height - 1);
	    }
	    
	    if (index < count)
		x += (table.getColumnModel().getColumn(index)).
		    getWidth() + horizontalSpacing;
	}
    }
    
    private void paintRow(Graphics g, int row) {
	Rectangle rect = g.getClipBounds();
	int column = 0;
	boolean drawn = false;
	int draggedColumnIndex = -1;
	Rectangle draggedCellRect = null;
	Dimension spacing = table.getIntercellSpacing();
	JTableHeader header = table.getTableHeader();
	
	// Set up the cellRect
	Rectangle cellRect = new Rectangle();
	cellRect.height = ((MultiheightTable)table).getRowHeight(row) + spacing.height;
	cellRect.y = table.getCellRect(row,0,false).y;                  //row * cellRect.height;
	
	Enumeration enumeration = table.getColumnModel().getColumns();
	
	// Paint the non-dragged table cells first
	while (enumeration.hasMoreElements()) {
	    TableColumn aColumn = (TableColumn)enumeration.nextElement();
	    
	    cellRect.width = aColumn.getWidth() + spacing.width;
	    if (cellRect.intersects(rect)) {
		drawn = true;
		if ((header == null) || (aColumn != header.getDraggedColumn())) {
		    paintCell(g, cellRect, row, column);
		}
		else {
		    // Paint a gray well in place of the moving column
		    // This would be unnecessary if we drew the grid more cleverly
		    g.setColor(table.getParent().getBackground());
		    g.fillRect(cellRect.x, cellRect.y, cellRect.width, cellRect.height);
		    draggedCellRect = new Rectangle(cellRect);
		    draggedColumnIndex = column;
		}
	    }
	    else {
		if (drawn)
		    // Don't need to iterate through the rest
		    break;
	    }
	    
	    cellRect.x += cellRect.width;
	    column++;
	}
	
	// paint the dragged cell if we are dragging
	if (draggedColumnIndex != -1 && draggedCellRect != null) {
	    draggedCellRect.x += header.getDraggedDistance();
	    
	    // Fill the background
	    g.setColor(table.getBackground());
	    g.fillRect(draggedCellRect.x, draggedCellRect.y,
		       draggedCellRect.width, draggedCellRect.height);
	    
	    // paint grid if necessary.
	    g.setColor(table.getGridColor());
	    int x1 = draggedCellRect.x;
	    int y1 = draggedCellRect.y;
	    int x2 = x1 + draggedCellRect.width - 1;
	    int y2 = y1 + draggedCellRect.height - 1;
	    if (table.getShowVerticalLines()) {
		// Left
		// g.drawLine(x1-1, y1, x1-1, y2);
		// Right
		g.drawLine(x2, y1, x2, y2);
	    }
	    // Bottom
	    if (table.getShowHorizontalLines()) {
		g.drawLine(x1, y2, x2, y2);
	    }
	    
	    // Render the cell value
	    paintCell(g, draggedCellRect, row, draggedColumnIndex);
	}
    }
    
    private void paintCell(Graphics g, Rectangle cellRect, int row, int column) {
	// The cellRect is inset by half the intercellSpacing before painted
	int spacingHeight = table.getRowMargin();
	int spacingWidth = table.getColumnModel().getColumnMargin();
	
	// Round so that when the spacing is 1 the cell does not paint obscure lines.
	cellRect.setBounds(cellRect.x + spacingWidth/2, cellRect.y + spacingHeight/2,
			   cellRect.width - spacingWidth, cellRect.height -
			   spacingHeight);
	
	if (table.isEditing() && table.getEditingRow()==row &&
	    table.getEditingColumn()==column) {
	    Component component = table.getEditorComponent();
	    component.setBounds(cellRect);
	    component.validate();
	}
	else {
	    TableCellRenderer renderer = table.getCellRenderer(row, column);
	    Component component = table.prepareRenderer(renderer, row, column);
						 
	    if (component.getParent() == null) {
		rendererPane.add(component);
	    }
	    rendererPane.paintComponent(g, component, table, cellRect.x, cellRect.y,
					cellRect.width,
					cellRect.height, true);
	}
	// Have to restore the cellRect back to it's orginial size
	cellRect.setBounds(cellRect.x - spacingWidth/2, cellRect.y - spacingHeight/2,
			   cellRect.width + spacingWidth, cellRect.height +
			   spacingHeight);
	
    }
    
    private int lastVisibleRow(Rectangle clip) {
	int lastIndex = table.rowAtPoint(new Point(0, clip.y + clip.height - 1));
	// If the table does not have enough rows to fill the view we'll get -1.
	// Replace this with the index of the last row.
	if (lastIndex == -1) {
	    lastIndex = table.getRowCount() -1;
	}
	return lastIndex;
    }
}
