/**
 * MultiheightTable
 *
 * A JTable implementation allowing rows to have varying heights. Based on
 * an example written by Zafir Anjum at
 * http://codeguru.earthweb.com/java/articles/121.shtml
 */

package org.bbop.swing;

import java.util.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;

import org.bbop.swing.multiheighttable.*;

import org.apache.log4j.*;

public class MultiheightTable extends JTable
	{

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultiheightTable.class);
    /**
	 * 
	 */
	private static final long serialVersionUID = 1147784799846275231L;

	public MultiheightTable() {
	this(null, null, null);
    }
    
    public MultiheightTable(TableModel dm) {
	this(dm, null, null);
    }
    
    public MultiheightTable(TableModel dm, TableColumnModel cm) {
	this(dm, cm, null);
    }
    
    public MultiheightTable(TableModel dm, TableColumnModel cm, ListSelectionModel sm) {
	super(dm,cm,sm);
	setUI( new MultiheightTableUI() );
    }

    public MultiheightTable(int numRows, int numColumns) {
	this(new DefaultTableModel(numRows, numColumns));
    }
    
    public MultiheightTable(final Vector rowData, final Vector columnNames) {
	super( rowData, columnNames );
	setUI( new MultiheightTableUI() );
    }
    
    public MultiheightTable(final Object[][] rowData, final Object[] columnNames) {
	super( rowData, columnNames );
	setUI( new MultiheightTableUI() );
    }
    
    public int rowAtPoint(Point point) {
	int y = point.y;
	if( y < 0 ) return -1;
	
	int rowSpacing = getIntercellSpacing().height;
	int rowCount = getRowCount();
	int rowHeight = 0;
	for( int i = 0; i < rowCount; i++ )
	    {
		rowHeight += getRowHeight(i) + rowSpacing;
		if( y < rowHeight )
		    return i;
	    }
	return -1;
    }
    
    public Rectangle getCellRect(int row, int column, boolean includeSpacing) {
	int index = 0;
	Rectangle cellFrame;
	int columnMargin = getColumnModel().getColumnMargin();
	Enumeration enumeration = getColumnModel().getColumns();
	TableColumn aColumn;
	
	cellFrame = new Rectangle();
	cellFrame.height = getRowHeight(row) + rowMargin;
	int rowSpacing = getIntercellSpacing().height;
	int y = 0;
	for( int i = 0; i < row; i++ )
	    {
		y += getRowHeight(i) + rowSpacing;
	    }
	cellFrame.y = y;
	
	while (enumeration.hasMoreElements()) {
	    aColumn = (TableColumn)enumeration.nextElement();
	    cellFrame.width = aColumn.getWidth() + columnMargin;
	    
	    if (index == column)
		break;
	    
	    cellFrame.x += cellFrame.width;
	    index++;
	}
	
	if (!includeSpacing) {
	    Dimension spacing = getIntercellSpacing();
	    // This is not the same as grow(), it rounds differently.
	    cellFrame.setBounds(cellFrame.x +         spacing.width/2,
				cellFrame.y +     spacing.height/2,
				cellFrame.width -  spacing.width,
				cellFrame.height - spacing.height);
	}
	return cellFrame;
    }
    
    public void tableChanged(TableModelEvent e) {
	if (e == null || e.getFirstRow() == TableModelEvent.HEADER_ROW) {
	    // The whole thing changed
	    clearSelection();
	    
	    if (getAutoCreateColumnsFromModel())
		createDefaultColumnsFromModel();
	    
	    resizeAndRepaint();
	    if (tableHeader != null) {
		tableHeader.resizeAndRepaint();
	    }
	    return;
	}
	
	if (e.getType() == TableModelEvent.INSERT) {
	    tableRowsInserted(e);
	    return;
	}
	
	if (e.getType() == TableModelEvent.DELETE) {
	    tableRowsDeleted(e);
	    return;
	}
	
	int modelColumn = e.getColumn();
	int start = e.getFirstRow();
	int end = e.getLastRow();
	
	if (start == TableModelEvent.HEADER_ROW) {
	    start = 0;
	    end = Integer.MAX_VALUE;
	}
	
	//              int rowHeight = getRowHeight() + rowMargin;
	Rectangle dirtyRegion;
	if (modelColumn == TableModelEvent.ALL_COLUMNS) {
	    // 1 or more rows changed
	    //                      dirtyRegion = new Rectangle(0, start * rowHeight,
	    dirtyRegion = new Rectangle(0, getCellRect(start,0,false).y ,
					
					getColumnModel().getTotalColumnWidth(), 0);
	}
	else {
	    // A cell or column of cells has changed.
	    // Unlike the rest of the methods in the JTable, the TableModelEvent
	    // uses the co-ordinate system of the model instead of the view.
	    // This is the only place in the JTable where this "reverse mapping"
	    // is used.
	    int column = convertColumnIndexToView(modelColumn);
	    dirtyRegion = getCellRect(start, column, false);
	}
	
	// Now adjust the height of the dirty region according to the value of "end".
	// Check for Integer.MAX_VALUE as this will cause an overflow.
	if (end != Integer.MAX_VALUE) {
	    //dirtyRegion.height = (end-start+1)*rowHeight;
	    dirtyRegion.height = getCellRect(end+1,0,false).y - dirtyRegion.y;
	    repaint(dirtyRegion.x, dirtyRegion.y, dirtyRegion.width, dirtyRegion.height);
	}
	// In fact, if the end is Integer.MAX_VALUE we need to revalidate anyway
	// because the scrollbar may need repainting.
	else {
	    resizeAndRepaint();
	}
    }
    
    private void tableRowsInserted(TableModelEvent e) {
	int start = e.getFirstRow();
	int end = e.getLastRow();
	if (start < 0)
	    start = 0;
	
	// Move down row height info - for rows below the first inserted row
	int rowCount = getRowCount();
	int rowsInserted = end - start + 1;
	for( int r = start; r < rowCount; r++ )
	    {
		Integer height = (Integer)rowHeights.get( new Integer(r) );
		if( height == null ) continue;
		rowHeights.put( new Integer( r+rowsInserted ), height  );
	    }
	
	// 1 or more rows added, so we have to repaint from the first
	// new row to the end of the table.  (Everything shifts down)
	//              int rowHeight = getRowHeight() + rowMargin;
	Rectangle drawRect = new Rectangle(0, getCellRect(start,0,false).y ,
					   
					   getColumnModel().getTotalColumnWidth(), 0);
	// (getRowCount()-start) * rowHeight);
	drawRect.height = getCellRect(rowCount,0,false).y - drawRect.y;
	
	// Adjust the selection to account for the new rows
	if (selectionModel != null) {
	    if (end < 0)
		end = getRowCount()-1;
	    int length = end - start + 1;
	    
	    selectionModel.insertIndexInterval(start, length, true);
	}
	revalidate();
	// PENDING(philip) Find a way to stop revalidate calling repaint
	// repaint(drawRect);
    }
    
    /*
     * Invoked when rows have been removed from the table.
     *
     * @param e the TableModelEvent encapsulating the deletion
     */
    private void tableRowsDeleted(TableModelEvent e) {
	int start = e.getFirstRow();
	int end = e.getLastRow();
	if (start < 0)
	    start = 0;
	
	int deletedCount = end - start + 1;
	int previousRowCount = getRowCount() + deletedCount;
	
	// Remove any height information for deleted rows
	for( int i = start; i <= end; i++ )
	    resetRowHeight(i);
	// Move up row height info - for rows below the last deleted row
	for( int r = end + 1; r < previousRowCount; r++ )
	    {
		Integer height = (Integer)rowHeights.get( new Integer(r) );
		if( height == null ) continue;
		rowHeights.put( new Integer( r-deletedCount ), height  );
	    }
	
	// 1 or more rows added, so we have to repaint from the first
	// new row to the end of the table.  (Everything shifts up)
	//              int rowHeight = getRowHeight() + rowMargin;
	Rectangle drawRect = new Rectangle(0, getCellRect(start,0,false).y ,				       getColumnModel().getTotalColumnWidth(),0);
	// (previousRowCount - start) * rowHeight);
	drawRect.height = getCellRect(previousRowCount,0,false).y - drawRect.y;
	
	// Adjust the selection to account for the new rows
	if (selectionModel != null) {
	    if (end < 0)
		end = getRowCount()-1;
	    
	    selectionModel.removeIndexInterval(start, end);
	}
	revalidate();
	// PENDING(philip) Find a way to stop revalidate calling repaint
	// repaint(drawRect);
    }
    
    public int getRowHeight( int row )
    {
	Object o = rowHeights.get( new Integer(row) );
	if( o == null ) return getRowHeight(false);
	return ((Integer)o).intValue();
    }

    public int getRowHeight() {
	double totalHeight = 0.0;
	double rows;
	for(rows = 0; rows < dataModel.getRowCount(); rows++) {
	    totalHeight += getRowHeight((int) rows);
	}
	return (int) Math.ceil(totalHeight/rows);
    }

    public int getRowHeight(boolean getAverage) {
	if (getAverage)
	    return getRowHeight();
	else
	    return super.getRowHeight();
    }
    
    public void setRowHeight( int row, int height )
    {
	rowHeights.put( new Integer( row ), new Integer( height ) );
	revalidate();
    }
    
    public void resetRowHeight( int row )
    {
	rowHeights.remove( new Integer( row ) );
	revalidate();
    }
    
    public void resetRowHeight()
    {
	rowHeights.clear();
	revalidate();
    }

    protected int getPreferredRowHeight(int row) {
	int maxSize = getRowHeight(false);
	for(int j=0; j < dataModel.getColumnCount(); j++) {
	    Component c = prepareRenderer(getCellRenderer(row, j),
					  row,j);
	    int preferredHeight = (int) c.
		getPreferredSize().getHeight();
	    if (preferredHeight > maxSize)
		maxSize = preferredHeight;
	}
	return maxSize;
    }

    public void autosizeRows() {
	for(int i=0; i < dataModel.getRowCount(); i++) {
	    setRowHeight(i,getPreferredRowHeight(i));
	}
    }
    
    protected HashMap rowHeights = new HashMap();

    public static void main(String [] args) throws Exception {
	JPanel thing = new JPanel();
	thing.add(new JButton("Hi"));
	thing.add(new JLabel("there"));

	Vector rows = new Vector();
	Vector rowA = new Vector();
	Vector rowB = new Vector();
	Vector rowC = new Vector();
	rowA.addElement("Twas brillig\nand the slithy toves\ndid gyre and gimble\nin the wabe");
	rowA.addElement("I once knew a man from Nantucket");
	rowB.addElement("Hey baby");
	rowB.addElement("Got a quarter?");
	rowC.addElement("Eat beets");
	rowC.addElement(thing);
	rows.addElement(rowA);
	rows.addElement(rowB);
	rows.addElement(rowC);
	rows.addElement(rowA.clone());
	rows.addElement(rowB.clone());
	rows.addElement(rowC.clone());
	rows.addElement(rowA.clone());
	rows.addElement(rowB.clone());
	rows.addElement(rowA.clone());
	rows.addElement(rowB.clone());
	rows.addElement(rowC.clone());
	rows.addElement(rowA.clone());
	rows.addElement(rowB.clone());
	rows.addElement(rowA.clone());
	rows.addElement(rowB.clone());
	rows.addElement(rowC.clone());
	rows.addElement(rowA.clone());
	rows.addElement(rowB.clone());
	Vector rowNames = new Vector();
	rowNames.addElement("left");
	rowNames.addElement("right");
	MultiheightTable table = new MultiheightTable(new DefaultTableModel(rows, rowNames));
	table.setDefaultRenderer(Class.forName("java.awt.Component"), new ComponentCellRenderer());
	table.setDefaultRenderer(Class.forName("java.lang.Object"), new TextAreaTableCellRenderer());
	table.setCellEditor(null);
	table.setCellSelectionEnabled(false);
	table.setRowSelectionAllowed(false);
	table.setColumnSelectionAllowed(false);
	//	table.getTableHeader().setReorderingAllowed(false);
	table.autosizeRows();
	JOptionPane.showMessageDialog(null, new JScrollPane(table));
    }

}  // End of Class MultiheightTable

class ComponentCellRenderer implements TableCellRenderer {
    Hashtable rowHeights = new Hashtable();

    public Component getTableCellRendererComponent(JTable table,
						   Object value,
						   boolean isSelected,
						   boolean hasFocus,
						   int row,
						   int column) {
	return (Component) value;
    }
}

class TextAreaTableCellRenderer implements TableCellRenderer {
    public Component getTableCellRendererComponent(JTable table,
						   Object value,
						   boolean isSelected,
						   boolean hasFocus,
						   int row,
						   int column) {
	JTextArea out = new JTextArea();
	if (!isSelected) {
	    out.setForeground(Color.blue);
	    out.setBackground(Color.white);
	} else {
	    out.setForeground(Color.black);
	    out.setBackground(Color.blue);
	}
	out.setText(value.toString());
	return out;
    }
}
