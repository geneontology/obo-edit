package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.util.List;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;

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

	private ButtonRenderer buttonRenderer;
	private DefinitionsTableCellRenderer definitionsTableCellRenderer;
	
	/**
	 * Constructs a {@link DefinitionsTable}
	 */
	public DefinitionsTable()
	{
		super(new DefinitionsTableModel());
		setGridColor(Color.LIGHT_GRAY);
		setRowHeight(getRowHeight() + 4);
		setAlignmentY(TOP_ALIGNMENT);
		setAlignmentX(LEFT_ALIGNMENT);
		getColumnModel().getColumn(0).setMaxWidth(50);
		getColumnModel().getColumn(0).setResizable(false);
		getColumnModel().getColumn(2).setMaxWidth(30);
		getColumnModel().getColumn(0).setCellEditor(new ButtonEditor(new JCheckBox()));
		setPreferredScrollableViewportSize(new Dimension(minScrollableViewPortHeight, maxScrollableViewPortHeight));
		tableHeader.setReorderingAllowed(false);
		
		buttonRenderer = new ButtonRenderer();
		definitionsTableCellRenderer = new DefinitionsTableCellRenderer();
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

//	@Override
//	public String getToolTipText(MouseEvent e)
//	{
//		String tip = null;
//		StringBuffer toolTipBuffer = new StringBuffer();
//
//		java.awt.Point p = e.getPoint();
//		int rowIndex = rowAtPoint(p);
//		int colIndex = columnAtPoint(p);
//		int realColumnIndex = convertColumnIndexToModel(colIndex);
//
//		if (realColumnIndex == 2) {
//			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
//			tip = "Click for more information on sources of the definitions";
//		}
//		else {
//			/*
//			 * You can omit this part if you know you don't have any renderer that supply their own tool tips.
//			 */
//			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
//			tip = super.getToolTipText(e);
//		}
//		return tip;
//	}
	
	@Override
	public TableCellRenderer getCellRenderer(int row, int column)
	{
		if (column == 0) {
			return buttonRenderer;
		}
		else if (column == 1) {
			return definitionsTableCellRenderer;
		}
		else {
			return super.getCellRenderer(row, column);
		}
	}
	
	private String setToolTip(String htmlDefinition)
	{
		final int MAX_LINE_LENGTH = 40;
		String toolTipText = "";
		
		int lineCount = 0;
		// counts the number of characters outside HTML tags
		int numberOfCharsRead = 0;
		boolean openedTag = false;
		for (int i = 0; i < htmlDefinition.length(); i++) {
			if (!openedTag) {
				if (htmlDefinition.charAt(i) == '<') {
					openedTag = true;
				} 
				else {
					numberOfCharsRead++;
				}
			} else {
				if (htmlDefinition.charAt(i) == '>') {
					openedTag = false;
				}
			}
			// if the maximum line length is reached, start a new line by appending "<br/>"
			if ((numberOfCharsRead % MAX_LINE_LENGTH) == 0 && !openedTag && htmlDefinition.charAt(i) != '>') {
				if (numberOfCharsRead > 0) {
					int nextSpace = htmlDefinition.indexOf(" ", i);
					if (nextSpace != -1) {
						toolTipText += htmlDefinition.substring(i, nextSpace);
						i = nextSpace;
						toolTipText += "<br/>";
					}
					else {
						toolTipText += htmlDefinition.substring(i);
					}
					
				}
				lineCount++;
			}
			
			toolTipText += htmlDefinition.charAt(i);
		}
		
		return toolTipText;
	}


	private class DefinitionsTableCellRenderer extends DefaultTableCellRenderer {

		private static final long serialVersionUID = -8655538136483302823L;

		@Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
			    boolean hasFocus, int row, int column)
		{
			// Check if the row belongs to a definition
			JLabel comp = (JLabel) super.getTableCellRendererComponent(table, value, isSelected,
			    hasFocus, row, column);
			
			String htmlDefinition = (String)getModel().getValueAt(row, column);
			
			comp.setText(htmlDefinition);
			
			// add multi-line tooltip displaying the full HTML-formatted definition.
			comp.setToolTipText(setToolTip(htmlDefinition));
			
			return comp;
		}
	}
}
