package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.JTable;

import org.obo.datamodel.LinkedObject;

/**
 * {@link JTable} to display {@link List} of {@link LinkedObject}.
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class OBOTermsTable extends JTable
{
	private static final long serialVersionUID = -5517462579527283469L;
	private int minScrollableViewPortHeight;
	private int maxScrollableViewPortHeight;
//	private JComboBox relationTypeComboBox;

	/**
	 * Constructs a {@link OBOTermsTable}
	 */
	public OBOTermsTable()
	{
		super(new OBOTermsTableModel());
		setGridColor(Color.LIGHT_GRAY);
		setRowHeight(getRowHeight() + 4);
		getColumnModel().getColumn(0).setMaxWidth(50);
		getColumnModel().getColumn(0).setResizable(false);
		getColumnModel().getColumn(1).setMinWidth(250);
		getColumnModel().getColumn(2).setMinWidth(80);
		getColumnModel().getColumn(2).setMaxWidth(80);
		getColumnModel().getColumn(2).setResizable(false);
		// TODO Add a editable combobox with known relationship types and select the one predicted/known 
		//		relationTypeComboBox = new JComboBox();
		//		relationTypeComboBox.addItem("is_a"); // TODO
		//		relationTypeComboBox.addItem("part_of");
		//		relationTypeComboBox.addItem("have_fun");
		//		getColumnModel().getColumn(2).setCellEditor(new DefaultCellEditor(relationTypeComboBox));
		
		getColumnModel().getColumn(3).setMinWidth(130);
		getColumnModel().getColumn(3).setMaxWidth(130);
		getColumnModel().getColumn(3).setResizable(false);
		getColumnModel().getSelectionModel().addListSelectionListener(this);
	}

	/**
	 * Set the {@link List} of {@link LinkedObject} to be contained in the {@link TermsTable} and resize table if
	 * necessary.
	 * 
	 * @param results
	 */
	public void setTerms(Collection<LinkedObject> results)
	{
		getModel().setTerms(results);
		JTableHelper.recalculateScrollableViewportSize(this, minScrollableViewPortHeight, maxScrollableViewPortHeight);
	}

	/**
	 * Remove all instances of {@link LinkedObject} from the {@link TermsTable} and resize table if necessary.
	 * 
	 * @param terms
	 */
	public void removeTerms(Collection<LinkedObject> terms)
	{
		getModel().removeAll(terms);
		JTableHelper.recalculateScrollableViewportSize(this, minScrollableViewPortHeight, maxScrollableViewPortHeight);
	}

	/**
	 * Remove all instances of {@link LinkedObject} from the {@link TermsTable} and resize table if necessary.
	 * 
	 * @param terms
	 */
	public void removeAllTerms()
	{
		getModel().removeAll();
		JTableHelper.recalculateScrollableViewportSize(this, minScrollableViewPortHeight, maxScrollableViewPortHeight);
	}

	/**
	 * Set the minimal height of the visible area for the {@link TermsTable}
	 * 
	 * @param minHeight
	 */
	public void setMinimumPreferedeScrollableViewportHeight(int minHeight)
	{
		this.minScrollableViewPortHeight = minHeight;
	}

	/**
	 * Set the maximal height of the visible area for the {@link TermsTable}
	 * 
	 * @param maxHeight
	 */
	public void setMaximumPreferedeScrollableViewportHeight(int maxHeight)
	{
		this.maxScrollableViewPortHeight = maxHeight;
	}

	/*
	 * OVERRIDDEN METHODS
	 */

	@Override
	public OBOTermsTableModel getModel()
	{
		return (OBOTermsTableModel) super.getModel();
	}

	@Override
	public String getToolTipText(MouseEvent e)
	{
		String tip = null;
		// java.awt.Point p = e.getPoint();
		// int rowIndex = rowAtPoint(p);
		// int colIndex = columnAtPoint(p);
		// int realColumnIndex = convertColumnIndexToModel(colIndex);
		//
		// if (realColumnIndex == 3) {
		// setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		// tip = "Click to see GoPubMed(www.gopubmed.org) resutls for this term";
		//
		// }
		// else if (realColumnIndex == 1) {
		// setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		// tip = getModel().getValueAt(rowIndex, colIndex).toString();
		//
		// }
		// else if (realColumnIndex == 2) {
		// setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		// tip = "Click to get definitions for this term";
		// }
		// else {
		// /*
		// * You can omit this part if you know you don't have any renderer that supply their own tool tips.
		// */
		// setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		// tip = super.getToolTipText(e);
		// }

		return tip;
	}

	private String lastRegex = new String();

	/**
	 * Update displayed candidate terms. Filter by regex provided.
	 * 
	 * @param regex
	 */
	public void findTerm(String regex)
	{
		if (regex != null && !lastRegex.equals(regex)) {
			lastRegex = regex;
			Pattern p = null;

			try {
				p = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
			}
			catch (PatternSyntaxException exception) {
				return;
			}
			Iterator<LinkedObject> it = getModel().getAllTerms().iterator();
			int index = 0;
			while (it.hasNext()) {
				LinkedObject term = it.next();
				String name = term.getName();
				if (p.matcher(name).find()) {
					getSelectionModel().setSelectionInterval(index, index);
					JTableHelper.scrollToCenter(this, index, 2);
					return;
				}
				index++;
			}
		}
	}
}
