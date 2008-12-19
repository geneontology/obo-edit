package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.event.MouseEvent;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.JTable;

/**
 * {@link JTable} to display {@link List} of {@link CandidateTerm}.
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class TermsTable extends JTable
{
	private static final long serialVersionUID = 1184929335454420009L;
	private String lastRegex = new String();
	private int lastVisibleRow = -1;
	private int firstVisibleRow = -1;

	/**
	 * Constructs a {@link TermsTable}
	 * 
	 * @param numberOfColumnsToShow
	 * @param clipboard
	 */
	public TermsTable(CandidateTermCache clipboard, int numberOfColumnsToShow, boolean isMainTermsTable)
	{
		super(new TermsTableModel(clipboard, numberOfColumnsToShow, isMainTermsTable));
		setGridColor(Color.LIGHT_GRAY);
		setRowHeight(getRowHeight() + 4);
		getColumnModel().getSelectionModel().addListSelectionListener(this);
	}

	/**
	 * Set the {@link List} of {@link CandidateTerm} to be contained in the {@link TermsTable} and resize table if
	 * necessary.
	 * 
	 * @param results
	 */
	public void setTerms(List<CandidateTerm> results)
	{
		getModel().setTerms(results);
		setCurrentFirstVisibleRow(-1);
		setCurrentLastVisibleRow(-1);
	}

	/**
	 * Remove all instances of {@link CandidateTerm} from the {@link TermsTable} and resize table if necessary.
	 * 
	 * @param terms
	 */
	public void removeTerms(List<CandidateTerm> terms)
	{
		getModel().removeAll(terms);
		setCurrentFirstVisibleRow(-1);
		setCurrentLastVisibleRow(-1);
	}

	/**
	 * Remove all instances of {@link CandidateTerm} from the {@link TermsTable} and resize table if necessary.
	 * 
	 * @param terms
	 */
	public void removeAllTerms()
	{
		getModel().removeAll();
		setCurrentFirstVisibleRow(-1);
		setCurrentLastVisibleRow(-1);
	}

	/*
	 * OVERRIDDEN METHODS
	 */

	@Override
	public TermsTableModel getModel()
	{
		return (TermsTableModel) super.getModel();
	}

	@Override
	public String getToolTipText(MouseEvent e)
	{
		String tip = null;
		java.awt.Point p = e.getPoint();
		int rowIndex = rowAtPoint(p);
		int colIndex = columnAtPoint(p);
		int realColumnIndex = convertColumnIndexToModel(colIndex);

		if (realColumnIndex == 3) {
			setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			tip = "Click to see GoPubMed(www.gopubmed.org) resutls for this term";

		}
		else if (realColumnIndex == 1) {
			tip = getModel().getValueAt(rowIndex, colIndex).toString();
		}
		else if (realColumnIndex == 2) {
			setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			tip = "Click to get definitions for this term";
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
			Iterator<CandidateTerm> it = getModel().getAllTerms().iterator();
			int index = 0;
			while (it.hasNext()) {
				CandidateTerm term = it.next();
				String name = term.getGeneratedLabel();
				if (p.matcher(name).find()) {
					getSelectionModel().setSelectionInterval(index, index);
					JTableHelper.scrollToCenter(this, index, 2);
					return;
				}
				index++;
			}
		}
	}

	/**
	 * @return
	 */
	public int getCurrentLastVisibleRow()
	{
		return lastVisibleRow;
	}

	/**
	 * @return
	 */
	public int getCurrentFirstVisibleRow()
	{
		return firstVisibleRow;
	}

	/**
	 * @param firstVisibleRow
	 */
	public void setCurrentFirstVisibleRow(int firstVisibleRow)
	{
		this.firstVisibleRow = firstVisibleRow;
	}

	/**
	 * @param lastVisibleRow
	 */
	public void setCurrentLastVisibleRow(int lastVisibleRow)
	{
		this.lastVisibleRow = lastVisibleRow;

	}

}
