package org.oboedit.gui.components.ontologyGeneration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupTerm;

/**
 * {@link TermsTableModel} to hold instances of {@link CandidateTerm} for display in {@link JTable}
 * 
 * @author Atif Iqbal, 2008
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class TermsTableModel extends AbstractTableModel
{
	private static final long serialVersionUID = -4583758138652357683L;
	private final List<CandidateTerm> results;
	private String lastRegex = new String();
	private final int numberOfColumnsToShow;
	private final CandidateTermCache clipboard;
	private final boolean isMainTermsTable;
	private boolean onlyShowExistingTerms;
	private List<CandidateTerm> backupResults;
	
	/**
	 * Constructs a {@link TermsTableModel}.
	 * 
	 * @param isMainTermsTable
	 * @param clipboard
	 */
	public TermsTableModel(CandidateTermCache clipboard, int numberOfColumnsToShow, boolean isMainTermsTable)
	{
		this.numberOfColumnsToShow = numberOfColumnsToShow;
		this.clipboard = clipboard;
		this.isMainTermsTable = isMainTermsTable;
		this.results = new ArrayList<CandidateTerm>();
		this.backupResults = new ArrayList<CandidateTerm>();
		this.onlyShowExistingTerms = false;
	}

	/*
	 * IMPLEMENTED METHODS
	 */

	/**
	 * @return
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	public int getColumnCount()
	{
		return numberOfColumnsToShow;
	}

	/**
	 * @return
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	public int getRowCount()
	{
		return getVisibleElements().size();
	}

	/**
	 * @param rowIndex
	 * @param columnIndex
	 * @return
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	public Object getValueAt(int rowIndex, int columnIndex)
	{
		List<CandidateTerm> visibleElements = getVisibleElements();
		if (rowIndex < visibleElements.size()) {
			CandidateTerm term = visibleElements.get(rowIndex);

			if (columnIndex == 0) {
				return term.isTicked();
			}
			else if (columnIndex == 1) {
				StringBuffer buffer = new StringBuffer();
				buffer.append(term.getGeneratedLabel());
				StringBuffer typebuffer = new StringBuffer();
				if (term.getTypes() != null && !isMainTermsTable) {
					for (String type : term.getTypes()) {
						if (!type.equals(CandidateTerm.TYPE_GENERATED) && !type.equals(CandidateTerm.TYPE_LOADED)) {
							typebuffer.append(type);
							typebuffer.append(" ");
						}
					}
				}
				String string = typebuffer.toString().trim();
				if (string.length() > 0) {
					buffer.append(" (");
					buffer.append(string);
					buffer.append(")");
				}
				if (term.getExistingOntologyTerms() != null && term.getExistingOntologyTerms().size() > 0) {
					buffer.append("  [");
					int i = 0;
					for (OBOLookupTerm oboLookupTerm : term.getExistingOntologyTerms()) {
						buffer.append(oboLookupTerm.getOboID());
						i++;
						if (i < term.getExistingOntologyTerms().size()) {
							buffer.append(", ");
						}
					}
					buffer.append("]");
				}
				return buffer.toString();
			}
		}
		return null;
	}

	/*
	 * OVERRIDDEN METHODS
	 */

	@Override
	public String getColumnName(int column)
	{
		if (column == 0) {
			return "";
		}
		else if (column == 1) {
			return "Terms";
		}
		else
			return "";

	}

	@Override
	public Class<?> getColumnClass(int columnIndex)
	{
		if (columnIndex == 0) {
			return Boolean.class;
		}
		return super.getColumnClass(columnIndex);
	}

	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex)
	{
		return columnIndex == 0;
	}

	@Override
	public void setValueAt(Object aValue, int rowIndex, int columnIndex)
	{
		CandidateTerm term = getVisibleElements().get(rowIndex);
		if (columnIndex == 0) {
			if (aValue instanceof Boolean) {
				Boolean isTicked = (Boolean) aValue;
				if (isTicked == true) {
					addTermToClipboard(term);
				}
				else {
					removeTermFromClipboard(term);
				}
				fireTableCellUpdated(rowIndex, columnIndex);
			}
		}
	}

	/*
	 * OTHER PUBLIC METHODS
	 */

	/**
	 * Sets the list of {@link CandidateTerm}.
	 * 
	 * @param list
	 */
	public void setTerms(List<CandidateTerm> list)
	{
		synchronized (this) {
			if (results.isEmpty()) {
				backupResults.addAll(list);
			}
			
			this.results.clear();
			
			for (CandidateTerm term : list) {
				if (!onlyShowExistingTerms || (onlyShowExistingTerms && term.isPresentInOntology())) {
					this.results.add(term);
				}
			}
			//this.results.addAll(list);
			fireTableDataChanged();
		}
	}

	/**
	 * Returns all candidate terms
	 * 
	 * @return unmodifiable list of {@link CandidateTerm}
	 */
	public List<CandidateTerm> getAllTerms()
	{
		return Collections.unmodifiableList(results);
	}

	/**
	 * Returns candidate term displayed at specified position
	 * 
	 * @param rowIndex
	 * @return candidateTerm, the {@link CandidateTerm} at the displayed rowIndex
	 */
	public CandidateTerm getTermAt(int rowIndex)
	{
		CandidateTerm term = getVisibleElements().get(rowIndex);
		return term;
	}

	/**
	 * Update displayed candidate terms. Filter by regex provided.
	 * 
	 * @param regex
	 */
	public void applyFilter(String regex)
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

			Iterator<CandidateTerm> it = results.iterator();
			while (it.hasNext()) {
				CandidateTerm term = it.next();
				String name = term.getGeneratedLabel();
				if (regex.length() == 0) {
					term.setVisible(true);
				}
				else if (p.matcher(name).find()) {
					term.setVisible(true);
				}
				else {
					term.setVisible(false);
				}
			}
			fireTableDataChanged();
		}
	}

	/**
	 * Untick all instances {@link CandidateTerm}
	 */
	public void unTickAll()
	{
		for (CandidateTerm term : results) {
			removeTermFromClipboard(term);
			term.setTicked(false);
		}
	}

	/**
	 * Remove all instances of {@link CandidateTerm} provided in terms from the {@link TableModel}
	 * 
	 * @param terms
	 */
	public void removeAll(List<CandidateTerm> terms)
	{
		synchronized (this) {
			results.removeAll(terms);
			fireTableDataChanged();
		}
	}

	/**
	 * Remove all instances of {@link CandidateTerm} from the {@link TableModel}
	 */
	public void removeAll()
	{
		synchronized (this) {
			results.clear();
			fireTableDataChanged();
		}
	}


	/**
	 * Check if generated term is contained in clipboard. Compare by label in lower case.
	 * 
	 * @param generatedTerm
	 * @return
	 */
	public boolean isInClipboard(CandidateTerm generatedTerm)
	{
		return clipboard.hasCandidateTerm(generatedTerm);
	}
	
	public boolean isPresentInOntology(int rowIndex)
	{
		return this.getTermAt(rowIndex).isPresentInOntology();
	}

	/*
	 * PRIVATE AND PROTECTED METHODS
	 */
	
	/**
	 * Get the list of elements currently visible in the table (all, not only displayed terms)
	 * 
	 * @return {@link List} of {@link CandidateTerm}
	 */
	private List<CandidateTerm> getVisibleElements()
	{
		ArrayList<CandidateTerm> list = new ArrayList<CandidateTerm>();
		for (CandidateTerm term : results) {
			if (term.isVisible()) {
				list.add(term);
			}
		}
		return list;
	}
	
	
	/**
	 * Remove term from clipboard.
	 * 
	 * @param term
	 */
	void removeTermFromClipboard(CandidateTerm term)
	{
		term.setTicked(false);
		clipboard.removeTerm(term);
	}

	/**
	 * Add term to clipboard.
	 * 
	 * @param term
	 */
	void addTermToClipboard(CandidateTerm term)
	{
		term.setTicked(true);
		clipboard.addTerm(term);
	}
	
	public void setOnlyShowExistingTerms(boolean onlyShowExistingTerms) {
		this.onlyShowExistingTerms = onlyShowExistingTerms;
		if (! results.isEmpty()) {
			if (onlyShowExistingTerms) {
				// add all elements to backup list
				backupResults.clear();
				backupResults.addAll(results);
			}
			setTerms(backupResults);
		}
	}
	
}
