package org.oboedit.gui.components.ontologyGeneration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyModelAdapterInterface;

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
	private final OntologyModelAdapterInterface<?,?> adapter;

	/**
	 * Constructs a {@link TermsTableModel}.
	 * 
	 * @param isMainTermsTable
	 * @param clipboard
	 */
	public TermsTableModel(OntologyModelAdapterInterface<?,?> adapter, CandidateTermCache clipboard, int numberOfColumnsToShow, boolean isMainTermsTable)
	{
		this.adapter = adapter;
		this.numberOfColumnsToShow = numberOfColumnsToShow;
		this.clipboard = clipboard;
		this.isMainTermsTable = isMainTermsTable;
		this.results = new ArrayList<CandidateTerm>();
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
				buffer.append(term.getLabel());
				if (term.isInLoadedOntology() && !term.getLabel().equals(term.getGeneratedLabel())) {
					buffer.append(Messages.getString("TermsTableModel.Generated")); //$NON-NLS-1$
					buffer.append(term.getGeneratedLabel());
					buffer.append("')"); //$NON-NLS-1$
			    }
				StringBuffer typebuffer = new StringBuffer();
				if (term.getTypes() != null && !isMainTermsTable) {
					for (String type : term.getTypes()) {
						if (!type.equals(CandidateTerm.TYPE_GENERATED) && !type.equals(CandidateTerm.TYPE_LOADED)) {
							typebuffer.append(type);
							typebuffer.append(" "); //$NON-NLS-1$
						} 
					}
				}
				String string = typebuffer.toString().trim();
				if (string.length() > 0) {
					buffer.append(" ("); //$NON-NLS-1$
					buffer.append(string);
					buffer.append(")"); //$NON-NLS-1$
				}
				if (term.getExistingLookupTerms() != null && term.getExistingLookupTerms().size() > 0) {
					buffer.append("  ["); //$NON-NLS-1$
					int i = 0;
					for (OBOLookupTerm oboLookupTerm : term.getExistingLookupTerms()) {
						buffer.append(oboLookupTerm.getOboID());
						i++;
						if (i < term.getExistingLookupTerms().size()) {
							buffer.append(", "); //$NON-NLS-1$
						}
					}
					buffer.append("]"); //$NON-NLS-1$
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
			return ""; //$NON-NLS-1$
		}
		else if (column == 1) {
			return Messages.getString("TermsTableModel.TermsColumnName"); //$NON-NLS-1$
		}
		else
			return ""; //$NON-NLS-1$

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
			this.results.clear();
			if (list != null)
				this.results.addAll(list);
			updateExistingTermsInExternalModel();
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
				if (onlyShowExistingTerms) {
					if (term.isInLoadedOntology()) {
						list.add(term);
					}
				}
				else {
					list.add(term);
				}
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

	/**
	 * Set if only ontology terms which are loaded in OBOEdit should be displayed
	 * 
	 * @param onlyShowExistingTerms
	 */
	public void setOnlyShowExistingTerms(boolean onlyShowExistingTerms)
	{
		this.onlyShowExistingTerms = onlyShowExistingTerms;
		fireTableDataChanged();
	}

	/**
	 * Update the internal {@link Map} for mappings from {@link CandidateTerm} to ids of terms in the external ontology model.
	 */
	public void updatePresentInOntology()
	{
		updateExistingTermsInExternalModel();
		fireTableDataChanged();
	}
	
	private void updateExistingTermsInExternalModel() {
		// check terms against ontology labels and add to lookup list
		for (CandidateTerm candidateTerm : this.results) {
			candidateTerm.setExistingOntologyClass(adapter.getOntologyClassForCandidateTerm(candidateTerm));
		}		
	}
}
