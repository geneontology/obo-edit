package org.oboedit.gui.components.ontologyGeneration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.text.TableView.TableRow;


/**
 * Model for definitionsTable, holds candidateDefinition objects and provide operation on definitionsTable
 * 
 * @author Atif Iqbal, 2008
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class DefinitionsTableModel extends AbstractTableModel
{

	private static final long serialVersionUID = 3393092862004698314L;
	private final List<CandidateDefinition> definitions;
	private String lastRegex = new String();
//	private static Logger logger = Logger.getLogger(DefinitionsTableModel.class.getSimpleName());

	public DefinitionsTableModel()
	{
		definitions = new ArrayList<CandidateDefinition>();
	}

	/*
	 * IMPLEMENTED METHODS
	 */

	/**
     * Get the current number of {@link TableColumn} in the {@link TableModel}
     *
     * @return number of columns
     * @see javax.swing.table.TableModel#getColumnCount()
     */
    public int getColumnCount()
    {
    	return 4;
    }

	/**
     * Get the current number of {@link TableRow} in the {@link TableModel}
     *
     * @return number of rows
     * @see javax.swing.table.TableModel#getRowCount()
     */
	public int getRowCount()
	{
		return getVisibleElements().size();
	}

	/**
     * Return the value of a specific cell
     *
     * @param rowIndex
     * @param columnIndex
     * @return the Object contained in the cell
     * @see javax.swing.table.TableModel#getValueAt(int, int)
     */
    public Object getValueAt(int rowIndex, int columnIndex)
    {
		List<CandidateDefinition> visibleElements = getVisibleElements();
		if (rowIndex < visibleElements.size()) {
			CandidateDefinition definition = visibleElements.get(rowIndex);
			
			if (columnIndex == 1) {
				return definition.getFavicon();
			}
			if (columnIndex == 2) {
				return definition.getDefinitionHTMLFormatted();
			}
		}
		return null;
    }
    
    /*
     * OVERRIDDEN
     */
    
	@Override
	public String getColumnName(int column)
	{
		if (column == 2) {
			return Messages.getString("DefinitionsTableModel.Definitions"); //$NON-NLS-1$
		}
		else {
			return ""; //$NON-NLS-1$
		}

	}

	@Override
	public Class<?> getColumnClass(int columnIndex)
	{
		if (columnIndex == 0) {
			return JButton.class;
		} else if (columnIndex == 1) {
			return ImageIcon.class;
		}
		else {
			return super.getColumnClass(columnIndex);
		}

	}

	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex)
	{
		return (columnIndex == 0);
	}

	/*
	 * PUBLIC METHODS
	 */

	/**
	 * Removes all definitions from the {@link TableModel}
	 */
	public void removeAllDefinitions()
	{
		definitions.clear();
	}

	/**
	 * Filters the instances of {@link CandidateDefinition} contained in the {@link TableModel} matching the
	 * {@link RegexCreator} specified. Each instance of {@link CandidateDefinition} is set to visible or not.
	 * 
	 * @param regex, the regular expression used for filtering
	 */
	public void applyFilter(String regex)
	{
		if (regex == null || lastRegex.equals(regex))
			return;
		
		lastRegex = regex;
		Pattern p = null;

		try {
			p = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
		} catch (PatternSyntaxException exception) {
			return;
		}

		for (CandidateDefinition definition : definitions) {
			String name = definition.getDefinition();
			String baseURL = definition.getFaviconBaseURL();
			if (regex.length() == 0 || p.matcher(name).find() || (baseURL != null && p.matcher(baseURL).find())) {
				definition.setVisible(true);
			} else {
				definition.setVisible(false);
			}
		}

		fireTableDataChanged();
	}

	/**
	 * Sets the definitions to be displayed in the table
	 *
	 * @param definitions
	 */
	public void setDefinitions(List<CandidateDefinition> definitions)
	{
		this.definitions.clear();
//		for (final CandidateDefinition candidateDefinition : definitions) {
//			candidateDefinition.addPropertyChangeListener(new PropertyChangeListener(){
//				public void propertyChange(PropertyChangeEvent evt)
//				{
//					if (evt.getPropertyName().equals(CandidateDefinition.PROPERTY_DEFINITION)){
//						organizeDefinition(candidateDefinition);
//					}
//					// TODO, add listerners to gear display of defintitions in popup and elsewhere
//				}});};
		this.definitions.addAll(definitions);
		fireTableDataChanged();
	}

	/**
	 * Get the {@link List} of {@link CandidateDefinition} contained in the table model
	 *
	 * @return definitions the list of {@link CandidateDefinition} hold in the {@link TableModel}
	 */
	public List<CandidateDefinition> getDefinitions()
	{
		List<CandidateDefinition> def = new ArrayList<CandidateDefinition>();
		def.addAll(definitions);
		return def;
	}

	/**
	 * Get the {@link CandidateDefinition} at rowIndex
	 *
	 * @param rowIndex the row index for which a {@link CandidateDefinition} is return
	 * @return the {@link CandidateDefinition} a the specified rowIndex
	 */
	public CandidateDefinition getDefinitionAt(int rowIndex)
	{
		return getVisibleElements().get(rowIndex);
	}


	/*
	 * PRIVATE METHDOS
	 */
	
	/**
	 * Get the list of elements currently visible in the table (all, not only displayed terms)
	 * 
	 * @return {@link List} of {@link CandidateDefinition}
	 */
	private List<CandidateDefinition> getVisibleElements()
	{
		List<CandidateDefinition> list = new ArrayList<CandidateDefinition>();
		for (CandidateDefinition definition : getDefinitions()) {
			if (definition.isVisible())
				list.add(definition);
		}
		return Collections.unmodifiableList(list);
	}
	
	
	
//	private void organizeDefinition(CandidateDefinition changedDefinition)
//	{
//		if (changedDefinition != null) {
//			boolean duplicateDefinition = false;
//
//			String defStr = changedDefinition.getDefinition();
//			if (defStr.endsWith(" ...")) {
//				defStr = defStr.substring(0, defStr.length() - 4);
//			}
//
//			for (int i = 0; i < definitions.size(); i++) {
//				CandidateDefinition candDef = definitions.get(i);
//
//				String candDefStr = candDef.getDefinition();
//				if (candDefStr.endsWith(" ...")) {
//					candDefStr = candDefStr.substring(0, candDefStr.length() - 4);
//				}
//
//				if (defStr.equals(candDefStr)) {
//					candDef.addURLs(changedDefinition.getUrls());
//					candDef.addCachedURLs(changedDefinition.getCachedURLs());
//					duplicateDefinition = true;
//				}
//				else if (candDefStr.contains(defStr) || defStr.contains(candDefStr)) {
//					duplicateDefinition = true;
//
//					boolean duplicateAlternativeDefinition = false;
//					if (candDef.getAlternativeDefinitions() != null) {
//						for (CandidateDefinition definition : candDef.getAlternativeDefinitions()) {
//							// Try to find identical alternative
//							// definition.
//							if (definition.getDefinition().equals(changedDefinition.getDefinition())) {
//								duplicateAlternativeDefinition = true;
//
//								definition.addURLs(changedDefinition.getUrls());
//								definition.addCachedURLs(changedDefinition.getCachedURLs());
//							}
//						}
//					}
//
//					// If no identical alternative definition is
//					// found,
//					// add a new alternative definition.
//					if (!duplicateAlternativeDefinition) {
//						// TODO, candidateDefinition.getIndex() is correct at this position
//						final CandidateDefinition alternativeCandidateDefinition = new CandidateDefinition(changedDefinition.getIndex(), changedDefinition.getDefinition(), changedDefinition.getDefinitionHTMLFormatted(), changedDefinition.getUrls(), changedDefinition.getCachedURLs(), changedDefinition.getParentTermCount(), false);
//
//						if (changedDefinition.getDefinition().length() > candDef.getDefinition().length()) {
//							// swap candidateDefinition and
//							// alternative Definition
//							alternativeCandidateDefinition.addAlternativeDefinition(candDef);
//							if (candDef.getAlternativeDefinitions() != null) {
//								for (CandidateDefinition candDefAltDef : candDef.getAlternativeDefinitions()) {
//									alternativeCandidateDefinition.addAlternativeDefinition(candDefAltDef);
//								}
//								candDef.resetAlternativeDefinitions();
//							}
//							candDef.removePropertyChangeListeners();
//							int pos = definitions.indexOf(candDef);
//							definitions.remove(candDef);
//							definitions.add(pos, changedDefinition);
////
////							alternativeCandidateDefinition.addPropertyChangeListener(new PropertyChangeListener()
////							{
////								public void propertyChange(PropertyChangeEvent evt)
////								{
////									if (evt.getPropertyName().equals(CandidateDefinition.PROPERTY_DEFINITION)) {
////										updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable, definitionTable);
////									}
////								}
////							});
//
//						}
//						else {
//							candDef.addAlternativeDefinition(alternativeCandidateDefinition);
//						}
//					}
//				}
//			}
//			// Otherwise, add new definition to list.
//			if (!duplicateDefinition) {
//				final CandidateDefinition alternativeCandidateDefinition = new CandidateDefinition(changedDefinition.getIndex(), changedDefinition.getDefinition(), changedDefinition.getDefinitionHTMLFormatted(), changedDefinition
//						.getUrls(), changedDefinition.getCachedURLs(), changedDefinition.getParentTermCount(), false);
////				alternativeCandidateDefinition.addListener(new UpdateListenerInterface()
////				{
////					public void update()
////					{
////						updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable, definitionTable);
////					}
////				});
//				definitions.add(alternativeCandidateDefinition);
//			}
//		}
//		else {
//			logger.trace("A retrieved definition was null");
//		}
//
//	}

}
