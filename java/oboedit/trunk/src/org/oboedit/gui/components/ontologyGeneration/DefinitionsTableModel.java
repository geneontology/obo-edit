package org.oboedit.gui.components.ontologyGeneration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.text.TableView.TableRow;

import org.jdesktop.swingx.PatternModel.RegexCreator;

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
    	return 3;
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
			if (columnIndex == 0) {
				return definition.isTicked();
			}
			else if (columnIndex == 1) {
				return definition.getDefinition();
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
		if (column == 1) {
			return "Definitions";
		}
		else {
			return "";
		}

	}

	@Override
	public Class<?> getColumnClass(int columnIndex)
	{

		if (columnIndex == 0) {
			return Boolean.class;
		}
		else
			return super.getColumnClass(columnIndex);

	}

	@Override
	public void setValueAt(Object aValue, int rowIndex, int columnIndex)
	{
		CandidateDefinition def = getVisibleElements().get(rowIndex);
		if (((Boolean) aValue) == true) {
			def.setTicked(true);
		}
		else {
			def.setTicked(false);
		}
	}

	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex)
	{
		return columnIndex == 0;
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

		if (regex != null && !lastRegex.equals(regex)) {
			lastRegex = regex;
			Pattern p = null;

			try {
				p = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
			}
			catch (PatternSyntaxException exception) {
				return;
			}

			Iterator<CandidateDefinition> it = definitions.iterator();
			while (it.hasNext()) {
				CandidateDefinition definition = it.next();
				String name = definition.getDefinition();
				if (regex.length() == 0) {
					definition.setVisible(true);
				}
				else if (p.matcher(name).find()) {
					definition.setVisible(true);
				}
				else {
					definition.setVisible(false);
				}
			}
			fireTableDataChanged();
		}
	}

	/**
	 * Sets the definitions to be displayed in the table
	 *
	 * @param definitions
	 */
	public void setDefinitions(List<CandidateDefinition> definitions)
	{
		this.definitions.clear();
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

	/**
	 * Select the {@link CandidateDefinition}
	 *
	 * @param definition the {@link CandidateDefinition} to be selected
	 */
	public void selectDefinition(CandidateDefinition definition)
	{
		if (definitions.contains(definition)) {
			definition.setTicked(true);
		}
		// fireTableDataChanged();
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
		for (CandidateDefinition definition : definitions) {
			if (definition.isVisible())
				list.add(definition);
		}
		return Collections.unmodifiableList(list);
	}

    
    

}
