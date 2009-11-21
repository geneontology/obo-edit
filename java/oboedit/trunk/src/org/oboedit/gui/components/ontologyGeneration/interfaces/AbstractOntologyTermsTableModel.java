package org.oboedit.gui.components.ontologyGeneration.interfaces;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.JTable;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

import org.obo.datamodel.LinkedObject;

/**
 * {@link AbstractOntologyTermsTableModel} to hold instances of
 * {@link LinkedObject} for display in {@link JTable}
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public abstract class AbstractOntologyTermsTableModel<T, R> extends AbstractTableModel
{

	public enum columns  { Selector, Predicted, Relation, Term, Comment }

	private static final long serialVersionUID = 4146368118692966602L;

	private String lastRegex = new String();

	private Set<String> ticked = new HashSet<String>();
	private Set<String> visible = new HashSet<String>();

	private Map<String, T> allTermsMap;
	private List<String> allTermsIDList;
	private boolean sortingNeeded = true;
	private List<Set<String>> rankingOrder;
	private Set<String> termsSelected = new HashSet<String>();
	private Set<String> termsSameAsCandidateTerm = new HashSet<String>();
	private Set<String> termsKnownParentsOfCandidateTerm = new HashSet<String>();
	private Set<String> termsPredictedParentsOfCandidateTerm = new HashSet<String>();
	private Set<String> termsFromDefinitions = new HashSet<String>();
	private Set<String> termsSimilarToCandidateTerm = new HashSet<String>();

	private Map<String, Integer> termsRankInDefinitions = new HashMap<String, Integer>();

	private Map<String, String> commentForKnownParentTerms = new HashMap<String, String>();

	private Object[][] order = { { termsSelected, "selected term", null }, //
			{ termsSameAsCandidateTerm, "same as existing term", "identical" }, //
			{ termsKnownParentsOfCandidateTerm, "validated", commentForKnownParentTerms },//
			{ termsPredictedParentsOfCandidateTerm, "predicted", "sub_class_of" }, //
			{ termsFromDefinitions, "predicted", "sub_class_of" }, //
			{ termsSimilarToCandidateTerm, "similar term", null } };

	private boolean showOnlyTicked;

	private R[] relationTypes = getDefaultRelationTypes();

	private Map<T, R> selectedRelationType = new HashMap<T, R>(1);

	/**
	 * Constructs a {@link AbstractOntologyTermsTableModel}.
	 */
	@SuppressWarnings("unchecked")
	public AbstractOntologyTermsTableModel()
	{
		allTermsMap = new HashMap<String, T>();
		allTermsIDList = new ArrayList<String>();
		rankingOrder = new ArrayList<Set<String>>(order.length);
		for (int i = 0; i < order.length; i++) {
			rankingOrder.add((Set<String>) order[i][0]);
		}
	}

	public void addFromCandidateDefinition(String termID, int rank)
	{
		termsFromDefinitions.add(termID);
		addTermsRank(termID, rank);
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void addFromUserDefinedDefinition(String termID, int rank)
	{
		termsFromDefinitions.add(termID);
		addTermsRank(termID, rank);
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/*
	 * IMPLEMENTED METHODS
	 */

	public void addParentsTermsOfExistingCandidateTerm(Map<String, String> parentTerms)
	{
		ticked.removeAll(termsKnownParentsOfCandidateTerm);
		termsKnownParentsOfCandidateTerm.clear();
		if (parentTerms != null) {
			for (Entry<String, String> entry : parentTerms.entrySet()) {
				termsKnownParentsOfCandidateTerm.add(entry.getKey());
				commentForKnownParentTerms.put(entry.getKey(), entry.getValue());
				ticked.add(entry.getKey());
			}
			sortingNeeded = true;
			fireTableDataChanged();
		}
	}

	protected void addPredictedParentsOfCandidateTerm(Collection<T> terms)
	{
		for (T term : terms) {
			termsPredictedParentsOfCandidateTerm.add(getTermId(term));
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void addSameAsCandidateTerm(Collection<T> terms)
	{
		for (T term : terms) {
			termsSameAsCandidateTerm.add(getTermId(term));
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void addSelectedT(Collection<T> terms)
	{
		for (T term : terms) {
			termsSelected.add(getTermId(term));
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void addSimilarToCandidateTerm(Collection<T> terms)
	{
		for (T term : terms) {
			termsSimilarToCandidateTerm.add(getTermId(term));
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	private void addTermsRank(String termID, int rank)
	{
		if (!termsRankInDefinitions.containsKey(termID) || rank < termsRankInDefinitions.get(termID)) {
			termsRankInDefinitions.put(termID, rank);
		}
	}

	/**
	 * Update displayed candidate terms. Filter by regex provided.
	 * 
	 * @param regex
	 */
	public void applyFilter(String regex)
	{
		if (regex != null && !lastRegex.equals(regex)) {
			visible.clear();
			lastRegex = regex;
			Pattern p = null;

			try {
				p = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
			}
			catch (PatternSyntaxException exception) {
				return;
			}

			for (String termID : this.allTermsIDList) {
				T term = allTermsMap.get(termID);
				String name = getTermName(term);
				if (name != null) {
					if (regex.length() == 0) {
						visible.add(getTermId(term));
					}
					else if (p.matcher(name).find()) {
						visible.add(getTermId(term));
					}
					else {
						visible.remove(term);
					}
				}
			}
			fireTableDataChanged();
		}
		else if (regex == null) {
			visible.clear();
			visible.addAll(allTermsIDList);
		}
	}

	/**
	 * Returns index of first occurrence of potential substring in string
	 * 
	 * @param string
	 * @param potentialSubString
	 * @return
	 */
	private int calcFirstIndexOf(String string, String potentialSubString)
	{
		if (string == null || potentialSubString == null) {
			return -1;
		}
		return string.toLowerCase().indexOf(potentialSubString.toLowerCase());
	}

	/*
	 * OTHER PUBLIC METHODS
	 */

	protected void clearPredictedParentsOfCandidateTerm()
	{
		termsPredictedParentsOfCandidateTerm.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void clearSameAsCandidateTerms()
	{
		termsSameAsCandidateTerm.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/*
	 * UPDATE PARENTS
	 */
	protected void clearSelectedTs()
	{
		termsSelected.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void clearSimiliarToCandidateTerm()
	{
		termsSimilarToCandidateTerm.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void clearTermsFromDefinitions()
	{
		termsFromDefinitions.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/**
	 * Returns all terms
	 * 
	 * @return unmodifiable set of {@link T}
	 */
	public List<T> getAllTerms()
	{
		List<T> list = new ArrayList<T>();
		for (T term : list) {
			list.add(term);
		}
		return Collections.unmodifiableList(list);
	}

	@Override
	public Class<?> getColumnClass(int columnIndex)
	{
		if (columnIndex == 0) {
			return Boolean.class;
		}
		return super.getColumnClass(columnIndex);
	}

	/**
	 * @return
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	public int getColumnCount()
	{
		return columns.values().length;
	}

	@Override
	public String getColumnName(int column)
	{
		if (column < columns.values().length) {
			return columns.values()[column].name();
		}
		else {
			return "";
		}

	}

	public abstract R[] getDefaultRelationTypes();

	public abstract R getDefaultRelationType();

	public R[] getRelationTypes()
	{
		return this.relationTypes;
	}

	/**
	 * @return
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	public int getRowCount()
	{
		return this.getVisibleElements().size();
	}

	public int getRowFromTerm(String id)
	{
		return getVisibleElements().indexOf(allTermsMap.get(id));
	}

	/**
	 * Returns candidate term displayed at specified position
	 * 
	 * @param rowIndex
	 * @return T, the {@link T} at the displayed rowIndex
	 */
	public T getTermAt(int rowIndex)
	{
		T term = getVisibleElements().get(rowIndex);
		return term;
	}

	public abstract String getTermId(T term);

	/*
	 * ABSTRACT METHODS
	 */
	public abstract String getTermName(T term);

	/**
	 * Returns ticked candidate terms ids
	 * 
	 * @return tickedTs, the list of ids which are ticked
	 */
	public Set<String> getTickedTerms()
	{
		return Collections.unmodifiableSet(ticked);
	}

	/**
	 * @param rowIndex
	 * @param columnIndex
	 * @return
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@SuppressWarnings("unchecked")
	public Object getValueAt(int rowIndex, int columnIndex)
	{
		List<T> visibleElements = this.getVisibleElements();
		if (rowIndex < visibleElements.size()) {
			T term = visibleElements.get(rowIndex);
			if (columnIndex == columns.Selector.ordinal()) {
				return ticked.contains(getTermId(term));
			}
			else if (columnIndex == columns.Term.ordinal()) {
				StringBuffer buffer = new StringBuffer();
				buffer.append(getTermName(term));
				buffer.append(" (");
				buffer.append(getTermId(term));
				buffer.append(" )");
				return buffer.toString();
			}
			else if (columnIndex == columns.Relation.ordinal()) {
				if (selectedRelationType.containsKey(term)) {
					return selectedRelationType.get(term);
				}
				else {
					return getDefaultRelationType();
				}
			}
			else if (columnIndex == columns.Predicted.ordinal()) {
				for (Object[] objects : order) {
					Set<String> set = (Set<String>) objects[0];
					if (set.contains(getTermId(term))) {
						if (objects[2] instanceof String)
							return objects[2];
						else if (objects[2] == commentForKnownParentTerms) {
							return commentForKnownParentTerms.get(getTermId(term));
						}
					}
				}
				return null;
			}
			else if (columnIndex == columns.Comment.ordinal()) {
				StringBuffer buffer = new StringBuffer();
				for (Object[] objects : order) {
					Set<String> set = (Set<String>) objects[0];
					if (set.contains(getTermId(term))) {
						if (buffer.length() > 0) {
							buffer.append(", ");
						}
						buffer.append(objects[1]);
					}
				}
				if (buffer.length() == 0) {
					buffer.append("existing term");
				}
				return buffer.toString();
			}
		}
		return null;
	}

	private synchronized List<T> getVisibleElements()
	{
		if (sortingNeeded) {
			sortElements();
			applyFilter(null);
			sortingNeeded = false;
		}
		ArrayList<T> list = new ArrayList<T>();
		for (String termID : allTermsIDList) {
			if (showOnlyTicked) {
				if (visible.contains(termID) && ticked.contains(termID)) {
					list.add(allTermsMap.get(termID));
				}
			}
			else {
				if (visible.contains(termID)) {
					list.add(allTermsMap.get(termID));
				}
			}
			if (list.size() == 100) {
				break;
			}

		}
		return Collections.unmodifiableList(list);
	}

	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex)
	{
		if (columnIndex == columns.Selector.ordinal() || columnIndex == columns.Relation.ordinal()) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 * @param term
	 * @return
	 */
	protected boolean isTicked(T term)
	{
		return ticked.contains(term);
	}

	/**
	 * Remove all instances of {@link T} from the {@link TableModel}
	 */
	public void removeAll()
	{
		allTermsMap.clear();
		allTermsIDList.clear();
		ticked.clear();
		visible.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/**
	 * Remove all instances of {@link T} provided in terms from the
	 * {@link TableModel}
	 * 
	 * @param terms
	 */
	public void removeAll(Collection<T> terms)
	{
		for (T term : terms) {
			allTermsIDList.remove(getTermId(term));
			allTermsMap.remove(getTermId(term));
		}
		ticked.removeAll(terms);
		visible.removeAll(terms);
		fireTableDataChanged();
	}

	public void setRelationTypes(R[] relationTypes)
	{
		this.relationTypes = relationTypes;
		// fire tabel changed event
		TableModelEvent e = new TableModelEvent(this, 0, this.getRowCount(), columns.Relation.ordinal(), TableModelEvent.UPDATE);
		fireTableChanged(e);
	}

	/**
	 * Set <code>true</code> to show only ticked {@link T}s
	 * 
	 * @param isTicked
	 */
	public void setShowOnlyTicked(boolean isTicked)
	{
		this.showOnlyTicked = isTicked;
		fireTableDataChanged();
	}

	/**
	 * Sets the collection of {@link T}.
	 * 
	 * @param collection
	 */
	protected void setTerms(Collection<T> collection)
	{
		this.allTermsMap.clear();
		this.allTermsIDList.clear();
		this.ticked.clear();
		this.visible.clear();

		for (T term : collection) {
			this.allTermsMap.put(getTermId(term), term);
			this.allTermsIDList.add(getTermId(term));
		}
		this.visible.addAll(this.allTermsIDList);
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/**
	 * @param term
	 * @param isTicked
	 */
	protected void setTicked(T term, boolean isTicked)
	{
		if (isTicked == false) {
			ticked.remove(getTermId(term));
		}
		else {
			ticked.add(getTermId(term));
		}
	}

	public void setTickedTerms(Set<String> ticked)
	{
		this.ticked.clear();
		this.ticked.addAll(ticked);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void setValueAt(Object aValue, int rowIndex, int columnIndex)
	{
		T term = this.getVisibleElements().get(rowIndex);
		if (columnIndex == columns.Selector.ordinal()) {
			if (aValue instanceof Boolean) {
				Boolean isTicked = (Boolean) aValue;
				setTicked(term, isTicked);
				fireTableCellUpdated(rowIndex, columnIndex);
			}
		}
		else if (columnIndex == columns.Relation.ordinal()) {
			selectedRelationType.put(this.getTermAt(rowIndex), (R) aValue);
		}
	}

	/*
	 * PRIVATE METHODS
	 */
	private void sortElements()
	{
		Collections.sort(allTermsIDList, new AddToOntologyTComparator(rankingOrder));
	}

	public void updateSimilarTerms(Collection<String> labels, List<String> idsOfSimilarTerms)
	{
		for (String id : idsOfSimilarTerms) {
			T term = allTermsMap.get(id);
			String termLabel = getTermName(term);
			for (String label : labels) {
				if (termLabel.equalsIgnoreCase(label)) {
					addSameAsCandidateTerm(Collections.singleton(term));
				}
				else if (columns.Selector.ordinal() < calcFirstIndexOf(termLabel, label) || columns.Selector.ordinal() < calcFirstIndexOf(label, termLabel)) {
					addSimilarToCandidateTerm(Collections.singleton(term));
				}
			}
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/**
	 * Updating linked object in the table (usually in cases where those
	 * {@link T}s have been changed)
	 * 
	 * @param terms
	 *            to update
	 */
	public void updateTerms(List<T> terms)
	{
		removeAll(terms);
		for (T term : terms) {
			allTermsIDList.add(getTermId(term));
			allTermsMap.put(getTermId(term), term);
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/**
	 * Comparator for Ts displayed in step 3. Add to Ontology
	 */
	private class AddToOntologyTComparator implements Comparator<String>
	{
		private List<Set<String>> idSetsOrdered;

		protected AddToOntologyTComparator(List<Set<String>> idSetsOrdered)
		{
			this.idSetsOrdered = idSetsOrdered;
		}

		/**
		 * Compares T objects for display
		 * 
		 * @param o1
		 * @param o2
		 * @return
		 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
		 */
		public int compare(String o1, String o2)
		{
			// test for null
			if (o1 == null && o2 == null) {
				return 0;
			}
			else if (o1 == null && o2 != null) {
				return 1;
			}
			else if (o1 != null && o2 == null) {
				return -1;
			}

			// test identity
			if (o1.equals(o2)) {
				return 0;
			}

			// test rankingOrder
			int i = 0;
			int cnt1 = 100;
			for (Set<String> set : idSetsOrdered) {
				if (set.contains(o1)) {
					cnt1 = i;
					break;
				}
				i++;
			}
			int j = 0;
			int cnt2 = 100;
			for (Set<String> set : idSetsOrdered) {
				if (set.contains(o2)) {
					cnt2 = j;
					break;
				}
				j++;
			}

			if (cnt1 < cnt2) {
				return -1;
			}
			if (cnt1 > cnt2) {
				return 1;
			}

			// then compare by rank
			Integer integer = termsRankInDefinitions.get(o1);
			Integer integer2 = termsRankInDefinitions.get(o2);
			if (integer != null && integer2 != null) {
				if (!integer.equals(integer2)) {
					return integer.compareTo(integer2);
				}
			}

			// compare by label length
			String name = getTermName(allTermsMap.get(o1));
			String name2 = getTermName(allTermsMap.get(o2));

			if (name == null && name2 != null) {
				return 1;
			}
			else if (name != null && name2 == null) {
				return -1;
			}
			else if (name == null && name2 == null) {
				return 0;
			}
			return Integer.valueOf(name.length()).compareTo(Integer.valueOf(name2.length()));
		}
	}

	public R getRelationType(String parentId)
	{
		T term = allTermsMap.get(parentId);
		R relationType = selectedRelationType.get(term);
		if (relationType == null) {
			return getDefaultRelationType();
		}
		else {
			return relationType;
		}
	}

	public T getTermById(String parentId)
	{
		return allTermsMap.get(parentId);
	}
}
