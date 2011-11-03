package org.oboedit.gui.components.ontologyGeneration.interfaces;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.JTable;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

import org.oboedit.gui.components.ontologyGeneration.Messages;

/**
 * {@link AbstractOntologyTermsTableModel} to hold instances of T for display in
 * {@link JTable}
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2010
 */
public abstract class AbstractOntologyTermsTableModel<T, R> extends AbstractTableModel {

	public enum columns {
		Selector, Open, Predicted, Relation, Term, Identifier, Comment
	}

	private static final long serialVersionUID = 4146368118692966602L;

	private String lastRegex = new String();

	private Set<String> ticked = new HashSet<String>();
	private Set<String> visible = new HashSet<String>();
	private Map<String, String> visibleThroughSynonymMap = new HashMap<String, String>();

	private Map<String, T> allTermsMap;
	private List<String> allTermsidList;
	private boolean sortingNeeded = true;
	private List<Set<String>> rankingOrder;
	private Set<String> termsSelected = new HashSet<String>();
	private Set<String> termsSameAsCandidateTerm = new HashSet<String>();
	private Set<String> termsKnownParentsOfCandidateTerm = new HashSet<String>();
	private Set<String> termsPredictedParentsOfCandidateTerm = new HashSet<String>();
	private Set<String> termsFromDefinitions = new HashSet<String>();
	private Set<String> termsSimilarToCandidateTerm = new HashSet<String>();

	private Map<String, Integer> termsRankInDefinitions = new HashMap<String, Integer>();

	private Object[][] order = { { termsSelected, Messages.getString("AbstractOntologyTermsTableModel.SelectedTerm"), null }, // //$NON-NLS-1$
			{ termsSameAsCandidateTerm, Messages.getString("AbstractOntologyTermsTableModel.SameAsExistingTerm"), Messages.getString("AbstractOntologyTermsTableModel.Identical") }, // //$NON-NLS-1$ //$NON-NLS-2$
			{ termsKnownParentsOfCandidateTerm, Messages.getString("AbstractOntologyTermsTableModel.Validated"), Messages.getString("AbstractOntologyTermsTableModel.ExistingParentTerm") },// //$NON-NLS-1$ //$NON-NLS-2$
			{ termsPredictedParentsOfCandidateTerm, Messages.getString("AbstractOntologyTermsTableModel.Predicted"), Messages.getString("AbstractOntologyTermsTableModel.SubClassOf") }, // //$NON-NLS-1$ //$NON-NLS-2$
			{ termsFromDefinitions, Messages.getString("AbstractOntologyTermsTableModel.Predicted"), Messages.getString("AbstractOntologyTermsTableModel.SubClassOf") }, // //$NON-NLS-1$ //$NON-NLS-2$
			{ termsSimilarToCandidateTerm, Messages.getString("AbstractOntologyTermsTableModel.SimilarTerm"), null } }; //$NON-NLS-1$

	private boolean showOnlyTicked;

	private R[] relationTypes = getDefaultRelationTypes();

	private Map<T, R> selectedRelationType = new HashMap<T, R>(1);
	
	private String language;

	/**
	 * Constructs a {@link AbstractOntologyTermsTableModel}.
	 */
	@SuppressWarnings("unchecked")
	public AbstractOntologyTermsTableModel() {
		allTermsMap = new HashMap<String, T>();
		allTermsidList = new ArrayList<String>();
		rankingOrder = new ArrayList<Set<String>>(order.length);
		for (int i = 0; i < order.length; i++) {
			rankingOrder.add((Set<String>) order[i][0]);
		}
		
		language = Locale.getDefault().getLanguage();
	}

	public void addFromCandidateDefinition(String termId, int rank) {
		termsFromDefinitions.add(termId);
		addTermsRank(termId, rank);
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void addFromUserDefinedDefinition(String termId, int rank) {
		termsFromDefinitions.add(termId);
		addTermsRank(termId, rank);
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/*
	 * IMPLEMENTED METHODS
	 */

	public void addParentsTermsOfExistingCandidateTerm(Map<String, String> parentTerms) {
		ticked.removeAll(termsKnownParentsOfCandidateTerm);
		termsKnownParentsOfCandidateTerm.clear();
		if (parentTerms != null) {
			for (Entry<String, String> entry : parentTerms.entrySet()) {
				termsKnownParentsOfCandidateTerm.add(entry.getKey());
				ticked.add(entry.getKey());
			}
			sortingNeeded = true;
			fireTableDataChanged();
		}
	}

	protected void addPredictedParentsOfCandidateTerm(Collection<T> terms) {
		for (T term : terms) {
			termsPredictedParentsOfCandidateTerm.add(getTermId(term));
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void addSameAsCandidateTerm(Collection<T> terms) {
		for (T term : terms) {
			termsSameAsCandidateTerm.add(getTermId(term));
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void addSelectedT(Collection<T> terms) {
		for (T term : terms) {
			termsSelected.add(getTermId(term));
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void addSimilarToCandidateTerm(Collection<T> terms) {
		for (T term : terms) {
			termsSimilarToCandidateTerm.add(getTermId(term));
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	private void addTermsRank(String termId, int rank) {
		if (!termsRankInDefinitions.containsKey(termId) || rank < termsRankInDefinitions.get(termId)) {
			termsRankInDefinitions.put(termId, rank);
		}
	}

	/**
	 * Update displayed candidate terms. Filter by regex provided.
	 * 
	 * @param regex
	 */
	public void applyFilter(String regex) {
		if (regex != null && !lastRegex.equals(regex)) {
			visible.clear();
			visibleThroughSynonymMap.clear();
			lastRegex = regex;
			Pattern p = null;

			try {
				p = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
			} catch (PatternSyntaxException exception) {
				return;
			}

			for (String termId : this.allTermsidList) {
				T term = allTermsMap.get(termId);
				if (regex.length() == 0) {
					visible.add(termId);
				}
				if (!visible.contains(termId)) {
					String name = getTermName(term);
					if (name != null && p.matcher(name).find() ||
						p.matcher(termId).find()) {
						visible.add(termId);
					}
				}
				if (!visible.contains(termId)) {
					Collection<String> synonyms = getSynonymNames(term);
					for (String syn : synonyms) {
						if (p.matcher(syn).find()) {
							visible.add(termId);
							visibleThroughSynonymMap.put(termId, syn);
							break;
						}
					}
				}
			}
			fireTableDataChanged();
		} else if (regex == null) {
			visible.clear();
			visibleThroughSynonymMap.clear();
			visible.addAll(allTermsidList);
		}
	}

	/**
	 * Returns index of first occurrence of potential substring in string
	 * 
	 * @param string
	 * @param potentialSubString
	 * @return
	 */
	private int calcFirstIndexOf(String string, String potentialSubString) {
		if (string == null || potentialSubString == null) {
			return -1;
		}
		return string.toLowerCase().indexOf(potentialSubString.toLowerCase());
	}

	/*
	 * OTHER PUBLIC METHODS
	 */

	protected void clearPredictedParentsOfCandidateTerm() {
		termsPredictedParentsOfCandidateTerm.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void clearSameAsCandidateTerms() {
		termsSameAsCandidateTerm.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/*
	 * UPDATE PARENTS
	 */
	protected void clearSelectedTs() {
		termsSelected.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void clearSimiliarToCandidateTerm() {
		termsSimilarToCandidateTerm.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	public void clearTermsFromDefinitions() {
		termsFromDefinitions.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/**
	 * Returns all terms
	 * 
	 * @return unmodifiable set of {@link T}
	 */
	public List<T> getAllTerms() {
		List<T> list = new ArrayList<T>();
		
		// FIXME: there's a bug in here, this list will always be empty
		for (T term : list) {
			list.add(term);
		}
		return Collections.unmodifiableList(list);
	}

	@Override
	public Class<?> getColumnClass(int columnIndex) {
		if (columnIndex == 0) {
			return Boolean.class;
		}
		return super.getColumnClass(columnIndex);
	}

	/**
	 * @return
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	public int getColumnCount() {
		return columns.values().length;
	}

	@Override
	public String getColumnName(int column) {
		if (column < columns.values().length) {
			return columns.values()[column].name();
		} else {
			return ""; //$NON-NLS-1$
		}

	}

	public abstract R[] getDefaultRelationTypes();

	public abstract R getDefaultRelationType();

	public R[] getRelationTypes() {
		return this.relationTypes;
	}

	/**
	 * @return
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	public int getRowCount() {
		return this.getVisibleElements().size();
	}

	public int getRowFromTerm(String id) {
		return getVisibleElements().indexOf(allTermsMap.get(id));
	}

	/**
	 * Returns candidate term displayed at specified position
	 * 
	 * @param rowIndex
	 * @return T, the {@link T} at the displayed rowIndex
	 */
	public T getTermAt(int rowIndex) {
		T term = getVisibleElements().get(rowIndex);
		return term;
	}

	public abstract String getTermId(T term);

	/*
	 * ABSTRACT METHODS
	 */
	public abstract String getTermName(T term);

	public abstract Collection<String> getSynonymNames(T term);

	/**
	 * Returns ticked candidate terms ids
	 * 
	 * @return tickedTs, the list of ids which are ticked
	 */
	public Set<String> getTickedTerms() {
		return Collections.unmodifiableSet(ticked);
	}

	/**
	 * @param rowIndex
	 * @param columnIndex
	 * @return
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@SuppressWarnings("unchecked")
	public Object getValueAt(int rowIndex, int columnIndex) {
		List<T> visibleElements = this.getVisibleElements();
		
		if (rowIndex >= visibleElements.size())
			return null;
		
		T term = visibleElements.get(rowIndex);
		if (columnIndex == columns.Selector.ordinal()) {
			return ticked.contains(getTermId(term));
		} else if (columnIndex == columns.Term.ordinal()) {
			return this.getTermName(term);
		} else if (columnIndex == columns.Identifier.ordinal()) {
			return this.getTermId(term);
		} else if (columnIndex == columns.Relation.ordinal()) {
			if (selectedRelationType.containsKey(term)) {
				return selectedRelationType.get(term);
			} else {
				return getDefaultRelationType();
			}
		} else if (columnIndex == columns.Predicted.ordinal()) {
			for (Object[] objects : order) {
				Set<String> set = (Set<String>) objects[0];
				if (set.contains(getTermId(term))) {
					if (objects[2] instanceof String) {
						return objects[2];
					}
				}
			}
			return null;
		} else if (columnIndex == columns.Comment.ordinal()) {
			StringBuffer buffer = new StringBuffer();
			for (Object[] objects : order) {
				Set<String> set = (Set<String>) objects[0];
				if (set.contains(getTermId(term))) {
					if (buffer.length() > 0) {
						buffer.append(", "); //$NON-NLS-1$
					}
					buffer.append(objects[1]);
				}
			}
			if (buffer.length() == 0) {
				buffer.append(Messages.getString("AbstractOntologyTermsTableModel.ExistingTerm")); //$NON-NLS-1$
			}
			return buffer.toString();
		}
		
		return null;
	}

	private synchronized List<T> getVisibleElements() {
		if (sortingNeeded) {
			sortElements();
			applyFilter(null);
			sortingNeeded = false;
		}
		ArrayList<T> list = new ArrayList<T>();
		for (String termId : allTermsidList) {
			if (showOnlyTicked) {
				if (visible.contains(termId) && ticked.contains(termId)) {
					list.add(allTermsMap.get(termId));
				}
			} else {
				if (visible.contains(termId)) {
					list.add(allTermsMap.get(termId));
				}
			}
			if (list.size() == 100) {
				break;
			}

		}
		return Collections.unmodifiableList(list);
	}

	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex) {
		if (columnIndex == columns.Selector.ordinal() || columnIndex == columns.Relation.ordinal()) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * @param term
	 * @return
	 */
	protected boolean isTicked(T term) {
		return ticked.contains(getTermId(term));
	}

	/**
	 * @param term
	 * @return
	 */
	protected boolean isFoundBySynonym(T term) {
		return visibleThroughSynonymMap.containsKey(getTermId(term));
	}

	/**
	 * @param term
	 * @return
	 */
	protected String getSynonymNameMatch(T term) {
		return visibleThroughSynonymMap.get(getTermId(term));
	}

	/**
	 * Remove all instances of {@link T} from the {@link TableModel}
	 */
	public void removeAll() {
		allTermsMap.clear();
		allTermsidList.clear();
		ticked.clear();
		visible.clear();
		visibleThroughSynonymMap.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/**
	 * Remove all instances of {@link T} provided in terms from the
	 * {@link TableModel}
	 * 
	 * @param terms
	 */
	public void removeAll(Collection<T> terms) {
		for (T term : terms) {
			String termId = getTermId(term);
			allTermsidList.remove(termId);
			allTermsMap.remove(termId);
			ticked.remove(termId);
			visible.remove(termId);
			visibleThroughSynonymMap.remove(termId);
		}
		fireTableDataChanged();
	}

	public void setRelationTypes(R[] relationTypes) {
		this.relationTypes = relationTypes;
		// fire tabel changed event
		TableModelEvent e = new TableModelEvent(this, 0, this.getRowCount(), columns.Relation.ordinal(),
				TableModelEvent.UPDATE);
		fireTableChanged(e);
	}

	/**
	 * Set <code>true</code> to show only ticked {@link T}s
	 * 
	 * @param isTicked
	 */
	public void setShowOnlyTicked(boolean isTicked) {
		this.showOnlyTicked = isTicked;
		fireTableDataChanged();
	}

	/**
	 * Sets the collection of {@link T}.
	 * 
	 * @param collection
	 */
	protected void setTerms(Collection<T> collection) {
		this.allTermsMap.clear();
		this.allTermsidList.clear();
		this.ticked.clear();
		this.visible.clear();
		this.visibleThroughSynonymMap.clear();

		for (T term : collection) {
			this.allTermsMap.put(getTermId(term), term);
			this.allTermsidList.add(getTermId(term));
		}
		this.visible.addAll(this.allTermsidList);
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/**
	 * @param term
	 * @param isTicked
	 */
	protected void setTicked(T term, boolean isTicked) {
		String termId = getTermId(term);
		if (isTicked == false) {
			ticked.remove(termId);
		} else {
			ticked.add(termId);
		}
	}

	public void setTickedTerms(Set<String> ticked) {
		this.ticked.clear();
		this.ticked.addAll(ticked);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
		T term = this.getVisibleElements().get(rowIndex);
		if (columnIndex == columns.Selector.ordinal()) {
			if (aValue instanceof Boolean) {
				Boolean isTicked = (Boolean) aValue;
				setTicked(term, isTicked);
				fireTableCellUpdated(rowIndex, columnIndex);
			}
		} else if (columnIndex == columns.Relation.ordinal()) {
			selectedRelationType.put(this.getTermAt(rowIndex), (R) aValue);
		}
	}

	/*
	 * PRIVATE METHODS
	 */
	private void sortElements() {
		Collections.sort(allTermsidList, new AddToOntologyTComparator(rankingOrder));
	}

	public void updateSimilarTerms(Collection<String> labels, List<String> idsOfSimilarTerms) {
		for (String id : idsOfSimilarTerms) {
			if (allTermsMap.containsKey(id)) {
				T term = allTermsMap.get(id);
				String termLabel = getTermName(term);
				for (String label : labels) {
					if (termLabel.equalsIgnoreCase(label)) {
						addSameAsCandidateTerm(Collections.singleton(term));
					} else if (columns.Selector.ordinal() < calcFirstIndexOf(termLabel, label)
							|| columns.Selector.ordinal() < calcFirstIndexOf(label, termLabel)) {
						addSimilarToCandidateTerm(Collections.singleton(term));
					}
				}
			} else {
				throw new RuntimeException("id not known!"); //$NON-NLS-1$
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
	public void updateTerms(List<T> terms) {
		removeAll(terms);
		for (T term : terms) {
			allTermsidList.add(getTermId(term));
			allTermsMap.put(getTermId(term), term);
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/**
	 * Comparator for Ts displayed in step 3. Add to Ontology
	 */
	private class AddToOntologyTComparator implements Comparator<String> {
		private List<Set<String>> idSetsOrdered;

		protected AddToOntologyTComparator(List<Set<String>> idSetsOrdered) {
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
		public int compare(String o1, String o2) {
			// test for null
			if (o1 == null && o2 == null) {
				return 0;
			} else if (o1 == null && o2 != null) {
				return 1;
			} else if (o1 != null && o2 == null) {
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
			} else if (name != null && name2 == null) {
				return -1;
			} else if (name == null && name2 == null) {
				return 0;
			}
			return Integer.valueOf(name.length()).compareTo(Integer.valueOf(name2.length()));
		}
	}

	public R getRelationType(String parentId) {
		T term = allTermsMap.get(parentId);
		R relationType = selectedRelationType.get(term);
		if (relationType == null) {
			return getDefaultRelationType();
		} else {
			return relationType;
		}
	}

	public T getTermById(String parentId) {
		return allTermsMap.get(parentId);
	}
	
	public String getLanguageSetting() {
    	return language;
    }

	public void setLanguage(String language) {
    	this.language = language;
    }
}

