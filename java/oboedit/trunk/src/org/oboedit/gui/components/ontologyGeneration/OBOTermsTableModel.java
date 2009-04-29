package org.oboedit.gui.components.ontologyGeneration;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

import org.obo.datamodel.LinkedObject;

/**
 * {@link OBOTermsTableModel} to hold instances of {@link LinkedObject} for
 * display in {@link JTable}
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class OBOTermsTableModel extends AbstractTableModel {
	/**  */
	private static final long serialVersionUID = 4146368118692966602L;
	private int numberOfColumns = 4;
	private String lastRegex = new String();

	private Set<String> ticked = new HashSet<String>();
	private Set<String> visible = new HashSet<String>();
	private Set<String> termsSelected = new HashSet<String>();
	private Set<String> termsSameAsCandidateTerm = new HashSet<String>();
	private Set<String> termsParentsOfSelectedLinkedObject = new HashSet<String>();
	private Set<String> termsPredictedParentsOfCandidateTerm = new HashSet<String>();
	private Set<String> termsFromUserDefinedDefinitions = new HashSet<String>();
	private Set<String> termsFromTickedDefinitions = new HashSet<String>();
	private Set<String> termsSimilarToCandidateTerm = new HashSet<String>();
	private Map<String, LinkedObject> allTermsMap;
	private List<String> allTermsIDList;
	private boolean sortingNeeded = true;
	private List<Set<String>> rankingOrder;
	private Object[][] order = { { termsSameAsCandidateTerm, "same as existing term", "identical" },
			{ termsSelected, "selected term", null },
			{ termsPredictedParentsOfCandidateTerm, "predicted parent of candidate", "sub_class_of" },
			{ termsFromUserDefinedDefinitions, "in definition (user-defined)", null },
			{ termsFromTickedDefinitions, "in definition", null },
			{ termsParentsOfSelectedLinkedObject, "parent of selected term", null },
			{ termsSimilarToCandidateTerm, "similar term", null } };
	private boolean showOnlyTicked;

	/**
	 * Constructs a {@link OBOTermsTableModel}.
	 */
	@SuppressWarnings("unchecked")
	public OBOTermsTableModel() {
		allTermsMap = new HashMap<String, LinkedObject>();
		allTermsIDList = new ArrayList<String>();
		rankingOrder = new ArrayList<Set<String>>(order.length);
		for (int i = 0; i < order.length; i++) {
			rankingOrder.add((Set<String>) order[i][0]);
		}
	}

	/*
	 * IMPLEMENTED METHODS
	 */

	/**
	 * @return
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	public int getColumnCount() {
		return numberOfColumns;
	}

	/**
	 * @return
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	public int getRowCount() {
		return this.getVisibleElements().size();
	}

	/**
	 * @param rowIndex
	 * @param columnIndex
	 * @return
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@SuppressWarnings("unchecked")
	public Object getValueAt(int rowIndex, int columnIndex) {
		List<LinkedObject> visibleElements = this.getVisibleElements();
		if (rowIndex < visibleElements.size()) {
			LinkedObject term = visibleElements.get(rowIndex);
			if (columnIndex == 0) {
				return ticked.contains(term.getID());
			} else if (columnIndex == 1) {
				StringBuffer buffer = new StringBuffer();
				buffer.append(term.getName());
				buffer.append(" (");
				buffer.append(term.getID());
				buffer.append(" )");
				return buffer.toString();
			} else if (columnIndex == 2) {
				for (Object[] objects : order) {
					Set<String> set = (Set<String>) objects[0];
					if (set.contains(term.getID())) {
						return objects[2];
					}
				}
				return null;
			} else if (columnIndex == 3) {
				StringBuffer buffer = new StringBuffer();
				for (Object[] objects : order) {
					Set<String> set = (Set<String>) objects[0];
					if (set.contains(term.getID())) {
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

	/*
	 * OVERRIDDEN METHODS
	 */

	@Override
	public Class<?> getColumnClass(int columnIndex) {
		if (columnIndex == 0) {
			return Boolean.class;
		}
		return super.getColumnClass(columnIndex);
	}

	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex) {
		return columnIndex == 0;
	}

	@Override
	public String getColumnName(int column) {
		if (column == 0) {
			return "";
		} else if (column == 1) {
			return "Term";
		} else if (column == 2) {
			return "Relation";
		} else if (column == 3) {
			return "Comment";
		} else
			return "";

	}

	@Override
	public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
		LinkedObject term = this.getVisibleElements().get(rowIndex);
		if (columnIndex == 0) {
			if (aValue instanceof Boolean) {
				Boolean isTicked = (Boolean) aValue;
				setTicked(term, isTicked);
				fireTableCellUpdated(rowIndex, columnIndex);
			}
		}
	}

	// @Override
	// public void fireTableCellUpdated(int i, int i1)
	// {
	// super.fireTableCellUpdated(i, i1);
	// }

	/*
	 * OTHER PUBLIC METHODS
	 */

	/**
	 * Returns ticked candidate terms ids
	 * 
	 * @return tickedLinkedObjects, the list of ids which are ticked
	 */
	public Set<String> getTickedTerms() {
		return Collections.unmodifiableSet(ticked);
	}

	/**
	 * Returns all terms
	 * 
	 * @return unmodifiable set of {@link LinkedObject}
	 */
	public List<LinkedObject> getAllTerms() {
		List<LinkedObject> list = new ArrayList<LinkedObject>();
		for (LinkedObject term : list) {
			list.add(term);
		}
		return Collections.unmodifiableList(list);
	}

	/**
	 * Returns candidate term displayed at specified position
	 * 
	 * @param rowIndex
	 * @return LinkedObject, the {@link LinkedObject} at the displayed rowIndex
	 */
	public LinkedObject getTermAt(int rowIndex) {
		LinkedObject term = getVisibleElements().get(rowIndex);
		return term;
	}

	/**
	 * Update displayed candidate terms. Filter by regex provided.
	 * 
	 * @param regex
	 */
	public void applyFilter(String regex) {
		if (regex != null && !lastRegex.equals(regex)) {
			visible.clear();
			lastRegex = regex;
			Pattern p = null;

			try {
				p = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
			} catch (PatternSyntaxException exception) {
				return;
			}

			for (String termID : this.allTermsIDList) {
				LinkedObject term = allTermsMap.get(termID);
				String name = term.getName();
				if (name != null) {
					if (regex.length() == 0) {
						visible.add(term.getID());
					} else if (p.matcher(name).find()) {
						visible.add(term.getID());
					} else {
						visible.remove(term);
					}
				}
			}
			fireTableDataChanged();
		} else if (regex == null) {
			visible.clear();
			visible.addAll(allTermsIDList);
		}
	}

	/**
	 * Remove all instances of {@link LinkedObject} provided in terms from the
	 * {@link TableModel}
	 * 
	 * @param terms
	 */
	public void removeAll(Collection<LinkedObject> terms) {
		for (LinkedObject object : terms) {
			allTermsIDList.remove(object.getID());
			allTermsMap.remove(object.getID());
		}
		ticked.removeAll(terms);
		visible.removeAll(terms);
		fireTableDataChanged();
	}

	/**
	 * Remove all instances of {@link LinkedObject} from the {@link TableModel}
	 */
	public void removeAll() {
		allTermsMap.clear();
		allTermsIDList.clear();
		ticked.clear();
		visible.clear();
		fireTableDataChanged();
	}

	/*
	 * PRIVATE METHODS
	 */
	private void sortElements() {
		Collections.sort(allTermsIDList, new AddToOntologyLinkedObjectComparator(rankingOrder));
	}

	private List<LinkedObject> getVisibleElements() {
		if (sortingNeeded) {
			sortElements();
			applyFilter(null);
			sortingNeeded = false;
		}
		ArrayList<LinkedObject> list = new ArrayList<LinkedObject>();
		for (String termID : allTermsIDList) {
			if (showOnlyTicked) {
				if (visible.contains(termID) && ticked.contains(termID)) {
					list.add(allTermsMap.get(termID));
				}
			} else {
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

	/**
	 * Comparator for LinkedObjectes displayed in step 3. Add to Ontology
	 * 
	 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), Dec
	 *         11, 2008
	 */
	private class AddToOntologyLinkedObjectComparator implements Comparator<String> {
		private List<Set<String>> idSetsOrdered;

		protected AddToOntologyLinkedObjectComparator(List<Set<String>> idSetsOrdered) {
			this.idSetsOrdered = idSetsOrdered;
		}

		/**
		 * Compares LinkedObject objects for display
		 * 
		 * @param o1
		 * @param o2
		 * @return
		 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
		 */
		public int compare(String o1, String o2) {
			// test for null
			if (o1 == null && o2 != null) {
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

			// compare by name
			String name = allTermsMap.get(o1).getName();
			String name2 = allTermsMap.get(o2).getName();

			if (name == null && name2 != null) {
				return 1;
			} else if (name != null && name2 == null) {
				return -1;
			} else if (name == null && name2 == null) {
				return 0;
			}
			return name.compareTo(name2);
		}
	}

	/**
	 * @param linkedObject
	 * @return
	 */
	protected boolean isTicked(LinkedObject linkedObject) {
		return ticked.contains(linkedObject);
	}

	/**
	 * @param linkedObject
	 * @param isTicked
	 */
	protected void setTicked(LinkedObject linkedObject, boolean isTicked) {
		if (isTicked == false) {
			ticked.remove(linkedObject.getID());
		} else {
			ticked.add(linkedObject.getID());
		}
	}

	/**
	 * Sets the collection of {@link LinkedObject}.
	 * 
	 * @param collection
	 */
	protected void setTerms(Collection<LinkedObject> collection) {
		this.allTermsMap.clear();
		this.allTermsIDList.clear();
		this.ticked.clear();
		this.visible.clear();

		for (LinkedObject linkedObject : collection) {
			this.allTermsMap.put(linkedObject.getID(), linkedObject);
			this.allTermsIDList.add(linkedObject.getID());
		}
		this.visible.addAll(this.allTermsIDList);
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void clearTermsFromDefinitions() {
		termsFromUserDefinedDefinitions.clear();
		termsFromTickedDefinitions.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void addFromUserDefinedDefinition(LinkedObject... linkedObjects) {
		for (LinkedObject linkedObject : linkedObjects) {
			termsFromUserDefinedDefinitions.add(linkedObject.getID());
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void addFromTickedDefinition(LinkedObject... linkedObjects) {
		for (LinkedObject linkedObject : linkedObjects) {
			termsFromTickedDefinitions.add(linkedObject.getID());
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/*
	 * UPDATE PARENTS
	 */
	protected void clearSelectedLinkedObjects() {
		termsSelected.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void addSelectedLinkedObject(LinkedObject... linkedObjects) {
		for (LinkedObject linkedObject : linkedObjects) {
			termsSelected.add(linkedObject.getID());
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void clearParentsOfSelectedLinkedObject() {
		termsParentsOfSelectedLinkedObject.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void addParentsTermsOfSelectedLinkedObject(LinkedObject... linkedObjects) {
		for (LinkedObject linkedObject : linkedObjects) {
			termsParentsOfSelectedLinkedObject.add(linkedObject.getID());
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void clearPredictedParentsOfCandidateTerm() {
		termsPredictedParentsOfCandidateTerm.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void addPredictedParentsOfCandidateTerm(LinkedObject... linkedObjects) {
		for (LinkedObject linkedObject : linkedObjects) {
			termsPredictedParentsOfCandidateTerm.add(linkedObject.getID());
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void clearSimiliarToCandidateTerm() {
		termsSimilarToCandidateTerm.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void addSimilarToCandidateTerm(LinkedObject... linkedObjects) {
		for (LinkedObject linkedObject : linkedObjects) {
			termsSimilarToCandidateTerm.add(linkedObject.getID());
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void clearSameAsCandidateTerms() {
		termsSameAsCandidateTerm.clear();
		sortingNeeded = true;
		fireTableDataChanged();
	}

	protected void addSameAsCandidateTerm(LinkedObject... linkedObjects) {
		for (LinkedObject linkedObject : linkedObjects) {
			termsSameAsCandidateTerm.add(linkedObject.getID());
		}
		sortingNeeded = true;
		fireTableDataChanged();
	}

	/**
	 * TODO describe me!
	 * 
	 * @param linkedObjects
	 */
	public void updateTerms(List<LinkedObject> linkedObjects) {
		removeAll(linkedObjects);
		for (LinkedObject linkedObject : linkedObjects) {
			allTermsIDList.add(linkedObject.getID());
			allTermsMap.put(linkedObject.getID(), linkedObject);
		}
		fireTableDataChanged();
	}

	/**
	 * TODO describe me!
	 * 
	 * @param b
	 */
	public void setShowOnlyTicked(boolean b) {
		this.showOnlyTicked = b;
		fireTableDataChanged();
	}
}