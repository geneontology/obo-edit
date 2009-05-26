package org.oboedit.gui.components.ontologyGeneration;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JOptionPane;

import org.apache.log4j.Logger;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.impl.DbxrefImpl;
import org.obo.history.AddDbxrefHistoryItem;
import org.obo.history.AddSynonymHistoryItem;
import org.obo.history.ChangeSynScopeHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.CreateObjectHistoryItem;
import org.obo.history.DefinitionChangeHistoryItem;
import org.obo.history.DelSynonymHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.DestroyObjectHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.NameChangeHistoryItem;
import org.obo.history.NamespaceHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.HistoryAppliedEvent;
import org.oboedit.gui.event.HistoryListener;
import org.oboedit.gui.event.OntologyReloadListener;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.RootChangeEvent;
import org.oboedit.gui.event.RootChangeListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.util.GUIUtil;

import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupRelation;
import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupTerm;

public class OBOOntologyModelAdapter implements OBOOntologyModelAdapterInterface {	
	/**
	 * 
	 */
	private static final long serialVersionUID = -6595210039049170585L;
	private static final Logger logger = Logger.getLogger(OBOOntologyModelAdapter.class);

	private OntologyGenerationComponentService service;

	
	//Variables
	private LinkedObject selectedLinkedObject = null;
	private SessionManager sessionManager = SessionManager.getManager();
	/*
	 * Instantiate listeners to OBOEdit
	 */

	private HistoryListener historyListener = new HistoryListener() {
		public void applied(HistoryAppliedEvent arg0) {
			Set<String> idsUndergoingChange = new HashSet<String>();
			HistoryItem historyItem = arg0.getHistoryItem();
			collectAllHistoryItems(idsUndergoingChange, historyItem);
			updateParentAsAnyExistingLinkedObject(idsUndergoingChange);
		}

		/**
		 * @param idsUndergoingChange
		 * @param historyItem
		 */
//		@SuppressWarnings("unchecked")
		private void collectAllHistoryItems(Set<String> idsUndergoingChange, HistoryItem historyItem) {
			if (historyItem instanceof TermMacroHistoryItem) {
				TermMacroHistoryItem termMacroHistoryItem = (TermMacroHistoryItem) historyItem;
				Iterator<HistoryItem> iter = termMacroHistoryItem.getHistoryItems();
				while (iter.hasNext()) {
					HistoryItem hi = iter.next();
					String targetID = hi.getTarget();
					if (targetID != null) {
						if (hi instanceof NameChangeHistoryItem //
								|| hi instanceof CreateLinkHistoryItem //
								|| hi instanceof DeleteLinkHistoryItem //
								|| hi instanceof CreateObjectHistoryItem //
								|| hi instanceof DestroyObjectHistoryItem //
								|| hi instanceof AddSynonymHistoryItem //
								|| hi instanceof DelSynonymHistoryItem // 
								|| hi instanceof NameChangeHistoryItem //
								|| hi instanceof DelSynonymHistoryItem // 
								|| hi instanceof ChangeSynScopeHistoryItem) {
							idsUndergoingChange.add(targetID);
						} else if (hi instanceof TermMacroHistoryItem) {
							collectAllHistoryItems(idsUndergoingChange, hi);
						}
					}
				}
			}
		}

		public void reversed(HistoryAppliedEvent arg0) {
			System.err.println("REVERSED");
		}
	};
	
	//caches
	//TODO default muss hier reichen, damit TermsTableModel zugriff hat
	static Map<String, Set<OBOClass>> temporaryOBOTermLookupMap = null;
	
	/*
	 * Instantiate listeners to OBOEdit
	 */
	
	private OntologyReloadListener ontologyReloadListener = new OntologyReloadListener() {
		public void reload() {
			updateParentAsAnyExistingLinkedObject();
		}
	};
	
	private SelectionListener selectionListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			updateSelectedLinkedObjectAndParents();
		}
	};
	
	private RootChangeListener rootChangeListener = new RootChangeListener() {
		public void changeRoot(RootChangeEvent arg0) {
			updateParentAsAnyExistingLinkedObject();
		}
	};

	/**
	 * TODO es sind aber obo spezifische Teile vorhanden und es werden tabellen benötigt
	 * Feed specified terms known to OBOEdit into the {@link OBOTermsTable}
	 * 
	 * @param idsUndergoingChange
	 */
	private void updateParentAsAnyExistingLinkedObject(Collection<String> idsUndergoingChange) {
		logger.trace(String.format("UPDATE updateExistingLinkedObjects() for: %s + (%d)", service.getSelectedCandidateTerm(),
				idsUndergoingChange.size()));
		LinkDatabase currentLinkDatabase = sessionManager.getCurrentLinkDatabase();
		List<LinkedObject> linkedObjects = new ArrayList<LinkedObject>();
		for (String id : idsUndergoingChange) {
			IdentifiedObject objects = currentLinkDatabase.getObject(id);
			if (objects instanceof LinkedObject)
				linkedObjects.add((LinkedObject) objects);
		}
		
		if (linkedObjects.size() > 0) {
			service.getOboTermsTable().getModel().updateTerms(linkedObjects);
		}
	}
	
	/**
	 * Feed all terms known to OBOEdit into the {@link OBOTermsTable}
	 * 
	 * @param idsUndergoingChange
	 * FIXME LinkedObjects sind obo spezifisch,deshalb nicht in der innerenComponente
	 */
	private void updateParentAsAnyExistingLinkedObject() {
		logger.trace("UPDATE updateExistingLinkedObjects() for :" + service.getSelectedCandidateTerm());
		List<LinkedObject> linkedObjects = new ArrayList<LinkedObject>();
		for (IdentifiedObject identifiedObject : sessionManager.getCurrentLinkDatabase().getObjects()) {
			if (identifiedObject instanceof LinkedObject) {
				LinkedObject linkedObject = (LinkedObject) identifiedObject;
				linkedObjects.add(linkedObject);
			}
		}
		service.getOboTermsTable().setTerms(linkedObjects);
	}
	

	/**
	 * Add listeners from {@link SessionManager}
	 */
	public void addListener() {
		SelectionManager.getManager().addSelectionListener(selectionListener);
		SessionManager.getManager().addOntologyReloadListener(ontologyReloadListener);
		SessionManager.getManager().addHistoryListener(historyListener);
		SessionManager.getManager().addRootChangeListener(rootChangeListener);
	}
	
	/**
	 * Remove listeners from {@link SessionManager}
	 */
	public void removeListeners() {
		SelectionManager.getManager().removeSelectionListener(selectionListener);
		SessionManager.getManager().removeOntologyReloadListener(ontologyReloadListener);
		SessionManager.getManager().removeHistoryListener(historyListener);
		SessionManager.getManager().removeRootChangeListener(rootChangeListener);
	}
	
	public SessionManager getOboEditSessionManager() {
		return sessionManager;
	}
	
	public SelectionManager getSelectionManager() {
		return SelectionManager.getManager();
	}
	
	
	
	
	
	/**
	 * Update the internal {@link Map} for mappings from {@link String} to
	 * {@link OBOClass} to determine whether a string is already present in
	 * OBOEdit.
	 * 
	 * TODO maybe race condition with term generation
	 * TODO erzeugt ein internens Modell der Ontologie und kann angefragt werden. Schaut was in der 
	 */
	public void updateOBOTermsLookUpTable() {
		Collection<OBOClass> presentTerms = TermUtil.getTerms(getOboEditSessionManager().getSession());
		temporaryOBOTermLookupMap = new HashMap<String, Set<OBOClass>>(presentTerms.size() * 3);
		for (OBOClass term : presentTerms) {
			// add ontology labels
			String name = term.getName();
			Set<OBOClass> set;
			if (temporaryOBOTermLookupMap.containsKey(name)) {
				set = temporaryOBOTermLookupMap.get(name);
			} else {
				set = new HashSet<OBOClass>(1);
				temporaryOBOTermLookupMap.put(name, set);
			}
			set.add(term);

			// add ontology synonyms
			for (Synonym synonym : term.getSynonyms()) {
				String label = synonym.getText();
				if (temporaryOBOTermLookupMap.containsKey(label)) {
					set = temporaryOBOTermLookupMap.get(label);
				} else {
					set = new HashSet<OBOClass>(1);
					temporaryOBOTermLookupMap.put(label, set);
				}
				set.add(term);
			}
		}

		logger.trace("UPDATE OBOClass lookup table, " + temporaryOBOTermLookupMap.size() + " labels found");
	}
	
	private void tryCommitDefinition(CandidateTerm selectedCandidateTerm) {
//		CandidateTerm selectedCandidateTerm = service.getSelectedCandidateTerm();
		
		if (temporaryOBOTermLookupMap.containsKey(selectedCandidateTerm.getLabel())) {
			Set<OBOClass> set = temporaryOBOTermLookupMap.get(selectedCandidateTerm.getLabel());
			if (set.size() == 1) {
				TermMacroHistoryItem item = createDefinitionHistoryItem(set.iterator().next().getID(),
						selectedCandidateTerm);
				sessionManager.apply(item, false);
//				commit(set.iterator().next());
				Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
			} else {
				throw new RuntimeException("For term with label '" + selectedCandidateTerm.getLabel()
						+ "' there exist more OBOClass with the same label.");
			}
		}
	}
	


	/*
	 * INTERACTION WITH OBO EDIT ONTOLOGY MODEL
	 */
	
	public ReconfigEvent getNewReconfigEvent(OntologyGenerationComponent ontologyGenerationComponent) {
		return new ReconfigEvent(ontologyGenerationComponent);
	}
	
	public static String getOboEditVersion() {
		return Preferences.getVersion().toString();
	}
	
	
	/**
	 * Create a history item with the changed definitions from the specified
	 * {@link CandidateTerm}
	 * 
	 * @param id, the id of the newly added term
	 * @param candidateTerm, the term to add
	 * @return
	 */
	private TermMacroHistoryItem createDefinitionHistoryItem(String id, CandidateTerm candidateTerm) {
		TermMacroHistoryItem item;
		item = new TermMacroHistoryItem("Add definition to term " + id);
		if (candidateTerm.getUserDefinedDefinition() != null) {
			item.addItem(new DefinitionChangeHistoryItem(null, candidateTerm.getUserDefinedDefinition(), id));
			for (CandidateDefinition candidateDef : candidateTerm.getGeneratedDefinitions()) {
				if (candidateDef.isTicked()) {
					for (String url : candidateDef.getUrl()) {
						if (url != null) { // case for all which are generated
							// and not taken from OBOEdit
							item.addItem(new AddDbxrefHistoryItem(id,
									new DbxrefImpl("URL", url, DbxrefImpl.DEFINITION), true, null));
						}
					}
				}
			}
		}
		return item;
	}

	/**
	 * Takes the LinkedObject, locates it in the Ontology Tree and adds selected
	 * term as child
	 * 
	 * @param includeBranch
	 * @param includeChildren
	 * @param selectedObject, LinkedObject to locate in ontology tree
	 */
	public void addToOntologyAsChildOfLinkedObject(Set<String> parentIds, boolean includeChildren,
			boolean includeBranch, CandidateTerm selectedCandidateTerm) {
		TermMacroHistoryItem changeItem = new TermMacroHistoryItem();
		if (parentIds == null || parentIds.size() == 0) {
			JOptionPane.showMessageDialog(null,
					"Please select a term to add children or add root edit/Add Root/Add Root");
			return;
		} else {
			Iterator<String> selectedParentIdIterator = parentIds.iterator();
			String selectedCandidateTermID = null;
			List<IdentifiedObject> linkedObjectsIfExistLikeSelectedTerm = getLinkedObjectsIfExist(selectedCandidateTerm
					.getLabel());
			if (selectedParentIdIterator.hasNext()) {
				String parentId = selectedParentIdIterator.next();
				LinkedObject parentLinkedObject = (LinkedObject) sessionManager.getCurrentLinkDatabase()
						.getObject(parentId);
				if (linkedObjectsIfExistLikeSelectedTerm == null) {
					if (TermUtil.isProperty(parentLinkedObject))
						selectedCandidateTermID = getTypeID();
					else
						selectedCandidateTermID = GUIUtil.fetchID(parentLinkedObject);
					if (selectedCandidateTermID == null || selectedCandidateTermID.trim().length() == 0) {
						System.err.println("Could not generate ID! " + "Action cancelled.");
					}
					TermMacroHistoryItem item = createTermInOBOEdit(selectedCandidateTermID, selectedCandidateTerm,
							parentLinkedObject);
					changeItem.addItem(item);
					TermMacroHistoryItem addTerm = addTermToOBOEdit(selectedCandidateTermID, parentId, OBOProperty.IS_A
							.getID());
					changeItem.addItem(addTerm);

				} else if (linkedObjectsIfExistLikeSelectedTerm.size() == 1) {
					TermMacroHistoryItem addTerm = addTermToOBOEdit(
							linkedObjectsIfExistLikeSelectedTerm.get(0).getID(), parentId, OBOProperty.IS_A.getID());
					changeItem.addItem(addTerm);
					selectedCandidateTermID = linkedObjectsIfExistLikeSelectedTerm.get(0).getID();
				} else {
					throw new RuntimeException(String.format("There exist already more than one term with name '%s'",
							selectedCandidateTerm.getLabel()));
				}
			}
			while (selectedParentIdIterator.hasNext()) {
				TermMacroHistoryItem addTerm = addTermToOBOEdit(selectedCandidateTermID, selectedParentIdIterator
						.next(), OBOProperty.IS_A.getID());
				changeItem.addItem(addTerm);
			}
			sessionManager.apply(changeItem, false);

			/*
			 * TODO care for multiple for the same label and create a joint term
			 * with all these ids currently seperate terms with differentIds and
			 * different labels are added
			 */
			// include known children of the added term to the ontology
			if (includeChildren && !includeBranch) {
				addAllChildrenToOBOEditForTermWithID(selectedCandidateTermID, selectedCandidateTerm);
			} else if (includeBranch) {
				throw new RuntimeException("feature is not implemented");
				// ArrayList<String> list = new ArrayList<String>();
				// for (OBOLookupTerm term :
				// selectedCandidateTerm.getExistingOntologyTerms()) {
				// list.add(term.getOboID());
				// }
				// addAllDescendantsToOBOEdit(list);
			}
		}
	}
	
	public void updateParentAsTermFromDefinition(CandidateTerm selectedCandidateTerm, TermsTable termsTable,
			OBOTermsTable oboTermsTable, DefinitionsTable definitionsTable) {
		logger.trace("UPDATE TERMS FROM DEFINITION for :" + selectedCandidateTerm);

		if (null == selectedCandidateTerm) {
			int row = termsTable.rowAtPoint(termsTable.getMousePosition());
			selectedCandidateTerm = termsTable.getModel().getTermAt(row);
			logger.warn("Selection lost in terms table, recovered though mouse position");
		}
		// clear
		oboTermsTable.getModel().clearTermsFromDefinitions();
		Collection<IdentifiedObject> allIdentifiedObjects = sessionManager.getSession().getObjects();

		// process user definded definition
		Map<LinkedObject, Integer> objectsFromUserDefindedDef = new HashMap<LinkedObject, Integer>();
		if (null != selectedCandidateTerm.getUserDefinedDefinition()) {
			for (IdentifiedObject identifiedObject : allIdentifiedObjects) {
				if (!objectsFromUserDefindedDef.containsKey(identifiedObject)) {
					int firstOccurenceIndex = calcFirstIndexOf(selectedCandidateTerm.getUserDefinedDefinition(),
							identifiedObject.getName());
					if (firstOccurenceIndex > 0) {
						if (identifiedObject instanceof LinkedObject) {
							LinkedObject linkedObject = (LinkedObject) identifiedObject;
							oboTermsTable.getModel().addFromUserDefinedDefinition(linkedObject, firstOccurenceIndex);
							objectsFromUserDefindedDef.put(linkedObject, firstOccurenceIndex);
						}
					}
				}
			}
		}
		// process selected definitions
		Map<LinkedObject, Integer> objectsFromCandidateDef = new HashMap<LinkedObject, Integer>();
		List<CandidateDefinition> definitions = definitionsTable.getModel().getDefinitions();
		for (CandidateDefinition candidateDefinition : definitions) {
			if (candidateDefinition.isTicked()) {
				for (IdentifiedObject identifiedObject : allIdentifiedObjects) {
					if (!objectsFromCandidateDef.containsKey(identifiedObject)) {
						int firstOccurenceIndex = calcFirstIndexOf(candidateDefinition.getDefinition(),
								identifiedObject.getName());
						if (firstOccurenceIndex > 0) {
							if (identifiedObject instanceof LinkedObject) {
								LinkedObject linkedObject = (LinkedObject) identifiedObject;
								// oboTermsTable.getModel().addFromCandidateDefinition(linkedObject,
								// firstOccurenceIndex);
								oboTermsTable.getModel().addFromTickedDefinition(linkedObject, firstOccurenceIndex);
							}
						}
					}
				}
			}
		}
	}
	
	public String getTypeID() {
		String id = JOptionPane.showInputDialog("Please input an id");
		if (id == null || id.length() == 0) {
			System.err.println("Cannot create a new type " + "without an id");
			return null;
		} else {
			if (sessionManager.getSession().getObject(id) != null) {
				System.err.println("ID " + id + " already in use!");
				return null;
			} else if (!IDUtil.isLegalID(id)) {
				System.err.println("ID " + id + " contains illegal characters");
				return null;
			}
		}
		return id;
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
	
	/**
	 * Checks for the selected {@link LinkedObject} and updates the GUI
	 * 
	 * TODO schwieriger fall, wahrscheinlich in die OBOTermsTable schieben
	 */
	public void updateSelectedLinkedObjectAndParents() {
		logger.trace("UPDATE SELF updateSelectedLinkedObject() for :" + service.getSelectedCandidateTerm());
		OBOTermsTable oboTermsTable = service.getOboTermsTable();
		// update OBOclass Lookup map
		updateOBOTermsLookUpTable();

		Collection<PathCapable> paths = getSelectionManager().getGlobalSelection().getAllSelectedObjects();
		oboTermsTable.getModel().clearSelectedLinkedObjects();
		oboTermsTable.getModel().clearParentsOfSelectedLinkedObject();
		Set<LinkedObject> parents = new HashSet<LinkedObject>();
		for (PathCapable pathCapable : paths) {
			if (pathCapable instanceof LinkedObject) {
				LinkedObject linkedObject = (LinkedObject) pathCapable;
				selectedLinkedObject = (LinkedObject) pathCapable;
				String selectedLinkedObjectLabel = linkedObject.getName();
				if (selectedLinkedObjectLabel != null) {
					service.updateInputFieldsForSelectedLinkedObjectLabel(selectedLinkedObjectLabel);
					service.setTextSelectedLinkedObjectField(selectedLinkedObjectLabel);
				}
				oboTermsTable.getModel().addSelectedLinkedObject(linkedObject);
				for (Link link : sessionManager.getCurrentLinkDatabase().getParents(linkedObject)) {
					parents.add(link.getParent());
				}
			}
		}
		// add parents of selected
		LinkedObject[] parentArray = new LinkedObject[parents.size()];
		parentArray = parents.toArray(parentArray);
		oboTermsTable.getModel().addParentsTermsOfSelectedLinkedObject(parentArray);
	}
	
	/**
	 * Add similar (based on substring inclusion) terms to similiarTermsComboBox
	 * based on substring comparison
	 * 
	 * TODO in OBOTermsTable aber das Label des selectierten Terms �bergeben
	 * FIXME enthält auch mit IdentifiedObject und LinkedObject Obo spezifische Objekte
	 */
	public void updateParentAsSimiliarTerm(CandidateTerm selectedCandidateTerm, OBOTermsTable oboTermsTable) {
		logger.trace("UPDATE SIMILAR TERMS for :" + selectedCandidateTerm);
		
		if (null == selectedCandidateTerm) {
			throw new RuntimeException("No term selected");
		}

		// clearing
		oboTermsTable.getModel().clearSameAsCandidateTerms();
		oboTermsTable.getModel().clearSimiliarToCandidateTerm();

		// String savedDef;
		String selectedTermLabel = selectedCandidateTerm.getLabel();
		if (selectedTermLabel.length() != 0) {
			Collection<IdentifiedObject> allIdentifiedObjects = sessionManager.getSession().getObjects();
			for (IdentifiedObject identifiedObject : allIdentifiedObjects) {
				if (identifiedObject instanceof LinkedObject) {
					LinkedObject linkedObject = (LinkedObject) identifiedObject;
					String linkedObjectLabel = linkedObject.getName();
					if (linkedObjectLabel != null) {
						if (linkedObjectLabel.equalsIgnoreCase(selectedTermLabel)) {
							oboTermsTable.getModel().addSameAsCandidateTerm(linkedObject);
						} else if (0 < calcFirstIndexOf(linkedObjectLabel, selectedTermLabel)
								|| 0 < calcFirstIndexOf(selectedTermLabel, linkedObjectLabel)) {
							oboTermsTable.getModel().addSimilarToCandidateTerm(linkedObject);
						}
					}
				}
			}
		}
	}

	/**
	 * Returns the {@link LinkedObject}s known in OBOEdit with same label
	 * 
	 * @param label
	 * @return
	 */
	public List<IdentifiedObject> getLinkedObjectsIfExist(String label) {
		List<IdentifiedObject> list = null;
		Collection<IdentifiedObject> objects = sessionManager.getCurrentLinkDatabase().getObjects();
		for (IdentifiedObject identifiedObject : objects) {
			String identifiedObjectLabel = identifiedObject.getName();
			if (identifiedObjectLabel != null && identifiedObjectLabel.equalsIgnoreCase(label)) {
				if (list == null) {
					list = new ArrayList<IdentifiedObject>();
				}
				list.add(identifiedObject);
			}
		}
		return list;
	}
	
	

	/**
	 * TODO describe me!
	 * 
	 * @param selectedCandidateTermID
	 * @return
	 */
	private void addAllChildrenToOBOEditForTermWithID(String selectedCandidateTermID, CandidateTerm candidateTerm) {
		TermMacroHistoryItem changeItem = new TermMacroHistoryItem();
		// setup the map of id to name mappings
		HashMap<String, String> idToName = new HashMap<String, String>();
		for (OBOLookupTerm lookupTerm : candidateTerm.getExistingChildTerms()) {
			idToName.put(lookupTerm.getOboID(), lookupTerm.getLabel());
		}
		// create all and add all known children
		for (final OBOLookupRelation relation : candidateTerm.getExistingChildRelations()) {
			final String childLabel = idToName.get(relation.getOboChildTermID());
			if (childLabel != null) {
				final List<IdentifiedObject> linkedObjectsIfExist = getLinkedObjectsIfExist(childLabel);
				final LinkedObject parentLinkedObject = (LinkedObject) sessionManager
						.getCurrentLinkDatabase().getObject(selectedCandidateTermID);
				if (linkedObjectsIfExist == null) {
					final String[] lexicalRepresentation = { childLabel };
					final CandidateTerm childCandidateTerm = new CandidateTerm(childLabel, new String[0],
							lexicalRepresentation, Double.NaN, CandidateTerm.TYPE_OBO_TERM);
					final TermMacroHistoryItem createTermItem = createTermInOBOEdit(relation.getOboChildTermID(),
							childCandidateTerm, parentLinkedObject);
					changeItem.addItem(createTermItem);
					final TermMacroHistoryItem addTermItem = addTermToOBOEdit(relation.getOboChildTermID(),
							parentLinkedObject.getID(), relation.getOboRelationShipType());
					changeItem.addItem(addTermItem);
					// accumulate return value
				} else {
					for (final IdentifiedObject identifiedObject : linkedObjectsIfExist) {
						final Collection<Link> children = sessionManager.getCurrentLinkDatabase()
								.getChildren(parentLinkedObject);
						if (!children.contains(identifiedObject)) {
							final TermMacroHistoryItem addTermItem = addTermToOBOEdit(identifiedObject.getID(),
									parentLinkedObject.getID(), relation.getOboRelationShipType());
							changeItem.addItem(addTermItem);
							// accumulate return value
						}
					}
				}
			}
		}
		sessionManager.apply(changeItem, false);
	}
	
	/**
	 * Add a create new child to the OBO Ontology
	 * 
	 * @param id, the id of the newly added term
	 * @param candidateTerm, the term to add
	 * @param parentLinkedObject, the parent {@link LinkedObject} for the newly added term
	 * @return
	 * 
	 */
	private TermMacroHistoryItem createTermInOBOEdit(String id, CandidateTerm candidateTerm,
			LinkedObject parentLinkedObject) {
		TermMacroHistoryItem item = new TermMacroHistoryItem("Add and create child to " + parentLinkedObject);
		String label = candidateTerm.getLabel();
		item.addItem(new CreateObjectHistoryItem(id, parentLinkedObject.getType().getID()));
		item.addItem(new NameChangeHistoryItem(label, id, id));

		Namespace ns = parentLinkedObject.getNamespace();
		if (ns == null)
			ns = sessionManager.getSession().getDefaultNamespace();
		if (ns != null) {
			item.addItem(new NamespaceHistoryItem(null, ns, id));
		}

		item.addItem(createDefinitionHistoryItem(id, candidateTerm));

		// add synonyms
		for (String abbreviation : candidateTerm.getAbbreviations()) {
			AddSynonymHistoryItem addSynonymHistoryItem = new AddSynonymHistoryItem(id, abbreviation);
			item.addItem(addSynonymHistoryItem);
		}
		for (String lex : candidateTerm.getLexicalRepresentations()) {
			if (!lex.equals(label)) {
				item.addItem(new AddSynonymHistoryItem(id, lex));
			}
		}
		return item;
	}
	
	/**
	 * Add a new child to the OBO Ontology TODO find out, if it is necessary to
	 * take care of cycle detection
	 * 
	 * @param id, the id of the newly added term
	 * @param parentLinkedObject, the parent {@link LinkedObject} for the newly added term
	 * @return
	 */
	private TermMacroHistoryItem addTermToOBOEdit(String id, String parentId, String relationType) {
		TermMacroHistoryItem item = new TermMacroHistoryItem("Add new child to " + parentId);
		item.addItem(new CreateLinkHistoryItem(id, relationType, parentId));
		// item.setTarget(parentId); // TODO unclear if needed
		// item.setResult(id); // TODO unclear if needed
		return item;
	}
	
	public IDUtil getIDUtil() {
		return new IDUtil();
	}
	
	public LinkedObject getSelectedLinkedObject() {
		return selectedLinkedObject;
	}
	
	public SessionManager getSessionManager() {
		return sessionManager;
	}
	
	public void setSelectedLinkedObject(LinkedObject linkedObject) {
		selectedLinkedObject = linkedObject;
	}

	public void getTermsFromOntologyModel(Collection<String> idsUndergoingChange) {
		updateParentAsAnyExistingLinkedObject(idsUndergoingChange);
		
	}
	
	public void getTermsFromOntologyModel() {
		updateParentAsAnyExistingLinkedObject();
		
	}

	public TermMacroHistoryItem commitTermDefinitionsToOntologyModel(String id, CandidateTerm candidateTerm,
			LinkedObject parentLinkedObject) {
		return createTermInOBOEdit(id, candidateTerm, parentLinkedObject);
		
	}
	
	public TermMacroHistoryItem commitTermDefinitionsToOntologyModel(String id, CandidateTerm candidateTerm) {
		return createDefinitionHistoryItem(id, candidateTerm);
	}
	
	public void commitDefinition(CandidateTerm selectedCandidateTerm) {
		tryCommitDefinition(selectedCandidateTerm);
	}
	
	public void setService(OntologyGenerationComponentService service) {
		this.service = service;
	}
}
