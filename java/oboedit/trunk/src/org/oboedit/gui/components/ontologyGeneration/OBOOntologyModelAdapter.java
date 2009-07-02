package org.oboedit.gui.components.ontologyGeneration;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JOptionPane;

import org.apache.log4j.Logger;
import org.jdesktop.swingx.util.SwingWorker;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;
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

public class OBOOntologyModelAdapter implements OntologyModelAdapterInterface
{
	private static final int RANGE_FOR_PARENT_IN_DEFINITION = 5;
	private static final long serialVersionUID = -6595210039049170585L;
	private static final Logger logger = Logger.getLogger(OBOOntologyModelAdapter.class);

	private static OBOOntologyModelAdapter theInstance;

	private OntologyGenerationComponentService service;
	private LinkedObject selectedLinkedObject = null;
	private Collection<String> idsUndergoingChange;
	private SessionManager sessionManager;

	/**
	 * private constructor of the singleton
	 */
	private OBOOntologyModelAdapter()
	{
		sessionManager = SessionManager.getManager();
		idsUndergoingChange = new HashSet<String>(1);
	}

	/**
	 * @return the singleton instance of {@link OntologyModelAdapterInterface}
	 */
	public static synchronized OntologyModelAdapterInterface getInstance()
	{
		if (theInstance == null) {
			theInstance = new OBOOntologyModelAdapter();
		}
		return theInstance;
	}

	/**
	 * Worker to invoke the {@link UpdateOBOOntologyIndexWorker} in a separate thread
	 */
	private class UpdateOBOOntologyIndexWorker extends SwingWorker<Boolean, Void>
	{

		@Override
		public Boolean doInBackground()
		{
			Collection<OBOClass> presentTerms = TermUtil.getTerms(getOboEditSessionManager().getSession());
			logger.info(String.format("Start re-creating index for ontology loaded in OBO-Edit (%s terms)", String.valueOf(presentTerms.size())));
			OBOOntologyIndexManager indexWriter = OBOOntologyIndexManager.getInstance();
			indexWriter.recreateIndexWith(presentTerms);
			return true;
		}

		@Override
		public void done()
		{
			logger.info("Finished re-createating index for ontology loaded in OBO-Edit");
			updateCandidateTermToOBOTermMapping();
			updateOBOTermsTable();
			service.showProgressDlg(false, null);
		}

	}

	private synchronized void updateOntologyIndex()
	{
		service.showProgressDlg(false, null);
		UpdateOBOOntologyIndexWorker worker = new UpdateOBOOntologyIndexWorker();
		worker.execute();
		service.showProgressDlg(true, "<html>The loaded ontology is getting pre-processed<br>" + " for ontology generation. Please wait...</html>");
	}

	/**
	 * Create all ngrams contained in the texts in the specified list and looking up the term these ngrams match.
	 * 
	 * @param list
	 * @return
	 */
	public List<String> searchForOntologyTermsInStrings(Collection<String> list)
	{

		List<String> queries = new ArrayList<String>();
		List<String> queriesHead = new ArrayList<String>();
		List<String[]> listOfArrays = new ArrayList<String[]>(list.size());

		for (String string : list) {
			listOfArrays.add(string.trim().split(" |-"));
		}

		for (String[] stringArray : listOfArrays) {
			StringBuffer buffer = new StringBuffer();
			for (int i = 0; i < RANGE_FOR_PARENT_IN_DEFINITION; i++) {
				if (i < stringArray.length) {
					buffer.append(stringArray[i]);
					buffer.append(" ");
				}
			}
			queriesHead.add(0, buffer.toString().trim());
		}
		queries.addAll(queriesHead);
		return OBOOntologyIndexManager.getInstance().lookupStrict(queries);
	}

	/*
	 * Instantiate listeners to OBOEdit
	 */
	private HistoryListener historyListener = new HistoryListener()
	{
		public void applied(HistoryAppliedEvent arg0)
		{
			HistoryItem historyItem = arg0.getHistoryItem();
			collectAllHistoryItems(historyItem);
			updateParentAsAnyExistingLinkedObject();
		}

		/**
		 * @param idsUndergoingChange
		 * @param historyItem
		 */
		@SuppressWarnings("unchecked")
		private void collectAllHistoryItems(HistoryItem historyItem)
		{
			if (historyItem instanceof TermMacroHistoryItem) {
				TermMacroHistoryItem termMacroHistoryItem = (TermMacroHistoryItem) historyItem;
				for(HistoryItem hi : termMacroHistoryItem.getHistoryItems()){
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
						                || hi instanceof ChangeSynScopeHistoryItem //
						                || hi instanceof DefinitionChangeHistoryItem) {
							idsUndergoingChange.add(targetID);
						}
						else if (hi instanceof TermMacroHistoryItem) {
							collectAllHistoryItems(hi);
						}
					}
				}
			}
		}

		public void reversed(HistoryAppliedEvent arg0)
		{
			System.err.println("REVERSED");
		}
	};

	/*
	 * Instantiate listeners to OBOEdit
	 */

	private OntologyReloadListener ontologyReloadListener = new OntologyReloadListener()
	{
		public void reload()
		{
			updateOntologyIndex();
			updateCandidateTermToOBOTermMapping();
			updateOBOTermsTable();
		}
	};

	private SelectionListener selectionListener = new SelectionListener()
	{
		public void selectionChanged(SelectionEvent e)
		{
			updateParentAsAnyExistingLinkedObject();
			updateOBOTermsTable();
		}
	};

	private RootChangeListener rootChangeListener = new RootChangeListener()
	{
		public void changeRoot(RootChangeEvent arg0)
		{
			updateOntologyIndex();
			updateCandidateTermToOBOTermMapping();
			updateOBOTermsTable();
		}
	};

	public static String getOboEditVersion()
	{
		return Preferences.getVersion().toString();
	}

	/**
	 * Add listeners from {@link SessionManager}
	 */
	public void addListener()
	{
		SelectionManager.getManager().addSelectionListener(selectionListener);
		SessionManager.getManager().addOntologyReloadListener(ontologyReloadListener);
		SessionManager.getManager().addHistoryListener(historyListener);
		SessionManager.getManager().addRootChangeListener(rootChangeListener);
	}

	/**
	 * Remove listeners from {@link SessionManager}
	 */
	public void removeListeners()
	{
		SelectionManager.getManager().removeSelectionListener(selectionListener);
		SessionManager.getManager().removeOntologyReloadListener(ontologyReloadListener);
		SessionManager.getManager().removeHistoryListener(historyListener);
		SessionManager.getManager().removeRootChangeListener(rootChangeListener);
	}

	/**
	 * Takes the LinkedObject, locates it in the Ontology Tree and adds selected term as child
	 * 
	 * @param includeBranch
	 * @param includeChildren
	 * @param selectedObject , LinkedObject to locate in ontology tree
	 */
	public void addToOntologyAsChildOfLinkedObject(Set<String> parentIds, boolean includeChildren, boolean includeBranch, CandidateTerm selectedCandidateTerm)
	{
		TermMacroHistoryItem changeItem = new TermMacroHistoryItem();
		if (parentIds == null || parentIds.size() == 0) {
			JOptionPane.showMessageDialog(null, "Please select a term to add children or add root edit/Add Root/Add Root");
			return;
		}
		else {
			Iterator<String> selectedParentIdIterator = parentIds.iterator();
			String selectedCandidateTermID = null;
			List<IdentifiedObject> linkedObjectsIfExistLikeSelectedTerm = getLinkedObjectsIfExist(selectedCandidateTerm.getLabel());
			if (selectedParentIdIterator.hasNext()) {
				String parentId = selectedParentIdIterator.next();
				LinkedObject parentLinkedObject = (LinkedObject) sessionManager.getCurrentLinkDatabase().getObject(parentId);
				if (linkedObjectsIfExistLikeSelectedTerm == null) {
					if (TermUtil.isProperty(parentLinkedObject))
						selectedCandidateTermID = getTypeID();
					else
						selectedCandidateTermID = GUIUtil.fetchID(parentLinkedObject);
					if (selectedCandidateTermID == null || selectedCandidateTermID.trim().length() == 0) {
						System.err.println("Could not generate ID! " + "Action cancelled.");
					}
					TermMacroHistoryItem item = createTermInOBOEdit(selectedCandidateTermID, selectedCandidateTerm, parentLinkedObject);
					changeItem.addItem(item);
					TermMacroHistoryItem addTerm = addTermToOBOEdit(selectedCandidateTermID, parentId, OBOProperty.IS_A.getID());
					changeItem.addItem(addTerm);

				}
				else if (linkedObjectsIfExistLikeSelectedTerm.size() == 1) {
					TermMacroHistoryItem addTerm = addTermToOBOEdit(linkedObjectsIfExistLikeSelectedTerm.get(0).getID(), parentId, OBOProperty.IS_A.getID());
					changeItem.addItem(addTerm);
					selectedCandidateTermID = linkedObjectsIfExistLikeSelectedTerm.get(0).getID();
				}
				else {
					logger.error(String.format("There exist already more than one term with name '%s'", selectedCandidateTerm.getLabel()));
				}
			}
			while (selectedParentIdIterator.hasNext()) {
				TermMacroHistoryItem addTerm = addTermToOBOEdit(selectedCandidateTermID, selectedParentIdIterator.next(), OBOProperty.IS_A.getID());
				changeItem.addItem(addTerm);
			}
			sessionManager.apply(changeItem, false);

			// include known children of the added term to the ontology
			if (includeChildren && !includeBranch) {
				addAllChildrenToOBOEditForTermWithID(selectedCandidateTermID, selectedCandidateTerm);
			}
			else if (includeBranch) {
				throw new RuntimeException("feature is not implemented");
			}
		}
	}

	public String getTypeID()
	{
		String id = JOptionPane.showInputDialog("Please input an id");
		if (id == null || id.length() == 0) {
			System.err.println("Cannot create a new type " + "without an id");
			return null;
		}
		else {
			if (sessionManager.getSession().getObject(id) != null) {
				System.err.println("ID " + id + " already in use!");
				return null;
			}
			else if (!IDUtil.isLegalID(id)) {
				System.err.println("ID " + id + " contains illegal characters");
				return null;
			}
		}
		return id;
	}

	private void updateCandidateTermToOBOTermMapping()
	{
		service.getTermsTable().getModel().updatePresentInOntology();
	}

	/**
	 * Feed all terms known to OBOEdit into the {@link OBOTermsTable}
	 * 
	 * @param idsUndergoingChange
	 */
	private void updateParentAsAnyExistingLinkedObject()
	{
		logger.trace("UPDATE updateExistingLinkedObjects() for :" + service.getSelectedCandidateTerm());
		List<LinkedObject> linkedObjects = new ArrayList<LinkedObject>();
		for (IdentifiedObject identifiedObject : sessionManager.getCurrentLinkDatabase().getObjects()) {
			if (identifiedObject instanceof LinkedObject) {
				LinkedObject linkedObject = (LinkedObject) identifiedObject;
				if (!(identifiedObject instanceof OBOClass) || identifiedObject instanceof OBOClass && !((OBOClass) identifiedObject).isObsolete())
					linkedObjects.add(linkedObject);
			}
		}
		service.getOboTermsTable().setTerms(linkedObjects);
	}

	public void updateParentAsTermFromDefinition(CandidateTerm selectedCandidateTerm, TermsTable termsTable, OBOTermsTable oboTermsTable, DefinitionsTable definitionsTable)
	{
		logger.trace("UPDATE TERMS FROM DEFINITION for :" + selectedCandidateTerm);

		if (null == selectedCandidateTerm) {
			int row = termsTable.rowAtPoint(termsTable.getMousePosition());
			selectedCandidateTerm = termsTable.getModel().getTermAt(row);
			logger.warn("Selection lost in terms table, recovered though mouse position");
		}
		// clear
		oboTermsTable.getModel().clearTermsFromDefinitions();

		// process user defined definition
		if (null != selectedCandidateTerm.getUserDefinedDefinition()) {
			List<String> ids = searchForOntologyTermsInStrings(Collections.singletonList(selectedCandidateTerm.getUserDefinedDefinition()));
			int rank = 1;
			for (String id : ids) {
				oboTermsTable.getModel().addFromUserDefinedDefinition(id, rank);
				rank++;
			}
		}

		// process generated definitions
		List<CandidateDefinition> definitions = definitionsTable.getModel().getDefinitions();
		Set<String> definitionStringsToCheck = new HashSet<String>(definitions.size());
		for (CandidateDefinition definition : definitions) {
			String definitionalContext = definition.getDefinitionalContext();
			if (definitionalContext != null && definitionalContext.length() < definition.getDefinition().length()) {
				definitionStringsToCheck.add(definitionalContext);
			}
		}
		if (definitionStringsToCheck.size() > 0) {
			List<String> ids = searchForOntologyTermsInStrings(definitionStringsToCheck);
			int rank = 1;
			for (String id : ids) {
				oboTermsTable.getModel().addFromCandidateDefinition(id, rank);
				rank++;

			}
		}
	}

	/**
	 * Add similar (based on substring inclusion) terms to similiarTermsComboBox based on substring comparison
	 */
	public void updateParentAsSimiliarTerm(CandidateTerm selectedCandidateTerm, OBOTermsTable oboTermsTable)
	{
		logger.trace("UPDATE SIMILAR TERMS for :" + selectedCandidateTerm);

		if (null == selectedCandidateTerm) {
			logger.error("No term selected");
		}
		else {
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
							}
							else if (0 < calcFirstIndexOf(linkedObjectLabel, selectedTermLabel) || 0 < calcFirstIndexOf(selectedTermLabel, linkedObjectLabel)) {
								oboTermsTable.getModel().addSimilarToCandidateTerm(linkedObject);
							}
						}
					}
				}
			}
		}
	}

	/**
	 * Checks for the selected {@link LinkedObject} and updates the GUI
	 */
	public void updateSelectedLinkedObjectAndParents()
	{
		logger.trace("UPDATE SELF updateSelectedLinkedObject() for :" + service.getSelectedCandidateTerm());
		OBOTermsTable oboTermsTable = service.getOboTermsTable();
		// update OBOclass Lookup map
		// updateOBOTermsLookUpTable();

		getSelectionManager();
		Collection<PathCapable> paths = SelectionManager.getGlobalSelection().getAllSelectedObjects();
		oboTermsTable.getModel().clearSelectedLinkedObjects();
		oboTermsTable.getModel().clearParentsOfSelectedLinkedObject();
		Map<String, String> parents = new HashMap<String, String>();
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
				for (Link link : sessionManager.getCurrentLinkDatabase().getParents(linkedObject))
					parents.put(link.getParent().getID(), link.getType().getName());
			}
		}
		// add parents of selected
		// oboTermsTable.getModel().addParentsTermsOfSelectedLinkedObject(parents);
	}

	private void updateOBOTermsTable()
	{
		updateSelectedLinkedObjectAndParents();

		if (service.getSelectedCandidateTerm() != null && service.getOboTermsTable() != null) {
			updateParentAsSimiliarTerm(service.getSelectedCandidateTerm(), service.getOboTermsTable());
		}

		if (service.getSelectedCandidateTerm() != null) {
			updateParentAsTermFromDefinition(service.getSelectedCandidateTerm(), service.getTermsTable(), service.getOboTermsTable(), service.getDefinitionsTable());
		}
	}

	public boolean isSomeOntologyTermSelected()
	{
		return selectedLinkedObject != null;
	}

	/**
     * Select a term in OBO-Edit
     * 
     * @param id
     * @see org.oboedit.gui.components.ontologyGeneration.OntologyModelAdapterInterface#selectOntologyTerm(java.lang.String)
     */
    public void selectOntologyTerm(final String id)
    {
    	SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>()
    	{
    		@Override
    		protected Void doInBackground() throws Exception
    		{
    			IdentifiedObject idObj = sessionManager.getSession().getObject(id);
    			if (idObj != null && idObj instanceof OBOClass) {
    				SelectionManager.selectTerm(null, (LinkedObject) idObj);
    			}
    			return null;
    		}
    	};
    	worker.execute();
    }

	/**
	 * Returns the best matching ontology term id for the given candidate term
	 */
	public String findTermId(CandidateTerm candidateTerm)
	{
		List<String> lexicalRepresentations = new ArrayList<String>(candidateTerm.getLexicalRepresentations());
		List<String> ids = OBOOntologyIndexManager.getInstance().lookupExact(lexicalRepresentations);
		StringWriter writer = new StringWriter();
		for (Iterator<String> iterator = lexicalRepresentations.iterator(); iterator.hasNext();) {
			String string = iterator.next();
			writer.append("'");
			writer.append(string);
			writer.append("'");
			if (iterator.hasNext()) {
				writer.append(",");
			}
		}
		if (ids.size() > 0) {
			// pick the first matching id // TODO support form multiple hits
			return ids.iterator().next();
		}
		return null;
	}

	/**
	 * Returns the {@link LinkedObject}s known in OBOEdit with same label
	 * 
	 * @param label
	 * @return
	 */
	public List<IdentifiedObject> getLinkedObjectsIfExist(String label)
	{
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

	public void getTermsFromOntologyModel()
	{
		updateParentAsAnyExistingLinkedObject();

	}

	/**
	 * TODO describe me!
	 * 
	 * @param candidateTerm
	 * @return
	 * @see org.oboedit.gui.components.ontologyGeneration.OntologyModelAdapterInterface#getLabelForExistingTermIfExists(org.oboedit.gui.components.ontologyGeneration.CandidateTerm)
	 */
	public String getLabelForExistingTermIfExists(CandidateTerm candidateTerm)
	{
		String id = candidateTerm.getExistingIdOfLoadedTerm();
		String label = null;
		if (id != null) {
			OBOClass oboClass = (OBOClass) sessionManager.getCurrentLinkDatabase().getObject(id);
			if (oboClass != null) {
				label = oboClass.getName();
			}
		}
		return label;
	}

	/**
	 * Returns the {@link CandidateDefinition} instantiated with the real definition obtained from the ontology model.
	 * Return <code>null</code> if no definition could be found.
	 * 
	 * @return candidateDefinition
	 */
	public CandidateDefinition getExistingDefinitionIfExists(CandidateTerm candidateTerm)
	{
		// search the term with that id
		String id = findTermId(candidateTerm);
		CandidateDefinition candidateDefinition = null;
		if (id != null) {
			OBOClass oboClass = (OBOClass) sessionManager.getCurrentLinkDatabase().getObject(id);

			// create candidate definition
			candidateDefinition = new CandidateDefinition(0, oboClass.getDefinition(), oboClass.getDefinition());
			if (candidateTerm.getUserDefinedDefinition() != null && !candidateTerm.getUserDefinedDefinition().equals(oboClass.getDefinition())) {
				candidateTerm.setUserDefinedDefinition(candidateTerm.getUserDefinedDefinition() + "\n-----\n" + oboClass.getDefinition());
			}
			else {
				candidateTerm.setUserDefinedDefinition(oboClass.getDefinition());
			}

			// add references to for the definition
			for (Dbxref dbxref : oboClass.getDbxrefs()) {
				candidateDefinition.addAlternativeDefinition(new CandidateDefinition(0, oboClass.getDefinition(), oboClass.getDefinition(),
				    "datatbase=" + dbxref.getDatabase() + " databaseID=" + dbxref.getDatabaseID() + " id=" + dbxref.getID(), null));
			}
		}
		return candidateDefinition;
	}

	/**
	 * Returns the LinkedObject[] instantiated with the real parents obtained from the ontology model. Return
	 * <code>null</code> if no definition could be found.
	 */
	public Map<String, String> getExistingParentsIfExists(CandidateTerm selectedCandidateTerm)
	{
		if (selectedCandidateTerm != null) {
			String id = findTermId(selectedCandidateTerm);
			OBOClass oboClass = (OBOClass) sessionManager.getCurrentLinkDatabase().getObject(id);
			if (oboClass != null) {
				Collection<Link> parentLinks = oboClass.getParents();
				Map<String, String> parents = new HashMap<String, String>();
				for (Link parent : parentLinks)
					parents.put(parent.getParent().getID(), parent.getType().getName());
				return parents;
			}
		}
		return null;
	}

	public void setService(OntologyGenerationComponentService service)
	{
		this.service = service;
		updateOntologyIndex();
	}
	
	public void commitDefinition(CandidateTerm selectedCandidateTerm)
	{
		tryCommitDefinition(selectedCandidateTerm);
	}

	private void tryCommitDefinition(CandidateTerm selectedCandidateTerm)
	{
		String id = findTermId(selectedCandidateTerm);
		TermMacroHistoryItem item = createDefinitionHistoryItem(id, selectedCandidateTerm);
		sessionManager.apply(item, false);
		// commit(set.iterator().next());
		Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
	}

	/**
	 * Add a create new child to the OBO Ontology
	 * 
	 * @param id , the id of the newly added term
	 * @param candidateTerm , the term to add
	 * @param parentLinkedObject , the parent {@link LinkedObject} for the newly added term
	 * @return
	 */
	private TermMacroHistoryItem createTermInOBOEdit(String id, CandidateTerm candidateTerm, LinkedObject parentLinkedObject)
	{
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
	 * Create a history item with the changed definitions from the specified {@link CandidateTerm}
	 * 
	 * @param id , the id of the newly added term
	 * @param candidateTerm , the term to add
	 * @return
	 */
	private TermMacroHistoryItem createDefinitionHistoryItem(String id, CandidateTerm candidateTerm)
	{
		TermMacroHistoryItem item;
		item = new TermMacroHistoryItem("Add definition to term " + id);
		if (candidateTerm.getUserDefinedDefinition() != null) {
			item.addItem(new DefinitionChangeHistoryItem(null, candidateTerm.getUserDefinedDefinition(), id));
			for (CandidateDefinition candidateDef : candidateTerm.getGeneratedDefinitions()) {
				if (candidateDef.isTicked()) {
					for (String url : candidateDef.getUrl()) {
						if (url != null) { // case for all which are generated
							// and not taken from OBOEdit
							item.addItem(new AddDbxrefHistoryItem(id, new DbxrefImpl("URL", url, DbxrefImpl.DEFINITION), true, null));
						}
					}
				}
			}
		}
		return item;
	}

	/**
	 * Add a new child to the OBO Ontology TODO find out, if it is necessary to take care of cycle detection
	 * 
	 * @param id , the id of the newly added term
	 * @param parentLinkedObject , the parent {@link LinkedObject} for the newly added term
	 * @return
	 */
	private TermMacroHistoryItem addTermToOBOEdit(String id, String parentId, String relationType)
	{
		TermMacroHistoryItem item = new TermMacroHistoryItem("Add new child to " + parentId);
		item.addItem(new CreateLinkHistoryItem(id, relationType, parentId));
		// item.setTarget(parentId); // TODO unclear if needed
		// item.setResult(id); // TODO unclear if needed
		return item;
	}

	/**
	 * Commit changes regarding child terms to OBO-Edit
	 * 
	 * @param selectedCandidateTermID
	 * @return
	 */
	private void addAllChildrenToOBOEditForTermWithID(String selectedCandidateTermID, CandidateTerm candidateTerm)
	{
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
				final LinkedObject parentLinkedObject = (LinkedObject) sessionManager.getCurrentLinkDatabase().getObject(selectedCandidateTermID);
				if (linkedObjectsIfExist == null) {
					final String[] lexicalRepresentation = { childLabel };
					final CandidateTerm childCandidateTerm = new CandidateTerm(childLabel, new String[0], lexicalRepresentation, Double.NaN, CandidateTerm.TYPE_OBO_TERM);
					final TermMacroHistoryItem createTermItem = createTermInOBOEdit(relation.getOboChildTermID(), childCandidateTerm, parentLinkedObject);
					changeItem.addItem(createTermItem);
					final TermMacroHistoryItem addTermItem = addTermToOBOEdit(relation.getOboChildTermID(), parentLinkedObject.getID(), relation.getOboRelationShipType());
					changeItem.addItem(addTermItem);
					// accumulate return value
				}
				else {
					for (final IdentifiedObject identifiedObject : linkedObjectsIfExist) {
						final Collection<Link> children = sessionManager.getCurrentLinkDatabase().getChildren(parentLinkedObject);
						if (!children.contains(identifiedObject)) {
							final TermMacroHistoryItem addTermItem = addTermToOBOEdit(identifiedObject.getID(), parentLinkedObject.getID(), relation.getOboRelationShipType());
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

	private SessionManager getOboEditSessionManager()
	{
		return sessionManager;
	}

	private SelectionManager getSelectionManager()
	{
		return SelectionManager.getManager();
	}
}
