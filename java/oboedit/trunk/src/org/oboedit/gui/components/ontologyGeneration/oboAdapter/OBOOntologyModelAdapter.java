package org.oboedit.gui.components.ontologyGeneration.oboAdapter;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.swing.JOptionPane;

import org.apache.log4j.Logger;
import org.jdesktop.swingx.util.SwingWorker;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.impl.DbxrefImpl;
import org.obo.history.AddDbxrefHistoryItem;
import org.obo.history.AddSynonymHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.CreateObjectHistoryItem;
import org.obo.history.DefinitionChangeHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.NameChangeHistoryItem;
import org.obo.history.NamespaceHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;
import org.obo.util.VersionNumber;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.components.ontologyGeneration.CandidateDefinition;
import org.oboedit.gui.components.ontologyGeneration.CandidateTerm;
import org.oboedit.gui.components.ontologyGeneration.interfaces.AbstractOntologyTermsTable;
import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyGenerationComponentServiceInterface;
import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyModelAdapterInterface;
import org.oboedit.gui.components.ontologyGeneration.interfaces.ParentRelationEntry;
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

/**
 * Adapter to OBO.Edit Ontology Model
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2009
 */
public class OBOOntologyModelAdapter implements OntologyModelAdapterInterface<LinkedObject, OBOProperty> {
	private static final long serialVersionUID = -6595210039049170585L;
	private static final Logger logger = Logger.getLogger(OBOOntologyModelAdapter.class);
	private static OBOOntologyModelAdapter theInstance;

	/**
	 * @return the singleton instance of {@link OntologyModelAdapterInterface}
	 */
	public static synchronized OntologyModelAdapterInterface<LinkedObject, OBOProperty> getInstance() {
		if (theInstance == null) {
			theInstance = new OBOOntologyModelAdapter();
		}
		return theInstance;
	}

	private OntologyGenerationComponentServiceInterface<LinkedObject, OBOProperty> service;
	private SessionManager sessionManager;
	private Locale locale;
	private String language;

	/*
	 * Instantiate listeners to OBOEdit
	 */
	private HistoryListener historyListener = new HistoryListener() {
		public void applied(HistoryAppliedEvent arg0) {
			refillOntologyTermsTableWithExistingOntologyTerms();
		}

		public void reversed(HistoryAppliedEvent arg0) {
			logger.error("REVERSED");
		}
	};

	private OntologyReloadListener ontologyReloadListener = new OntologyReloadListener() {
		public void reload() {
			updateAllOnOntologyChange(true);
		}
	};

	private SelectionListener selectionListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			Collection<LinkedObject> terms = SelectionManager.getManager().getSelection().getTerms();
			if (terms.size() == 1) {
				LinkedObject source = terms.iterator().next();
				service.updateOnOntologyTermSelectionChange(source.getName());
			}
		}
	};

	private RootChangeListener rootChangeListener = new RootChangeListener() {
		public void changeRoot(RootChangeEvent arg0) {
			updateAllOnOntologyChange(true);
		}
	};

	private List<OBOProperty> relationTypes = null;

	/**
	 * private constructor of the singleton
	 */
	private OBOOntologyModelAdapter() {
		locale = Locale.getDefault();
		if (locale != null) {
			this.setLocale(locale);
		} else {
			this.setLocale(new Locale("en", "us"));
		}
		addListeners();

		sessionManager = SessionManager.getManager();
	}

	/**
	 * Cleanup
	 */
	public void cleanup() {
		removeListeners();
	}

	/**
	 * Takes the LinkedObject, locates it in the Ontology Tree and adds selected
	 * term as child
	 * 
	 * @param candidateTerm
	 * @param parentRelations
	 * @param includeChildren
	 * @param includeBranch
	 */
	public void commitAddToOntologyAsChildOfOntologyTerm(CandidateTerm selectedCandidateTerm,
			Collection<ParentRelationEntry<OBOProperty>> parentRelations, boolean includeChildren, boolean includeBranch) {
		if (parentRelations == null || parentRelations.size() == 0) {
			JOptionPane.showMessageDialog(null,
					"Please select a term to add children or add root edit/Add Root/Add Root");
			return;
		}

		String message = null;
		if (includeChildren && selectedCandidateTerm.getExistingChildTerms() != null
				&& selectedCandidateTerm.getExistingChildTerms().size() > 0) {
			message = String.format("Add term '%s' and its %s known child terms?", selectedCandidateTerm.getLabel(),
					String.valueOf(selectedCandidateTerm.getExistingChildTerms().size()));
		} else {
			message = String.format("Add term '%s'?", selectedCandidateTerm.getLabel());
		}

		int result = JOptionPane.showConfirmDialog(null, message);
		if (result > 0) {
			return;
		}
		tryCommitAddTerm(selectedCandidateTerm, parentRelations, includeChildren, includeBranch);
	}

	/**
	 * Commit {@link CandidateDefinition} available for the
	 * {@link CandidateTerm} to the OBO-Edit ontology model.
	 * 
	 * @param candidateTerm
	 */
	public void commitDefinition(CandidateTerm selectedCandidateTerm) {
		if (null != selectedCandidateTerm.getExistingIdOfLoadedTerm()) {
			tryCommitDefinition(selectedCandidateTerm);
		} else {
			logger.info("Term does not exist, no definition commited");
		}
	}

	public void commitLabel(CandidateTerm candidateTerm) {
		if (null != candidateTerm.getExistingIdOfLoadedTerm()) {
			tryCommitLabel(candidateTerm);
		} else {
			logger.info("Term does not exist, no label commited");
		}
	}

	/**
	 * Returns the best matching ontology term id for the given candidate term
	 */
	public String findTermId(CandidateTerm candidateTerm) {
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
			// pick the first matching id
			// TODO support form multiple hits
			return ids.iterator().next();
		}
		return null;
	}

	public String getDefinitionForCandidateTermAsExistingOntologyTerm(CandidateTerm candidateTerm) {

		if (candidateTerm != null) {
			String id = findTermId(candidateTerm);
			if (id != null) {
				OBOClass oboClass = (OBOClass) sessionManager.getCurrentLinkDatabase().getObject(id);
				if (oboClass != null) {
					return oboClass.getDefinition();
				}
			}
		}
		return null;
	}

	public String getLabelForCandidateTermAsExistingOntologyTerm(CandidateTerm candidateTerm) {
		if (candidateTerm != null) {
			String id = candidateTerm.getExistingIdOfLoadedTerm();
			if (id != null) {
				OBOClass oboClass = (OBOClass) sessionManager.getCurrentLinkDatabase().getObject(id);
				if (oboClass != null) {
					return oboClass.getName();
				}
			}
		}
		return null;
	}

	public String getLanguage() {
		return this.language;
	}

	public Locale getLocale() {
		return this.locale;
	}

	/**
	 * DOC Edit description
	 * 
	 * @return
	 * @see org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyModelAdapterInterface#getOntologyEditorVersion()
	 */
	public String getOntologyEditorVersion() {
		VersionNumber version = Preferences.getVersion();
		return version.getMajorVersion() + "." + version.getMinorVersion() + "." + version.getBetaVersion() + " "
				+ version.getRCVersion();
	}

	public Map<String, String> getParentsForExistingTerm(CandidateTerm candidateTerm) {
		if (candidateTerm != null) {
			String id = findTermId(candidateTerm);
			if (id != null) {
				OBOClass oboClass = (OBOClass) sessionManager.getCurrentLinkDatabase().getObject(id);
				if (oboClass != null) {
					Collection<Link> parentLinks = oboClass.getParents();
					Map<String, String> parents = new HashMap<String, String>();
					for (Link parent : parentLinks)
						parents.put(parent.getParent().getID(), parent.getType().getName());
					return parents;
				}
			}
		}
		return null;
	}

	public List<String> getSynonymsForOntologyTerm(LinkedObject linkedObject) {
		if (linkedObject instanceof SynonymedObject) {
			Set<Synonym> synonyms = ((SynonymedObject) linkedObject).getSynonyms();
			if (synonyms != null && !synonyms.isEmpty()) {
				List<String> list = new ArrayList<String>(synonyms.size());
				for (Synonym synonym : synonyms) {
					list.add(synonym.getText());
				}
				return list;
			}
		}
		return Collections.emptyList();
	}

	public List<String> lookupOntologyTermIdsFromIndex(Collection<String> queryStrings) {
		return OBOOntologyIndexManager.getInstance().lookupExact(queryStrings);
	}

	public List<String> lookupOntologyTermIdsFromIndexFuzzy(Collection<String> queryStrings) {
		return OBOOntologyIndexManager.getInstance().lookupFuzzy(queryStrings);

	}

	/**
	 * Feed all terms known to OBOEdit into the
	 * {@link AbstractOntologyTermsTable}
	 */
	public void refillOntologyTermsTableWithExistingOntologyTerms() {
		logger.trace("REFILL ontology terms table");
		List<LinkedObject> linkedObjects = new ArrayList<LinkedObject>();
		for (IdentifiedObject identifiedObject : sessionManager.getCurrentLinkDatabase().getObjects()) {
			if (identifiedObject instanceof LinkedObject) {
				LinkedObject linkedObject = (LinkedObject) identifiedObject;
				if (!(identifiedObject instanceof OBOClass) || identifiedObject instanceof OBOClass
						&& !((OBOClass) identifiedObject).isObsolete())
					linkedObjects.add(linkedObject);
			}
		}
		// set terms to ontology terms table in step 3
		updateOntologyIndex();
		updateRelationTypes();
		service.getOntologyTermsTable().setTerms(linkedObjects);
	}

	/**
	 * Select a term in OBO-Edit
	 * 
	 * @param id
	 * @see org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyModelAdapterInterface#selectOntologyTerm(java.lang.String)
	 */
	public void selectOntologyTerm(final String id) {
		SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
			@Override
			protected Void doInBackground() throws Exception {
				IdentifiedObject idObj = sessionManager.getSession().getObject(id);
				if (idObj != null && idObj instanceof OBOClass) {
					SelectionManager.selectTerm(null, (LinkedObject) idObj);
				}
				return null;
			}
		};
		worker.execute();
	}

	public void setLocale(Locale locale) {
		this.locale = locale;

		this.language = locale.getLanguage();

		if (service != null) {
			service.getOntologyTermsTable().getModel().setLanguage(language);
			updateAllOnOntologyChange(true);
		}
	}

	public void setService(OntologyGenerationComponentServiceInterface<LinkedObject, OBOProperty> service) {
		this.service = service;
	}

	/**
	 * Add listeners from {@link SessionManager}
	 */
	private void addListeners() {
		SelectionManager.getManager().addSelectionListener(selectionListener);
		SessionManager.getManager().addOntologyReloadListener(ontologyReloadListener);
		SessionManager.getManager().addHistoryListener(historyListener);
		SessionManager.getManager().addRootChangeListener(rootChangeListener);
	}

	/**
	 * Create a history item for adding a new term
	 * 
	 * @param candidateTerm
	 * @param id
	 * @param typeID
	 * @param ns
	 * @return
	 */
	private List<HistoryItem> createAddTermHistoryItem(CandidateTerm candidateTerm, String id, String typeID,
			Namespace ns) {
		List<HistoryItem> item = new ArrayList<HistoryItem>();
		String label = candidateTerm.getLabel();
		item.add(new CreateObjectHistoryItem(id, typeID));
		item.add(new NameChangeHistoryItem(label, id, id));

		if (ns == null)
			ns = sessionManager.getSession().getDefaultNamespace();
		if (ns != null) {
			item.add(new NamespaceHistoryItem(null, ns, id));
		}

		item.add(createDefinitionHistoryItem(id, candidateTerm));

		// add synonyms
		for (String abbreviation : candidateTerm.getAbbreviations()) {
			AddSynonymHistoryItem addSynonymHistoryItem = new AddSynonymHistoryItem(id, abbreviation);
			item.add(addSynonymHistoryItem);
		}
		for (String lex : candidateTerm.getLexicalRepresentations()) {
			if (!lex.equals(label)) {
				item.add(new AddSynonymHistoryItem(id, lex));
			}
		}
		List<OBOLookupTerm> childrenExistingExtern = candidateTerm.getExistingOntologyTerms();
		if (null != childrenExistingExtern && !childrenExistingExtern.isEmpty()) {
			// use OBO terms
			Iterator<OBOLookupTerm> iterator = childrenExistingExtern.iterator();
			while (iterator.hasNext()) {
				OBOLookupTerm refTerm = iterator.next();
				AddDbxrefHistoryItem addDbxrefHistoryItem = new AddDbxrefHistoryItem(id, new DbxrefImpl("REF_OBO",
						refTerm.getOboID()), false, refTerm.getLabel());
				item.add(addDbxrefHistoryItem);
			}
		}
		return item;
	}

	/**
	 * Add a new child to the OBO Ontology
	 * 
	 * @param id
	 *            , the id of the child
	 * @param parentId
	 *            , the id of the parent
	 * @param relationId
	 *            ,the id of the relation
	 * @return
	 */
	private List<HistoryItem> createdAddLinkHistoryItem(String id, String parentId, String relationId) {
		String relationIdToUse = getValidRelation(relationId);
		if (null != relationIdToUse) {
			List<HistoryItem> item = new ArrayList<HistoryItem>();
			item.add(new CreateLinkHistoryItem(id, relationIdToUse, parentId));
			// item.setTarget(parentId); // TODO unclear if needed
			// item.setResult(id); // TODO unclear if needed
			return item;
		} else {
			logger.error("Relation type not supported: " + relationIdToUse);
			return null;
		}
	}

	/**
	 * Create a history item with the changed definitions from the specified
	 * {@link CandidateTerm}
	 * 
	 * @param id
	 *            , the id of the newly added term
	 * @param candidateTerm
	 *            , the term to add
	 * @return
	 */
	private TermMacroHistoryItem createDefinitionHistoryItem(String id, CandidateTerm candidateTerm) {
		TermMacroHistoryItem item;
		item = new TermMacroHistoryItem("Add definition to term " + id);
		if (candidateTerm.getUserDefinedDefinition() != null) {
			item.addItem(new DefinitionChangeHistoryItem(null, candidateTerm.getUserDefinedDefinition(), id));
			for (CandidateDefinition candidateDef : candidateTerm.getGeneratedDefinitions()) {
				if (candidateDef.isTicked()) {
					for (String url : candidateDef.getUrls()) {
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
	 * Create a history item with the changed the label of a term from the
	 * specified {@link CandidateTerm}
	 * 
	 * @param id
	 *            , the id of the newly added term
	 * @param candidateTerm
	 *            , the term to add
	 * @return
	 */
	private HistoryItem createLabelHistoryItem(String id, CandidateTerm candidateTerm) {
		IdentifiedObject object = getOboEditSessionManager().getCurrentFullLinkDatabase().getObject(id);
		return new NameChangeHistoryItem(candidateTerm.getUserDefinedLabel(), object.getName(), id);
	}

	/**
	 * Returns the {@link LinkedObject}s known in OBOEdit with same label
	 * 
	 * @param label
	 * @return
	 */
	private List<IdentifiedObject> getLinkedObjectsIfExist(String label) {
		List<IdentifiedObject> list = new ArrayList<IdentifiedObject>(10);
		Collection<IdentifiedObject> objects = sessionManager.getCurrentLinkDatabase().getObjects();
		for (IdentifiedObject identifiedObject : objects) {
			String identifiedObjectLabel = identifiedObject.getName();
			if (identifiedObjectLabel != null && identifiedObjectLabel.equalsIgnoreCase(label)) {
				list.add(identifiedObject);
			}
		}
		return list;
	}

	private SessionManager getOboEditSessionManager() {
		return sessionManager;
	}

	private String getTypeID() {
		String id = JOptionPane.showInputDialog("Please input an id");
		if (id == null || id.length() == 0) {
			logger.error("Cannot create a new type " + "without an id");
			return null;
		} else {
			if (sessionManager.getSession().getObject(id) != null) {
				logger.error("ID " + id + " already in use!");
				return null;
			} else if (!IDUtil.isLegalID(id)) {
				logger.error("ID " + id + " contains illegal characters");
				return null;
			}
		}
		return id;
	}

	private String getValidRelation(String relationId) {
		String relationIdToUse = null;
		boolean isValidRelation = false;
		for (OBOProperty property : relationTypes) {
			if (property.getID().equals(relationId)) {
				relationIdToUse = property.getID();
				isValidRelation = true;
				break;
			}
		}
		if (!isValidRelation) {
			for (OBOProperty property : relationTypes) {
				if (property.getName().equals(relationId)) {
					relationIdToUse = property.getID();
					isValidRelation = true;
				}
			}
		}
		return relationIdToUse;
	}

	/**
	 * Remove listeners from {@link SessionManager}
	 */
	private void removeListeners() {
		SelectionManager.getManager().removeSelectionListener(selectionListener);
		SessionManager.getManager().removeOntologyReloadListener(ontologyReloadListener);
		SessionManager.getManager().removeHistoryListener(historyListener);
		SessionManager.getManager().removeRootChangeListener(rootChangeListener);
	}

	private void tryCommitAddAllChildren(String id, CandidateTerm candidateTerm) {
		TermMacroHistoryItem changeItem = new TermMacroHistoryItem();
		// setup the map of id to name mappings
		Map<String, Set<String>> nameToIds = new HashMap<String, Set<String>>();
		Map<String, String> idToRelation = new HashMap<String, String>();

		List<OBOLookupTerm> existingChildTerms = candidateTerm.getExistingChildTerms();

		for (OBOLookupTerm lookupTerm : existingChildTerms) {
			List<String> knownIds = lookupOntologyTermIdsFromIndex(Collections.singleton(lookupTerm.getLabel()));
			if (knownIds.isEmpty() && lookupTerm.getLabel() != null && lookupTerm.getLabel().length() > 0) {
				if (!nameToIds.containsKey(lookupTerm.getLabel())) {
					HashSet<String> set = new HashSet<String>();
					set.add(lookupTerm.getOboID());
					nameToIds.put(lookupTerm.getLabel(), set);
				} else {
					nameToIds.get(lookupTerm.getLabel()).add(lookupTerm.getOboID());
				}
			}
		}
		for (OBOLookupRelation relation : candidateTerm.getExistingChildRelations()) {
			idToRelation.put(relation.getOboChildTermID(), relation.getOboRelationShipType());
		}

		for (String label : nameToIds.keySet()) {
			Set<String> set = nameToIds.get(label);
			final CandidateTerm childCandidateTerm = new CandidateTerm(label, new String[0], new String[0], Double.NaN,
					CandidateTerm.TYPE_OBO_TERM);

			String relString = idToRelation.get(set.iterator().next());
			String relationId = getValidRelation(relString);
			if (null == relationId) {
				tryCommitCreateNewRelation(relString);
				relationId = getValidRelation(relString);
			}
			OBOProperty relation = (OBOProperty) sessionManager.getCurrentFullLinkDatabase().getObject(relationId);
			ParentRelationEntry<OBOProperty> parentRelationEntry = new ParentRelationEntry<OBOProperty>(id, relation);
			tryCommitAddTerm(childCandidateTerm, Collections.singletonList(parentRelationEntry), false, false);
		}
		sessionManager.apply(changeItem, false);
	}

	private void tryCommitAddTerm(CandidateTerm candidateTerm,
			Collection<ParentRelationEntry<OBOProperty>> parentRelations, boolean includeChildren, boolean includeBranch) {
		List<HistoryItem> items = new ArrayList<HistoryItem>();
		List<IdentifiedObject> termsExistingIntern = getLinkedObjectsIfExist(candidateTerm.getLabel());
		Iterator<ParentRelationEntry<OBOProperty>> parentExistingInternIterator = parentRelations.iterator();
		String childTermID = null;
		while (parentExistingInternIterator.hasNext()) {
			ParentRelationEntry<OBOProperty> entry = parentExistingInternIterator.next();
			String parentLinkedObjectId = entry.getParentTermId();
			OBOProperty relationShipType = entry.getRelationShipType();

			LinkedObject parentLinkedObject = (LinkedObject) sessionManager.getCurrentFullLinkDatabase().getObject(
					parentLinkedObjectId);
			// check for existing term in OBO-Edit
			if (termsExistingIntern.isEmpty()) {
				// check for existing term
				if (TermUtil.isProperty(parentLinkedObject)) {
					childTermID = getTypeID();
				} else {
					childTermID = GUIUtil.fetchID(parentLinkedObject);
				}
				if (childTermID == null || childTermID.trim().length() == 0) {
					logger.error("Could not generate ID! " + "Action cancelled.");
				}
				List<HistoryItem> addTermItems = createAddTermHistoryItem(candidateTerm, childTermID,
						parentLinkedObject.getType().getID(), parentLinkedObject.getNamespace());
				items.addAll(addTermItems);
			} else if (termsExistingIntern.size() == 1) {
				childTermID = termsExistingIntern.get(0).getID();
			} else {
				throw new RuntimeException("Multiple terms with this label exist.");
			}
			if (childTermID != null) {
				List<HistoryItem> addRelationItems = createdAddLinkHistoryItem(childTermID, parentLinkedObject.getID(),
						relationShipType.getID());
				items.addAll(addRelationItems);
			}
		}
		TermMacroHistoryItem historyItem = new TermMacroHistoryItem(items);
		sessionManager.apply(historyItem, false);
		// include known children of the added term to the ontology
		if (childTermID != null) {
			if (includeChildren && !includeBranch) {
				tryCommitAddAllChildren(childTermID, candidateTerm);
			} else if (includeBranch) {
				throw new RuntimeException("feature is not implemented");
			}
		}
	}

	private void tryCommitCreateNewRelation(String relString) {
		TermMacroHistoryItem item = new TermMacroHistoryItem("Created new relation:" + relString);
		String typeID = OBOClass.OBO_PROPERTY.getID();
		item.addItem(new CreateObjectHistoryItem(relString, typeID));
		item.addItem(new NameChangeHistoryItem(relString, relString, relString));

		Namespace ns = SessionManager.getManager().getSession().getDefaultNamespace();
		if (ns != null)
			item.addItem(new NamespaceHistoryItem(null, ns, relString));
		sessionManager.apply(item);
	}

	private void tryCommitDefinition(CandidateTerm candidateTerm) {
		String id = candidateTerm.getExistingIdOfLoadedTerm();
		TermMacroHistoryItem item = createDefinitionHistoryItem(id, candidateTerm);
		sessionManager.apply(item, false);
		Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
	}

	private void tryCommitLabel(CandidateTerm candidateTerm) {
		String id = candidateTerm.getExistingIdOfLoadedTerm();
		HistoryItem item = createLabelHistoryItem(id, candidateTerm);
		sessionManager.apply(item, false);
		Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
	}

	private void updateAllOnOntologyChange(boolean externalOntologyModelChanged) {
		if (externalOntologyModelChanged) {
			refillOntologyTermsTableWithExistingOntologyTerms();
		}
		service.updateAllDependedOnSelectedCandidateTerm();
	}

	private synchronized void updateOntologyIndex() {
		service.showProgressDlg(false, null);
		SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
			@Override
			public Boolean doInBackground() {
				Collection<OBOClass> presentTerms = TermUtil.getTerms(getOboEditSessionManager().getSession());
				logger.info(String.format("Start re-creating index for ontology loaded in OBO-Edit (%s terms)",
						String.valueOf(presentTerms.size())));
				OBOOntologyIndexManager indexWriter = OBOOntologyIndexManager.getInstance();
				indexWriter.recreateIndexWith(presentTerms);
				return true;
			}

			@Override
			public void done() {
				logger.info("Finished re-createating index for ontology loaded in OBO-Edit");
				service.getTermsTable().getModel().updatePresentInOntology();
				service.showProgressDlg(false, null);
			}
		};
		worker.execute();
		service.showProgressDlg(true, "<html>The loaded ontology is getting pre-processed<br>"
				+ " for ontology generation. Please wait...</html>");
	}

	private synchronized void updateRelationTypes() {
		List<OBOProperty> list = new ArrayList<OBOProperty>(10);
		for (OBOProperty property : OBOProperty.BUILTIN_TYPES) {
			list.add(property);
		}
		LinkDatabase currentLinkDatabase = sessionManager.getCurrentLinkDatabase();
		Collection<OBOProperty> properties = currentLinkDatabase.getProperties();
		list.addAll(properties);
		OBOProperty[] array = new OBOProperty[list.size()];
		this.relationTypes = list;
		if (service != null)
			service.getOntologyTermsTable().getModel().setRelationTypes(list.toArray(array));
	}

}
