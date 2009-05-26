package org.oboedit.gui.components.ontologyGeneration;

import java.util.Collection;
import java.util.Set;

import org.obo.datamodel.LinkedObject;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.IDUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;


public interface OBOOntologyModelAdapterInterface {

	public abstract void removeListeners();
	public abstract void addListener();
	public abstract void getTermsFromOntologyModel(Collection<String> idsUndergoingChange);
	public abstract void getTermsFromOntologyModel();
	public abstract TermMacroHistoryItem commitTermDefinitionsToOntologyModel(String id, CandidateTerm candidateTerm);
	public abstract TermMacroHistoryItem commitTermDefinitionsToOntologyModel(String id, CandidateTerm candidateTerm,
			LinkedObject parentLinkedObject);
	
	public abstract void commitDefinition(CandidateTerm selectedCandidateTerm);
	public abstract SessionManager getSessionManager();
	
	public abstract void updateParentAsSimiliarTerm(CandidateTerm selectedCandidateTerm, OBOTermsTable oboTermsTable);
	public abstract void updateOBOTermsLookUpTable();
	public abstract IDUtil getIDUtil();
	public abstract void addToOntologyAsChildOfLinkedObject(Set<String> parentIds, boolean includeChildren,
			boolean includeBranch, CandidateTerm selectedCandidateTerm);
	public abstract void setService(OntologyGenerationComponentService service);
	public abstract LinkedObject getSelectedLinkedObject();
	public abstract SelectionManager getSelectionManager();
	public abstract void updateParentAsTermFromDefinition(CandidateTerm selectedCandidateTerm, TermsTable termsTable,
			OBOTermsTable oboTermsTable, DefinitionsTable definitionsTable);
	
	public abstract void updateSelectedLinkedObjectAndParents();
}
