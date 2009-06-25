package org.oboedit.gui.components.ontologyGeneration;

import java.util.Map;
import java.util.Set;

public interface OntologyModelAdapterInterface {

	public abstract void setService(OntologyGenerationComponentService service);

	public abstract void removeListeners();

	public abstract void addListener();

	public abstract void updateParentAsSimiliarTerm(CandidateTerm selectedCandidateTerm, OBOTermsTable oboTermsTable);

	public abstract void updateParentAsTermFromDefinition(CandidateTerm selectedCandidateTerm, TermsTable termsTable,
			OBOTermsTable oboTermsTable, DefinitionsTable definitionsTable);

	public abstract void updateSelectedLinkedObjectAndParents();

	public abstract String findTermId(CandidateTerm selectedCandidateTerm);
		
    public abstract String getLabelForExistingTermIfExists(CandidateTerm candidateTerm);

    public abstract CandidateDefinition getExistingDefinitionIfExists(CandidateTerm candidateTerm);
	
	public abstract Map<String, String> getExistingParentsIfExists(CandidateTerm selectedCandidateTerm);

	public abstract void addToOntologyAsChildOfLinkedObject(Set<String> parentIds, boolean includeChildren,
			boolean includeBranch, CandidateTerm selectedCandidateTerm);

	public abstract void commitDefinition(CandidateTerm selectedCandidateTerm);

	public abstract void getTermsFromOntologyModel();

	public abstract boolean isSomeOntologyTermSelected();
	
	public abstract void selectOntologyTerm(String id);
}
