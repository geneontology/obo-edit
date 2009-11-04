package org.oboedit.gui.components.ontologyGeneration;

import java.util.List;
import java.util.Map;
import java.util.Set;

public interface OntologyModelAdapterInterface
{

	/*
	 * Needed interface methods
	 */
	public abstract void refillOBOTermsTableWithExistingTerms();

	public abstract String findTermId(CandidateTerm candidateTerm);

	public abstract List<String> lookupOntologyTermIdsFromIndex(List<String> queries);

	public abstract String getLabelForExistingTerm(CandidateTerm candidateTerm);

	public abstract String getDefinitionForExistingTerm(CandidateTerm candidateTerm);

	public abstract Map<String, String> getParentsForExistingTerm(CandidateTerm candidateTerm);
	
	public abstract void selectOntologyTerm(String id);

	public abstract void commitLabel(CandidateTerm candidateTerm);

	public abstract void commitDefinition(CandidateTerm candidateTerm);

	public abstract void commitAddToOntologyAsChildOfLinkedObject(Set<String> parentIds, boolean includeChildren, boolean includeBranch,
			CandidateTerm selectedCandidateTerm);


	/*
	 * Wrapper organizational methods
	 */

	public abstract void setService(OntologyGenerationComponentService service);

	public abstract void removeListeners();

	public abstract void addListener();

	/*
	 * Methods to be revised
	 */
	public abstract void updateParentAsSimiliarTerm(CandidateTerm candidateTerm, OBOTermsTable oboTermsTable);

}
