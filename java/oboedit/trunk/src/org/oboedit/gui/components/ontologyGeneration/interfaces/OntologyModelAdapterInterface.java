package org.oboedit.gui.components.ontologyGeneration.interfaces;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.oboedit.gui.components.ontologyGeneration.CandidateTerm;

public interface OntologyModelAdapterInterface
{

	/*
	 * Needed interface methods
	 */
	
	public abstract void refillOBOTermsTableWithExistingTerms();

	public abstract String findTermId(CandidateTerm candidateTerm);

	public abstract List<String> lookupOntologyTermIdsFromIndex(Collection<String> queries);

	public abstract List<String> lookupOntologyTermIdsFromIndexFuzzy(Collection<String> queries);

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

	public abstract void addListener();
	
	public abstract void setService(OntologyGenerationComponentServiceInterface service);
	
	public abstract void removeListeners();



	/*
	 * Methods to be revised
	 */
	
//	public abstract void updateParentAsSimiliarTerm(CandidateTerm candidateTerm, OBOTermsTable oboTermsTable);

}
