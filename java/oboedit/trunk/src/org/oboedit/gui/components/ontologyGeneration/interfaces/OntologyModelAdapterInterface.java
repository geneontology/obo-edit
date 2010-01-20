package org.oboedit.gui.components.ontologyGeneration.interfaces;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.obo.datamodel.LinkedObject;
import org.oboedit.gui.components.ontologyGeneration.CandidateTerm;
import org.oboedit.gui.components.ontologyGeneration.oboAdapter.ParentRelationEntry;

public interface OntologyModelAdapterInterface<T, R>
{

	/*
	 * Needed interface methods
	 */

	public abstract void refillOBOTermsTableWithExistingTerms();

	public abstract String findTermId(CandidateTerm candidateTerm);

	public abstract List<String> lookupOntologyTermIdsFromIndex(Collection<String> queries);

	public abstract List<String> lookupOntologyTermIdsFromIndexFuzzy(Collection<String> queries);

	public abstract String getLabelForExistingTerm(CandidateTerm candidateTerm);

	public abstract List<String> getSynonymsForOntologyTerm(LinkedObject linkedObject);

	public abstract String getDefinitionForExistingTerm(CandidateTerm candidateTerm);

	public abstract Map<String, String> getParentsForExistingTerm(CandidateTerm candidateTerm);

	public abstract void selectOntologyTerm(String id);

	public abstract void commitLabel(CandidateTerm candidateTerm);

	public abstract void commitDefinition(CandidateTerm candidateTerm);

	public abstract void commitAddToOntologyAsChildOfLinkedObject(CandidateTerm selectedCandidateTerm, Collection<ParentRelationEntry<R>> parentRelations, boolean includeChildren,
	    boolean includeBranch);

	/*
	 * Wrapper organizational methods
	 */

	public abstract void setService(OntologyGenerationComponentServiceInterface<T, R> service);

	public abstract void cleanup();

}
