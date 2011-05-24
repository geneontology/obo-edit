package org.oboedit.gui.components.ontologyGeneration.interfaces;

import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.oboedit.gui.components.ontologyGeneration.CandidateTerm;

/**
 * Interface holding common methods to enable generic implemetation of the
 * ontology generation tool in Protege and OBO-Edit
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2010
 * 
 * @param <T>
 *            concept type of the external ontology model
 * @param <R>
 *            relationShipType used in the external ontology model
 */
public interface OntologyModelAdapterInterface<T, R> {

	public abstract String getOntologyEditorVersion();

	public abstract void refillOntologyTermsTableWithExistingOntologyTerms();

	public abstract String findTermId(CandidateTerm candidateTerm);

	public abstract List<String> lookupOntologyTermIdsFromIndex(Collection<String> queries);

	public abstract List<String> lookupOntologyTermIdsFromIndexFuzzy(Collection<String> queries);

	public abstract String getLabelForCandidateTermAsExistingOntologyTerm(CandidateTerm candidateTerm);

	public abstract List<String> getSynonymsForOntologyTerm(T linkedObject);
	
	public abstract String getDefinitionForCandidateTermAsExistingOntologyTerm(CandidateTerm candidateTerm);

	public abstract Map<String, String> getParentsForExistingTerm(CandidateTerm candidateTerm);

	public abstract void selectOntologyTerm(String id);

	public abstract void commitLabel(CandidateTerm candidateTerm);

	public abstract void commitDefinition(CandidateTerm candidateTerm);

	public abstract void commitAddToOntologyAsChildOfOntologyTerm(CandidateTerm selectedCandidateTerm,
			Collection<ParentRelationEntry<R>> parentRelations, boolean includeChildren, boolean includeBranch);

	public abstract Locale getLocale();

	public abstract void setLocale(Locale locale);

	/*
	 * Wrapper organizational methods
	 */

	public abstract void setService(OntologyGenerationComponentServiceInterface<T, R> service);

	public abstract void cleanup();


}
