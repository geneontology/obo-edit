package org.oboedit.gui.components.ontologyGeneration.interfaces;

import java.util.Collection;
import java.util.List;
import java.util.Locale;

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
public interface OntologyModelAdapterInterface<T extends OntologyClassInterface,R> {
	/*
	 * Needed interface methods
	 */

	public abstract String getOntologyEditorVersion();

	public abstract void refillOntologyTermsTableWithExistingOntologyTerms();

	public abstract String findTermId(CandidateTerm candidateTerm);

	public abstract List<String> lookupOntologyTermIdsFromIndex(Collection<String> queries);

	public abstract List<String> lookupOntologyTermIdsFromIndexFuzzy(Collection<String> queries);

	public abstract void selectOntologyTerm(String id);

	public abstract void commitLabel(CandidateTerm candidateTerm);

	public abstract void commitDefinition(CandidateTerm candidateTerm);

	public abstract void commitAddToOntologyAsChildOfOntologyTerm(CandidateTerm selectedCandidateTerm,
			Collection<ParentRelationEntry<R>> parentRelations, boolean includeChildren, boolean includeBranch);

	public abstract String getLanguage();
	
	public abstract Locale getLocale();

	public abstract void setLocale(Locale locale);

	/*
	 * Wrapper organizational methods
	 */

	public abstract void setService(OntologyGenerationComponentServiceInterface<T,R> service);

	public abstract void cleanup();

	public abstract OntologyClassInterface getSelectedOntologyTerm();

	public void registerForOntologyTermSelectionChange(OntologyGenerationComponentServiceInterface<T,R> component);
	
	public abstract OntologyClassInterface getOntologyClassForCandidateTerm(CandidateTerm candidateTerm);
	
	public abstract CandidateTerm getCandidateTermForOntologyClass(OntologyClassInterface ontologyClass);
}
