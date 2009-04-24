package org.oboedit.gui.components.ontologyGeneration;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.obo.datamodel.OBOClass;
import org.obo.datamodel.Synonym;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;

import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupRelation;
import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupTerm;

/**
 * Internal representation of a candidate term
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class CandidateTerm
{
	public static final String TYPE_GENERATED = "GENERATED";
	public static final String TYPE_SYNONYM = "Synonym";
	public static final String TYPE_ABBREVIATION = "Abbreviation";
	public static final String TYPE_OBO_TERM = "OBO_TERM";
	public static final String TYPE_OBO_CHILD = "OBO_CHILD";
	public static final String TYPE_OBO_DESCENDANT = "OBO_DESC";
	public static final String TYPE_LOADED = "LOADED";

	private Set<String> types;
	private Set<String> abbreviations;
	private List<CandidateDefinition> generatedDefinitions;
	private List<OBOLookupTerm> existingOntologyTerms;
	private List<OBOLookupTerm> existingChildTerms;
	private List<OBOLookupRelation> existingChildRelations;

	private String generatedLabel;
	private boolean isTicked;
	private boolean isVisible;
	private Set<String> lexicalRepresentations;
	private double score;
	private String userDefinedDefinition;
	private String userDefinedLabel;

	/**
	 * Constructs a {@link CandidateTerm}
	 */
	public CandidateTerm()
	{
	}

	/**
	 * Constructs a {@link CandidateTerm}
	 * 
	 * @param name
	 * @param abbreviations
	 * @param lexicalRepresentations
	 * @param scr
	 */
	public CandidateTerm(String name, String[] abbreviations, String[] lexicalRepresentations, double scr, String type)
	{
		this.generatedLabel = name;
		this.setUserDefinedLabel(null);
		if (abbreviations != null) {
			this.abbreviations = new HashSet<String>(1);
			Collections.addAll(this.abbreviations, abbreviations);
		}
		if (lexicalRepresentations != null) {
			this.setLexicalRepresentations(new HashSet<String>(1));
			Collections.addAll(this.getLexicalRepresentations(), lexicalRepresentations);
		}
		this.types = new HashSet<String>();
		this.types.add(type);
		this.userDefinedDefinition = null;
		this.generatedDefinitions = null;
		this.score = scr;
		this.isVisible = true;
	}

	public boolean isTicked()
	{
		return this.isTicked;
	}

	public boolean isVisible()
	{
		return isVisible;
	}

	public void addAbbreviation(String abbreviation)
	{
		if (abbreviation != null) {
			if (this.abbreviations == null) {
				this.abbreviations = new HashSet<String>(1);
			}
			this.abbreviations.add(abbreviation);
		}
	}

	public void addLexicalRepresentation(String lexicalRepresentation)
	{
		if (lexicalRepresentation != null) {
			if (this.lexicalRepresentations == null) {
				this.lexicalRepresentations = new HashSet<String>(1);
			}
			this.lexicalRepresentations.add(lexicalRepresentation);
		}
	}

	public void addType(String... type)
	{
		if (type != null) {
			if (this.types == null) {
				this.types = new HashSet<String>();
			}
			for (int i = 0; i < type.length; i++) {
				this.types.add(type[i]);
			}
		}
	}

	/**
	 * @return the types
	 */
	public Set<String> getTypes()
	{
		return this.types;
	}

	/**
	 * @param types the types to set
	 */
	public void setTypes(Set<String> types)
	{
		this.types = types;
	}

	/**
	 * @return the abbreviations
	 */
	public Set<String> getAbbreviations()
	{
		if (this.abbreviations == null) {
			return Collections.emptySet();
		}
		return this.abbreviations;
	}

	/**
	 * @param abbreviations the abbreviations to set
	 */
	public void setAbbreviations(Set<String> abbreviations)
	{
		this.abbreviations = abbreviations;
	}

	/**
	 * @return the generatedDefinitions
	 */
	public List<CandidateDefinition> getGeneratedDefinitions()
	{
		if (this.generatedDefinitions == null) {
			return Collections.emptyList();
		}
		return this.generatedDefinitions;
	}

	/**
	 * @param generatedDefinitions the generatedDefinitions to set
	 */
	public void setGeneratedDefinitions(List<CandidateDefinition> generatedDefinitions)
	{
		this.generatedDefinitions = generatedDefinitions;
	}

	/**
	 * @return the existingOntologyTerms
	 */
	public List<OBOLookupTerm> getExistingOntologyTerms()
	{
		return this.existingOntologyTerms;
	}

	/**
	 * @param existingOntologyTerms the existingOntologyTerms to set
	 */
	public void setExistingOntologyTerms(List<OBOLookupTerm> existingOntologyTerms)
	{
		this.existingOntologyTerms = existingOntologyTerms;
	}

	/**
	 * @return the existingChildTerms
	 */
	public List<OBOLookupTerm> getExistingChildTerms()
	{
		return this.existingChildTerms;
	}

	/**
	 * @param existingChildTerms the existingChildTerms to set
	 */
	public void setExistingChildTerms(List<OBOLookupTerm> existingChildTerms)
	{
		this.existingChildTerms = existingChildTerms;
	}

	/**
	 * @return the existingChildRelations
	 */
	public List<OBOLookupRelation> getExistingChildRelations()
	{
		return this.existingChildRelations;
	}

	/**
	 * @param existingChildRelations the existingChildRelations to set
	 */
	public void setExistingChildRelations(List<OBOLookupRelation> existingChildRelations)
	{
		this.existingChildRelations = existingChildRelations;
	}

	/**
	 * @return the generatedLabel
	 */
	public String getGeneratedLabel()
	{
		return this.generatedLabel;
	}

	/**
	 * @param generatedLabel the generatedLabel to set
	 */
	public void setGeneratedLabel(String generatedLabel)
	{
		this.generatedLabel = generatedLabel;
	}

	/**
	 * @return the lexicalRepresentations
	 */
	public Set<String> getLexicalRepresentations()
	{
		if (this.lexicalRepresentations == null) {
			return Collections.emptySet();
		}
		return this.lexicalRepresentations;
	}

	/**
	 * @param lexicalRepresentations the lexicalRepresentations to set
	 */
	public void setLexicalRepresentations(Set<String> lexicalRepresentations)
	{
		this.lexicalRepresentations = lexicalRepresentations;
	}

	/**
	 * @return the score
	 */
	public double getScore()
	{
		return this.score;
	}

	/**
	 * @param score the score to set
	 */
	public void setScore(double score)
	{
		this.score = score;
	}

	/**
	 * @return the userDefinedDefinition
	 */
	public String getUserDefinedDefinition()
	{
		return this.userDefinedDefinition;
	}

	/**
	 * @param userDefinedDefinition the userDefinedDefinition to set
	 */
	public void setUserDefinedDefinition(String userDefinedDefinition)
	{
		this.userDefinedDefinition = userDefinedDefinition;
	}

	/**
	 * @return the userDefinedLabel
	 */
	public String getUserDefinedLabel()
	{
		return this.userDefinedLabel;
	}

	/**
	 * @param userDefinedLabel the userDefinedLabel to set
	 */
	public void setUserDefinedLabel(String userDefinedLabel)
	{
		this.userDefinedLabel = userDefinedLabel;
	}

	/**
	 * @param isTicked the isTicked to set
	 */
	public void setTicked(boolean isTicked)
	{
		this.isTicked = isTicked;
	}

	/**
	 * @param isVisible the isVisible to set
	 */
	public void setVisible(boolean isVisible)
	{
		this.isVisible = isVisible;
	}

	/**
	 * String representation of a {@link CandidateTerm}
	 * 
	 * @return
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString()
	{
		StringBuffer buffer = new StringBuffer();
		buffer.append(this.getGeneratedLabel());
		if (this.getAbbreviations().size() > 0) {
			buffer.append(" [");
			for (String abbrString : this.getAbbreviations()) {
				buffer.append(abbrString);
			}
			buffer.append("]");
		}
		return buffer.toString();
	}

	/**
	 * Returns the label of a {@link CandidateTerm}
     * @return the current label
     */
    public String getLabel()
    {
	    if (userDefinedLabel != null) {
	    	return userDefinedLabel;
	    } else {
	    	return generatedLabel;
	    }
    }
    
    /**
     * Checks if the term is already present in the current ontology (as a term or a synonym)
     * 
     * @return <code>true</code>, if the term is present, <code>false</code> otherwise
     */
    public boolean isPresentInOntology()
    {
    	SessionManager sessionManager = SessionManager.getManager();
    	Collection<OBOClass> presentTerms = TermUtil.getTerms(sessionManager.getSession());
    	for (OBOClass term : presentTerms) {
    		if (term.getName().equals(this.getLabel())) {
    			return true;
    		}
    		for (Synonym synonym : term.getSynonyms()) {
    			if (synonym.getText().equals(this.getLabel())) {
    				return true;
    			}
    		}
    	}
    	return false;
    }

}
