package org.oboedit.gui.components.ontologyGeneration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyClassInterface;

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
	public static final String TYPE_GENERATED_SIBLING = "Sibling";
	public static final String TYPE_SYNONYM = "Synonym";
	public static final String TYPE_ABBREVIATION = "Abbreviation";
	public static final String TYPE_OBO_TERM = "OBO_TERM";
	public static final String TYPE_OBO_CHILD = "OBO_CHILD";
	public static final String TYPE_OBO_DESCENDANT = "OBO_DESC";
	public static final String TYPE_LOADED = "LOADED";
	public static final String TYPE_MANUAL = "MANUAL";

	private Set<String> types;
	private Set<String> abbreviations;
	
	private List<OBOLookupTerm> existingLookupTerms;
	private List<OBOLookupTerm> existingLookupChildTerms;
	private List<OBOLookupRelation> existingLookupChildRelations;
	
	private OntologyClassInterface existingOntologyClass;

	private String generatedLabel;
	private boolean isTicked;
	private boolean isVisible;
	private Set<String> lexicalRepresentations;
	private double score;
	private String userDefinedDefinition;
	private String userDefinedLabel;
    private String existingLabel;

    private List<CandidateDefinition> generatedDefinitions;

	/**
	 * Constructs a {@link CandidateTerm}
	 */
	public CandidateTerm()
	{
		this.isVisible = true;
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
		this.score = scr;
		this.isVisible = true;
	}
	
	public CandidateTerm(String name, Set<String> abbreviations, Set<String> lexicalRepresentations, double score, String type)
	{
		this();
		this.generatedLabel = name;
		this.abbreviations = abbreviations;
		this.lexicalRepresentations = lexicalRepresentations;
		this.score = score;
		this.types = new HashSet<String>(1);
		this.types.add(type);
		this.userDefinedDefinition = null;
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
		return Collections.unmodifiableSet(this.types);
	}

	/**
	 * @param types the types to set
	 */
	public void setTypes(Set<String> types)
	{
		this.types = new HashSet<String>(types.size());
		this.types.addAll(types);
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
		this.abbreviations = new HashSet<String>(abbreviations.size());
		this.abbreviations.addAll(abbreviations);
	}

	/**
	 * @return the generatedDefinitions
	 */
	public List<CandidateDefinition> getGeneratedDefinitions()
	{
		List<CandidateDefinition> list = new ArrayList<CandidateDefinition>();
		if (null != generatedDefinitions) {
			list.addAll(generatedDefinitions);
		}
		return list;
	}

	/**
	 * @param generatedDefinitions the generatedDefinitions to set
	 */
	public void setGeneratedDefinitions(List<CandidateDefinition> generatedDefinitions)
	{
		if (generatedDefinitions == null) {
			this.generatedDefinitions = new ArrayList<CandidateDefinition>();
			return;
		}
		this.generatedDefinitions = new ArrayList<CandidateDefinition>(generatedDefinitions);
	}

	/**
	 * @return the existingOntologyTerms
	 */
	public List<OBOLookupTerm> getExistingLookupTerms()
	{
		if (this.existingLookupTerms != null) {
			return Collections.unmodifiableList(this.existingLookupTerms);
		}
		return null;
	}

	/**
	 * @param existingLookupOntologyTerms the existingOntologyTerms to set
	 */
	public void setExistingLookupOntologyTerms(List<OBOLookupTerm> existingLookupOntologyTerms)
	{
		if (existingLookupOntologyTerms == null){
			this.existingLookupTerms = new ArrayList<OBOLookupTerm>();
			return;
		}
		this.existingLookupTerms = new ArrayList<OBOLookupTerm>(existingLookupOntologyTerms);
	}

	/**
	 * @return the existingChildTerms
	 */
	public List<OBOLookupTerm> getExistingLookupChildTerms()
	{
		if (this.existingLookupChildTerms != null) {
			return Collections.unmodifiableList(this.existingLookupChildTerms);
		}
		return null;
	}

	/**
	 * @param existingLookupChildTerms the existingChildTerms to set
	 */
	public void setExistingLookupChildTerms(List<OBOLookupTerm> existingLookupChildTerms)
	{
		if (existingLookupChildTerms == null) {
			this.existingLookupChildTerms = new ArrayList<OBOLookupTerm>();
			return;
		}
		this.existingLookupChildTerms = new ArrayList<OBOLookupTerm>(existingLookupChildTerms);
	}

	/**java.util.Collection
	 * @return the existingChildRelations
	 */
	public List<OBOLookupRelation> getExistingLookupChildRelations()
	{
		if (this.existingLookupChildRelations != null) {
			return Collections.unmodifiableList(this.existingLookupChildRelations);
		}
		return null;
	}

	/**
	 * @param existingLookupChildRelations the existingChildRelations to set
	 */
	public void setExistingLookupChildRelations(List<OBOLookupRelation> existingLookupChildRelations)
	{
		this.existingLookupChildRelations = new ArrayList<OBOLookupRelation>(existingLookupChildRelations);
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
		if (lexicalRepresentations == null) {
			this.lexicalRepresentations = new HashSet<String>();
			return;
		}
		this.lexicalRepresentations = new HashSet<String>(lexicalRepresentations);
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
    	String label = null;
	    if (userDefinedLabel != null) {
	    	label = userDefinedLabel;
	    } else if (isInLoadedOntology()) {
	    	label = existingLabel;
	    }
	    if (label == null) {
	    	label = generatedLabel;
	    }
    	return label;
    }

	/**
     * Returns true if the candidate term could be found in the loaded ontology
     * @return
     */
    public boolean isInLoadedOntology()
    {
	    return (this.getExistingOntologyClass() != null);
    }

	/**
     * @param ontologyClass the existingIdOfLoadedTerm to set
     */
    public void setExistingOntologyClass(OntologyClassInterface ontologyClass)
    {
	    this.existingOntologyClass = ontologyClass;
    }

	/**
     * @return the existingIdOfLoadedTerm
     */
    public OntologyClassInterface getExistingOntologyClass()
    {
	    return existingOntologyClass;
    }

    /**
     * @return the existingLabel
     */
    public String getExistingLabel()
    {
        return this.existingLabel;
    }

    /**
     * @param existingLabel the existingLabel to set
     */
    public void setExistingLabel(String existingLabel)
    {
        this.existingLabel = existingLabel;
    }

}
