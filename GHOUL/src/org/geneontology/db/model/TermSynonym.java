package org.geneontology.db.model;

import java.io.Serializable;

/**
 * The TermDefinition class corresponds to the GO term_definition table.  
 * @author Suzanna Lewis
 *
 */
public class TermSynonym extends GOModel implements Serializable {
		
	protected Term term;

	protected String synonym;
	
	protected String alternateID;
	
	protected Term synonymType;
	
	protected Term synonymCategory;

	public TermSynonym(){
		String[] uniqueConstraintFields = {"term", "synonym"};
		this.initUniqueConstraintFields(TermSynonym.class,uniqueConstraintFields);
	}

	public Term getTerm() {
		return term;
	}

	public void setTerm(Term term) {
		this.term = term;
	}

	public String getSynonym() {
		return synonym;
	}

	public void setSynonym(String synonym) {
		this.synonym = synonym;
	}

	public String getAlternateID() {
		return alternateID;
	}

	public void setAlternateID(String alternateID) {
		this.alternateID = alternateID;
	}

	public Term getSynonymType() {
		return synonymType;
	}

	public void setSynonymType(Term synonymType) {
		this.synonymType = synonymType;
	}

	public Term getSynonymCategory() {
		return synonymCategory;
	}

	public void setSynonymCategory(Term synonymCategory) {
		this.synonymCategory = synonymCategory;
	}

}