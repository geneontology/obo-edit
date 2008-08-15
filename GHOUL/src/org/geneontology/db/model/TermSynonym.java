package org.geneontology.db.model;

/**
 * The TermDefinition class corresponds to the GO term_definition table.  
 * @author Suzanna Lewis
 *
 */
public class TermSynonym extends GOModel {
		
	protected int term_id;

	protected String synonym;
	
	protected String alternateID;
	
	protected Term synonymType;
	
	protected Term synonymCategory;

	public TermSynonym(){
	}

	public int getTerm_id() {
		return term_id;
	}

	public void setTerm_id(int term_id) {
		this.term_id = term_id;
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