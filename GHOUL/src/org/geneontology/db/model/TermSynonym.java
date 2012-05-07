package org.geneontology.db.model;

import java.io.Serializable;

/**
 * The TermDefinition class corresponds to the GO term_definition table.  
 * @author Suzanna Lewis
 *
 */
public class TermSynonym extends GOModel implements Serializable {
		
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public final static String UNKNOWN_SCOPE = "alt_id";
	public final static String RELATED_SYNONYM = "related";
	public final static String EXACT_SYNONYM = "exact";
	public final static String NARROW_SYNONYM = "narrow";
	public final static String BROAD_SYNONYM = "related";

	protected Term term;

	protected String synonym;
	
	protected String alternateAcc;
	
	protected Term synonymType;
	
	protected Term synonymCategory;

	public TermSynonym(){
		final String[] uniqueConstraintFields = {"term", "synonym"};
		this.initUniqueConstraintFields(TermSynonym.class,uniqueConstraintFields);
	}

	public Term getTerm() {
		return term;
	}

	public void setTerm(final Term term) {
		this.term = term;
	}

	public String getSynonym() {
		return synonym;
	}

	public void setSynonym(final String synonym) {
		this.synonym = synonym;
	}

	public String getAlternateAcc() {
		return alternateAcc;
	}

	public void setAlternateAcc(final String alternateAcc) {
		this.alternateAcc = alternateAcc;
	}

	public Term getSynonymType() {
		return synonymType;
	}

	public void setSynonymType(final Term synonymType) {
		this.synonymType = synonymType;
	}

	public Term getSynonymCategory() {
		return synonymCategory;
	}

	public void setSynonymCategory(final Term synonymCategory) {
		this.synonymCategory = synonymCategory;
	}

}