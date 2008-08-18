package org.geneontology.db.model;

/**
 * The TermDefinition class corresponds to the GO term_definition table.  
 * @author Suzanna Lewis
 *
 */
public class TermDefinition extends GOModel {
		
	/** root of graph flag */
	protected int term_id;

	protected String term_definition;
	
	protected int dbxref_id;

	protected String term_comment;
	
	protected String reference;
	
	protected Term term;
	
	public TermDefinition(){
		String[] fieldNames = {"term_id"};
		this.initUniqueConstraintFields(TermDefinition.class, fieldNames);
	}

	public int getTerm_id() {
		return term_id;
	}

	public void setTerm_id(int term_id) {
		this.term_id = term_id;
	}

	public String getTerm_definition() {
		return term_definition;
	}

	public void setTerm_definition(String term_definition) {
		this.term_definition = term_definition;
	}

	public int getDbxref_id() {
		return dbxref_id;
	}

	public void setDbxref_id(int dbxref_id) {
		this.dbxref_id = dbxref_id;
	}

	public String getTerm_comment() {
		return term_comment;
	}

	public void setTerm_comment(String term_comment) {
		this.term_comment = term_comment;
	}

	public String getReference() {
		return reference;
	}

	public void setReference(String reference) {
		this.reference = reference;
	}

	public Term getTerm() {
		return term;
	}

	public void setTerm(Term term) {
		this.term = term;
	}

}