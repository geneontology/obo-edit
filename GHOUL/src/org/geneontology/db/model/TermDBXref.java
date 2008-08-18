package org.geneontology.db.model;

import java.io.Serializable;

public class TermDBXref  extends GOModel implements Serializable {
	
	private Term term;
	private DBXref dbxref;
	private int is_for_definition;
	
	
	public TermDBXref(){
		String[] fieldNames = {"term", "dbxref", "is_for_definition"};
		this.initUniqueConstraintFields(TermDBXref.class, fieldNames);
	}
	
	public Term getTerm() {
		return term;
	}

	public void setTerm(Term term) {
		this.term = term;
	}

	public DBXref getDbxref() {
		return dbxref;
	}
	
	public void setDbxref(DBXref dbxref) {
		this.dbxref = dbxref;
	}
	
	public int getIs_for_definition() {
		return is_for_definition;
	}

	
	public void setIs_for_definition(int is_for_definition) {
		this.is_for_definition = is_for_definition;
	}
		
}