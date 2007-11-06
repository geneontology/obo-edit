package org.bbop.rdbms.impl;

public class SqlSimpleTerm extends AbstractRelationalTerm {

	protected String term;
	
	public SqlSimpleTerm(String s) {
		this.term = s;
	}
	
	public String toSQL() {
		// TODO Auto-generated method stub
		return term;
	}

}
