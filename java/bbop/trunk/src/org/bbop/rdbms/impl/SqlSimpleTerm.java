package org.bbop.rdbms.impl;

import org.apache.log4j.*;

public class SqlSimpleTerm extends AbstractRelationalTerm {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SqlSimpleTerm.class);

	protected String term;
	
	public SqlSimpleTerm(String s) {
		this.term = s;
	}
	
	public String toSQL() {
		// TODO Auto-generated method stub
		return term;
	}

}
