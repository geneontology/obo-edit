package org.bbop.rdbms.impl;

import java.util.HashMap;

public abstract class SqlModificationStatement extends AbstractRelationalTerm {

	protected String table;
	protected SqlConstraintImpl constraintTerm = new SqlConstraintImpl();
	protected HashMap<String,Object> colVals = new HashMap<String,Object>();
	
	public String getTable() {
		return table;
	}

	public void setTable(String table) {
		this.table = table;
	}

	public SqlConstraintImpl getConstraintTerm() {
		return constraintTerm;
	}
	public void setConstraintTerm(SqlConstraintImpl constraintTerm) {
		this.constraintTerm = constraintTerm;
	}
	

}
