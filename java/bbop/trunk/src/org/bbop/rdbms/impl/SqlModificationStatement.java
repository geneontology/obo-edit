package org.bbop.rdbms.impl;

import java.util.HashMap;

import org.bbop.rdbms.ConstraintSet;

public abstract class SqlModificationStatement extends AbstractRelationalTerm {

	protected String table;
	protected ConstraintSet constraintTerm = new SqlConstraintSetImpl();
	protected HashMap<String,Object> colVals = new HashMap<String,Object>();
	
	public String getTable() {
		return table;
	}

	public void setTable(String table) {
		this.table = table;
	}

	public ConstraintSet getConstraintTerm() {
		return constraintTerm;
	}
	public void setConstraintTerm(ConstraintSet constraintTerm) {
		this.constraintTerm = constraintTerm;
	}
	

}
