package org.bbop.rdbms.impl;

import java.util.LinkedList;

public class SqlConstraintImpl extends AbstractRelationalTerm {

	protected LinkedList<String> constraints = new LinkedList<String>();

	public LinkedList<String> getConstraints() {
		return constraints;
	}

	public boolean isEmpty() {
		return (getConstraints().size() == 0);
	}
	
	public void setConstraints(LinkedList<String> constraints) {
		this.constraints = constraints;
	}

	public void addConstraint(String s) {
		this.constraints.add(s);	
	}

	public String toSQL() {
		return concat(",", constraints);
	}

}
