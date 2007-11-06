package org.bbop.rdbms.impl;

import java.util.LinkedList;

import org.bbop.rdbms.FromClause;
import org.bbop.rdbms.WhereClause;

public class SqlWhereClauseImpl extends AbstractRelationalTerm implements
		WhereClause {

	protected SqlConstraintImpl constraintTerm = new SqlConstraintImpl();
	
	SqlWhereClauseImpl(String s) {
		super();
		constraintTerm.addConstraint(s);
	}
	SqlWhereClauseImpl() {
		super();
	}

	
	public SqlConstraintImpl getConstraintTerm() {
		return constraintTerm;
	}
	public void setConstraintTerm(SqlConstraintImpl constraintTerm) {
		this.constraintTerm = constraintTerm;
	}
	
	public String toSQL() {
		if (constraintTerm.isEmpty()) {
			return "";
		}
		return "WHERE " + constraintTerm.toSQL();
	}


}
