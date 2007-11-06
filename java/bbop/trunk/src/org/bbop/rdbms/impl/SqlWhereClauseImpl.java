package org.bbop.rdbms.impl;

import java.util.Collection;
import java.util.LinkedList;

import org.bbop.rdbms.FromClause;
import org.bbop.rdbms.WhereClause;

public class SqlWhereClauseImpl extends AbstractRelationalTerm implements
		WhereClause {

	protected SqlConstraintImpl constraintTerm = new SqlConstraintImpl();
	protected Collection<Object> placeHolderVals = new LinkedList<Object>();
	
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
	
	public void addConstraint(String s) {
		this.constraintTerm.addConstraint(s);
	}
	
	public void addInConstraint(String s, Collection in) {
		this.constraintTerm.addConstraint(s + " IN (" + concatValues(",",in) + ")");
	}
	public void addEqualityConstraint(String col, Object val) {
		this.constraintTerm.addConstraint(col + " = ?");
		placeHolderVals.add(val);
	}
	
	
	public String toSQL() {
		if (constraintTerm.isEmpty()) {
			return "";
		}
		return "WHERE " + constraintTerm.toSQL();
	}
	
	public Collection<Object> getPlaceHolderVals() {
		return placeHolderVals;
	}
	public void setPlaceHolderVals(Collection<Object> placeHolderVals) {
		this.placeHolderVals = placeHolderVals;
	}
	


}
