package org.bbop.rdbms.impl;

import java.util.Collection;
import java.util.LinkedList;

import org.bbop.rdbms.ConstraintSet;
import org.bbop.rdbms.FromClause;
import org.bbop.rdbms.RelationalQuery;
import org.bbop.rdbms.RelationalTerm;
import org.bbop.rdbms.WhereClause;
import org.bbop.rdbms.WhereClause.BooleanOperator;

public class SqlWhereClauseImpl extends AbstractRelationalTerm implements
		WhereClause {

	protected ConstraintSet constraintSet = new SqlConstraintSetImpl();
	protected Collection<Object> placeHolderVals = new LinkedList<Object>();
	
	SqlWhereClauseImpl(String s) {
		super();
		constraintSet.addConstraint(s);
	}
	public SqlWhereClauseImpl() {
		super();
	}

	
	public ConstraintSet getConstraintSet() {
		return constraintSet;
	}
	public void setConstraintSet(ConstraintSet constraintTerm) {
		this.constraintSet = constraintTerm;
	}
	
	public void addConstraint(String s) {
		this.constraintSet.addConstraint(s);
	}
	
	public void addOperatorConstraint(String op, String arg1, String arg2) {
		this.constraintSet.addConstraint(arg1 + " " + op + " ?");
		placeHolderVals.add(arg2);		
	}
	
	
	public void addInConstraint(String s, Collection in) {
		this.constraintSet.addConstraint(s + " IN (" + concatValues(",",in) + ")");
	}
	
	public void addInConstraint(String s, RelationalQuery subQuery) {
		// TODO: delayed access?
		this.constraintSet.addConstraint(s + " IN (" + subQuery.toSQL() + ")");
		placeHolderVals.addAll(subQuery.getPlaceHolderVals());
	}
	public void addNotInConstraint(String s, RelationalQuery subQuery) {
		// TODO: delayed access?
		this.constraintSet.addConstraint(s + " NOT IN (" + subQuery.toSQL() + ")");
		placeHolderVals.addAll(subQuery.getPlaceHolderVals());
	}
	public void addEqualityConstraint(String col, Object val) {
		this.constraintSet.addConstraint(col + " = ?");
		placeHolderVals.add(val);
	}
	
	
	public BooleanOperator getOperator() {
		return constraintSet.getOperator();
	}
	public void setOperator(BooleanOperator operator) {
		constraintSet.setOperator(operator);
	}
	public String toSQL() {
		if (constraintSet.isEmpty()) {
			return "";
		}
		return "WHERE " + constraintSet.toSQL();
	}
	
	public Collection<Object> getPlaceHolderVals() {
		return placeHolderVals;
	}
	public void setPlaceHolderVals(Collection<Object> placeHolderVals) {
		this.placeHolderVals = placeHolderVals;
	}
	
	public void addDisjunctiveConstraints(WhereClause subClause) {
		//subClause.setOperator(BooleanOperator.OR);
		//constraintTerm.addConstraint(subClause);
		subClause.setOperator(BooleanOperator.OR);
		this.constraintSet.addConstraint("(" + subClause.getConstraintSet().toSQL() + ")");
		placeHolderVals.addAll(subClause.getPlaceHolderVals());
	}


	


}
