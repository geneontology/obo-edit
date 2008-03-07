package org.bbop.rdbms;

import java.util.Collection;

public interface WhereClause extends RelationalTerm {
	
	public enum BooleanOperator {AND, OR, NOT}
	
	public ConstraintSet getConstraintSet();
	public void setConstraintSet(ConstraintSet constraintTerm);

	public void addInConstraint(String s, Collection<?> in);

	public void addEqualityConstraint(String col, Object val);
	public void addJoinConstraint(String col1, String col2);
	
	public void addOperatorConstraint(String op, String arg1, Object arg2);
	
	public void addConstraint(String constr);

	public Collection<Object> getPlaceHolderVals();

	public void addInConstraint(String string, RelationalQuery subQuery);
	public void addNotInConstraint(String string, RelationalQuery subQuery);

	public void addDisjunctiveConstraints(WhereClause booleanWhereClause);
	
	public BooleanOperator getOperator() ;

	public void setOperator(BooleanOperator operator);

}
