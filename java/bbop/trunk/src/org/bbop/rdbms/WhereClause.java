package org.bbop.rdbms;

import java.util.Collection;

public interface WhereClause extends RelationalTerm {
	
	public enum BooleanOperator {AND, OR, NOT}
	
	public ConstraintSet getConstraintSet();
	public void setConstraintSet(ConstraintSet constraintTerm);

	/**
	 * translates to <col> IN (in1, in2, ..)
	 * @param col  
	 * @param in
	 */
	public void addInConstraint(String col, Collection<?> in);

	public void addEqualityConstraint(String col, Object val);
	public void addJoinConstraint(String col1, String col2);
	
	public void addOperatorConstraint(String op, String arg1, Object arg2);
	
	public void addContainsAnyConstraint(String colName,String in);
	public void addContainsAllConstraint(String colName,String in);
	
	/**
	 * @author cartik1.0
	 * @param colName
	 * @param in
	 */
	public void addLikeConstraint(String colName, String in);
	
	public void addConstraint(String constr);

	public Collection<Object> getPlaceHolderVals();

	public void addInConstraint(String string, RelationalQuery subQuery);
	public void addNotInConstraint(String string, RelationalQuery subQuery);

	
	
	public void addDisjunctiveConstraints(WhereClause booleanWhereClause);
	
	public BooleanOperator getOperator() ;

	public void setOperator(BooleanOperator operator);

}
