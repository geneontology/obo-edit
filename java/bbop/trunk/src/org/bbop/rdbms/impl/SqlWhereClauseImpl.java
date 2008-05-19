package org.bbop.rdbms.impl;

import java.util.Collection;
import java.util.LinkedList;

import org.bbop.rdbms.ConstraintSet;
import org.bbop.rdbms.RelationalQuery;
import org.bbop.rdbms.WhereClause;


import org.apache.log4j.*;

public class SqlWhereClauseImpl extends AbstractRelationalTerm implements
	WhereClause {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SqlWhereClauseImpl.class);

	protected ConstraintSet constraintSet = new SqlConstraintSetImpl();
	protected Collection<Object> placeHolderVals = new LinkedList<Object>();
	
	public SqlWhereClauseImpl(String s) {
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
	
	public void addOperatorConstraint(String op, String arg1, Object arg2) {
		this.constraintSet.addConstraint(arg1 + " " + op + " ?");
		placeHolderVals.add(arg2);		
	}
	
	
	public void addInConstraint(String s, Collection<?> in) {
		this.constraintSet.addConstraint(s + " IN (" + concatValues(",",in) + ")");
	}
	
	public void addContainsAllConstraint(String colName,String in){
		
		String[] parts = in.split("\\s");
		
		String constraints = "(";
		for (int i=0;i<parts.length;i++){
			if (parts[i].trim().length()>0){
				if (i>0){
					constraints += " AND ";
				}
				constraints += (lower(colName) + " LIKE " + lower("?"));
				placeHolderVals.add(("%" + parts[i].trim() + "%"));
			}
		}
		constraints += ")";
		this.constraintSet.addConstraint(constraints);
		
	}
	
	public void addContainsAnyConstraint(String colName,String in){
		String[] parts = in.split("\\s");
		
		String constraints = "(";
		for (int i=0;i<parts.length;i++){
			if (parts[i].trim().length()>0){
				if (i>0){
					constraints += " OR ";
				}
				constraints += (lower(colName) + " LIKE " + lower("?"));
				placeHolderVals.add(("%" + parts[i].trim() + "%"));
			}
		}
		constraints += ")";
		this.constraintSet.addConstraint(constraints);
	}
	
	private String lower(String s){
		return "lower(" + s + ")";
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
	
	public void addJoinConstraint(String col1, String col2) {
		constraintSet.addConstraint(col1+" = "+col2);
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
