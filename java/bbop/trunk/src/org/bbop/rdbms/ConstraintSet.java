package org.bbop.rdbms;

import java.util.LinkedList;

import org.bbop.rdbms.WhereClause.BooleanOperator;


public interface ConstraintSet {

	public abstract LinkedList<RelationalTerm> getConstraints();

	public abstract BooleanOperator getOperator();

	public abstract void setOperator(BooleanOperator operator);

	public abstract boolean isEmpty();

	public abstract void setConstraints(LinkedList<String> terms);

	public abstract void addConstraint(String s);

	public abstract void addEqualityConstraint(String s, Object o);

	public abstract String toSQL();

	public abstract void addConstraint(WhereClause subClause);

}