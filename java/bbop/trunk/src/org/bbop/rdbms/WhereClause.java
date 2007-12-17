package org.bbop.rdbms;

import java.util.Collection;

public interface WhereClause extends RelationalTerm {

	public void addInConstraint(String s, Collection in);

	public void addEqualityConstraint(String col, Object val);
	
	public void addOperatorConstraint(String op, String arg1, String arg2);
	
	public void addConstraint(String constr);

	public Collection<Object> getPlaceHolderVals();

	public void addInConstraint(String string, RelationalQuery subQuery);
}
