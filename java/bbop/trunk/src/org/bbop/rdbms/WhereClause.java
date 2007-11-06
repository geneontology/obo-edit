package org.bbop.rdbms;

import java.util.Collection;

public interface WhereClause extends RelationalTerm {

	public void addInConstraint(String s, Collection in);

	public void addEqualityConstraint(String col, Object val);

	public Collection<Object> getPlaceHolderVals();
}
