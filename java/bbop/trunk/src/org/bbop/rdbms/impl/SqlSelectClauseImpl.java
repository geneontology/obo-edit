package org.bbop.rdbms.impl;

import java.util.Collection;
import java.util.LinkedList;

import org.bbop.rdbms.SelectClause;

public class SqlSelectClauseImpl extends SqlColumnListClause implements SelectClause {

	SqlSelectClauseImpl(String s) {
		super(s);
		// TODO Auto-generated constructor stub
	}
	SqlSelectClauseImpl() {
		super();
	}

	protected boolean isDistinct;

	public boolean isDistinct() {
		return isDistinct;
	}

	public void setDistinct(boolean isDistinct) {
		this.isDistinct = isDistinct;
	}

	public String toSQL() {
		// TODO Auto-generated method stub
		return "SELECT " + (isDistinct ? "DISTINCT " : "") + 
		(getColumns().size() == 0 ? "*" : concat(",", columns));
	}


}
