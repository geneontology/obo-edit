package org.bbop.rdbms.impl;

import org.bbop.rdbms.SelectClause;

import org.apache.log4j.*;

public class SqlSelectClauseImpl extends SqlColumnListClause implements SelectClause {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SqlSelectClauseImpl.class);

	public SqlSelectClauseImpl(String s) {
		super(s);
		// TODO Auto-generated constructor stub
	}
	public SqlSelectClauseImpl() {
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
