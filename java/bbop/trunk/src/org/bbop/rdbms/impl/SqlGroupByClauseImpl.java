package org.bbop.rdbms.impl;

import org.bbop.rdbms.GroupByClause;

import org.apache.log4j.*;

public class SqlGroupByClauseImpl extends SqlColumnListClause implements
	GroupByClause {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SqlGroupByClauseImpl.class);

	public SqlGroupByClauseImpl() {
		super();
	}

	public SqlGroupByClauseImpl(String s) {
		super(s);
	}

	public String toSQL() {
		// TODO Auto-generated method stub
		if (getColumns().size() == 0) {
			return "";
		}
		return "GROUP BY "+ concat(",", getColumns());
	}

}
