package org.bbop.rdbms.impl;

import org.bbop.rdbms.OrderByClause;

import org.apache.log4j.*;

public class SqlOrderByClauseImpl extends SqlColumnListClause implements
	OrderByClause {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SqlOrderByClauseImpl.class);

	public String toSQL() {
		// TODO Auto-generated method stub
		if (getColumns().size() == 0) {
			return "";
		}
		return "ORDER BY "+ concat(",", getColumns());
	}

}
