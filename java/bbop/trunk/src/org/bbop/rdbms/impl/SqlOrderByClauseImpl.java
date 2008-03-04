package org.bbop.rdbms.impl;

import org.bbop.rdbms.OrderByClause;

public class SqlOrderByClauseImpl extends SqlColumnListClause implements
		OrderByClause {

	public String toSQL() {
		// TODO Auto-generated method stub
		if (getColumns().size() == 0) {
			return "";
		}
		return "ORDER BY "+ concat(",", getColumns());
	}

}
