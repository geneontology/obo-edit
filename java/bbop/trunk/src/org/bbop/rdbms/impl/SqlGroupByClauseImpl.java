package org.bbop.rdbms.impl;

import org.bbop.rdbms.GroupByClause;

public class SqlGroupByClauseImpl extends SqlColumnListClause implements
		GroupByClause {

	public String toSQL() {
		// TODO Auto-generated method stub
		if (getColumns().size() == 0) {
			return "";
		}
		return "GROUP BY "+ concat(",", getColumns());
	}

}
