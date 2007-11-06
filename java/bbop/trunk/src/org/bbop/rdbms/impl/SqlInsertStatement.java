package org.bbop.rdbms.impl;

import org.bbop.rdbms.InsertStatement;

public class SqlInsertStatement extends SqlModificationStatement implements
		InsertStatement {

	public String toSQL() {
		// TODO Auto-generated method stub
		return "INSERT INTO "+getTable();
	}

}
