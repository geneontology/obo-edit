package org.bbop.rdbms.impl;

import org.bbop.rdbms.InsertStatement;

import org.apache.log4j.*;

public class SqlInsertStatement extends SqlModificationStatement implements
	InsertStatement {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SqlInsertStatement.class);

	public String toSQL() {
		// TODO Auto-generated method stub
		return "INSERT INTO "+getTable();
	}

}
