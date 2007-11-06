package org.bbop.rdbms.impl;

import java.util.LinkedList;

public abstract class SqlColumnListClause extends AbstractRelationalTerm {

	SqlColumnListClause(String s) {
		super();
		addColumn(s);
	}
	SqlColumnListClause() {
		super();
	}
	
	protected LinkedList<String> columns = new LinkedList<String>();
	public LinkedList<String> getColumns() {
		return columns;
	}

	public void setColumns(LinkedList<String> columns) {
		this.columns = columns;
	}
	
	public void addColumn(String column) {
		getColumns().add(column);
	}


}
