package org.bbop.rdbms.impl;

import java.util.LinkedList;

import org.bbop.rdbms.ColumnListClause;

public abstract class SqlColumnListClause extends AbstractRelationalTerm implements ColumnListClause  {

	SqlColumnListClause(String s) {
		super();
		addColumn(s);
	}
	SqlColumnListClause() {
		super();
	}
	
	protected LinkedList<String> columns = new LinkedList<String>();
	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ColumnListClause#getColumns()
	 */
	public LinkedList<String> getColumns() {
		return columns;
	}

	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ColumnListClause#setColumns(java.util.LinkedList)
	 */
	public void setColumns(LinkedList<String> columns) {
		this.columns = columns;
	}
	
	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ColumnListClause#addColumn(java.lang.String)
	 */
	public void addColumn(String column) {
		getColumns().add(column);
	}


}
