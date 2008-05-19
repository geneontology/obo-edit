package org.bbop.rdbms;

import java.util.LinkedList;

public interface ColumnListClause extends RelationalTerm {

	public abstract LinkedList<String> getColumns();

	public abstract void setColumns(LinkedList<String> columns);

	public abstract void addColumn(String column);

}
