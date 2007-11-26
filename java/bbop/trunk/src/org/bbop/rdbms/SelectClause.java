package org.bbop.rdbms;

import java.util.LinkedList;

public interface SelectClause extends RelationalTerm {
	
	public LinkedList<String> getColumns();

	public void setColumns(LinkedList<String> columns);
	
	public void addColumn(String column);


}
