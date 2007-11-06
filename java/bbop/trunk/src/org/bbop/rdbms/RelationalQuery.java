package org.bbop.rdbms;

public interface RelationalQuery extends RelationalTerm {
		
	public void setSelectClause(SelectClause sc);
	public SelectClause getSelectClause();
	
	
}
