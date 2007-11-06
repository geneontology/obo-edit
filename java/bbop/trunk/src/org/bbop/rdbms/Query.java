package org.bbop.rdbms;

public interface Query extends RelationalTerm {
		
	public void setSelectClause(SelectClause sc);
	public SelectClause getSelectClause();
	
	
}
