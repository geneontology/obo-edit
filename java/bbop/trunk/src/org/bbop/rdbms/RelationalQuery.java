package org.bbop.rdbms;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;

public interface RelationalQuery extends RelationalTerm {
		
	public void setSelectClause(SelectClause sc);
	public SelectClause getSelectClause();

	public void setFromClause(FromClause sc);
	public FromClause getFromClause();
	
	public void addTable(String tbl);


	public GroupByClause getGroupByClause();
	public void setGroupByClause(GroupByClause groupByClause);

	public OrderByClause getOrderByClause();
	public void setOrderByClause(OrderByClause orderByClause);

	public WhereClause getWhereClause();
	public void setWhereClause(WhereClause whereClause);
	
	public Collection<Object> getPlaceHolderVals();
	
	public ResultSet execute(Connection conn) throws SQLException;
}
