package org.bbop.rdbms;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;

/**
 * Representation of a query over a relational resource
 * @author cjm
 *
 */
public interface RelationalQuery extends RelationalTerm {
		
	public void setSelectClause(SelectClause sc);
	public void setSelectClause(String string);
	public SelectClause getSelectClause();

	public void setFromClause(FromClause sc);
	public FromClause getFromClause();
	
	public void addTable(String tbl);
	public void addTable(String tbl, String alias);
	public String addAutoAliasedTable(String tbl);
	
	public String addAutoAliasedTable(String tbl, String preferredAlias);


	public GroupByClause getGroupByClause();
	public void setGroupByClause(GroupByClause groupByClause);
	public void setGroupByClause(String groupByClause);

	public OrderByClause getOrderByClause();
	public void setOrderByClause(OrderByClause orderByClause);

	public WhereClause getWhereClause();
	public void setWhereClause(WhereClause whereClause);
	
	public Collection<Object> getPlaceHolderVals();
	
	public ResultSet execute(Connection conn) throws SQLException;
	/**
	 * 
	 * @param joinCol
	 * @param table
	 * @return
	 */
	public String getTableAliasReferencedInJoin(String joinCol, String table);
}
