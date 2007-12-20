package org.bbop.rdbms.impl;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.bbop.rdbms.FromClause;
import org.bbop.rdbms.GroupByClause;
import org.bbop.rdbms.OrderByClause;
import org.bbop.rdbms.RelationalQuery;
import org.bbop.rdbms.SelectClause;
import org.bbop.rdbms.WhereClause;


public class SqlQueryImpl extends AbstractRelationalTerm implements RelationalQuery {

	protected SelectClause selectClause = new SqlSelectClauseImpl();
	protected FromClause fromClause = new SqlFromClauseImpl();
	protected WhereClause whereClause = new SqlWhereClauseImpl();
	protected OrderByClause orderByClause = new SqlOrderByClauseImpl();
	protected GroupByClause groupByClause = new SqlGroupByClauseImpl();
	
	protected Map<String,Integer> aliasNumByTable = new HashMap<String,Integer>();
	
	SqlQueryImpl(SelectClause sc,
			FromClause fc,
			WhereClause wc,
			OrderByClause obc,
			GroupByClause gbc) {
		if (sc != null)
			selectClause = sc;
		if (fc != null)
			fromClause = fc;
		if (wc != null)
			whereClause = wc;
		if (obc != null)
			orderByClause = obc;
		if (gbc != null)
			groupByClause = gbc;
	}
	

	SqlQueryImpl(SelectClause sc,
			FromClause fc,
			WhereClause wc) {
		this(sc,fc,wc,null,null);
	}
	
	SqlQueryImpl(String sc,
			String fc,
			String wc) {
		this(new SqlSelectClauseImpl(sc),
				new SqlFromClauseImpl(fc),
				new SqlWhereClauseImpl(wc),
				null,null);
	}

	public SqlQueryImpl() {
		// TODO Auto-generated constructor stub
	}


	public String toSQL() {
		return 
			getSelectClause().toSQL() + " " +
			getFromClause().toSQL() + " " +
			getWhereClause().toSQL() + " " +
			getOrderByClause().toSQL() + " " +
			getGroupByClause().toSQL();
	}
	

	public SelectClause getSelectClause() {
		return selectClause;
	}

	public void setSelectClause(SelectClause selectClause) {
		this.selectClause = selectClause;
	}
	public void setSelectClause(String selectClause) {
		this.selectClause = new SqlSelectClauseImpl(selectClause);
	}
	
	public void addTable(String tbl) {
		fromClause.addRelation(tbl);
	}
	public void addTable(String tbl, String alias) {
		fromClause.addRelation(tbl + " AS "+alias);
	}
	
	public String addAutoAliasedTable(String tbl) {
		int num = 0;
		if (!aliasNumByTable.containsKey(tbl))
			aliasNumByTable.put(tbl,num);
		else {
			num = aliasNumByTable.get(tbl);
			num++;
			aliasNumByTable.put(tbl,num);
		}
		String aliasTbl = tbl+"__"+num;
		addTable(tbl, aliasTbl);
		return aliasTbl;
	}
	
	

	public FromClause getFromClause() {
		return fromClause;
	}

	public void setFromClause(FromClause fromClause) {
		this.fromClause = fromClause;
	}

	public GroupByClause getGroupByClause() {
		return groupByClause;
	}

	public void setGroupByClause(GroupByClause groupByClause) {
		this.groupByClause = groupByClause;
	}

	public OrderByClause getOrderByClause() {
		return orderByClause;
	}

	public void setOrderByClause(OrderByClause orderByClause) {
		this.orderByClause = orderByClause;
	}

	public WhereClause getWhereClause() {
		return whereClause;
	}

	public void setWhereClause(WhereClause whereClause) {
		this.whereClause = whereClause;
	}
	
	public Collection<Object> getPlaceHolderVals() {
		return whereClause.getPlaceHolderVals();
	}


	public ResultSet execute(Connection conn) throws SQLException {
		String sql = toSQL();
		Logger.getLogger("org.bbop.rdbms").info(toSQL());
	
		PreparedStatement stmt = conn.prepareStatement(sql);
		
		// TODO: there must be a more generic way to do this!!
		int i=1;
		for (Object v : getPlaceHolderVals()) {
			Logger.getLogger("org.bbop.rdbms").info("  ?= "+v);
			if (v instanceof String)
				stmt.setString(i, (String)v);
			else if (v instanceof Boolean)
				stmt.setBoolean(i, (Boolean)v);
			else if (v instanceof Integer)
				stmt.setInt(i, (Integer)v);
			else if (v instanceof Float)
				stmt.setFloat(i, (Float)v);
			else
				throw new SQLException("dunno what to do with "+v);
			// TODO
			i++;
		}
		
		return stmt.executeQuery();
	}




	
}
