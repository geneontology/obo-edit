package org.bbop.rdbms.impl;

import org.bbop.rdbms.FromClause;
import org.bbop.rdbms.GroupByClause;
import org.bbop.rdbms.OrderByClause;
import org.bbop.rdbms.Query;
import org.bbop.rdbms.SelectClause;
import org.bbop.rdbms.WhereClause;


public class SqlQueryImpl extends AbstractRelationalTerm implements Query {

	protected SelectClause selectClause = new SqlSelectClauseImpl();
	protected FromClause fromClause = new SqlFromClauseImpl();
	protected WhereClause whereClause = new SqlWhereClauseImpl();
	protected OrderByClause orderByClause = new SqlOrderByClauseImpl();
	protected GroupByClause groupByClause = new SqlGroupByClauseImpl();
	
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
	


	public void addTable(String tbl) {
		return;
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
	
}
