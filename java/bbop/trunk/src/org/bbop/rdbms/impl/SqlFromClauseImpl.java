package org.bbop.rdbms.impl;

import java.util.LinkedList;

import org.bbop.rdbms.FromClause;

public class SqlFromClauseImpl extends AbstractRelationalTerm implements
		FromClause {

	SqlFromClauseImpl() {
		super();
	}
	SqlFromClauseImpl(String s) {
		super();
		addRelation(s);
	}

	protected LinkedList<String> relations = new LinkedList<String>();
	
	public String toSQL() {
		if (relations.size() == 0) {
			return "";
		}
		return "FROM " + concat(",", relations);
	}

	public LinkedList<String> getRelations() {
		return relations;
	}

	public void setRelations(LinkedList<String> relations) {
		this.relations = relations;
	}
	
	public void addRelation(String relation) {
		getRelations().add(relation);
	}

}
