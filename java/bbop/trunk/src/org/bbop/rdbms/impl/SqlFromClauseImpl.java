package org.bbop.rdbms.impl;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

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

	public Map<String,String> getAliasMap() {
		Map<String,String> map = new HashMap<String,String>();
		for (String r :getRelations()) {
			if (r.toLowerCase().contains(" as ")) {
				String[] toks = r.toLowerCase().split(" +as +",2);
				map.put(toks[1], toks[0]);
			}
			else {
				map.put(r, r);
			}
		}
		return map;
	}
	
}
