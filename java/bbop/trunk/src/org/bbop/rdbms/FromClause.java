package org.bbop.rdbms;

import java.util.LinkedList;
import java.util.Map;

public interface FromClause extends RelationalTerm {

	public LinkedList<String> getRelations();
	public void setRelations(LinkedList<String> relations);
	
	public void addRelation(String relation);

	public Map<String,String> getAliasMap();

}
