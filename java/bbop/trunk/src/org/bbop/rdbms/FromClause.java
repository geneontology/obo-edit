package org.bbop.rdbms;

import java.util.LinkedList;

public interface FromClause extends RelationalTerm {

	public LinkedList<String> getRelations();
	public void setRelations(LinkedList<String> relations);
	
	public void addRelation(String relation);

}
