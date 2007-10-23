package org.obo.datamodel;

/*
 * A category to which a term synonym may belong. Synonym categories may
 * specify an optional category scope. If a scope is specified in a
 * synonym category, that scope will override the assigned scope of any
 * synonym that belongs to the category.
 *
 * @see Synonym#getSynonymCategory()
 * @see SynonymCategoryHistoryItem
 */

public interface SynonymCategory extends Cloneable {

	public String getID();

	public String getName();

	public int getScope();

	public void setID(String id);

	public void setName(String name);

	public void setScope(int scope);

	public Object clone();
}
