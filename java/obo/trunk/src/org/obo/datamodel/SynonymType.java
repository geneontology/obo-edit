package org.obo.datamodel;

/*
 * Type of synonym for a term. Synonym types may
 * specify an optional  scope. If a scope is specified for a
 * synonym type, that scope will override the assigned scope of any
 * synonym that belongs to the type.
 *
 * @see Synonym#getSynonymCategory()
 * @see SynonymCategoryHistoryItem
 */

public interface SynonymType extends Cloneable {

	public String getID();

	public String getName();

	public int getScope();

	public void setID(String id);

	public void setName(String name);

	public void setScope(int scope);

	public Object clone();
}
