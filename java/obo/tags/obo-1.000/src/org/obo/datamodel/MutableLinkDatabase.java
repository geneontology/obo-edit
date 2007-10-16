package org.obo.datamodel;

import java.util.Collection;

public interface MutableLinkDatabase extends LinkDatabase {

	public void addObject(IdentifiedObject lo);

	public void removeObject(IdentifiedObject lo);

	public void addParent(Link link);

	public void removeParent(Link link);
	
	/**
	 * Provides a mechanism for MutableLinkDatabases to hand off their indexing task to some
	 * other index.
	 * @param index
	 */
	public void setIdentifiedObjectIndex(IdentifiedObjectIndex index);

	public void clear();
	
	public void clearParents(LinkedObject lo);
	
	public void setParents(LinkedObject lo, Collection<Link> parents);
}
