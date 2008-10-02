package org.obo.datamodel;

import java.util.*;
import java.io.Serializable;

public interface LinkDatabase extends IdentifiedObjectIndex, Serializable, Cloneable {

	public Collection<Link> getChildren(LinkedObject lo);

	public Collection<Link> getParents(LinkedObject lo);

	public Collection<Link> getLinks(OBOProperty p);
	
	/**
	 * Whether {@link LinkedObject} a has a relationship of type prop to
	 * {@link LinkedObject} b.
	 * 
	 * @param a
	 * @param prop
	 * @param b
	 * @return
	 */
	public Link hasRelationship(LinkedObject a, OBOProperty prop,
			LinkedObject b);

	public Collection<IdentifiedObject> getObjects();
	
	public boolean hasChildren(LinkedObject lo);
	
	public boolean hasParents(LinkedObject lo);
	
	public Collection<OBOProperty> getProperties();
	
	public OBOSession getSession();
	
	
}
