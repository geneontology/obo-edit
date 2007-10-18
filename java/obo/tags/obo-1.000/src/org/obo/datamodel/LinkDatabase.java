package org.obo.datamodel;

import java.util.*;
import java.io.Serializable;

public interface LinkDatabase extends IdentifiedObjectIndex, Serializable, Cloneable {

	public Collection<Link> getChildren(LinkedObject lo);

	public Collection<Link> getParents(LinkedObject lo);

	public Collection<IdentifiedObject> getObjects();
	
	public boolean hasChildren(LinkedObject lo);
	
	public boolean hasParents(LinkedObject lo);
}
