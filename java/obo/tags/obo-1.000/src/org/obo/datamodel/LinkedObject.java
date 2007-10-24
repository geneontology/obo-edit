package org.obo.datamodel;

import java.util.Collection;

public interface LinkedObject extends IdentifiedObject, PathCapable {

	public Collection<Link> getParents();

	public Collection<Link> getChildren();

	public void addParent(Link tr);

	public void addChild(Link tr);

	public void removeParent(Link tr);

	public void removeChild(Link tr);

	public void atomicAddParent(Link tr);

	public void atomicAddChild(Link tr);

	public void atomicRemoveParent(Link tr);

	public void atomicRemoveChild(Link tr);
}
