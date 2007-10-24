package org.obo.datamodel;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;

public class LinkLinkedObject implements LinkedObject {
	
	protected Link link;
	
	public LinkLinkedObject(Link link) {
		setLink(link);
	}
	
	public Object clone() {
		return new LinkLinkedObject(link);
	}
	
	public boolean equals(Object o) {
		if (o instanceof IdentifiedObject) {
			return getID().equals(((IdentifiedObject) o).getID());
		} else
			return false;
	}
	
	public int hashCode() {
		return link.hashCode();
	}
	
	public LinkLinkedObject() {
		
	}
	
	public Link getLink() {
		return link;
	}
	
	public void setLink(Link link) {
		this.link = link;
	}

	public void addChild(Link tr) {
		// do nothing
	}

	public void addParent(Link tr) {
		// do nothing
	}

	public void atomicAddChild(Link tr) {
		// do nothing
	}

	public void atomicAddParent(Link tr) {
		// do nothing
	}

	public void atomicRemoveChild(Link tr) {
		// do nothing
	}

	public void atomicRemoveParent(Link tr) {
		// do nothing
	}

	public Collection<Link> getChildren() {
		return Collections.emptySet();
	}

	public Collection<Link> getParents() {
		return Collections.emptySet();
	}

	public boolean isRoot() {
		return false;
	}

	public void removeChild(Link tr) {
		// do nothing
	}

	public void removeParent(Link tr) {
		// do nothing
	}

	public boolean rootHint() {
		return false;
	}

	public void setRoot(boolean root) {
		// do nothing
	}

	public void addPropertyValue(PropertyValue pv) {
		// do nothing
	}

	public NestedValue getAnonymousExtension() {
		return null;
	}

	public NestedValue getIDExtension() {
		return null;
	}

	public String getName() {
		return getID();
	}

	public NestedValue getNameExtension() {
		return null;
	}

	public Set getPropertyValues() {
		return null;
	}

	public Type getType() {
		return OBOClass.OBO_LINK;
	}

	public NestedValue getTypeExtension() {
		return null;
	}

	public boolean isAnonymous() {
		return false;
	}

	public boolean isBuiltIn() {
		return false;
	}

	public void removePropertyValue(PropertyValue pv) {
		// do nothing
	}

	public void setAnonymousExtension(NestedValue nv) {
		// do nothing
	}

	public void setIDExtension(NestedValue nv) {
		// do nothing
	}

	public void setIsAnonymous(boolean isAnonymous) {
		// do nothing
	}

	public void setName(String name) {
		// do nothing
	}

	public void setNameExtension(NestedValue nv) {
		// do nothing
	}

	public void setTypeExtension(NestedValue value) {
		// do nothing
	}

	public String getID() {
		return link.getID();
	}

	public Namespace getNamespace() {
		return null;
	}

	public NestedValue getNamespaceExtension() {
		return null;
	}

	public void setNamespace(Namespace namespace) {
		// do nothing
	}

	public void setNamespaceExtension(NestedValue nv) {
		// do nothing
	}

	public String toString() {
		return link.toString();
	}
}
