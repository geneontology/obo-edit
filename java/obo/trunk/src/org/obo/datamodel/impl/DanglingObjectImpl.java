package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import java.util.*;

//public class DanglingObjectImpl implements DanglingObject {
public class DanglingObjectImpl extends OBOClassImpl implements DanglingObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5373912147333845047L;
	protected Set<Link> parents = new HashSet<Link>();
	protected Set<Link> children = new HashSet<Link>();
	protected String id;
	protected String name;
	protected Namespace namespace;
	protected boolean root;

	public DanglingObjectImpl(String id) {
	    super(id);
		this.id = id;
		setName(id); // ?
	}

    public boolean isDangling() {
	return true;
    }

	public String getID() {
		return id;
	}
	
	public void setID(String id) {
		this.id = id;
	}

	public String getName() {

	    // Note: making the name be null caused a lot of exceptions (now
	    // fixed) in places where there are comparisons done on getName(),
	    // but Chris said we can't just give it a fake name (such as the ID).
		return null; // CJM - changed from id to null. Need to thoroughly test
	}

	public boolean isAnonymous() {
		return false;
	}

	public Type<OBOClass> getType() {
		return OBOClass.OBO_UNKNOWN;
	}

	public boolean isBuiltIn() {
		return false;
	}

	public Collection<Link> getParents() {
		return parents;
	}

	public Collection<Link> getChildren() {
		return children;
	}

	public boolean isRoot() {
		return root;
	}

	public boolean rootHint() {
		return false;
	}

	public void addChild(Link tr) {
		atomicAddChild(tr);
		if (tr.getChild() != null)
			tr.getChild().atomicAddParent(tr);
	}

	public void removeChild(Link tr) {
		atomicRemoveChild(tr);
		if (tr.getChild() != null)
			tr.getChild().atomicRemoveParent(tr);
	}

	public void addParent(Link tr) {
		atomicAddParent(tr);
		if (tr.getParent() != null)
			tr.getParent().atomicAddChild(tr);
	}

	public void removeParent(Link tr) {
		atomicRemoveParent(tr);
		if (tr.getParent() != null)
			tr.getParent().atomicRemoveChild(tr);
	}

	public void atomicAddChild(Link tr) {
		children.add(tr);
	}

	public void atomicRemoveChild(Link tr) {
		children.remove(tr);
	}

	public void atomicAddParent(Link tr) {
		parents.add(tr);
	}

	public void atomicRemoveParent(Link tr) {
		parents.remove(tr);
	}

	public NestedValue getTypeExtension() {
		return null;
	}

	public NestedValue getNameExtension() {
		return null;
	}

	public NestedValue getNamespaceExtension() {
		return null;
	}

	public NestedValue getIDExtension() {
		return null;
	}

	public NestedValue getAnonymousExtension() {
		return null;
	}

	public Set getPropertyValues() {
		return Collections.EMPTY_SET;
	}

	public void setName(String name) {
		this.name = name;
	}


	public void setRoot(boolean root) {
		this.root = root;
		// throw new UnsupportedOperationException("Dangling objects can never
		// be roots");
	}

	public void setIsAnonymous(boolean isAnonymous) {
		throw new UnsupportedOperationException(
				"Dangling objects cannot be anonymous");
	}

	public void setTypeExtension(NestedValue value) {
		throw new UnsupportedOperationException(
				"Dangling objects can never have extension information");
	}

	public void setNameExtension(NestedValue value) {
		throw new UnsupportedOperationException(
				"Dangling objects can never have extension information");
	}

	public void setNamespaceExtension(NestedValue nv) {
		throw new UnsupportedOperationException(
				"Dangling objects can never have extension information");
	}

	public void setAnonymousExtension(NestedValue nv) {
		throw new UnsupportedOperationException(
				"Dangling objects can never have extension information");
	}

	public void setIDExtension(NestedValue nv) {
		throw new UnsupportedOperationException(
				"Dangling objects can never have extension information");
	}

	public void addPropertyValue(PropertyValue pv) {
		throw new UnsupportedOperationException(
				"Dangling objects have no editable property values");
	}

	public void removePropertyValue(PropertyValue pv) {
		throw new UnsupportedOperationException(
				"Dangling objects have no editable property values");
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (Exception ex) {
			return null;
		}
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof IdentifiedObject) {
			return ((IdentifiedObject) o).getID().equals(getID());
		} else
			return false;
	}

	@Override
	public int hashCode() {
		return getID().hashCode();
	}

	@Override
	public String toString() {
		return "/" + getID() + "\\";
	}

	// CJM: allowing get/set of namespaces partially violates the
	// point of DanglingObjects, but is necessary for certain scenarios;
	// i.e. loading in associations for which only IDs and namespaces are known.
	// overall the whole principle of DanglingObjects needs a rethink.
	// ideally we would have something like OWL where different things can
	// be said about an ID in different files, there is no binary dangling/not
	// dangling
	public Namespace getNamespace() {
		return namespace;
	}

	public void setNamespace(Namespace namespace) {
		this.namespace = namespace;
	}
}
