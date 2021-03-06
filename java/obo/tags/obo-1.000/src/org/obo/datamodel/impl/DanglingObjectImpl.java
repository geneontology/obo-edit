package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import java.util.*;

public class DanglingObjectImpl implements DanglingObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5373912147333845047L;
	protected Set<Link> parents = new HashSet<Link>();
	protected Set<Link> children = new HashSet<Link>();
	protected String id;
	protected String name;
	protected boolean root;

	public DanglingObjectImpl(String id) {
		this.id = id;
		setName(id);
	}

	public String getID() {
		return id;
	}
	
	public void setID(String id) {
		this.id = id;
	}

	public String getName() {
		return name;
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

	public Namespace getNamespace() {
		return null;
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

	public void setNamespace(Namespace name) {
		throw new UnsupportedOperationException(
				"Dangling cannot be given names");
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
		} catch (CloneNotSupportedException ex) {
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
}
