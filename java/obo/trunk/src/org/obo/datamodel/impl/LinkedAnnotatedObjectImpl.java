package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Iterator;

public abstract class LinkedAnnotatedObjectImpl extends AnnotatedObjectImpl
		implements OBOObject {

	protected Collection<Link> parents = null;

	protected Collection<Link> children = null;

	public LinkedAnnotatedObjectImpl(String id) {
		super(id);
	}

	public Collection<Link> getChildren() {
		if (children == null)
			return Collections.emptySet();
		else
			return children;
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
		tr.setParent(this);
		if (children == null)
			children = createCollection();
		children.add(tr);
	}

	protected Collection<Link> createCollection() {
		return new LinkedHashSet<Link>();
	}

	public void atomicRemoveChild(Link tr) {
		if (children != null) {
			children.remove(tr);
			if (children.size() == 0)
				children = null;
		}
	}

	public void atomicAddParent(Link tr) {
		tr.setChild(this);
		if (parents == null)
			parents = createCollection();
		parents.add(tr);
	}

	public void atomicRemoveParent(Link tr) {
		if (parents != null) {
			parents.remove(tr);
			if (parents.size() == 0)
				parents = null;
		}
	}

	public Collection<Link> getParents() {
		if (parents == null)
			return Collections.emptySet();
		else
			return parents;
	}

	@Override
	public Object clone() {
		try {
			LinkedAnnotatedObjectImpl out = (LinkedAnnotatedObjectImpl) super
					.clone();
			out.children = createCollection();
			for(Link tr : getChildren()){
				out.atomicAddChild((Link) tr.clone());
			}
			out.parents = createCollection();
			for(Link tr : getParents()){
				out.atomicAddParent((Link) tr.clone());
			}

			return out;
		} catch (Exception ex) {
			return null;
		}
	}
	/** Comparable */
	public int compareTo(Object in) {
		if (in instanceof OBOObject) {
			String namea = getName();
			String nameb = ((OBOObject) in).getName();
			if (namea == null && nameb == null)
			    return 0;
			if (namea == null)
			    return -1;
			if (nameb == null)
			    return 1;

			int cmp = getName().toUpperCase().compareTo(
				((OBOObject) in).getName().toUpperCase());
			if (cmp == 0)
				return getID().toUpperCase().compareTo(
					((OBOObject) in).getID().toUpperCase());
			else
				return cmp;
		} else {
			return toString().compareToIgnoreCase(in.toString());
		}
	}
}
