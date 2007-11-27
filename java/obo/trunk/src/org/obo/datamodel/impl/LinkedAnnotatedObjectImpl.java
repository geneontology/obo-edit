package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import java.util.*;

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

			Iterator it;

			out.children = createCollection();
			it = getChildren().iterator();
			while (it.hasNext()) {
				Link tr = (Link) it.next();
				out.atomicAddChild((Link) tr.clone());
			}

			out.parents = createCollection();
			it = getParents().iterator();
			while (it.hasNext()) {
				Link tr = (Link) it.next();
				out.atomicAddParent((Link) tr.clone());
			}

			return out;
		} catch (Exception ex) {
			return null;
		}
	}
}
