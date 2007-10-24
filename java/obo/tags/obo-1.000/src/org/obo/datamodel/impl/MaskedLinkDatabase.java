package org.obo.datamodel.impl;

import java.util.*;

import org.bbop.util.Subset;
import org.bbop.util.VectorFilter;
import org.obo.datamodel.*;

public class MaskedLinkDatabase extends AbstractLinkDatabase {

	/**
	 * 
	 */
	private static final long serialVersionUID = 7258651844551493906L;
	protected LinkDatabase linkDatabase;
	protected Set<IdentifiedObject> visibleObjects = new HashSet<IdentifiedObject>();
	
	protected VectorFilter parentFilter = new VectorFilter() {
		public boolean satisfies(Object in) {
			return visibleObjects.contains(((Link) in).getParent());
		}
	};

	protected VectorFilter childFilter = new VectorFilter() {
		public boolean satisfies(Object in) {
			return visibleObjects.contains(((Link) in).getChild());
		}
	};
	
	public MaskedLinkDatabase() {
	}

	public MaskedLinkDatabase(LinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
	}

	public void setVisible(IdentifiedObject io, boolean visible) {
		if (visible)
			visibleObjects.add(io);
		else
			visibleObjects.remove(io);
	}
	
	public void setVisible(Collection<? extends IdentifiedObject> c, boolean visible) {
		if (visible)
			visibleObjects.addAll(c);
		else
			visibleObjects.removeAll(c);
	}

	public Collection<IdentifiedObject> getObjects() {
		return visibleObjects;
	}

	/*
	 * This method can be overridden by subclasses to repopulate the visible set
	 */
	public void recache() {
		visibleObjects.clear();
	}

	public Collection<Link> getParents(LinkedObject lo) {
		return new Subset(parentFilter, linkDatabase.getParents(lo));
	}

	public Collection<Link> getChildren(LinkedObject lo) {
		return new Subset(childFilter, linkDatabase.getChildren(lo));
	}

	public IdentifiedObject getObject(String id) {
		IdentifiedObject out = linkDatabase.getObject(id);
		if (visibleObjects.contains(out))
			return out;
		else
			return null;
	}
}
