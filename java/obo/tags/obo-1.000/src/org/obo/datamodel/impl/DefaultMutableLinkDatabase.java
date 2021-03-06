package org.obo.datamodel.impl;

import java.util.*;

import org.bbop.util.FastSuperset;
import org.obo.datamodel.*;

public class DefaultMutableLinkDatabase extends AbstractLinkDatabase implements
		MutableLinkDatabase {

	/**
	 * 
	 */
	private static final long serialVersionUID = 3770486191021916503L;
	protected Map<LinkedObject, Collection<Link>> parentLinkMap = new LinkedHashMap<LinkedObject, Collection<Link>>(
			100, .75f, true);
	protected Map<LinkedObject, Collection<Link>> childLinkMap;
	protected Map<String, IdentifiedObject> objects;
	protected IdentifiedObjectIndex index;
	protected boolean returnNulls;

	public DefaultMutableLinkDatabase() {
		this(false, false);
	}

	public void setIdentifiedObjectIndex(IdentifiedObjectIndex index) {
		this.index = index;
	}

	public DefaultMutableLinkDatabase(boolean cacheChildren) {
		this(cacheChildren, false);
	}

	public DefaultMutableLinkDatabase(boolean cacheChildren, boolean returnNulls) {
		if (cacheChildren)
			childLinkMap = new HashMap<LinkedObject, Collection<Link>>();
		this.returnNulls = returnNulls;
	}

	public Collection<IdentifiedObject> getObjects() {
		if (objects == null)
			return Collections.emptySet();
		else
			return objects.values();
	}

	public Collection<Link> getParents(LinkedObject lo) {
		Collection<Link> s = parentLinkMap.get(lo);
		if (s == null && !returnNulls)
			s = Collections.emptySet();
		return s;
	}

	public Collection<Link> getChildren(LinkedObject lo) {
		if (childLinkMap != null) {
			Collection<Link> s = childLinkMap.get(lo);
			if (s == null && !returnNulls)
				s = Collections.emptySet();
			return s;
		} else {
			Collection<Link> s = new HashSet<Link>();
			Iterator<Collection<Link>> it = parentLinkMap.values().iterator();
			while (it.hasNext()) {
				Collection<Link> parents = it.next();
				for (Link link : parents) {
					if (link.getParent().equals(lo))
						s.add(link);
				}
			}
			return s;
		}
	}

	public void addParent(Link link) {
		Collection<Link> s = parentLinkMap.get(link.getChild());
		if (s == null) {
			s = new HashSet<Link>();
			parentLinkMap.put(link.getChild(), s);
		}
		s.add(link);
		addObject(link.getParent());
		addObject(link.getChild());
		if (childLinkMap != null) {
			Collection<Link> children = childLinkMap.get(link.getParent());
			if (children == null) {
				children = new HashSet<Link>();
				childLinkMap.put(link.getParent(), children);
			}
			children.add(link);
		}
	}

	public void removeParent(Link link) {
		Collection<Link> s = parentLinkMap.get(link.getChild());
		if (s != null) {
			s.remove(link);
			if (s.size() == 0)
				parentLinkMap.remove(link.getChild());
		}
		if (childLinkMap != null) {
			Collection<Link> children = childLinkMap.get(link.getParent());
			if (children != null) {
				children.remove(link);
				if (children.size() == 0)
					childLinkMap.remove(link.getParent());
			}
		}
	}

	public void setParents(LinkedObject lo, Collection<Link> parents) {
		clearParents(lo);
		for (Link link : parents)
			addParent(link);
	}

	public void clearParents(LinkedObject lo) {
		Collection<Link> parents = new LinkedList<Link>();
		parents.addAll(getParents(lo));
		for (Link link : parents) {
			removeParent(link);
		}
	}

	public void clear() {
		parentLinkMap.clear();
		if (childLinkMap != null)
			childLinkMap.clear();
	}

	public void addObject(IdentifiedObject lo) {
		if (objects == null) {
			objects = new HashMap<String, IdentifiedObject>();
		}
		objects.put(lo.getID(), lo);
	}

	public LinkedObject getLeastRecentlyAccessedLinkKey() {
		if (parentLinkMap.size() == 0)
			return null;
		return parentLinkMap.entrySet().iterator().next().getKey();
	}

	public void dropLinkKey(LinkedObject lo) {
		parentLinkMap.remove(lo);
		if (childLinkMap != null)
			childLinkMap.remove(lo);
	}

	public void removeObject(IdentifiedObject lo) {
		if (objects != null)
			objects.remove(lo.getID());
	}

	public IdentifiedObject getObject(String id) {
		if (index != null)
			return index.getObject(id);
		else if (objects != null)
			return (IdentifiedObject) objects.get(id);
		else
			return null;
	}
}
