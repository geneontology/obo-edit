package org.obo.datamodel.impl;

import java.util.*;

import org.bbop.util.FastSuperset;
import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DefaultMutableLinkDatabase extends AbstractLinkDatabase implements
MutableLinkDatabase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultMutableLinkDatabase.class);


	/**
	 * 
	 */
	private static final long serialVersionUID = 3770486191021916503L;
	protected Map<LinkedObject, Collection<Link>> parentLinkMap = new LinkedHashMap<LinkedObject, Collection<Link>>(
			100, .75f, true);
	protected Map<LinkedObject, Collection<Link>> childLinkMap;
	protected Map<OBOProperty, Collection<Link>> propertyLinkMap;
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
		propertyLinkMap = new HashMap<OBOProperty, Collection<Link>>();

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
		if (propertyLinkMap != null) {
			OBOProperty prop = link.getType();
			if (!propertyLinkMap.containsKey(prop)) {
				propertyLinkMap.put(prop, new HashSet<Link>());
			}
			propertyLinkMap.get(prop).add(link);
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
		if (propertyLinkMap != null) {
			OBOProperty prop = link.getType();
			if (prop == null) {
				logger.error("no property in link "+link);
			}
			else
				propertyLinkMap.get(prop).remove(link);
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
		if (propertyLinkMap != null)
			propertyLinkMap.clear();
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

	@Override
	public Collection<Link> getLinks(OBOProperty p) {
		return propertyLinkMap.get(p);
	}

	
	
	

}
