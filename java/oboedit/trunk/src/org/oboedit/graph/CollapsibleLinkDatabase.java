package org.oboedit.graph;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.AbstractLinkDatabase;
import org.obo.util.TermUtil;
import org.oboedit.gui.event.ExpandCollapseListener;
import org.oboedit.gui.event.ExpansionEvent;

import java.util.*;

import javax.security.auth.Refreshable;

import org.apache.log4j.*;

public class CollapsibleLinkDatabase extends AbstractLinkDatabase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CollapsibleLinkDatabase.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -2787578809614867236L;

	public LinkDatabase linkDatabase;

	// protected Collection lastVisibleObjects = new LinkedList();

	protected Collection<IdentifiedObject> visibleObjects = new LinkedHashSet<IdentifiedObject>();

	protected Collection<IdentifiedObject> defaultVisibleObjects = new LinkedList<IdentifiedObject>();

	protected List<ExpandCollapseListener> listeners = new LinkedList<ExpandCollapseListener>();

	public CollapsibleLinkDatabase(LinkDatabase linkDatabase) {
		setLinkDatabase(linkDatabase);
	}

	public void setLinkDatabase(LinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
		defaultVisibleObjects.addAll(TermUtil.getRoots(linkDatabase));
		recache();
	}

	public void cleanupCache() {
		visibleObjects = refresh(visibleObjects);
		defaultVisibleObjects = refresh(defaultVisibleObjects);
	}

	protected Collection<IdentifiedObject> refresh(
			Collection<IdentifiedObject> ios) {
		Collection<IdentifiedObject> out = new LinkedHashSet<IdentifiedObject>();
		for (IdentifiedObject io : ios) {
			IdentifiedObject fetched = linkDatabase.getObject(io.getID());
			if (fetched != null)
				out.add(fetched);
		}
		return out;

	}

	public void addListener(ExpandCollapseListener listener) {
		listeners.add(listener);
	}

	public void removeListener(ExpandCollapseListener listener) {
		listeners.remove(listener);
	}

	public LinkDatabase getLinkDatabase() {
		return linkDatabase;
	}

	public void setDefaultVisibleObjects(
			Collection<? extends IdentifiedObject> c) {
		defaultVisibleObjects = new LinkedList<IdentifiedObject>(c);
	}

	/*
	 * public Collection getRedrawObjects() { return lastVisibleObjects; }
	 */
	public void recache() {
		visibleObjects.clear();
		visibleObjects.addAll(defaultVisibleObjects);
	}

	public Collection<IdentifiedObject> getObjects() {
		return visibleObjects;
	}

	public boolean isVisible(LinkedObject lo) {
		return visibleObjects.contains(lo);
	}

	public int getChildExpansionCount(LinkedObject lo) {
		int expansionCount = 0;
		Iterator it = linkDatabase.getChildren(lo).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isVisible(link.getChild()))
				expansionCount++;
		}
		return expansionCount;
	}

	public int getParentExpansionCount(LinkedObject lo) {
		int expansionCount = 0;
		Iterator it = linkDatabase.getParents(lo).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isVisible(link.getParent()))
				expansionCount++;
		}
		return expansionCount;
	}

	protected boolean isChildExpanded(LinkedObject lo) {
		Iterator it = linkDatabase.getChildren(lo).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isVisible(link.getChild()))
				return true;
		}
		return false;
	}

	protected boolean isParentExpanded(LinkedObject lo) {
		Iterator it = linkDatabase.getParents(lo).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isVisible(link.getParent()))
				return true;
		}
		return false;
	}

	public void setVisibleObjects(
			Collection<? extends IdentifiedObject> objects, boolean setDefault) {
		Collection<IdentifiedObject> oldVisible = new ArrayList<IdentifiedObject>(
				visibleObjects);
		Collection<IdentifiedObject> added = new ArrayList<IdentifiedObject>(
				objects);
		Collection<IdentifiedObject> deleted = new ArrayList<IdentifiedObject>(
				visibleObjects);
		added.removeAll(oldVisible);
		deleted.removeAll(objects);
		if (!(visibleObjects.size() == objects.size() && objects
				.containsAll(visibleObjects))) {
			if (setDefault) {
				setDefaultVisibleObjects(objects);
				recache();
				fireExpansionStateChanged(added, deleted);
			} else {
				setDefaultVisibleObjects(Collections.EMPTY_SET);
				recache();
				visibleObjects.addAll(objects);
				fireExpansionStateChanged(added, deleted);
			}
		}
	}

	protected void fireExpansionStateChanged(
			Collection<IdentifiedObject> shown,
			Collection<IdentifiedObject> hidden) {
		ExpansionEvent e = null;
		int size = listeners.size();
		for (int i = 0; i < size && i < listeners.size(); i++) {
			ExpandCollapseListener listener = (ExpandCollapseListener) listeners
					.get(i);
			if (e == null) {
				e = new ExpansionEvent(this, shown, hidden);
			}
			listener.expandStateChanged(e);
		}
	}

	public boolean shouldBeTrimmed(Link link) {
		if (!TermUtil.isImplied(link))
			return false;
		Iterator it;

		// trim any links to parents that are redundant with
		// a GRANDPARENT link
		// if any trimmed link is a GIVEN link, it is redundant
		it = getParents(link.getChild(), true).iterator();
		// for each parent link
		while (it.hasNext()) {
			Link parentLink = (Link) it.next();

			if (parentLink.equals(link)) {
				continue;
			}
			if (!(parentLink.getType().equals(link.getType()) || parentLink
					.getType().equals(OBOProperty.IS_A)))
				continue;
			boolean sawType = parentLink.getType().equals(link.getType());

			Iterator it2 = getParents(parentLink.getParent(), true).iterator();

			// for each grandparent link accessible via the current
			// parent link...
			while (it2.hasNext()) {
				Link gpLink = (Link) it2.next();
				// see if the grandparent link has the same type
				// and parent as the current link. if it does,
				// the current link is redundant with the grandparent
				// link and should be removed

				if ((((!sawType || link.getType().isTransitive()) && link
						.getType().equals(gpLink.getType())) || (sawType && gpLink
						.getType().equals(OBOProperty.IS_A)))
						&& link.getParent().equals(gpLink.getParent())) {

					return true;
				}
			}

			// add a section where we trim links that have a sibling link
			// with the same parent, but a more specific type,
			// than the current link
		}

		return false;
	}

	public Collection<Link> getChildren(LinkedObject lo) {
		return getChildren(lo, false);
	}

	public Collection<Link> getChildren(LinkedObject lo, boolean ignoreTrimming) {
		Set children = new HashSet();
		Iterator it = linkDatabase.getChildren(lo).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isVisible(link.getChild())
					&& (ignoreTrimming || !shouldBeTrimmed(link)))
				children.add(link);
		}
		return children;
	}

	public Collection<Link> getParents(LinkedObject lo) {
		return getParents(lo, false);
	}

	public Collection<Link> getParents(LinkedObject lo, boolean ignoreTrimming) {
		Set parents = new HashSet();
		Iterator it = linkDatabase.getParents(lo).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isVisible(link.getParent())
					&& (ignoreTrimming || !shouldBeTrimmed(link)))
				parents.add(link);
		}
		return parents;
	}

	public IdentifiedObject getObject(String id) {
		IdentifiedObject out = linkDatabase.getObject(id);
		if (visibleObjects.contains(out))
			return out;
		else
			return null;
	}

	public Collection<OBOProperty> getProperties() {
		return linkDatabase.getProperties(); // delegate
	}

	

}
