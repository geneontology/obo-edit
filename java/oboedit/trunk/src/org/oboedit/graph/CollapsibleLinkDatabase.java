package org.oboedit.graph;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.AbstractLinkDatabase;
import org.obo.util.TermUtil;

import java.util.*;

public class CollapsibleLinkDatabase extends AbstractLinkDatabase {

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
		Iterator<IdentifiedObject> it = visibleObjects.iterator();
		while(it.hasNext()) {
			IdentifiedObject io = it.next();
			if (!linkDatabase.getObjects().contains(io))
				it.remove();
		}
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

	public void setDefaultVisibleObjects(Collection<? extends IdentifiedObject> c) {
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

	public void setParentSetVisible(Set objects, boolean visible) {
		// cacheVisibleObjects();
		if (visible) {
			Iterator it = objects.iterator();
			while (it.hasNext()) {
				LinkedObject lo = (LinkedObject) it.next();
				doSetVisible(lo, true);
			}
		} else {
			Set keepers = new HashSet();
			Iterator it;
			it = objects.iterator();
			// ignore everything that isn't currently visible
			while (it.hasNext()) {
				LinkedObject lo = (LinkedObject) it.next();
				if (!isVisible(lo)) {
					it.remove();
				}
			}
			it = objects.iterator();
			while (it.hasNext()) {
				LinkedObject lo = (LinkedObject) it.next();
				Iterator it2 = getChildren(lo).iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					if (!objects.contains(link.getChild())) {
						keepers.add(link.getChild());
						break;
					}
				}
			}
			it = objects.iterator();
			while (it.hasNext()) {
				LinkedObject lo = (LinkedObject) it.next();
				if (!keepers.contains(lo))
					doSetVisible(lo, false);
			}
		}
		fireExpansionStateChanged();
	}

	public void setChildSetVisible(Set objects, boolean visible) {
		// cacheVisibleObjects();
		if (visible) {
			Iterator it = objects.iterator();
			while (it.hasNext()) {
				LinkedObject lo = (LinkedObject) it.next();
				doSetVisible(lo, true);
			}
		} else {
			Set keepers = new HashSet();
			Iterator it;
			it = objects.iterator();
			// ignore everything that isn't currently visible
			while (it.hasNext()) {
				LinkedObject lo = (LinkedObject) it.next();
				if (!isVisible(lo)) {
					it.remove();
				}
			}
			it = objects.iterator();
			while (it.hasNext()) {
				LinkedObject lo = (LinkedObject) it.next();
				Iterator it2 = getParents(lo).iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					if (!objects.contains(link.getParent())) {
						keepers.add(link.getParent());
						break;
					}
				}
			}
			it = objects.iterator();
			while (it.hasNext()) {
				LinkedObject lo = (LinkedObject) it.next();
				if (!keepers.contains(lo))
					doSetVisible(lo, false);
			}
		}
		fireExpansionStateChanged();
	}

	public void setVisibleObjects(Collection<? extends IdentifiedObject> objects,
			boolean setDefault) {
		if (setDefault) {
			setDefaultVisibleObjects(objects);
			recache();
		} else {
			setDefaultVisibleObjects(Collections.EMPTY_SET);
			recache();
			visibleObjects.addAll(objects);
		}
		fireExpansionStateChanged();

	}

	public void setVisible(LinkedObject lo, boolean visible) {
		setVisible(lo, visible, true);
	}

	public void setVisible(LinkedObject lo, boolean visible, boolean fireEvents) {
		// cacheVisibleObjects();
		doSetVisible(lo, visible);
		if (fireEvents)
			fireExpansionStateChanged();
	}

	protected void doSetVisible(LinkedObject lo, boolean visible) {
		if (visible) {
			visibleObjects.add(lo);
			// lastVisibleObjects.add(lo);
		} else {
			visibleObjects.remove(lo);
		}
	}

	public boolean toggleParentExpand(LinkedObject lo) {
		int expansionCount = getParentExpansionCount(lo);
		if (expansionCount == TermUtil.getParentCount(linkDatabase, lo)) {
			setParentsExpanded(lo, false);
			return false;
		} else {
			setParentsExpanded(lo, true);
			return true;
		}
	}

	public boolean toggleChildExpand(LinkedObject lo) {
		int expansionCount = getChildExpansionCount(lo);
		if (expansionCount == TermUtil.getChildCount(linkDatabase, lo)) {
			setChildrenExpanded(lo, false);
			return false;
		} else {
			setChildrenExpanded(lo, true);
			return true;
		}
	}

	public void hideParent(LinkedObject lo, boolean cascade) {
		doHideParent(lo, cascade);
		fireExpansionStateChanged();
	}

	public void doHideParent(LinkedObject lo, boolean cascade) {
		doSetVisible(lo, false);
		if (cascade) {
			LinkedList hideem = new LinkedList();
			Iterator it = getParents(lo).iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				if (getChildren(link.getChild()).isEmpty())
					hideem.add(link.getChild());
			}
			it = hideem.iterator();
			while (it.hasNext()) {
				LinkedObject hideme = (LinkedObject) it.next();
				hideParent(hideme, true);
			}
		}
	}

	public void hideChild(LinkedObject lo, boolean cascade) {
		// cacheVisibleObjects();
		doHideChild(lo, new HashSet(), cascade);
		fireExpansionStateChanged();
	}

	protected void fireExpansionStateChanged() {
		ExpansionEvent e = null;
		int size = listeners.size();
		for (int i = 0; i < size && i < listeners.size(); i++) {
			ExpandCollapseListener listener = (ExpandCollapseListener) listeners
					.get(i);
			if (e == null) {
				e = new ExpansionEvent(this);
			}
			listener.expandStateChanged(e);
		}
	}

	protected void doHideChild(LinkedObject lo, Set seenem, boolean cascade) {
		if (seenem.contains(lo))
			return;
		seenem.add(lo);

		doSetVisible(lo, false);
		if (cascade) {
			LinkedList hideem = new LinkedList();
			Iterator it = getChildren(lo).iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				if (getParents(link.getChild()).isEmpty())
					hideem.add(link.getChild());
			}
			it = hideem.iterator();
			while (it.hasNext()) {
				LinkedObject hideme = (LinkedObject) it.next();
				doHideChild(hideme, seenem, true);
			}
		}
	}

	public void expandChildren(LinkedObject lo) {
		setChildrenExpanded(lo, true);
	}

	public void collapseChildren(LinkedObject lo) {
		setChildrenExpanded(lo, false);
	}

	public void setChildrenExpanded(LinkedObject lo, boolean expanded) {
		// cacheVisibleObjects();
		if (expanded) {
			Iterator it = linkDatabase.getChildren(lo).iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				doSetVisible(link.getChild(), true);
			}
		} else {
			Iterator it = getChildren(lo).iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				doHideChild(link.getChild(), new HashSet(), true);
			}
		}
		fireExpansionStateChanged();
	}

	/*
	 * protected void cacheVisibleObjects() { lastVisibleObjects.clear();
	 * lastVisibleObjects.addAll(visibleObjects); }
	 */
	public void setParentsExpanded(LinkedObject lo, boolean expanded) {
		// cacheVisibleObjects();
		if (expanded) {
			Iterator it = linkDatabase.getParents(lo).iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				doSetVisible(link.getParent(), true);
			}
		} else {
			Iterator it = getParents(lo).iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				hideParent(link.getParent(), true);
			}
		}
		fireExpansionStateChanged();
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
}
